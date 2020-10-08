%%%
%%% Withdrawal session machine
%%%
%%% TODOs
%%%
%%%  - The way we ask `fistful` for a machinery backend smells like a circular
%%%    dependency injection.
%%%  - Dehydrate events upon saving.
%%%

-module(ff_withdrawal_session_machine).
-behaviour(machinery).

-define(NS, 'ff/withdrawal/session_v2').

%% API

-export([session/1]).
-export([ctx/1]).

-export([create/3]).
-export([get/1]).
-export([get/2]).
-export([events/2]).
-export([repair/2]).
-export([process_callback/1]).

%% machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

%%
%% Types
%%

-type repair_error() :: ff_repair:repair_error().
-type repair_response() :: ff_repair:repair_response().

-export_type([repair_error/0]).
-export_type([repair_response/0]).

%%
%% Internal types
%%

-type id() :: machinery:id().
-type data() :: ff_withdrawal_session:data().
-type params() :: ff_withdrawal_session:params().

-type machine()      :: ff_machine:machine(event()).
-type result()       :: ff_machine:result(event()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

-type st()        :: ff_machine:st(session()).
-type session() :: ff_withdrawal_session:session_state().
-type event() :: ff_withdrawal_session:event().
-type action() :: ff_withdrawal_session:action().
-type event_range() :: {After :: non_neg_integer() | undefined, Limit :: non_neg_integer() | undefined}.

-type callback_params() :: ff_withdrawal_session:callback_params().
-type process_callback_response() :: ff_withdrawal_session:process_callback_response().
-type process_callback_error() ::
    {unknown_session, {tag, id()}} |
    ff_withdrawal_session:process_callback_error().

-type process_result() :: ff_withdrawal_session:process_result().

-type ctx() :: ff_entity_context:context().

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%
%% API
%%

-define(MAX_SESSION_NOTIFICATION_TIMEOUT, 4 * 60 * 60).

-spec session(st()) -> session().

session(St) ->
    ff_machine:model(St).

-spec ctx(st()) ->
    ctx().

ctx(St) ->
    ff_machine:ctx(St).

%%

-spec create(id(), data(), params()) ->
    ok | {error, exists}.
create(ID, Data, Params) ->
    do(fun () ->
        Events = unwrap(ff_withdrawal_session:create(ID, Data, Params)),
        unwrap(machinery:start(?NS, ID, Events, backend()))
    end).

-spec get(id()) ->
    {ok, st()}        |
    {error, notfound} .
get(ID) ->
    ff_machine:get(ff_withdrawal_session, ?NS, ID).

-spec get(id(), event_range()) ->
    {ok, st()} |
    {error, notfound}.

get(ID, {After, Limit}) ->
    ff_machine:get(ff_withdrawal_session, ?NS, ID, {After, Limit, forward}).

-spec events(id(), event_range()) ->
    {ok, [{integer(), ff_machine:timestamped_event(event())}]} |
    {error, notfound}.

events(ID, {After, Limit}) ->
    do(fun () ->
        History = unwrap(ff_machine:history(ff_withdrawal_session, ?NS, ID, {After, Limit, forward})),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

-spec repair(id(), ff_repair:scenario()) ->
    {ok, repair_response()} | {error, notfound | working | {failed, repair_error()}}.
repair(ID, Scenario) ->
    machinery:repair(?NS, ID, Scenario, backend()).

-spec process_callback(callback_params()) ->
    {ok, process_callback_response()} |
    {error, process_callback_error()}.

process_callback(#{tag := Tag} = Params) ->
    call({tag, Tag}, {process_callback, Params}).

%% machinery callbacks

-spec init([event()], machine(), handler_args(), handler_opts()) ->
    result().
init(Events, #{}, _, _Opts) ->
    #{
        events => ff_machine:emit_events(Events),
        action => continue,
        aux_state => #{ctx => ff_entity_context:new()}
    }.

-spec process_timeout(machine(), handler_args(), handler_opts()) ->
    result().
process_timeout(Machine, _, _Opts) ->
    State = ff_machine:collapse(ff_withdrawal_session, Machine),
    Session = session(State),
    process_result(ff_withdrawal_session:process_session(Session), State).

-spec process_call(any(), machine(), handler_args(), handler_opts()) ->
    {ok, result()}.
process_call({process_callback, Params}, Machine, _, _Opts) ->
    do_process_callback(Params, Machine);
process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    {ok, {repair_response(), result()}} | {error, repair_error()}.
process_repair(Scenario, Machine, _Args, _Opts) ->
    ScenarioProcessors = #{
        set_session_result => fun(Args, RMachine) ->
            State = ff_machine:collapse(ff_withdrawal_session, RMachine),
            {Action, Events} = ff_withdrawal_session:set_session_result(Args, session(State)),
             %@todo repair events are emitted in ff_repair, while calls and timeouts are emitted per handler. Unify?
            {ok, {ok, #{action => set_action(Action, State), events => Events}}}
        end
    },
    ff_repair:apply_scenario(ff_withdrawal_session, Machine, Scenario, ScenarioProcessors).

%%
%% Internals
%%

-spec process_result(process_result(), st()) ->
    result().
process_result({Action, Events}, St) ->
    genlib_map:compact(#{
        events => set_events(Events),
        action => set_action(Action, St)
    }).

-spec set_events([event()]) ->
    undefined | ff_machine:timestamped_event(event()).
set_events([]) ->
    undefined;
set_events(Events) ->
    ff_machine:emit_events(Events).

-spec set_action(action(), st()) ->
    undefined | machinery:action() | [machinery:action()].
set_action(continue, _St) ->
    continue;
set_action(undefined, _St) ->
    undefined;
set_action({setup_callback, Tag, Timer}, _St) ->
    [tag_action(Tag), timer_action(Timer)];
set_action({setup_timer, Timer}, _St) ->
    timer_action(Timer);
set_action(finish, _St) ->
    unset_timer;
set_action(retry, St) ->
    Now = machinery_time:now(),
    timer_action({timeout, compute_retry_timeout(Now, St)}).

compute_retry_timeout(Now, St) ->
    MaxTimeout = genlib_app:env(ff_transfer, max_session_notification_timeout, ?MAX_SESSION_NOTIFICATION_TIMEOUT),
    Timeout0 = machinery_time:interval(Now, ff_machine:updated(St)) div 1000,
    erlang:min(MaxTimeout, erlang:max(1, Timeout0)).

-spec timer_action(machinery:timer()) ->
    machinery:action().
timer_action(Timer) ->
    {set_timer, Timer}.

-spec tag_action(machinery:tag()) ->
    machinery:action().
tag_action(Tag) ->
    {tag, Tag}.

backend() ->
    fistful:backend(?NS).

call(Ref, Call) ->
    case machinery:call(?NS, Ref, Call, backend()) of
        {ok, Reply} ->
            Reply;
        {error, notfound} ->
            {error, {unknown_session, Ref}}
    end.

-spec do_process_callback(callback_params(), machine()) -> {Response, result()} when
    Response ::
        {ok, process_callback_response()} |
        {error, ff_withdrawal_session:process_callback_error()}.

do_process_callback(Params, Machine) ->
    St= ff_machine:collapse(ff_withdrawal_session, Machine),
    case ff_withdrawal_session:process_callback(Params, session(St)) of
        {ok, {Response, Result}} ->
            {{ok, Response}, process_result(Result, St)};
        {error, {Reason, _Result}} ->
            {{error, Reason}, #{}}
    end.
