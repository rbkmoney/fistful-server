%%%
%%% P2P session machine
%%%

-module(p2p_session_machine).
-behaviour(machinery).

-define(NS, 'ff/p2p_transfer/session_v1').

%% API

-export([session/1]).

-export([create/3]).
-export([get/1]).
-export([events/2]).
-export([p2p_session/1]).
-export([process_callback/2]).
-export([repair/2]).

%% machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

-define(MAX_SESSION_POLL_TIMEOUT, 4 * 60 * 60).

%%
%% Types
%%

-type process_callback_error() ::
    p2p_session:process_callback_error() |
    unknown_p2p_session_error().

-type unknown_p2p_session_error() ::
    {unknown_p2p_session, id()}.

-export_type([process_callback_error/0]).

%%
%% Internal types
%%

-type id() :: machinery:id().
-type transfer_params() :: p2p_session:transfer_params().
-type params() :: p2p_session:params().

-type machine() :: ff_machine:machine(event()).
-type result() :: ff_machine:result(event()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

-type st() :: ff_machine:st(session()).
-type session() :: p2p_session:session().
-type event() :: p2p_session:event().

-type p2p_session() :: p2p_session:session().
-type callback_params() :: p2p_session:callback_params().
-type process_callback_response() :: p2p_session:process_callback_response().

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%
%% API
%%

-spec session(st()) -> session().

session(St) ->
    ff_machine:model(St).

%%

-spec create(id(), transfer_params(), params()) ->
    ok | {error, exists}.
create(ID, TransferParams, Params) ->
    do(fun () ->
        Events = unwrap(p2p_session:create(ID, TransferParams, Params)),
        unwrap(machinery:start(?NS, ID, Events, backend()))
    end).

-spec get(id()) ->
    {ok, st()}        |
    {error, notfound} .
get(ID) ->
    ff_machine:get(p2p_session, ?NS, ID).

-spec events(id(), machinery:range()) ->
    {ok, [{integer(), ff_machine:timestamped_event(event())}]} |
    {error, notfound}.

events(ID, Range) ->
    do(fun () ->
        #{history := History} = unwrap(machinery:get(?NS, ID, Range, backend())),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

-spec p2p_session(st()) ->
    p2p_session().

p2p_session(St) ->
    ff_machine:model(St).

-spec process_callback(id(), callback_params()) ->
    {ok, process_callback_response()} |
    {error, process_callback_error()}.

process_callback(SessionID, Params) ->
    call(SessionID, {process_callback, Params}).

-spec repair(id(), ff_repair:scenario()) ->
    ok | {error, notfound | working}.
repair(ID, Scenario) ->
    machinery:repair(?NS, ID, Scenario, backend()).

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
    State = ff_machine:collapse(p2p_session, Machine),
    #{events := Events} = Result = p2p_session:process_session(session(State)),
    Result#{
        events => ff_machine:emit_events(Events)
    }.

-spec process_call(any(), machine(), handler_args(), handler_opts()) ->
    {Response, result()} | no_return() when
    Response ::
        {ok, process_callback_response()} |
        {error, p2p_session:process_callback_error()}.

process_call({process_callback, Params}, Machine, _, _Opts) ->
    do_process_callback(Params, Machine);
process_call(CallArgs, _Machine, _, _Opts) ->
    erlang:error({unexpected_call, CallArgs}).

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    result().
process_repair(Scenario, Machine, _Args, _Opts) ->
    ScenarioProcessors = #{
        set_session_result => fun(Args, RMachine) ->
            State = ff_machine:collapse(p2p_session, RMachine),
            p2p_session:set_session_result(Args, session(State))
        end
    },
    ff_repair:apply_scenario(p2p_session, Machine, Scenario, ScenarioProcessors).

%%
%% Internals
%%

backend() ->
    fistful:backend(?NS).

process_result({Action, Events}, St) ->
    genlib_map:compact(#{
        events => set_events(Events),
        action => set_action(Action, St)
    }).

set_events([]) ->
    undefined;
set_events(Events) ->
    ff_machine:emit_events(Events).

set_action(continue, _St) ->
    continue;
set_action(undefined, _St) ->
    undefined;
set_action(poll, St) ->
    Now = machinery_time:now(),
    {set_timer, {timeout, compute_poll_timeout(Now, St)}}.

compute_poll_timeout(Now, St) ->
    MaxTimeout = genlib_app:env(ff_transfer, max_session_poll_timeout, ?MAX_SESSION_POLL_TIMEOUT),
    Timeout0 = machinery_time:interval(Now, ff_machine:updated(St)) div 1000,
    erlang:min(MaxTimeout, erlang:max(1, Timeout0)).

call(ID, Call) ->
    case machinery:call(?NS, ID, Call, backend()) of
        {ok, Reply} ->
            Reply;
        {error, notfound} ->
            {error, {unknown_p2p_session, ID}}
    end.

-spec do_process_callback(callback_params(), machine()) -> {Response, result()} when
    Response ::
        {ok, process_callback_response()} |
        {error, p2p_session:process_callback_error()}.

do_process_callback(Params, Machine) ->
    St = ff_machine:collapse(p2p_session, Machine),
    case p2p_session:process_callback(Params, p2p_session(St)) of
        {ok, {Response, Result}} ->
            {{ok, Response}, process_result(Result, St)};
        {error, _Reason} = Error ->
            {Error, #{}}
    end.
