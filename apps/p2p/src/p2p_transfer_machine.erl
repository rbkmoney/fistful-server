%%%
%%% P2PTransfer machine
%%%

-module(p2p_transfer_machine).

-behaviour(machinery).

%% API

-type ref() :: machinery:ref().
-type id() :: machinery:id().
-type change() :: p2p_transfer:event().
-type event() :: {integer(), ff_machine:timestamped_event(change())}.
-type event_id() :: integer().
-type events() :: [{event_id(), ff_machine:timestamped_event(change())}].
-type event_range() :: {After :: non_neg_integer() | undefined, Limit :: non_neg_integer() | undefined}.
-type st() :: ff_machine:st(p2p_transfer()).
-type p2p_transfer() :: p2p_transfer:p2p_transfer_state().
-type external_id() :: id().
-type action() :: p2p_transfer:action().

-type params() :: p2p_transfer:params().
-type create_error() ::
    p2p_transfer:create_error()
    | exists.

-type start_adjustment_error() ::
    p2p_transfer:start_adjustment_error()
    | unknown_p2p_transfer_error().

-type unknown_p2p_transfer_error() ::
    {unknown_p2p_transfer, id()}.

-type repair_error() :: ff_repair:repair_error().
-type repair_response() :: ff_repair:repair_response().

-export_type([id/0]).
-export_type([st/0]).
-export_type([action/0]).
-export_type([change/0]).
-export_type([event/0]).
-export_type([events/0]).
-export_type([params/0]).
-export_type([p2p_transfer/0]).
-export_type([external_id/0]).
-export_type([create_error/0]).
-export_type([start_adjustment_error/0]).
-export_type([repair_error/0]).
-export_type([repair_response/0]).

%% API

-export([create/2]).
-export([get/1]).
-export([get/2]).
-export([events/2]).

-export([start_adjustment/2]).
-export([repair/2]).

%% Accessors

-export([p2p_transfer/1]).
-export([ctx/1]).

%% Machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%% Internal types

-type ctx() :: ff_entity_context:context().

-type adjustment_params() :: p2p_transfer:adjustment_params().

-type call() ::
    {start_adjustment, adjustment_params()}.

-define(NS, 'ff/p2p_transfer_v1').

%% API

-spec create(params(), ctx()) ->
    ok
    | {error, p2p_transfer:create_error() | exists}.
create(Params, Ctx) ->
    do(fun() ->
        #{id := ID} = Params,
        Events = unwrap(p2p_transfer:create(Params)),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, st()}
    | {error, unknown_p2p_transfer_error()}.
get(ID) ->
    case ff_machine:get(p2p_transfer, ?NS, ID) of
        {ok, _Machine} = Result ->
            Result;
        {error, notfound} ->
            {error, {unknown_p2p_transfer, ID}}
    end.

-spec get(id(), event_range()) ->
    {ok, st()}
    | {error, unknown_p2p_transfer_error()}.
get(Ref, {After, Limit}) ->
    case ff_machine:get(p2p_transfer, ?NS, Ref, {After, Limit, forward}) of
        {ok, _Machine} = Result ->
            Result;
        {error, notfound} ->
            {error, {unknown_p2p_transfer, Ref}}
    end.

-spec events(id(), event_range()) ->
    {ok, events()}
    | {error, unknown_p2p_transfer_error()}.
events(ID, {After, Limit}) ->
    case ff_machine:history(p2p_transfer, ?NS, ID, {After, Limit, forward}) of
        {ok, History} ->
            {ok, [{EventID, TsEv} || {EventID, _, TsEv} <- History]};
        {error, notfound} ->
            {error, {unknown_p2p_transfer, ID}}
    end.

-spec start_adjustment(id(), adjustment_params()) ->
    ok
    | {error, start_adjustment_error()}.
start_adjustment(P2PTransferID, Params) ->
    call(P2PTransferID, {start_adjustment, Params}).

-spec repair(ref(), ff_repair:scenario()) ->
    {ok, repair_response()} | {error, notfound | working | {failed, repair_error()}}.
repair(Ref, Scenario) ->
    machinery:repair(?NS, Ref, Scenario, backend()).

%% Accessors

-spec p2p_transfer(st()) -> p2p_transfer().
p2p_transfer(St) ->
    ff_machine:model(St).

-spec ctx(st()) -> ctx().
ctx(St) ->
    ff_machine:ctx(St).

%% Machinery

-type machine() :: ff_machine:machine(change()).
-type result() :: ff_machine:result(change()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

-define(MAX_SESSION_POLL_TIMEOUT, 4 * 60 * 60).

backend() ->
    fistful:backend(?NS).

-spec init({[change()], ctx()}, machine(), handler_args(), handler_opts()) -> result().
init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events => ff_machine:emit_events(Events),
        action => continue,
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), handler_args(), handler_opts()) -> result().
process_timeout(Machine, _, _Opts) ->
    St = ff_machine:collapse(p2p_transfer, Machine),
    P2PTransfer = p2p_transfer(St),
    process_result(p2p_transfer:process_transfer(P2PTransfer), St).

-spec process_call(call(), machine(), handler_args(), handler_opts()) -> {Response, result()} | no_return() when
    Response :: ok | {error, p2p_transfer:start_adjustment_error()}.
process_call({start_adjustment, Params}, Machine, _, _Opts) ->
    do_start_adjustment(Params, Machine);
process_call(CallArgs, _Machine, _, _Opts) ->
    erlang:error({unexpected_call, CallArgs}).

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    {ok, {repair_response(), result()}} | {error, repair_error()}.
process_repair(Scenario, Machine, _Args, _Opts) ->
    ff_repair:apply_scenario(p2p_transfer, Machine, Scenario).

-spec do_start_adjustment(adjustment_params(), machine()) -> {Response, result()} when
    Response :: ok | {error, p2p_transfer:start_adjustment_error()}.
do_start_adjustment(Params, Machine) ->
    St = ff_machine:collapse(p2p_transfer, Machine),
    case p2p_transfer:start_adjustment(Params, p2p_transfer(St)) of
        {ok, Result} ->
            {ok, process_result(Result, St)};
        {error, _Reason} = Error ->
            {Error, #{}}
    end.

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
    MaxTimeout = genlib_app:env(p2p_transfer, max_session_poll_timeout, ?MAX_SESSION_POLL_TIMEOUT),
    Timeout0 = machinery_time:interval(Now, ff_machine:updated(St)) div 1000,
    erlang:min(MaxTimeout, erlang:max(1, Timeout0)).

call(ID, Call) ->
    case machinery:call(?NS, ID, Call, backend()) of
        {ok, Reply} ->
            Reply;
        {error, notfound} ->
            {error, {unknown_p2p_transfer, ID}}
    end.
