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
-export([process_callback/1]).
-export([repair/2]).

%% machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

%%
%% Types
%%

-type process_callback_error() ::
    p2p_session:process_callback_error() |
    unknown_p2p_session_error().

-type unknown_p2p_session_error() ::
    {unknown_p2p_session, ref()}.

-export_type([process_callback_error/0]).

%%
%% Internal types
%%

-type ref() :: machinery:ref().
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
-type event_id() :: integer().
-type events() :: [{event_id(), ff_machine:timestamped_event(event())}].

-type callback_params() :: p2p_session:p2p_callback_params().
-type process_callback_response() :: p2p_session:process_callback_response().

-export_type([events/0]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%
%% API
%%

-spec get(ref()) ->
    {ok, st()}        |
    {error, unknown_p2p_session_error()}.

get(Ref) ->
    case ff_machine:get(p2p_session, ?NS, Ref) of
        {ok, _Machine} = Result ->
            Result;
        {error, notfound} ->
            {error, {unknown_p2p_session, Ref}}
    end.

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

-spec events(id(), machinery:range()) ->
    {ok, events()} |
    {error, unknown_p2p_session_error()}.

events(Ref, Range) ->
    case machinery:get(?NS, Ref, Range, backend()) of
        {ok, #{history := History}} ->
            Events = [{EventID, TsEv} || {EventID, _, TsEv} <- History],
            {ok, Events};
        {error, notfound} ->
            {error, {unknown_p2p_session, Ref}}
    end.

-spec process_callback(callback_params()) ->
    {ok, process_callback_response()} |
    {error, process_callback_error()}.

process_callback(#{tag := Tag} = Params) ->
    call({tag, Tag}, {process_callback, Params}).

-spec repair(ref(), ff_repair:scenario()) ->
    ok | {error, notfound | working}.
repair(Ref, Scenario) ->
    machinery:repair(?NS, Ref, Scenario, backend()).

%% machinery callbacks

-spec init([event()], machine(), handler_args(), handler_opts()) ->
    result().
init(Events, #{}, _, _Opts) ->
    Session = lists:foldl(fun (Ev, St) -> p2p_session:apply_event(Ev, St) end, undefined, Events),
    {InitEvents, NewAction} = p2p_session:init(Session, continue),
    genlib_map:compact(#{
        events => ff_machine:emit_events(Events ++ InitEvents),
        action => NewAction,
        aux_state => #{ctx => ff_entity_context:new()}
    }).

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

call(Ref, Call) ->
    case machinery:call(?NS, Ref, Call, backend()) of
        {ok, Reply} ->
            Reply;
        {error, notfound} ->
            {error, {unknown_p2p_session, Ref}}
    end.

-spec do_process_callback(callback_params(), machine()) -> {Response, result()} when
    Response ::
        {ok, process_callback_response()} |
        {error, p2p_session:process_callback_error()}.

do_process_callback(Params, Machine) ->
    St = ff_machine:collapse(p2p_session, Machine),
    case p2p_session:process_callback(Params, session(St)) of
        {ok, {Response, #{events := Events} = Result}} ->
            {{ok, Response}, Result#{events => ff_machine:emit_events(Events)}};
        {ok, {Response, Result}} ->
            {{ok, Response}, Result};
        {error, {Reason, Result}} ->
            {{error, Reason}, Result}
    end.
