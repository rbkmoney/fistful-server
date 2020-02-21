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

-export([create/3]).
-export([get/1]).
-export([events/2]).
-export([repair/2]).

%% machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

%%
%% Types
%%

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
-type session() :: ff_withdrawal_session:session().
-type event() :: ff_withdrawal_session:event().


%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%
%% API
%%

-spec session(st()) -> session().

session(St) ->
    ff_machine:model(St).

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

-spec events(id(), machinery:range()) ->
    {ok, [{integer(), ff_machine:timestamped_event(event())}]} |
    {error, notfound}.

events(ID, Range) ->
    do(fun () ->
        History = unwrap(ff_machine:history(ff_withdrawal_session, ?NS, ID, Range)),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

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
    State = ff_machine:collapse(ff_withdrawal_session, Machine),
    #{events := Events} = Result = ff_withdrawal_session:process_session(session(State)),
    Result#{
        events => ff_machine:emit_events(Events)
    }.

-spec process_call(any(), machine(), handler_args(), handler_opts()) ->
    {ok, result()}.
process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    result().
process_repair(Scenario, Machine, _Args, _Opts) ->
    ScenarioProcessors = #{
        set_session_result => fun(Args, RMachine) ->
            State = ff_machine:collapse(ff_withdrawal_session, RMachine),
            ff_withdrawal_session:set_session_result(Args, session(State))
        end
    },
    ff_repair:apply_scenario(ff_withdrawal_session, Machine, Scenario, ScenarioProcessors).

%%
%% Internals
%%

backend() ->
    fistful:backend(?NS).
