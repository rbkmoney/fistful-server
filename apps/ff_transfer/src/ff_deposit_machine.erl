%%%
%%% Deposit machine
%%%

-module(ff_deposit_machine).

-behaviour(machinery).

%% API

-type id() :: machinery:id().
-type event() :: ff_deposit:event().
-type events() :: [{integer(), ff_machine:timestamped_event(event())}].
-type st() :: ff_machine:st(deposit()).
-type deposit() :: ff_deposit:deposit().
-type external_id() :: id().
-type action() :: poll | continue | undefined.

-type params() :: ff_deposit:params().
-type create_error() ::
    ff_deposit:create_error() |
    exists.

-export_type([id/0]).
-export_type([st/0]).
-export_type([action/0]).
-export_type([event/0]).
-export_type([events/0]).
-export_type([params/0]).
-export_type([deposit/0]).
-export_type([external_id/0]).
-export_type([create_error/0]).

%% API

-export([create/3]).
-export([get/1]).
-export([events/2]).

%% Accessors

-export([deposit/1]).
-export([ctx/1]).

%% Machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%% Internal types

-type ctx()           :: ff_ctx:ctx().

-define(NS, 'ff/deposit_v1').

%% API

-spec create(id(), params(), ctx()) ->
    ok |
    {error, ff_deposit:create_error() | exists}.

create(ID, Params, Ctx) ->
    do(fun () ->
        Events = unwrap(ff_deposit:create(ID, Params)),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, backend(?NS)))
    end).

-spec get(id()) ->
    {ok, st()} |
    {error, notfound}.

get(ID) ->
    ff_machine:get(ff_deposit, ?NS, ID).

-spec events(id(), machinery:range()) ->
    {ok, events()} |
    {error, notfound}.

events(ID, Range) ->
    do(fun () ->
        #{history := History} = unwrap(machinery:get(?NS, ID, Range, backend(?NS))),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

backend(NS) ->
    fistful:backend(NS).

%% Accessors

-spec deposit(st()) ->
    deposit().

deposit(St) ->
    ff_machine:model(St).

-spec ctx(st()) ->
    ctx().

ctx(St) ->
    ff_machine:ctx(St).

%% Machinery

-type machine()      :: ff_machine:machine(event()).
-type result()       :: ff_machine:result(event()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

-spec init({[event()], ctx()}, machine(), handler_args(), handler_opts()) ->
    result().

init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events    => ff_machine:emit_events(Events),
        action    => continue,
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), handler_args(), handler_opts()) ->
    result().

process_timeout(Machine, _, _Opts) ->
    St = ff_machine:collapse(ff_deposit, Machine),
    Deposit = deposit(St),
    process_result(ff_deposit:process_transfer(Deposit), St).

-spec process_call(_CallArgs, machine(), handler_args(), handler_opts()) ->
    no_return().

process_call(CallArgs, _Machine, _, _Opts) ->
    erlang:error({unexpected_call, CallArgs}).

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    result().

process_repair(Scenario, Machine, _Args, _Opts) ->
    ff_repair:apply_scenario(ff_deposit, Machine, Scenario).

process_result({ok, {Action, Events}}, _St) ->
    genlib_map:compact(#{
        events => set_events(Events),
        action => Action
    });
process_result({error, Reason}, St) ->
    {ok, {Action, Events}} = ff_deposit:process_failure(Reason, deposit(St)),
    genlib_map:compact(#{
        events => set_events(Events),
        action => Action
    }).

set_events([]) ->
    undefined;
set_events(Events) ->
    ff_machine:emit_events(Events).
