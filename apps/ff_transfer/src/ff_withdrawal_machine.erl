%%%
%%% Withdrawal machine
%%%

-module(ff_withdrawal_machine).

-behaviour(machinery).

%% API

-type id() :: machinery:id().
-type event() :: ff_withdrawal:event().
-type events() :: [{integer(), ff_machine:timestamped_event(event())}].
-type st() :: ff_machine:st(withdrawal()).
-type withdrawal() :: ff_withdrawal:withdrawal().
-type external_id() :: id().
-type action() :: ff_withdrawal:action().

-type params() :: ff_withdrawal:params().
-type create_error() ::
    ff_withdrawal:create_error() |
    exists.

-export_type([id/0]).
-export_type([st/0]).
-export_type([action/0]).
-export_type([event/0]).
-export_type([events/0]).
-export_type([params/0]).
-export_type([withdrawal/0]).
-export_type([external_id/0]).
-export_type([create_error/0]).

%% API

-export([create/3]).
-export([get/1]).
-export([events/2]).

%% Accessors

-export([withdrawal/1]).
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

-define(NS, 'ff/withdrawal_v2').

%% API

-spec create(id(), params(), ctx()) ->
    ok |
    {error, ff_withdrawal:create_error() | exists}.

create(ID, Params, Ctx) ->
    do(fun () ->
        Events = unwrap(ff_withdrawal:create(ID, Params)),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, backend(?NS)))
    end).

-spec get(id()) ->
    {ok, st()} |
    {error, notfound}.

get(ID) ->
    ff_machine:get(ff_withdrawal, ?NS, ID).

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

-spec withdrawal(st()) ->
    withdrawal().

withdrawal(St) ->
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

-define(MAX_SESSION_POLL_TIMEOUT, 4 * 60 * 60).

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
    St = ff_machine:collapse(ff_withdrawal, Machine),
    Withdrawal = withdrawal(St),
    process_result(ff_withdrawal:process_transfer(Withdrawal), St).

-spec process_call(_CallArgs, machine(), handler_args(), handler_opts()) ->
    no_return().

process_call(CallArgs, _Machine, _, _Opts) ->
    erlang:error({unexpected_call, CallArgs}).

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    result().

process_repair(Scenario, Machine, _Args, _Opts) ->
    ff_repair:apply_scenario(ff_withdrawal, Machine, Scenario).

process_result({ok, {Action, Events}}, St) ->
    genlib_map:compact(#{
        events => set_events(Events),
        action => set_action(Action, St)
    });
process_result({error, Reason}, St) ->
    {ok, {Action, Events}} = ff_withdrawal:process_failure(Reason, withdrawal(St)),
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
