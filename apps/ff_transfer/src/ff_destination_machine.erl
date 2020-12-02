%%%
%%% Destination machine
%%%

-module(ff_destination_machine).

%% API

-type id() :: machinery:id().
-type ctx() :: ff_entity_context:context().
-type destination() :: ff_destination:destination_state().
-type change() :: ff_destination:event().
-type event() :: {integer(), ff_machine:timestamped_event(change())}.
-type events() :: [event()].
-type event_range() :: {After :: non_neg_integer() | undefined, Limit :: non_neg_integer() | undefined}.

-type params() :: ff_destination:params().
-type st() :: ff_machine:st(destination()).

-type repair_error() :: ff_repair:repair_error().
-type repair_response() :: ff_repair:repair_response().

-export_type([id/0]).
-export_type([st/0]).
-export_type([event/0]).
-export_type([repair_error/0]).
-export_type([repair_response/0]).
-export_type([params/0]).
-export_type([event_range/0]).

%% API

-export([create/2]).
-export([get/1]).
-export([get/2]).
-export([events/2]).

%% Accessors

-export([destination/1]).
-export([ctx/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%
-define(NS, 'ff/destination_v2').

-spec create(params(), ctx()) ->
    ok
    | {error, ff_destination:create_error() | exists}.
create(#{id := ID} = Params, Ctx) ->
    do(fun() ->
        Events = unwrap(ff_destination:create(Params)),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, st()}
    | {error, notfound}.
get(ID) ->
    ff_machine:get(ff_destination, ?NS, ID).

-spec get(id(), event_range()) ->
    {ok, st()}
    | {error, notfound}.
get(ID, {After, Limit}) ->
    ff_machine:get(ff_destination, ?NS, ID, {After, Limit, forward}).

-spec events(id(), event_range()) ->
    {ok, events()}
    | {error, notfound}.
events(ID, {After, Limit}) ->
    do(fun() ->
        History = unwrap(ff_machine:history(ff_destination, ?NS, ID, {After, Limit, forward})),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

%% Accessors

-spec destination(st()) -> destination().
destination(St) ->
    ff_machine:model(St).

-spec ctx(st()) -> ctx().
ctx(St) ->
    ff_machine:ctx(St).

%% Machinery

-type machine() :: ff_machine:machine(change()).
-type result() :: ff_machine:result(change()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

-spec init({[change()], ctx()}, machine(), _, handler_opts()) -> result().
init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events => ff_machine:emit_events(Events),
        action => continue,
        aux_state => #{ctx => Ctx}
    }.

%%

-spec process_timeout(machine(), handler_args(), handler_opts()) -> result().
process_timeout(Machine, _, _Opts) ->
    St = ff_machine:collapse(ff_destination, Machine),
    process_timeout(deduce_activity(ff_machine:model(St)), St).

process_timeout(authorize, St) ->
    D0 = destination(St),
    case ff_destination:authorize(D0) of
        {ok, Events} ->
            #{
                events => ff_machine:emit_events(Events)
            }
    end.

deduce_activity(#{status := unauthorized}) ->
    authorize;
deduce_activity(#{}) ->
    undefined.

%%

-spec process_call(_CallArgs, machine(), handler_args(), handler_opts()) -> {ok, result()}.
process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    {ok, {repair_response(), result()}} | {error, repair_error()}.
process_repair(Scenario, Machine, _Args, _Opts) ->
    ff_repair:apply_scenario(ff_destination, Machine, Scenario).

%% Internals

backend() ->
    fistful:backend(?NS).
