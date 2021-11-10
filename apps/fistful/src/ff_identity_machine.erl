%%%
%%% Identity machine
%%%
%%% TODOs
%%%
%%%  - I'm still not sure how to intertwine two concepts together which are:
%%%     * it's better to persist only IDs / prototypes,
%%%     * it's easier to work with rich data in runtime.
%%%
%%%    It seems that the more concise way will be to keep refdatas around which
%%%    are IDs until they are materialised into ID + Data tuple any time a model
%%%    updated with these IDs.
%%%
%%%  - We do not handle challenge expiration yet.
%%%

-module(ff_identity_machine).

%% API

-type id() :: machinery:id().
-type identity() :: ff_identity:identity_state().
-type ctx() :: ff_entity_context:context().
-type event_range() :: {After :: non_neg_integer() | undefined, Limit :: non_neg_integer() | undefined}.

-type st() :: ff_machine:st(identity()).

-type repair_error() :: ff_repair:repair_error().
-type repair_response() :: ff_repair:repair_response().

-export_type([id/0]).
-export_type([params/0]).
-export_type([repair_error/0]).
-export_type([repair_response/0]).

-export([create/2]).
-export([get/1]).
-export([get/2]).
-export([events/2]).

%% Accessors

-export([identity/1]).
-export([ctx/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

-define(NS, 'ff/identity').

-type params() :: ff_identity:params().

-spec create(params(), ctx()) ->
    ok
    | {error,
        ff_identity:create_error()
        | exists}.
create(Params = #{id := ID}, Ctx) ->
    do(fun() ->
        % ct:print("Identity[create] params=~p~n", [Params]),
        Events = unwrap(ff_identity:create(Params)),
        % ct:print("[ff_identity_machine:create] backend: ~p~n", [backend()]),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, st()}
    | {error, notfound}.
get(ID) ->
    ff_machine:get(ff_identity, ?NS, ID).

-spec get(id(), event_range()) ->
    {ok, st()}
    | {error, notfound}.
get(ID, {After, Limit}) ->
    ff_machine:get(ff_identity, ?NS, ID, {After, Limit, forward}).

-spec events(id(), event_range()) ->
    {ok, [{integer(), ff_machine:timestamped_event(event())}]}
    | {error, notfound}.
events(ID, {After, Limit}) ->
    do(fun() ->
        #{history := History} = unwrap(machinery:get(?NS, ID, {After, Limit, forward}, backend())),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

backend() ->
    fistful:backend(?NS).

%% Accessors

-spec identity(st()) -> identity().
identity(St) ->
    ff_machine:model(St).

-spec ctx(st()) -> ctx().
ctx(St) ->
    ff_machine:ctx(St).

%% Machinery

-type event() ::
    ff_identity:event().

-type machine() :: ff_machine:machine(event()).
-type result() :: ff_machine:result(event()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

-spec init({[event()], ctx()}, machine(), _, handler_opts()) -> result().
init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events => ff_machine:emit_events(Events),
        aux_state => #{ctx => Ctx}
    }.

%%

-spec process_timeout(machine(), _, handler_opts()) -> result().
process_timeout(_Machine, _, _Opts) ->
    undefined.

%%

-type call() :: term().

-spec process_call(call(), machine(), handler_args(), handler_opts()) ->
    {ok, result()}.
process_call(_Call, _Machine, _Args, _Opts) ->
    {ok, #{}}.

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    {ok, {repair_response(), result()}} | {error, repair_error()}.
process_repair(Scenario, Machine, _Args, _Opts) ->
    ff_repair:apply_scenario(ff_identity, Machine, Scenario).

%%
