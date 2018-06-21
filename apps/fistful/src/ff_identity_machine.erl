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

-module(ff_identity_machine).

%% API

-type id()        :: machinery:id().
-type identity()  :: ff_identity:identity().
-type timestamp() :: machinery:timestamp().
-type ctx()       :: ff_ctx:ctx().

-type activity() ::
    idle.

-type st()        :: #{
    activity      := activity(),
    identity      := identity(),
    times         => {timestamp(), timestamp()},
    ctx           => ctx()
}.

-export_type([id/0]).

-export([identity/1]).
-export([ctx/1]).

-export([create/3]).
-export([get/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%%

-spec identity(st()) -> identity().
-spec ctx(st())      -> ctx().

identity(#{identity := V}) -> V.
ctx(#{ctx := V})           -> V.

%%

-define(NS, identity).

-type params() :: #{
    party    := ff_party:id(),
    provider := ff_provider:id(),
    class    := ff_identity:class_id()
}.

-spec create(id(), params(), ctx()) ->
    ok |
    {error,
        {provider, notfound} |
        {identity_class, notfound} |
        _SetupContractError |
        exists
    }.

create(ID, #{party := Party, provider := ProviderID, class := IdentityClassID}, Ctx) ->
    do(fun () ->
        Provider      = unwrap(provider, ff_provider:get(ProviderID)),
        IdentityClass = unwrap(identity_class, ff_provider:get_identity_class(IdentityClassID, Provider)),
        Identity      = unwrap(ff_identity:create(Party, Provider, IdentityClass)),
        Events        = unwrap(ff_identity:setup_contract(Identity)),
        unwrap(machinery:start(?NS, ID, {[{created, Identity}] ++ Events, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, identity()} |
    {error, notfound}.

get(ID) ->
    do(fun () ->
        identity(collapse(unwrap(machinery:get(?NS, ID, backend()))))
    end).

backend() ->
    fistful:backend(?NS).

%% Machinery

-type ts_ev(T) ::
    {ev, timestamp(), T}.

-type ev() ::
    {created, identity()} |
    ff_identity:ev().

-type auxst() ::
    #{ctx => ctx()}.

-type machine()      :: machinery:machine(ts_ev(ev()), auxst()).
-type result()       :: machinery:result(ts_ev(ev()), auxst()).
-type handler_opts() :: machinery:handler_opts(_).

-spec init({[ev()], ctx()}, machine(), _, handler_opts()) ->
    result().

init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events    => emit_ts_events(Events),
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

process_timeout(_Machine, _, _Opts) ->
    #{}.

-spec process_call(_, machine(), _, handler_opts()) ->
    {ok, result()}.

process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.

%%

collapse(#{history := History, aux_state := #{ctx := Ctx}}) ->
    collapse_history(History, #{ctx => Ctx}).

collapse_history(History, St) ->
    lists:foldl(fun merge_event/2, St, History).

merge_event({_ID, _Ts, TsEv}, St0) ->
    {EvBody, St1} = merge_ts_event(TsEv, St0),
    merge_event_body(EvBody, St1).

merge_event_body({created, Identity}, St) ->
    St#{
        activity => idle,
        identity => Identity
    };
merge_event_body(IdentityEv, St = #{identity := Identity}) ->
    St#{identity := ff_identity:apply_event(IdentityEv, Identity)}.

%%

emit_ts_events(Es) ->
    emit_ts_events(Es, machinery_time:now()).

emit_ts_events(Es, Ts) ->
    [{ev, Ts, Body} || Body <- Es].

merge_ts_event({ev, Ts, Body}, St = #{times := {Created, _Updated}}) ->
    {Body, St#{times => {Created, Ts}}};
merge_ts_event({ev, Ts, Body}, St = #{}) ->
    {Body, St#{times => {Ts, Ts}}}.
