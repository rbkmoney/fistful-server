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

-export([create/4]).
-export([get/2]).
-export([ctx/2]).

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

-type backend() :: machinery:backend(_).

-type params() :: #{
    party    := ff_party:id(),
    provider := ff_provider:id(),
    class    := ff_identity:class_id()
}.

-spec create(id(), params(), ctx(), backend()) ->
    {ok, st()} |
    {error,
        {provider, notfound} |
        {identity_class, notfound} |
        _SetupContractError |
        exists
    }.

create(ID, #{party := Party, provider := ProviderID, class := IdentityClassID}, Ctx, Be) ->
    do(fun () ->
        Provider        = unwrap(provider, ff_provider:get(ProviderID)),
        IdentityClass   = unwrap(identity_class, ff_provider:get_identity_class(IdentityClassID, Provider)),
        Identity0       = unwrap(ff_identity:create(Party, Provider, IdentityClass)),
        Identity1       = unwrap(ff_identity:setup_contract(Identity0)),
        ok              = unwrap(machinery:start(?NS, ID, {Identity1, Ctx}, Be)),
        unwrap(get(ID, Be))
    end).

-spec get(id(), backend()) ->
    {ok, identity()} |
    {error, notfound}.

get(ID, Be) ->
    do(fun () ->
        identity(collapse(unwrap(machinery:get(?NS, ID, Be))))
    end).

-spec ctx(id(), backend()) ->
    {ok, ctx()} |
    {error, notfound}.

ctx(ID, Be) ->
    do(fun () ->
        ctx(collapse(unwrap(machinery:get(?NS, ID, {undefined, 0, forward}, Be))))
    end).

%% Machinery

-type ev() ::
    {created, identity()}.

-type machine()      :: machinery:machine(ev()).
-type result()       :: machinery:result(ev()).
-type handler_opts() :: machinery:handler_opts().

-spec init({identity(), ctx()}, machine(), _, handler_opts()) ->
    result().

init({Identity, Ctx}, #{}, _, _Opts) ->
    #{
        events    => emit_ts_event({created, Identity}),
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

process_timeout(_Machine, _, _Opts) ->
    #{}.

-spec process_call(none(), machine(), _, handler_opts()) ->
    {_, result()}.

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
    }.

%%

emit_ts_event(E) ->
    emit_ts_events([E]).

emit_ts_events(Es) ->
    emit_ts_events(Es, machinery_time:now()).

emit_ts_events(Es, Ts) ->
    [{ev, Ts, Body} || Body <- Es].

merge_ts_event({ev, Ts, Body}, St = #{times := {Created, _Updated}}) ->
    {Body, St#{times => {Created, Ts}}};
merge_ts_event({ev, Ts, Body}, St = #{}) ->
    {Body, St#{times => {Ts, Ts}}}.
