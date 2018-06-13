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

-export([create/6]).
-export([get/3]).
-export([ctx/3]).

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

-type namespace() :: machinery:namespace().
-type backend()   :: machinery:backend(_).

-spec create(namespace(), id(), ff_party:id(), ff_identity:prototype(), ctx(), backend()) ->
    {ok, st()} |
    {error,
        _IdentityError |
        exists
    }.

create(NS, ID, PartyID, Prototype, Ctx, Be) ->
    do(fun () ->
        Identity        = unwrap(ff_identity:create(PartyID, Prototype)),
        ProviderID      = ff_identity:provider(Identity),
        Provider        = unwrap(provider, ff_provider:get(ProviderID)),
        IdentityClassID = ff_identity:class(Identity),
        IdentityClass   = unwrap(identity_class, ff_provider:get_identity_class(IdentityClassID, Provider)),
        {ok, Contract}  = ff_party:create_contract(PartyID, #{
            payinst           => ff_provider:payinst(Provider),
            contract_template => ff_identity:contract_template(IdentityClass)
        }),
        Identity1       = unwrap(ff_identity:set_contract(Contract, Identity)),
        ok              = unwrap(machinery:start(NS, ID, {Identity1, Ctx}, Be)),
        unwrap(get(NS, ID, Be))
    end).

-spec get(namespace(), id(), backend()) ->
    {ok, identity()} |
    {error, notfound}.

get(NS, ID, Be) ->
    do(fun () ->
        identity(collapse(unwrap(machinery:get(NS, ID, Be))))
    end).

-spec ctx(namespace(), id(), backend()) ->
    {ok, ctx()} |
    {error, notfound}.

ctx(NS, ID, Be) ->
    do(fun () ->
        ctx(collapse(unwrap(machinery:get(NS, ID, {undefined, 0, forward}, Be))))
    end).

%% Machinery

-type ev() ::
    {created      , identity()}.

-type machine()      :: machinery:machine(ev()).
-type result()       :: machinery:result(ev()).
-type handler_opts() :: machinery:handler_opts().

-spec init({identity(), ctx()}, machine(), _, handler_opts()) ->
    result().

init({Identity, Ctx}, #{}, _, _Opts) ->
    #{
        events    => emit_ts_event({created, Identity}),
        action    => continue,
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
