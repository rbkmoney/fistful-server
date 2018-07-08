%%%
%%% Destination machine
%%%

-module(ff_destination_machine).

%% API

-type id()          :: machinery:id().
-type timestamp()   :: machinery:timestamp().
-type ctx()         :: ff_ctx:ctx().
-type destination() :: ff_destination:destination().

-type activity() ::
    undefined      |
    authorize .

-type st()        :: #{
    activity      := activity(),
    destination   := destination(),
    ctx           := ctx(),
    times         => {timestamp(), timestamp()}
}.

-export_type([id/0]).

-export([create/3]).
-export([get/1]).

%% Accessors

-export([destination/1]).
-export([activity/1]).
-export([ctx/1]).
-export([created/1]).
-export([updated/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%%

-define(NS, 'ff/destination').

-type params() :: #{
    identity := ff_identity_machine:id(),
    name     := binary(),
    currency := ff_currency:id(),
    resource := ff_destination:resource()
}.

-spec create(id(), params(), ctx()) ->
    ok |
    {error,
        {identity, notfound} |
        {currency, notfound} |
        _DestinationError |
        exists
    }.

create(ID, #{identity := IdentityID, name := Name, currency := Currency, resource := Resource}, Ctx) ->
    do(fun () ->
        Identity = ff_identity_machine:identity(unwrap(identity, ff_identity_machine:get(IdentityID))),
        _        = unwrap(currency, ff_currency:get(Currency)),
        Events   = unwrap(ff_destination:create(ID, Identity, Name, Currency, Resource)),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, st()}        |
    {error, notfound} .

get(ID) ->
    do(fun () ->
        collapse(unwrap(machinery:get(?NS, ID, backend())))
    end).

backend() ->
    fistful:backend(?NS).

%% Accessors

-spec destination(st()) -> destination().
-spec activity(st())    -> activity().
-spec ctx(st())         -> ctx().
-spec created(st())     -> timestamp() | undefined.
-spec updated(st())     -> timestamp() | undefined.

destination(#{destination := V}) -> V.
activity(#{activity := V})       -> V.
ctx(#{ctx := V})                 -> V.
created(St)                      -> erlang:element(1, times(St)).
updated(St)                      -> erlang:element(2, times(St)).

times(St) ->
    genlib_map:get(times, St, {undefined, undefined}).

%% Machinery

-type ts_ev(T) ::
    {ev, timestamp(), T}.

-type ev() ::
    ff_destination:ev().

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
        action    => continue,
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

process_timeout(Machine, _, _Opts) ->
    process_timeout(collapse(Machine)).

process_timeout(#{activity := authorize} = St) ->
    D0 = destination(St),
    case ff_destination:authorize(D0) of
        {ok, Events} ->
            #{
                events => emit_ts_events(Events)
            }
    end.

-spec process_call(_CallArgs, machine(), _, handler_opts()) ->
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

merge_event_body(Ev, St) ->
    Destination = genlib_map:get(destination, St),
    St#{
        activity    => deduce_activity(Ev),
        destination => ff_destination:apply_event(ff_destination:hydrate(Ev, Destination), Destination)
    }.

deduce_activity({created, _}) ->
    undefined;
deduce_activity({wallet, _}) ->
    undefined;
deduce_activity({status_changed, unauthorized}) ->
    authorize;
deduce_activity({status_changed, authorized}) ->
    undefined.

%%

emit_ts_events(Es) ->
    emit_ts_events(Es, machinery_time:now()).

emit_ts_events(Es, Ts) ->
    [{ev, Ts, ff_destination:dehydrate(Body)} || Body <- Es].

merge_ts_event({ev, Ts, Body}, St = #{times := {Created, _Updated}}) ->
    {Body, St#{times => {Created, Ts}}};
merge_ts_event({ev, Ts, Body}, St = #{}) ->
    {Body, St#{times => {Ts, Ts}}}.
