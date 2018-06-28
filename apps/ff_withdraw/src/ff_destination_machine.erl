%%%
%%% Destination machine
%%%

-module(ff_destination_machine).

%% API

-type id()          :: machinery:id().
-type timestamp()   :: machinery:timestamp().
-type ctx()         :: ff_ctx:ctx().

-type destination()        :: ff_destination:destination().
-type destination_status() :: ff_destination:status().

-type activity() ::
    idle      |
    authorize .

-type st()        :: #{
    activity      := activity(),
    destination   := destination(),
    times         => {timestamp(), timestamp()},
    ctx           => ctx()
}.

-export_type([id/0]).

-export([create/3]).
-export([get/1]).
-export([get_state/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%%

-define(NS, destination).

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
        Identity    = unwrap(identity, ff_identity_machine:get(IdentityID)),
        _           = unwrap(currency, ff_currency:get(Currency)),
        Destination = unwrap(ff_destination:create(ID, Identity, Name, Currency, Resource)),
        unwrap(machinery:start(?NS, ID, {Destination, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, destination()} |
    {error, notfound}.

get(ID) ->
    do(fun () ->
        destination(do_get_state(ID))
    end).

-spec get_state(id()) ->
    {ok, st()}    |
    {error, notfound} .

get_state(ID) ->
    do(fun () ->
        do_get_state(ID)
    end).

do_get_state(ID) ->
    collapse(unwrap(machinery:get(?NS, ID, backend()))).

backend() ->
    ff_withdraw:backend(?NS).

%% Machinery

-type ev() ::
    {created, destination()} |
    {status_changed, destination_status()}.

-type auxst() ::
    #{ctx => ctx()}.

-type machine()      :: machinery:machine(ev(), auxst()).
-type result()       :: machinery:result(ev(), auxst()).
-type handler_opts() :: machinery:handler_opts(_).

-spec init({destination(), ctx()}, machine(), _, handler_opts()) ->
    result().

init({Destination, Ctx}, #{}, _, _Opts) ->
    #{
        events    => emit_ts_event({created, Destination}),
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
        {ok, D1} ->
            #{
                events => emit_ts_event({status_changed, ff_destination:status(D1)})
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

merge_event_body({created, Destination}, St) ->
    St#{
        activity    => authorize,
        destination => Destination
    };
merge_event_body({status_changed, Status}, St) ->
    St#{
        activity    := idle,
        destination := ff_destination:set_status(Status, destination(St))
    }.

destination(#{destination := V}) ->
    V.

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
