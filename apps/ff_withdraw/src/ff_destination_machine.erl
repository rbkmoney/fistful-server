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

-export([create/7]).
-export([get/3]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-type namespace() :: machinery:namespace().
-type backend()   :: machinery:backend(_).

-spec create(namespace(), id(), ff_identity:id(), ff_wallet:prototype(), ff_destination:resource(), ctx(), backend()) ->
    {ok, st()} |
    {error,
        _DestinationError |
        exists
    }.

create(NS, ID, IdentityID, Prototype, Resource, Ctx, Be) ->
    do(fun () ->
        Destination = ff_destination:create(IdentityID, Prototype, Resource),
        ok = unwrap(machinery:start(NS, ID, {Destination, Ctx}, Be)),
        unwrap(get(NS, ID, Be))
    end).

-spec get(namespace(), id(), backend()) ->
    {ok, destination()} |
    {error, notfound}.

get(NS, ID, Be) ->
    do(fun () ->
        destination(collapse(unwrap(machinery:get(NS, ID, Be))))
    end).

%% Machinery

-type ev() ::
    {created, destination()} |
    {status_changed, destination_status()}.

-type machine()      :: machinery:machine(ev()).
-type result()       :: machinery:result(ev()).
-type handler_opts() :: machinery:handler_opts().

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
