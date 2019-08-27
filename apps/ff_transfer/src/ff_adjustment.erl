-module(ff_adjustment).

-define(ACTUAL_FORMAT_VERSION, 1).

-type id()       :: binary().

-opaque adjustment() :: #{
    version          := ?ACTUAL_FORMAT_VERSION,
    id               := id(),
    changes_plan     := changes(),
    status           := status(),
    external_id      => id(),
    p_transfer       => p_transfer() | undefined
}.

-type params() :: #{
    id            := id(),
    changes_plan  := changes(),
    external_id   => id()
}.

-type changes() :: #{
    new_cash_flow => cash_flow_change(),
    new_status    => target_status()
}.

-type cash_flow_change() :: #{
    old_cash_flow_inverted := final_cash_flow(),
    new_cash_flow := final_cash_flow()
}.

-type status() ::
    pending |
    succeeded.

-type event() ::
    {created, adjustment()} |
    {p_transfer, ff_postings_transfer:event()} |
    {status_changed, status()}.

-type create_error() :: none().

-export_type([id/0]).
-export_type([event/0]).
-export_type([changes/0]).
-export_type([status/0]).
-export_type([adjustment/0]).
-export_type([params/0]).
-export_type([create_error/0]).

%% Accessors

-export([id/1]).
-export([status/1]).
-export([changes_plan/1]).
-export([external_id/1]).
-export([p_transfer/1]).

%% API

-export([create/1]).
-export([is_active/1]).
-export([is_finished/1]).

%% Transfer logic callbacks

-export([process_transfer/1]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/1]).

%% Internal types

-type target_status()   :: term().
-type final_cash_flow() :: ff_cash_flow:final_cash_flow().
-type p_transfer()      :: ff_postings_transfer:transfer().
-type action()          :: machinery:action() | undefined.
-type process_result()  :: {action(), [event()]}.
-type legacy_event()    :: any().
-type external_id()     :: id().

-type activity() ::
    p_transfer_start |
    p_transfer_prepare |
    p_transfer_commit |
    finish.

%% Accessors

-spec id(adjustment()) -> id().
id(#{id := V}) ->
    V.

-spec status(adjustment()) -> status().
status(#{status := V}) ->
    V.

-spec changes_plan(adjustment()) -> changes().
changes_plan(#{changes_plan := V}) ->
    V.

-spec external_id(adjustment()) -> external_id() | undefined.
external_id(T) ->
    maps:get(external_id, T, undefined).

-spec p_transfer(adjustment()) -> p_transfer() | undefined.
p_transfer(T) ->
    maps:get(p_transfer, T, undefined).

%% API

-spec create(params()) ->
    {ok, process_result()}.

create(Params) ->
    #{
        id            := ID,
        changes_plan  := Changes
    } = Params,
    Adjustment = genlib_map:compact(#{
        version         => ?ACTUAL_FORMAT_VERSION,
        id              => ID,
        changes_plan    => Changes,
        status          => pending,
        external_id     => maps:get(external_id, Params, undefined)
    }),
    {ok, {continue, [{created, Adjustment}]}}.

%% Transfer logic callbacks

-spec process_transfer(adjustment()) ->
    process_result().
process_transfer(Adjustment) ->
    Activity = deduce_activity(Adjustment),
    do_process_transfer(Activity, Adjustment).

%% Сущность в настоящий момент нуждается в передаче ей управления для совершения каких-то действий
-spec is_active(adjustment()) -> boolean().
is_active(#{status := succeeded}) ->
    false;
is_active(#{status := pending}) ->
    true.

%% Сущность приняла статус, который не будет меняться без внешних воздействий.
-spec is_finished(adjustment()) -> boolean().
is_finished(#{status := succeeded}) ->
    true;
is_finished(#{status := pending}) ->
    false.

%% Events utils

-spec apply_event(event() | legacy_event(), adjustment() | undefined) ->
    adjustment().
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev), T).

-spec apply_event_(event(), adjustment() | undefined) ->
    adjustment().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, S}, T) ->
    update_status(S, T);
apply_event_({p_transfer, Ev}, T) ->
    T#{p_transfer => ff_postings_transfer:apply_event(Ev, p_transfer(T))}.

-spec maybe_migrate(event() | legacy_event()) ->
    event().
maybe_migrate(Ev) ->
    Ev.

%% Internals

-spec p_transfer_status(adjustment()) ->
    ff_postings_transfer:status() | undefined.
p_transfer_status(Adjustment) ->
    case p_transfer(Adjustment) of
        undefined ->
            undefined;
        #{status := Status} ->
            Status
    end.

-spec deduce_activity(adjustment()) ->
    activity().
deduce_activity(Adjustment) ->
    Params = #{
        p_transfer => p_transfer_status(Adjustment),
        status => status(Adjustment),
        flow_planned => is_cash_flow_change_planned(Adjustment)
    },
    do_deduce_activity(Params).

do_deduce_activity(#{status := pending, p_transfer := undefined, flow_planned := true}) ->
    p_transfer_start;
do_deduce_activity(#{status := pending, p_transfer := created}) ->
    p_transfer_prepare;
do_deduce_activity(#{status := pending, p_transfer := prepared}) ->
    p_transfer_commit;
do_deduce_activity(#{status := pending, p_transfer := committed}) ->
    finish;
do_deduce_activity(#{status := pending, flow_planned := false}) ->
    finish.

do_process_transfer(p_transfer_start, Adjustment) ->
    create_p_transfer(Adjustment);
do_process_transfer(p_transfer_prepare, Adjustment) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Adjustment, fun ff_postings_transfer:prepare/1),
    {continue, Events};
do_process_transfer(p_transfer_commit, Adjustment) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Adjustment, fun ff_postings_transfer:commit/1),
    {continue, Events};
do_process_transfer(finish, Adjustment) ->
    process_transfer_finish(Adjustment).

-spec create_p_transfer(adjustment()) ->
    process_result().
create_p_transfer(Adjustment) ->
    #{new_cash_flow := CashFlowChange} = changes_plan(Adjustment),
    #{
        old_cash_flow_inverted := Old,
        new_cash_flow := New
    } = CashFlowChange,
    {ok, FinalCashFlow} = ff_cash_flow:combine(Old, New),
    PTransferID = construct_p_transfer_id(id(Adjustment)),
    {ok, PostingsTransferEvents} = ff_postings_transfer:create(PTransferID, FinalCashFlow),
    {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}.

-spec process_transfer_finish(adjustment()) ->
    process_result().
process_transfer_finish(_Adjustment) ->
    {undefined, [{status_changed, succeeded}]}.

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/adjustment/", ID/binary>>.

-spec update_status(status(), adjustment()) -> adjustment().
update_status(Status, Adjustment) ->
    Adjustment#{status := Status}.

%% Changes helpers

-spec is_cash_flow_change_planned(adjustment()) -> boolean().
is_cash_flow_change_planned(Adjustment) ->
    Changes = changes_plan(Adjustment),
    maps:is_key(new_cash_flow, Changes).
