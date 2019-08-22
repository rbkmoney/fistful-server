%%
%% Adjustment management helpers
%%

-module(ff_adjustment_index).

-opaque index() :: #{
    adjustments       := #{id() => adjustment()},
    inversed_order    := [id()],
    active            => id(),
    body              => body(),
    status            => target_status(),
    cash_flow         => final_cash_flow()
}.

-type wrapped_event() :: {adjustment, #{
    id      := id(),
    payload := event()
}}.

-export_type([index/0]).
-export_type([wrapped_event/0]).

%% API

-export([new/0]).

-export([set_body/2]).
-export([set_status/2]).
-export([set_cash_flow/2]).

-export([body/1]).
-export([status/1]).
-export([cash_flow/1]).

-export([adjustments/1]).
-export([is_active/1]).
-export([wrap_event/2]).
-export([wrap_events/2]).
-export([unwrap_event/1]).
-export([apply_event/2]).
-export([get_by_id/2]).
-export([process_adjustments/1]).

%% Internal types

-type id()              :: ff_adjustment:id().
-type target_id()       :: binary().
-type adjustment()      :: ff_adjustment:adjustment().
-type event()           :: ff_adjustment:event().
-type target_status()   :: term().
-type final_cash_flow() :: ff_cash_flow:final_cash_flow().
-type action()          :: machinery:action() | undefined.
-type body()            :: ff_transaction:body().
-type changes()         :: ff_adjustment:changes().

%% API

-spec new() -> index().
new() ->
    #{
        adjustments => #{},
        inversed_order => []
    }.

-spec set_body(body(), index()) -> index().
set_body(Body, Index) ->
    Index#{body => Body}.

-spec set_status(target_status(), index()) -> index().
set_status(Status, Index) ->
    Index#{status => Status}.

-spec set_cash_flow(final_cash_flow(), index()) -> index().
set_cash_flow(Body, Index) ->
    Index#{cash_flow => Body}.

-spec body(index()) -> body().
body(Index) ->
    maps:get(body, Index).

-spec status(index()) -> body().
status(Index) ->
    maps:get(status, Index).

-spec cash_flow(index()) -> body().
cash_flow(Index) ->
    maps:get(cash_flow, Index).

-spec adjustments(index()) -> [adjustment()].
adjustments(Index) ->
    #{
        adjustments := Map,
        inversed_order := Order
    } = Index,
    [maps:get(ID, Map) || ID <- lists:reverse(Order)].

-spec is_active(index()) -> boolean().
is_active(Index) ->
    maps:is_key(active, Index).

-spec wrap_events(target_id(), [event()]) -> [wrapped_event()].
wrap_events(ID, Events) ->
    [wrap_event(ID, Ev) || Ev <- Events].

-spec unwrap_event(wrapped_event()) -> {id(), event()}.
unwrap_event({adjustment, #{id := ID, payload := Event}}) ->
    {ID, Event}.

-spec wrap_event(id(), event()) -> wrapped_event().
wrap_event(ID, Event) ->
    {adjustment, #{id => ID, payload => Event}}.

-spec get_by_id(id(), index()) -> adjustment().
get_by_id(ID, Index) ->
    #{adjustments := #{ID := Adjustment}} = Index,
    Adjustment.

-spec apply_event(wrapped_event(), index()) -> index().
apply_event(WrappedEvent, Index0) ->
    {ID, Event} = unwrap_event(WrappedEvent),
    #{adjustments := Adjustments} = Index0,
    Adjustment0 = maps:get(ID, Adjustments, undefined),
    Adjustment1 = ff_adjustment:apply_event(Event, Adjustment0),
    Index1 = Index0#{adjustments := Adjustments#{ID => Adjustment1}},
    Index2 = update_order(Event, Index1),
    Index3 = update_active(Event, Adjustment1, Index2),
    Index4 = update_target_data(Event, Index3),
    Index4.

-spec process_adjustments(index()) ->
    {ok, {action(), [wrapped_event()]}}.
process_adjustments(Index) ->
    #{
        adjustments := Adjustments,
        active := ID
    } = Index,
    #{ID := Adjustment} = Adjustments,
    {AdjustmentAction, Events} = ff_adjustment:process_transfer(Adjustment),
    WrappedEvents = wrap_events(ID, Events),
    {ok, {AdjustmentAction, WrappedEvents}}.

%% Internals

-spec update_order(event(), index()) -> index().
update_order({created, Adjustment}, #{inversed_order := Order} = Index) ->
    Index#{inversed_order => [ff_adjustment:id(Adjustment) | Order]};
update_order(_OtherEvent, Index) ->
    Index.

-spec update_active(event(), adjustment(), index()) -> index().
update_active({created, Adjustment}, _Adjustment, Index) when not is_map_key(active, Index) ->
    Index#{active => ff_adjustment:id(Adjustment)};
update_active(_OtherEvent, Adjustment, Index) when is_map_key(active, Index) ->
    case ff_adjustment:is_active(Adjustment) of
        false ->
            maps:remove(active, Index);
        true ->
            Index
    end.

-spec update_target_data(event(), index()) -> index().
update_target_data({status_changed, {succeeded, Changes}}, Index0) ->
    Index1 = update_target_body(Changes, Index0),
    Index2 = update_target_status(Changes, Index1),
    Index3 = update_target_cash_flow(Changes, Index2),
    Index3;
update_target_data(_OtherEvent, Index) ->
    Index.

-spec update_target_body(changes(), index()) -> index().
update_target_body(#{new_body := Body}, Index) ->
    set_body(Body, Index);
update_target_body(_OtherChange, Index) ->
    Index.

-spec update_target_status(changes(), index()) -> index().
update_target_status(#{new_status := Status}, Index) ->
    set_status(Status, Index);
update_target_status(_OtherChange, Index) ->
    Index.

-spec update_target_cash_flow(changes(), index()) -> index().
update_target_cash_flow(#{new_cash_flow := CashFlowChange}, Index) ->
    #{new_cash_flow := CashFlow} = CashFlowChange,
    set_cash_flow(CashFlow, Index);
update_target_cash_flow(_OtherChange, Index) ->
    Index.
