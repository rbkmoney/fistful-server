-module(machinery_eventsink).

-export([get_events/3]).
-export([get_last_event_id/1]).

-export([unmarshal/2]).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type event_sink_id() :: ff_proto_base_thrift:'ID'().
-type event_id()      :: ff_proto_base_thrift:'EventID'().

-spec get_events(event_sink_id(), event_id(), integer()) ->
    {ok, list()}.
get_events(EventSinkID, After, Limit) ->
    {ok, get_history_range(EventSinkID, After, Limit)}.

-spec get_last_event_id(event_sink_id()) ->
    {ok, event_id()} | {error, no_last_event}.
get_last_event_id(EventSinkID) ->
    case get_history_range(EventSinkID, undefined, 1, backward) of
        [{ID, _, _, _}] ->
            {ok, ID};
        [] ->
            {error, no_last_event}
    end.

get_history_range(EventSinkID, After, Limit) ->
    get_history_range(EventSinkID, After, Limit, forward).

get_history_range(EventSinkID, After, Limit, Direction) ->
    HistoryRange = #'mg_stateproc_HistoryRange'{'after' = After, limit = Limit, direction = Direction},
    {ok, Events} = call_eventsink('GetHistory', EventSinkID, [HistoryRange]),
    map_sink_events(Events).

call_eventsink(Function, EventSinkID, Args) ->
    Service = {mg_proto_state_processing_thrift, 'EventSink'},
    ff_woody_client:call(eventsink, {Service, Function, [EventSinkID | Args]}).

map_sink_events(Events) ->
    [map_sink_event(Ev) || Ev <- Events].

map_sink_event(#'mg_stateproc_SinkEvent'{id = ID, source_ns = Ns, source_id = SourceID, event = Event}) ->
    #'mg_stateproc_Event'{id = EventID, created_at = Dt, event_payload = Payload} = Event,
    {ID, Ns, SourceID, {EventID, Dt, Payload}}.

%%

-spec unmarshal(term(), term()) -> term().

unmarshal(id, V) ->
    unmarshal(string, V);
unmarshal(namespace, V) ->
    unmarshal(atom, V);
unmarshal(event_id, V) ->
    unmarshal(integer, V);
unmarshal(timestamp, V) when is_binary(V) ->
    case rfc3339:parse(V) of
        {ok, {Date, Time, USec, TZOffset}} when TZOffset == undefined orelse TZOffset == 0 ->
            {{Date, Time}, USec};
        {ok, _} ->
            error(badarg, {timestamp, V, badoffset});
        {error, Reason} ->
            error(badarg, {timestamp, V, Reason})
    end;
unmarshal({schema, Schema, T}, V) ->
    machinery_mg_schema:unmarshal(Schema, T, V);
unmarshal(string, V) when is_binary(V) ->
    V;
unmarshal(atom, V) when is_binary(V) ->
    binary_to_existing_atom(V, utf8);
unmarshal(integer, V) when is_integer(V) ->
    V;
unmarshal(T, V) ->
    error(badarg, {T, V}).
