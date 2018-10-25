-module(machinery_eventsink).

-export([get_events/4]).
-export([get_last_event_id/1]).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type event_sink_id() :: ff_proto_base_thrift:'ID'().
-type event_id()      :: ff_proto_base_thrift:'EventID'().

-spec get_events(event_sink_id(), event_id(), integer(), atom()) ->
    {ok, list()}.
get_events(EventSinkID, After, Limit, Schema) ->
    {ok, get_history_range(EventSinkID, After, Limit, Schema)}.

-spec get_last_event_id(event_sink_id()) ->
    {ok, event_id()} | {error, no_last_event}.
get_last_event_id(EventSinkID) ->
    case get_history_range(EventSinkID, undefined, 1, backward) of
        [{ID, _, _, _}] ->
            {ok, ID};
        [] ->
            {error, no_last_event}
    end.

get_history_range(EventSinkID, After, Limit, Schema) ->
    get_history_range(EventSinkID, After, Limit, forward, Schema).

get_history_range(EventSinkID, After, Limit, Direction, Schema) ->
    {ok, Events} = call_eventsink('GetHistory', marshal(id, EventSinkID),
        [marshal(history_range, {After, Limit, Direction})]),
    unmarshal({list, {evsink_event, Schema}}, Events).

call_eventsink(Function, EventSinkID, Args) ->
    Service = {mg_proto_state_processing_thrift, 'EventSink'},
    ff_woody_client:call(eventsink, {Service, Function, [EventSinkID | Args]}).

%%

marshal(id, V) ->
    marshal(string, V);
marshal(history_range, {After, Limit, Direction}) ->
    #'mg_stateproc_HistoryRange'{
        'after'     = marshal(id, After),
        'limit'     = marshal(timestamp, Limit),
        'direction' = marshal(atom, Direction)
    };
marshal(timestamp, {{Date, Time}, USec} = V) ->
    case rfc3339:format({Date, Time, USec, 0}) of
        {ok, R} when is_binary(R) ->
            R;
        Error ->
            error(badarg, {timestamp, V, Error})
    end;
marshal(atom, V) when is_atom(V) ->
    atom_to_binary(V, utf8);
marshal(string, V) when is_binary(V) ->
    V.
%%

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
unmarshal(
    {evsink_event, Schema},
    #'mg_stateproc_SinkEvent'{
        'id'            = ID,
        'source_ns'     = Ns,
        'source_id'     = SourceID,
        'event'         = Event
    }
) ->
    #'mg_stateproc_Event'{id = EventID, created_at = CreatedAt, event_payload = Payload} = Event,
    {unmarshal(id, ID), unmarshal(namespace, Ns), unmarshal(id, SourceID),
        {
            unmarshal(event_id, EventID),
            unmarshal(timestamp, CreatedAt),
            unmarshal({schema, Schema, event}, Payload)}};

unmarshal({list, T}, V) when is_list(V) ->
    [unmarshal(T, E) || E <- V];
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
