-module(machinery_mg_eventsink).

-export([get_events/4]).
-export([get_last_event_id/2]).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-define(EVENTSINK_CORE_OPTS,
    schema := machinery_mg_schema:schema()
).

-type event_sink_id()  :: binary().
-type event_id()       :: ff_proto_base_thrift:'EventID'().
-type eventsink_opts() :: #{
    client := machinery_mg_client:client(),
    ?EVENTSINK_CORE_OPTS
}.

-spec get_events(event_sink_id(), event_id(), integer(), eventsink_opts()) ->
    {ok, list()}.
get_events(EventSinkID, After, Limit, Opts) ->
    {ok, get_history_range(EventSinkID, After, Limit, Opts)}.

-spec get_last_event_id(event_sink_id(), eventsink_opts()) ->
    {ok, event_id()} | {error, no_last_event}.
get_last_event_id(EventSinkID, Opts) ->
    case get_history_range(EventSinkID, undefined, 1, backward, Opts) of
        [{ID, _, _, _}] ->
            {ok, ID};
        [] ->
            {error, no_last_event}
    end.

get_history_range(EventSinkID, After, Limit, Opts) ->
    get_history_range(EventSinkID, After, Limit, forward, Opts).

get_history_range(EventSinkID, After, Limit, Direction, #{client := Client, schema := Schema}) ->
    {ok, Events} = call_eventsink('GetHistory', marshal(id, EventSinkID),
        [marshal(history_range, {After, Limit, Direction})], Client),
    unmarshal({list, {evsink_event, Schema}}, Events).

call_eventsink(Function, EventSinkID, Args, {Client, Context}) ->
    Service = {mg_proto_state_processing_thrift, 'EventSink'},
    woody_client:call({Service, Function, [EventSinkID | Args]}, Client, Context).

%%

marshal(id, V) ->
    marshal(string, V);
marshal(history_range, {After, Limit, Direction}) ->
    #'mg_stateproc_HistoryRange'{
        'after'     = After,
        'limit'     = Limit,
        'direction' = Direction
    };
marshal(string, V) when is_binary(V) ->
    V.

%%

% TODO refactor this copy of mg_backend unmarshal
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
    {unmarshal(event_id, ID), unmarshal(namespace, Ns), unmarshal(id, SourceID),
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
