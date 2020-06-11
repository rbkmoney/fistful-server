-module(machinery_mg_eventsink).

-export([get_events/4]).
-export([get_last_event_id/2]).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-define(EVENTSINK_CORE_OPTS,
    schema := machinery_mg_schema:schema()
).

-type eventsink_id()   :: binary().
-type event_id()       :: integer().
-type eventsink_opts() :: #{
    client := machinery_mg_client:client(),
    ?EVENTSINK_CORE_OPTS
}.

-type evsink_event(T)  :: #{
    id          := event_id(),
    ns          := binary(),
    source_id   := machinery:id(),
    event       := machinery:event(T)
}.

-export_type([evsink_event/1]).

-spec get_events(eventsink_id(), event_id(), integer(), eventsink_opts()) ->
    {ok, list(evsink_event(_))}.
get_events(EventSinkID, After, Limit, Opts) ->
    {ok, get_history_range(EventSinkID, After, Limit, Opts)}.

-spec get_last_event_id(eventsink_id(), eventsink_opts()) ->
    {ok, event_id()} | {error, no_last_event}.
get_last_event_id(EventSinkID, Opts) ->
    case get_history_range(EventSinkID, undefined, 1, backward, Opts) of
        [#{id := ID}] ->
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
    try
        MilliSec = genlib_rfc3339:parse(V, millisecond),
        case genlib_rfc3339:is_utc(V) of
            false ->
                erlang:error(badarg, [timestamp, V, badoffset]);
            true ->
                USec = MilliSec rem 1000,
                DateTime = calendar:system_time_to_universal_time(MilliSec, millisecond),
                {DateTime, USec}
        end
    catch
        error:Reason:St  ->
            erlang:raise(error, {timestamp, V, Reason}, St)
    end;
unmarshal(
    {evsink_event, Schema},
    #'mg_stateproc_SinkEvent'{
        'id'            = ID,
        'source_ns'     = NS0,
        'source_id'     = SourceID0,
        'event'         = Event
    }
) ->
    #mg_stateproc_Event{id = EventID, created_at = CreatedAt0, format_version = Format, data = Data0} = Event,
    SourceID1 = unmarshal(id, SourceID0),
    NS1 = unmarshal(namespace, NS0),
    CreatedAt1 = unmarshal(timestamp, CreatedAt0),
    Context = #{
        machine_ref => SourceID1,
        machine_ns => NS1,
        created_at => CreatedAt1
    },
    {Data1, Context} = unmarshal({schema, Schema, {event, Format}, Context}, Data0),
    #{
        id          => unmarshal(event_id, ID),
        ns          => NS1,
        source_id   => SourceID1,
        event       => {
            unmarshal(event_id, EventID),
            CreatedAt1,
            Data1
        }
    };

unmarshal({list, T}, V) when is_list(V) ->
    [unmarshal(T, E) || E <- V];
unmarshal({schema, Schema, T, Context}, V) ->
    machinery_mg_schema:unmarshal(Schema, T, V, Context);
unmarshal(string, V) when is_binary(V) ->
    V;
unmarshal(atom, V) when is_binary(V) ->
    binary_to_existing_atom(V, utf8);
unmarshal(integer, V) when is_integer(V) ->
    V;
unmarshal(T, V) ->
    error(badarg, {T, V}).
