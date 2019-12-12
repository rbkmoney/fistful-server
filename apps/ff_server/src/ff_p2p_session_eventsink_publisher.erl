-module(ff_p2p_session_eventsink_publisher).

-behaviour(ff_eventsink_publisher).

-export([publish_events/1]).

-include_lib("fistful_proto/include/ff_proto_p2p_session_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type event() :: ff_eventsink_publisher:event(p2p_session:event()).
-type sinkevent() :: ff_eventsink_publisher:sinkevent(
    ff_proto_p2p_session_thrift:'SinkEvent'()
).

%%
%% Internals
%%

-spec publish_events(list(event())) ->
    list(sinkevent()).

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

-spec publish_event(event()) ->
    sinkevent().

publish_event(#{
    id          := ID,
    source_id   := SourceID,
    event       := {
        EventID,
        Dt,
        {ev, EventDt, Payload}
    }
}) ->
    #p2p_session_SinkEvent{
        id            = marshal(event_id, ID),
        created_at    = marshal(timestamp, Dt),
        source        = marshal(id, SourceID),
        payload       = #p2p_session_EventSinkPayload{
            sequence   = marshal(event_id, EventID),
            occured_at = marshal(timestamp, EventDt),
            changes    = [marshal(change, p2p_session:maybe_migrate(Payload))]
        }
    }.

%% Internals

marshal(Type, Value) ->
    ff_p2p_session_codec:marshal(Type, Value).
