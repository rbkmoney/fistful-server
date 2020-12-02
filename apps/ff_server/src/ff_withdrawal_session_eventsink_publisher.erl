-module(ff_withdrawal_session_eventsink_publisher).

-behaviour(ff_eventsink_publisher).

-export([publish_events/1]).

-include_lib("fistful_proto/include/ff_proto_withdrawal_session_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type event() :: ff_eventsink_publisher:event(ff_withdrawal_session:event()).
-type sinkevent() :: ff_eventsink_publisher:sinkevent(
    ff_proto_withdrawal_session_thrift:'SinkEvent'()
).

%%
%% Internals
%%

-spec publish_events(list(event())) -> list(sinkevent()).
publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

-spec publish_event(event()) -> sinkevent().
publish_event(#{
    id := ID,
    source_id := SourceID,
    event := {
        EventID,
        Dt,
        {ev, EventDt, Payload}
    }
}) ->
    #wthd_session_SinkEvent{
        id = marshal(event_id, ID),
        created_at = marshal(timestamp, Dt),
        source = marshal(id, SourceID),
        payload = #wthd_session_Event{
            sequence = marshal(event_id, EventID),
            occured_at = marshal(timestamp, EventDt),
            changes = [marshal(change, Payload)]
        }
    }.

%% Internals

marshal(Type, Value) ->
    ff_withdrawal_session_codec:marshal(Type, Value).
