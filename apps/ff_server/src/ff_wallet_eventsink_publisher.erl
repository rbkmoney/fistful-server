-module(ff_wallet_eventsink_publisher).

-behaviour(ff_eventsink_publisher).

-export([publish_events/1]).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

-type event() :: ff_eventsink_publisher:event(ff_wallet:event()).
-type sinkevent() :: ff_eventsink_publisher:sinkevent(ff_proto_wallet_thrift:'SinkEvent'()).

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
    #wlt_SinkEvent{
        id = marshal(event_id, ID),
        created_at = marshal(timestamp, Dt),
        source = marshal(id, SourceID),
        payload = #wlt_Event{
            sequence = marshal(event_id, EventID),
            occured_at = marshal(timestamp, EventDt),
            changes = [marshal(change, Payload)]
        }
    }.

%%
%% Internals
%%

marshal(Type, Value) ->
    ff_wallet_codec:marshal(Type, Value).
