-module(ff_w2w_transfer_eventsink_publisher).

-behaviour(ff_eventsink_publisher).

-export([publish_events/1]).

-include_lib("fistful_proto/include/ff_proto_w2w_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_cashflow_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type event() :: ff_eventsink_publisher:event(w2w_transfer:event()).
-type sinkevent() :: ff_eventsink_publisher:sinkevent(ff_proto_w2w_transfer_thrift:'SinkEvent'()).

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
    #w2w_transfer_SinkEvent{
        id = marshal(event_id, ID),
        created_at = marshal(timestamp, Dt),
        source = marshal(id, SourceID),
        payload = #w2w_transfer_EventSinkPayload{
            sequence = marshal(event_id, EventID),
            occured_at = marshal(timestamp, EventDt),
            changes = [marshal(change, Payload)]
        }
    }.

%%

marshal(Type, Value) ->
    ff_w2w_transfer_codec:marshal(Type, Value).
