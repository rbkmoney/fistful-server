-module(ff_source_eventsink_publisher).

-behaviour(ff_eventsink_publisher).

-export([publish_events/1]).

-include_lib("fistful_proto/include/ff_proto_source_thrift.hrl").

-type event() :: ff_eventsink_publisher:event(ff_source:event()).
-type sinkevent() :: ff_eventsink_publisher:sinkevent(ff_proto_source_thrift:'SinkEvent'()).

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
    #src_SinkEvent{
        id            = marshal(event_id, ID),
        created_at    = marshal(timestamp, Dt),
        source        = marshal(id, SourceID),
        payload       = #src_Event{
            sequence   = marshal(event_id, EventID),
            occured_at = marshal(timestamp, EventDt),
            changes    = [marshal(event, Payload)]
        }
    }.

%%
%% Internals
%%

-spec marshal(term(), term()) -> term().

marshal(event, {created, Source}) ->
    {created, marshal(source, Source)};
marshal(event, {account, AccountChange}) ->
    {account, marshal(account_change, AccountChange)};
marshal(event, {status_changed, StatusChange}) ->
    {status, marshal(status_change, StatusChange)};

marshal(source, Params = #{
    name := Name,
    resource := Resource
}) ->
    ExternalID = maps:get(external_id, Params, undefined),
    #src_Source{
        name = marshal(string, Name),
        resource = marshal(resource, Resource),
        external_id = marshal(id, ExternalID)
    };
marshal(resource, #{type := internal} = Internal) ->
    {internal, marshal(internal, Internal)};
marshal(internal, Internal) ->
    Details = maps:get(details, Internal, undefined),
    #src_Internal{
        details = marshal(string, Details)
    };

marshal(status_change, unauthorized) ->
    {changed, {unauthorized, #src_Unauthorized{}}};
marshal(status_change, authorized) ->
    {changed, {authorized, #src_Authorized{}}};

marshal(T, V) ->
    ff_eventsink_publisher:marshal(T, V).

