-module(ff_destination_eventsink_publisher).

-behaviour(ff_eventsink_publisher).

-export([publish_events/1]).

-export([marshal/2]).

-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").

-type event() :: ff_eventsink_publisher:event(ff_destination:event()).
-type sinkevent() :: ff_eventsink_publisher:sinkevent(ff_proto_destinaion_thrift:'SinkEvent'()).

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
    #dst_SinkEvent{
        id            = marshal(event_id, ID),
        created_at    = marshal(timestamp, Dt),
        source        = marshal(id, SourceID),
        payload       = #dst_Event{
            sequence   = marshal(event_id, EventID),
            occured_at = marshal(timestamp, EventDt),
            changes    = [marshal(event, Payload)]
        }
    }.

%%
%% Internals
%%

-spec marshal(term(), term()) -> term().

marshal(event, {created, Destination}) ->
    {created, marshal(destination, Destination)};
marshal(event, {account, AccountChange}) ->
    {account, marshal(account_change, AccountChange)};
marshal(event, {status_changed, StatusChange}) ->
    {status, marshal(status_change, StatusChange)};

marshal(destination, Params = #{
    name := Name,
    resource := Resource
}) ->
    ExternalID = maps:get(external_id, Params, undefined),
    #dst_Destination{
        name = marshal(string, Name),
        resource = marshal(resource, Resource),
        external_id = marshal(id, ExternalID)
    };
marshal(resource, {bank_card, BankCard}) ->
    {bank_card, marshal(bank_card, BankCard)};
marshal(bank_card, BankCard = #{
    token := Token
}) ->
    PaymentSystem = maps:get(payment_system, BankCard, undefined),
    Bin = maps:get(bin, BankCard, undefined),
    MaskedPan = maps:get(masked_pan, BankCard, undefined),
    #'BankCard'{
        token = marshal(string, Token),
        payment_system = PaymentSystem,
        bin = marshal(string, Bin),
        masked_pan = marshal(string, MaskedPan)
    };

marshal(status_change, unauthorized) ->
    {changed, {unauthorized, #dst_Unauthorized{}}};
marshal(status_change, authorized) ->
    {changed, {authorized, #dst_Authorized{}}};

marshal(T, V) ->
    ff_eventsink_publisher:marshal(T, V).

