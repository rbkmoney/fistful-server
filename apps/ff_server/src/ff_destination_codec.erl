-module(ff_destination_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

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
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #dst_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, event}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(event, {created, Destination}) ->
    {created, unmarshal(destination, Destination)};
unmarshal(event, {account, AccountChange}) ->
    {account, unmarshal(account_change, AccountChange)};
unmarshal(event, {status, StatusChange}) ->
    {status_changed, unmarshal(status_change, StatusChange)};

unmarshal(destination, #dst_Destination{
    name = Name,
    resource = Resource,
    external_id = ExternalID
}) ->
    genlib_map:compact(#{
        name => unmarshal(string, Name),
        resource => unmarshal(resource, Resource),
        external_id => maybe_unmarshal(id, ExternalID)
    });
unmarshal(resource, {bank_card, BankCard}) ->
    {bank_card, unmarshal(bank_card, BankCard)};
unmarshal(bank_card, BankCard = #{
    token := Token
}) ->
    PaymentSystem = maps:get(payment_system, BankCard, undefined),
    Bin = maps:get(bin, BankCard, undefined),
    MaskedPan = maps:get(masked_pan, BankCard, undefined),
    #'BankCard'{
        token = unmarshal(string, Token),
        payment_system = PaymentSystem,
        bin = unmarshal(string, Bin),
        masked_pan = unmarshal(string, MaskedPan)
    };
unmarshal(bank_card, #'BankCard'{
    token = Token,
    payment_system = PaymentSystem,
    bin = Bin,
    masked_pan = MaskedPan
}) ->
    genlib_map:compact(#{
        token => unmarshal(string, Token),
        payment_system => PaymentSystem,
        bin => maybe_unmarshal(string, Bin),
        masked_pan => maybe_unmarshal(string, MaskedPan)
    });

unmarshal(status_change, {changed, {unauthorized, #dst_Unauthorized{}}}) ->
    unauthorized;
unmarshal(status_change, {changed, {authorized, #dst_Authorized{}}}) ->
    authorized;

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).
