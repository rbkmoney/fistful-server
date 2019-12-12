-module(ff_p2p_transfer_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

-define(PREFIX, #{
    transfer => <<"p2p_transfer">>,
    adjustment => <<"p2p_adj">>,
    status => <<"p2p_status">>
}).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(change, {created, Transfer}) ->
    {created, #p2p_transfer_CreatedChange{p2p_transfer = marshal(transfer, Transfer)}};
marshal(change, {status_changed, Status}) ->
    {status_changed, #p2p_transfer_StatusChange{status = marshal(status, Status)}};
marshal(change, {resource_got, Sender, Receiver}) ->
    {resource, marshal(resource_got, {Sender, Receiver})};
marshal(change, {risk_score_changed, RiskScore}) ->
    {risk_score, #p2p_transfer_RiskScoreChange{score = marshal(risk_score, RiskScore)}};
marshal(change, {route_changed, Route}) ->
    {route, marshal(route, Route)};
marshal(change, {p_transfer, TransferChange}) ->
    {transfer, #p2p_transfer_TransferChange{payload = ff_p_transfer_codec:marshal(event, TransferChange)}};
marshal(change, {session, Session}) ->
    {session, marshal(session, Session)};
marshal(change, {adjustment, #{id := ID, payload := Payload}}) ->
    {adjustment, #p2p_transfer_AdjustmentChange{
        id = ff_adjustment_codec:marshal(?PREFIX, id, ID),
        payload = ff_adjustment_codec:marshal(?PREFIX, change, Payload)
    }};

marshal(transfer, Transfer = #{
    status := Status,
    owner := Owner,
    sender := Sender,
    receiver := Receiver,
    body := Body,
    domain_revision := DomainRevision,
    party_revision := PartyRevision,
    operation_timestamp := OperationTimestamp,
    created_at := CreatedAt
}) ->
    ExternalID = maps:get(external_id, Transfer, undefined),
    Quote = maps:get(quote, Transfer, undefined),
    Deadline = maps:get(deadline, Transfer, undefined),
    ClientInfo = maps:get(client_info, Transfer, undefined),

    #p2p_transfer_P2PTransfer{
        owner = marshal(id, Owner),
        sender = marshal(participant, Sender),
        receiver = marshal(participant, Receiver),
        body = marshal(cash, Body),
        status = marshal(status, Status),
        created_at = marshal(timestamp, CreatedAt),
        domain_revision = marshal(integer, DomainRevision),
        party_revision = marshal(integer, PartyRevision),
        operation_timestamp = marshal(timestamp, OperationTimestamp),
        quote = maybe_marshal(quote, Quote),
        external_id = maybe_marshal(id, ExternalID),
        client_info = maybe_marshal(client_info, ClientInfo),
        deadline = maybe_marshal(timestamp, Deadline)
    };

marshal(quote, #{}) ->
    #p2p_transfer_P2PQuote{};

marshal(status, pending) ->
    {pending, #p2p_status_Pending{}};
marshal(status, succeeded) ->
    {succeeded, #p2p_status_Succeeded{}};
marshal(status, {failed, Failure}) ->
    {failed, #p2p_status_Failed{failure = marshal(failure, Failure)}};

marshal(participant, {raw, #{resource_params := Resource} = Raw}) ->
    ContactInfo = maps:get(contact_info, Raw, undefined),
    {resource, #p2p_transfer_RawResource{
        resource = marshal(resource, Resource),
        contact_info = maybe_marshal(contact_info, ContactInfo)
    }};

marshal(contact_info, ContactInfo) ->
    PhoneNumber = maps:get(phone_number, ContactInfo, undefined),
    Email = maps:get(email, ContactInfo, undefined),
    #'ContactInfo'{
        phone_number = marshal(string, PhoneNumber),
        email = marshal(string, Email)
    };

marshal(client_info, ClientInfo) ->
    IPAddress = maps:get(ip_address, ClientInfo, undefined),
    Fingerprint = maps:get(fingerprint, ClientInfo, undefined),
    #'ClientInfo'{
        ip_address = marshal(string, IPAddress),
        fingerprint = marshal(string, Fingerprint)
    };

marshal(resource_got, {Sender, Receiver}) ->
    #p2p_transfer_ResourceChange{payload = {got, #p2p_transfer_ResourceGot{
        sender = marshal(resource, Sender),
        receiver = marshal(resource, Receiver)
    }}};

marshal(risk_score, low) ->
    low;
marshal(risk_score, high) ->
    high;
marshal(risk_score, fatal) ->
    fatal;

marshal(route, #{provider_id := ProviderID}) ->
    #p2p_transfer_RouteChange{route = #p2p_transfer_Route{
        provider_id =  marshal(id, ProviderID)
    }};

marshal(session, {SessionID, started}) ->
    #p2p_transfer_SessionChange{
        id = marshal(id, SessionID),
        payload = {started, #p2p_transfer_SessionStarted{}}
    };
marshal(session, {SessionID, {finished, SessionResult}}) ->
    #p2p_transfer_SessionChange{
        id = marshal(id, SessionID),
        payload = {finished, #p2p_transfer_SessionFinished{
            result = marshal(session_result, SessionResult)
        }}
    };

marshal(session_result, success) ->
    {succeeded, #p2p_transfer_SessionSucceeded{}};
marshal(session_result, {failure, Failure}) ->
    {failed, #p2p_transfer_SessionFailed{failure = marshal(failure, Failure)}};

marshal(timestamp, Timestamp) when is_integer(Timestamp) ->
    ff_time:to_rfc3339(Timestamp);

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #p2p_transfer_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, change}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(change, {created, #p2p_transfer_CreatedChange{p2p_transfer = Transfer}}) ->
    {created, unmarshal(transfer, Transfer)};
unmarshal(change, {status_changed, #p2p_transfer_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};
unmarshal(change, {resource, #p2p_transfer_ResourceChange{
        payload = {got, #p2p_transfer_ResourceGot{sender = Sender, receiver = Receiver}
}}}) ->
    unmarshal(resource_got, {Sender, Receiver});
unmarshal(change, {risk_score, #p2p_transfer_RiskScoreChange{score = RiskScore}}) ->
    {risk_score_changed, unmarshal(risk_score, RiskScore)};
unmarshal(change, {route, #p2p_transfer_RouteChange{route = Route}}) ->
    {route_changed, unmarshal(route, Route)};
unmarshal(change, {transfer, #p2p_transfer_TransferChange{payload = TransferChange}}) ->
    {p_transfer, ff_p_transfer_codec:unmarshal(event, TransferChange)};
unmarshal(change, {session, #p2p_transfer_SessionChange{id = ID, payload = Payload}}) ->
    {session, unmarshal(session, {ID, Payload})};
unmarshal(change, {adjustment, #p2p_transfer_AdjustmentChange{id = ID, payload = Payload}}) ->
    {adjustment, #{
        id => ff_adjustment_codec:unmarshal(id, ID),
        payload => ff_adjustment_codec:unmarshal(change, Payload)
    }};

unmarshal(transfer, #p2p_transfer_P2PTransfer{
    owner = Owner,
    sender = Sender,
    receiver = Receiver,
    body = Body,
    status = Status,
    created_at = CreatedAt,
    domain_revision = DomainRevision,
    party_revision = PartyRevision,
    operation_timestamp = OperationTimestamp,
    quote = Quote,
    client_info = ClientInfo,
    external_id = ExternalID
}) ->
    genlib_map:compact(#{
        status => unmarshal(status, Status),
        owner => unmarshal(id, Owner),
        body => unmarshal(cash, Body),
        sender => unmarshal(participant, Sender),
        receiver => unmarshal(participant, Receiver),
        domain_revision => unmarshal(integer, DomainRevision),
        party_revision => unmarshal(integer, PartyRevision),
        operation_timestamp => ff_time:from_rfc3339(unmarshal(timestamp, OperationTimestamp)),
        created_at => ff_time:from_rfc3339(unmarshal(timestamp, CreatedAt)),
        quote => maybe_unmarshal(quote, Quote),
        client_info => maybe_unmarshal(client_info, ClientInfo),
        external_id => maybe_unmarshal(id, ExternalID)
    });

unmarshal(quote, #p2p_transfer_P2PQuote{}) ->
    #{};

unmarshal(status, {pending, #p2p_status_Pending{}}) ->
    pending;
unmarshal(status, {succeeded, #p2p_status_Succeeded{}}) ->
    succeeded;
unmarshal(status, {failed, #p2p_status_Failed{failure = Failure}}) ->
    {failed, unmarshal(failure, Failure)};

unmarshal(resource_got, {Sender, Receiver}) ->
    {resource_got, unmarshal(resource, Sender), unmarshal(resource, Receiver)};

unmarshal(participant, {resource, #p2p_transfer_RawResource{
    resource = Resource,
    contact_info = ContactInfo
}}) ->
    {raw, genlib_map:compact(#{
        resource_params => unmarshal(resource, Resource),
        contact_info => maybe_unmarshal(contact_info, ContactInfo)
    })};

unmarshal(contact_info, #'ContactInfo'{
    phone_number = PhoneNumber,
    email = Email
}) ->
    genlib_map:compact(#{
        phone_number => maybe_unmarshal(string, PhoneNumber),
        email => maybe_unmarshal(string, Email)
    });

unmarshal(client_info, #'ClientInfo'{
    ip_address = IPAddress,
    fingerprint = Fingerprint
}) ->
    genlib_map:compact(#{
        ip_address => maybe_unmarshal(string, IPAddress),
        fingerprint => maybe_unmarshal(string, Fingerprint)
    });

unmarshal(risk_score, low) ->
    low;
unmarshal(risk_score, high) ->
    high;
unmarshal(risk_score, fatal) ->
    fatal;

unmarshal(route, #p2p_transfer_Route{provider_id = ProviderID}) ->
    #{provider_id => unmarshal(id, ProviderID)};

unmarshal(session, {SessionID, {started, #p2p_transfer_SessionStarted{}}}) ->
    {SessionID, started};
unmarshal(session, {SessionID, {finished, #p2p_transfer_SessionFinished{result = SessionResult}}}) ->
    {SessionID, {finished, unmarshal(session_result, SessionResult)}};

unmarshal(session_result, {succeeded, #p2p_transfer_SessionSucceeded{}}) ->
    success;
unmarshal(session_result, {failed, #p2p_transfer_SessionFailed{failure = Failure}}) ->
    {failure, unmarshal(failure, Failure)};

unmarshal(timestamp, Timestamp) ->
    unmarshal(string, Timestamp);

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec p2p_transfer_codec_test() -> _.
p2p_transfer_codec_test() ->
    FinalCashFlow = #{
        postings => []
    },

    CashFlowChange = #{
        old_cash_flow_inverted => FinalCashFlow,
        new_cash_flow => FinalCashFlow
    },

    Plan = #{
        new_cash_flow => CashFlowChange,
        new_status => succeeded
    },

    Adjustment = #{
        id => genlib:unique(),
        status => pending,
        changes_plan => Plan,
        created_at => ff_time:now(),
        domain_revision => 123,
        party_revision => 321,
        operation_timestamp => ff_time:now(),
        external_id => genlib:unique()
    },

    Resource = {bank_card, #{
        token => genlib:unique(),
        bin_data_id => {binary, genlib:unique()}
    }},

    Participant = {raw, #{
        resource_params => Resource,
        contact_info => #{}
    }},

    P2PTransfer = #{
        status => pending,
        owner => genlib:unique(),
        body => {123, <<"RUB">>},
        created_at => ff_time:now(),
        sender => Participant,
        receiver => Participant,
        quote => #{},
        domain_revision => 123,
        party_revision => 321,
        operation_timestamp => ff_time:now(),
        external_id => genlib:unique()
    },

    PTransfer = #{
        final_cash_flow => FinalCashFlow
    },

    Changes = [
        {created, P2PTransfer},
        {resource_got, Resource, Resource},
        {risk_score_changed, low},
        {route_changed, #{provider_id => genlib:unique()}},
        {p_transfer, {created, PTransfer}},
        {session, {genlib:unique(), started}},
        {status_changed, succeeded},
        {adjustment, #{id => genlib:unique(), payload => {created, Adjustment}}}
    ],
    ?assertEqual(Changes, unmarshal({list, change}, marshal({list, change}, Changes))).

-endif.