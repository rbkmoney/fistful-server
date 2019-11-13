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

marshal(event, {created, Transfer}) ->
    {created, marshal(transfer, Transfer)};
marshal(event, {status_changed, Status}) ->
    {status_changed, #p2p_transfer_StatusChange{status = marshal(status, Status)}};
marshal(event, {resource_got, Sender, Receiver}) ->
    {resource, marshal(resource_got, {Sender, Receiver})};
marshal(event, {risk_score_changed, RiskScore}) ->
    {risk_score, #p2p_transfer_RiskScoreChange{score = marshal(risk_score, RiskScore)}};
marshal(event, {route_changed, Route}) ->
    {route, marshal(route, Route)};
marshal(event, {p_transfer, TransferChange}) ->
    {transfer, #p2p_transfer_TransferChange{payload = ff_p_transfer_codec:marshal(event, TransferChange)}};
marshal(event, {session, Session}) ->
    {session, marshal(session, Session)};
marshal(event, {adjustment, #{id := ID, payload := Payload}}) ->
    {adjustment, #p2p_transfer_AdjustmentChange{
        id = ff_adjustment_codec:marshal(?PREFIX, id, ID),
        payload = ff_adjustment_codec:marshal(?PREFIX, event, Payload)
    }};

marshal(transfer, Transfer = #{
    status := Status,
    identity_id := IdentityID,
    body := Body,
    created_at := CreatedAt
}) ->
    ExternalID = maps:get(external_id, Transfer, undefined),
    FeeQuote = maps:get(fees, Transfer, undefined),
    #p2p_transfer_P2PTransfer{
        owner = marshal(id, IdentityID),
        sender = {resource, #p2p_transfer_DisposableResource{
            resource = {crypto_wallet, #'CryptoWallet'{id = <<"TestID">>, currency = bitcoin}},
            contact_info = #'ContactInfo'{}
        }},
        receiver = {resource, #p2p_transfer_DisposableResource{
            resource = {crypto_wallet, #'CryptoWallet'{id = <<"TestID">>, currency = bitcoin}},
            contact_info = #'ContactInfo'{}
        }},
        body = marshal(cash, Body),
        status = marshal(status, Status),
        created_at = marshal(timestamp, ff_time:to_rfc3339(CreatedAt)),
        domain_revision = 123,
        party_revision = 321,
        operation_timestamp = marshal(timestamp, ff_time:to_rfc3339(ff_time:now())),
        quote = maybe_marshal(quote, FeeQuote),
        external_id = maybe_marshal(id, ExternalID)
    };

marshal(quote, #{}) ->
    #p2p_transfer_P2PQuote{};

marshal(status, pending) ->
    {pending, #p2p_status_Pending{}};
marshal(status, succeeded) ->
    {succeeded, #p2p_status_Succeeded{}};
marshal(status, {failed, Failure}) ->
    {failed, #p2p_status_Failed{failure = marshal(failure, Failure)}};

marshal(resource_got, {Sender, Receiver}) ->
    #p2p_transfer_ResourceChange{payload = {got, #p2p_transfer_ResourceGot{
        sender = marshal(participant, Sender),
        receiver = marshal(participant, Receiver)
    }}};

marshal(participant, {resource, #{instrument := {bank_card, {full, Resource}}}}) ->
    marshal(resource, {bank_card, Resource});

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

marshal(session, {started, SessionID}) ->
    #p2p_transfer_SessionChange{
        id = marshal(id, SessionID),
        payload = {started, #p2p_transfer_SessionStarted{}}
    };
marshal(session, {finished, {SessionID, SessionResult}}) ->
    #p2p_transfer_SessionChange{
        id = marshal(id, SessionID),
        payload = {finished, #p2p_transfer_SessionFinished{
            result = marshal(session_result, SessionResult)
        }}
    };

marshal(session_result, {success, TrxInfo}) ->
    {succeeded, #p2p_transfer_SessionSucceeded{trx_info = marshal(transaction_info, TrxInfo)}};
marshal(session_result, {failed, Failure}) ->
    {failed, #p2p_transfer_SessionFailed{failure = marshal(failure, Failure)}};

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #p2p_transfer_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, event}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(event, {created, Transfer}) ->
    {created, unmarshal(transfer, Transfer)};
unmarshal(event, {status_changed, #p2p_transfer_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};
unmarshal(event, {resource, #p2p_transfer_ResourceChange{
        payload = {got, #p2p_transfer_ResourceGot{sender = Sender, receiver = Receiver}
}}}) ->
    unmarshal(resource_got, {Sender, Receiver});
unmarshal(event, {risk_score, #p2p_transfer_RiskScoreChange{score = RiskScore}}) ->
    {risk_score_changed, unmarshal(risk_score, RiskScore)};
unmarshal(event, {route, #p2p_transfer_RouteChange{route = Route}}) ->
    {route_changed, unmarshal(route, Route)};
unmarshal(event, {transfer, #p2p_transfer_TransferChange{payload = TransferChange}}) ->
    {p_transfer, ff_p_transfer_codec:unmarshal(event, TransferChange)};
unmarshal(event, {session, #p2p_transfer_SessionChange{id = ID, payload = Payload}}) ->
    {session, unmarshal(session, {ID, Payload})};
unmarshal(event, {adjustment, #p2p_transfer_AdjustmentChange{id = ID, payload = Payload}}) ->
    {adjustment, #{
        id => ff_adjustment_codec:unmarshal(id, ID),
        payload => ff_adjustment_codec:unmarshal(event, Payload)
    }};

unmarshal(transfer, #p2p_transfer_P2PTransfer{
    owner = IdentityID,
    % sender = Sender,
    % receiver = Receiver,
    body = Body,
    status = Status,
    created_at = CreatedAt,
    % domain_revision = DomainRevision,
    % party_revision = PartyRevision,
    % operation_timestamp = OperationTimestamp,
    quote = FeeQuote,
    external_id = ExternalID
}) ->
    genlib_map:compact(#{
        status => unmarshal(status, Status),
        identity_id => unmarshal(id, IdentityID),
        body => unmarshal(cash, Body),
        created_at => ff_time:from_rfc3339(unmarshal(timestamp, CreatedAt)),
        fees => maybe_unmarshal(quote, FeeQuote),
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
    {resource_got, unmarshal(participant, Sender), unmarshal(participant, Receiver)};

unmarshal(participant, Resource) ->
    {resource, #{instrument => {bank_card, {full, unmarshal(resource, Resource)}}}};

unmarshal(risk_score, low) ->
    low;
unmarshal(risk_score, high) ->
    high;
unmarshal(risk_score, fatal) ->
    fatal;

unmarshal(route, #p2p_transfer_Route{provider_id = ProviderID}) ->
    #{provider_id => unmarshal(id, ProviderID)};

unmarshal(session, {SessionID, {started, #p2p_transfer_SessionStarted{}}}) ->
    {started, SessionID};
unmarshal(session, {SessionID, {finished, #p2p_transfer_SessionFinished{result = SessionResult}}}) ->
    {finished, {SessionID, unmarshal(session_result, SessionResult)}};

unmarshal(session_result, {succeeded, #p2p_transfer_SessionSucceeded{trx_info = TrxInfo}}) ->
    {success, unmarshal(transaction_info, TrxInfo)};
unmarshal(session_result, {failed, #p2p_transfer_SessionFailed{failure = Failure}}) ->
    {failed, unmarshal(failure, Failure)};

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

    Participant = {resource, #{
        instrument   => {bank_card, {full, #{
            token => genlib:unique(),
            bin_data_id => {binary, genlib:unique()}
        }}}}
    },

    P2PTransfer = #{
        status => pending,
        identity_id => genlib:unique(),
        body => {123, <<"RUB">>},
        created_at => ff_time:now(),
        fees => #{},
        external_id => genlib:unique()
    },

    PTransfer = #{
        final_cash_flow => FinalCashFlow
    },

    Events = [
        {created, P2PTransfer},
        {resource_got, Participant, Participant},
        {risk_score_changed, low},
        {route_changed, #{provider_id => genlib:unique()}},
        {p_transfer, {created, PTransfer}},
        {session, {started, genlib:unique()}},
        {status_changed, succeeded},
        {adjustment, #{id => genlib:unique(), payload => {created, Adjustment}}}
    ],
    Marshaled = marshal({list, event}, Events),
    io:format("Marshaled - ~p~n", [Marshaled]),
    Unmarshaled = unmarshal({list, event}, Marshaled),
    ?assertEqual(Events, Unmarshaled).

-endif.