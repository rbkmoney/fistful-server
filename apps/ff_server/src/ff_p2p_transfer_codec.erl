-module(ff_p2p_transfer_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").

-export([unmarshal_p2p_quote_params/1]).
-export([marshal_p2p_transfer_state/2]).
-export([unmarshal_p2p_transfer_params/1]).

-export([marshal_event/1]).
-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec unmarshal_p2p_quote_params(ff_proto_p2p_transfer_thrift:'QuoteParams'()) -> p2p_quote:params().
unmarshal_p2p_quote_params(#p2p_transfer_QuoteParams{
    identity_id = IdentityID,
    sender = Sender,
    receiver = Receiver,
    body = Body
}) ->
    #{
        identity_id => unmarshal(id, IdentityID),
        body => unmarshal(cash, Body),
        sender => unmarshal(resource, Sender),
        receiver => unmarshal(resource, Receiver)
    }.

-spec marshal_p2p_transfer_state(p2p_transfer:p2p_transfer_state(), ff_entity_context:context()) ->
    ff_proto_p2p_transfer_thrift:'P2PTransferState'().
marshal_p2p_transfer_state(P2PTransferState, Ctx) ->
    CashFlow = p2p_transfer:effective_final_cash_flow(P2PTransferState),
    Adjustments = p2p_transfer:adjustments(P2PTransferState),
    Sessions = p2p_transfer:sessions(P2PTransferState),
    #p2p_transfer_P2PTransferState{
        id = marshal(id, p2p_transfer:id(P2PTransferState)),
        owner = marshal(id, p2p_transfer:owner(P2PTransferState)),
        sender = marshal(participant, p2p_transfer:sender(P2PTransferState)),
        receiver = marshal(participant, p2p_transfer:receiver(P2PTransferState)),
        body = marshal(cash, p2p_transfer:body(P2PTransferState)),
        status = marshal(status, p2p_transfer:status(P2PTransferState)),
        domain_revision = marshal(domain_revision, p2p_transfer:domain_revision(P2PTransferState)),
        party_revision = marshal(party_revision, p2p_transfer:party_revision(P2PTransferState)),
        operation_timestamp = marshal(timestamp_ms, p2p_transfer:operation_timestamp(P2PTransferState)),
        created_at = marshal(timestamp_ms, p2p_transfer:created_at(P2PTransferState)),
        deadline = maybe_marshal(timestamp_ms, p2p_transfer:deadline(P2PTransferState)),
        quote = maybe_marshal(quote_state, p2p_transfer:quote(P2PTransferState)),
        client_info = maybe_marshal(client_info, p2p_transfer:client_info(P2PTransferState)),
        external_id = maybe_marshal(id, p2p_transfer:external_id(P2PTransferState)),
        metadata = marshal(ctx, p2p_transfer:metadata(P2PTransferState)),
        sessions = [marshal(session_state, S) || S <- Sessions],
        effective_route = maybe_marshal(route, p2p_transfer:route(P2PTransferState)),
        effective_final_cash_flow = ff_cash_flow_codec:marshal(final_cash_flow, CashFlow),
        adjustments = [ff_p2p_transfer_adjustment_codec:marshal(adjustment_state, A) || A <- Adjustments],
        context = marshal(ctx, Ctx)
    }.

-spec unmarshal_p2p_transfer_params(ff_proto_p2p_transfer_thrift:'P2PTransferParams'()) ->
    p2p_transfer_machine:params().
unmarshal_p2p_transfer_params(#p2p_transfer_P2PTransferParams{
    id = ID,
    identity_id = IdentityID,
    sender = Sender,
    receiver = Receiver,
    body = Body,
    quote = Quote,
    deadline = Deadline,
    client_info = ClientInfo,
    external_id = ExternalID,
    metadata = Metadata
}) ->
    genlib_map:compact(#{
        id => unmarshal(id, ID),
        identity_id => unmarshal(id, IdentityID),
        body => unmarshal(cash, Body),
        sender => unmarshal(participant, Sender),
        receiver => unmarshal(participant, Receiver),
        quote => maybe_unmarshal(quote, Quote),
        client_info => maybe_unmarshal(client_info, ClientInfo),
        deadline => maybe_unmarshal(timestamp_ms, Deadline),
        external_id => maybe_unmarshal(id, ExternalID),
        metadata => maybe_unmarshal(ctx, Metadata)
    }).

-spec marshal_event(p2p_transfer_machine:event()) -> ff_proto_p2p_transfer_thrift:'Event'().
marshal_event({EventID, {ev, Timestamp, Change}}) ->
    #p2p_transfer_Event{
        event = ff_codec:marshal(event_id, EventID),
        occured_at = ff_codec:marshal(timestamp, Timestamp),
        change = marshal(change, Change)
    }.

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) -> ff_codec:encoded_value().
marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];
marshal(timestamped_change, {ev, Timestamp, Change}) ->
    #p2p_transfer_TimestampedChange{
        change = marshal(change, Change),
        occured_at = ff_codec:marshal(timestamp, Timestamp)
    };
marshal(change, {created, Transfer}) ->
    {created, #p2p_transfer_CreatedChange{p2p_transfer = marshal(transfer, Transfer)}};
marshal(change, {status_changed, Status}) ->
    {status_changed, #p2p_transfer_StatusChange{status = marshal(status, Status)}};
marshal(change, {resource_got, Sender, Receiver}) ->
    {resource, marshal(resource_got, {Sender, Receiver})};
marshal(change, {risk_score_changed, RiskScore}) ->
    {risk_score, #p2p_transfer_RiskScoreChange{score = marshal(risk_score, RiskScore)}};
marshal(change, {route_changed, Route}) ->
    {route, #p2p_transfer_RouteChange{route = marshal(route, Route)}};
marshal(change, {p_transfer, TransferChange}) ->
    {transfer, #p2p_transfer_TransferChange{payload = ff_p_transfer_codec:marshal(change, TransferChange)}};
marshal(change, {session, Session}) ->
    {session, marshal(session, Session)};
marshal(change, {adjustment, #{id := ID, payload := Payload}}) ->
    {adjustment, #p2p_transfer_AdjustmentChange{
        id = marshal(id, ID),
        payload = ff_p2p_transfer_adjustment_codec:marshal(change, Payload)
    }};
marshal(
    transfer,
    Transfer = #{
        id := ID,
        status := Status,
        owner := Owner,
        sender := Sender,
        receiver := Receiver,
        body := Body,
        domain_revision := DomainRevision,
        party_revision := PartyRevision,
        operation_timestamp := OperationTimestamp,
        created_at := CreatedAt
    }
) ->
    ExternalID = maps:get(external_id, Transfer, undefined),
    Quote = maps:get(quote, Transfer, undefined),
    Deadline = maps:get(deadline, Transfer, undefined),
    ClientInfo = maps:get(client_info, Transfer, undefined),
    Metadata = maps:get(metadata, Transfer, undefined),

    #p2p_transfer_P2PTransfer{
        id = marshal(id, ID),
        owner = marshal(id, Owner),
        sender = marshal(participant, Sender),
        receiver = marshal(participant, Receiver),
        body = marshal(cash, Body),
        status = marshal(status, Status),
        created_at = marshal(timestamp_ms, CreatedAt),
        domain_revision = marshal(domain_revision, DomainRevision),
        party_revision = marshal(party_revision, PartyRevision),
        operation_timestamp = marshal(timestamp_ms, OperationTimestamp),
        quote = maybe_marshal(quote_state, Quote),
        external_id = maybe_marshal(id, ExternalID),
        client_info = maybe_marshal(client_info, ClientInfo),
        deadline = maybe_marshal(timestamp_ms, Deadline),
        metadata = maybe_marshal(ctx, Metadata)
    };
marshal(quote_state, Quote) ->
    #p2p_transfer_QuoteState{
        fees = maybe_marshal(fees, genlib_map:get(fees, Quote)),
        created_at = marshal(timestamp_ms, maps:get(created_at, Quote)),
        expires_on = marshal(timestamp_ms, maps:get(expires_on, Quote)),
        sender = marshal(resource_descriptor, maps:get(sender, Quote)),
        receiver = marshal(resource_descriptor, maps:get(receiver, Quote))
    };
marshal(quote, Quote) ->
    #p2p_transfer_Quote{
        body = marshal(cash, maps:get(amount, Quote)),
        fees = maybe_marshal(fees, genlib_map:get(fees, Quote)),
        created_at = marshal(timestamp_ms, maps:get(created_at, Quote)),
        expires_on = marshal(timestamp_ms, maps:get(expires_on, Quote)),
        identity_id = marshal(id, maps:get(identity_id, Quote)),
        party_revision = marshal(party_revision, maps:get(party_revision, Quote)),
        domain_revision = marshal(domain_revision, maps:get(domain_revision, Quote)),
        sender = marshal(compact_resource, maps:get(sender, Quote)),
        receiver = marshal(compact_resource, maps:get(receiver, Quote))
    };
marshal(compact_resource, {bank_card, BankCardResource}) ->
    #{
        token := Token,
        bin_data_id := BinDataId
    } = BankCardResource,
    marshal(resource, {bank_card, #{bank_card => #{token => Token, bin_data_id => BinDataId}}});
marshal(status, Status) ->
    ff_p2p_transfer_status_codec:marshal(status, Status);
marshal(participant, {raw, #{resource_params := ResourceParams} = Raw}) ->
    ContactInfo = maps:get(contact_info, Raw),
    {resource, #p2p_transfer_RawResource{
        resource = marshal(resource, ResourceParams),
        contact_info = marshal(contact_info, ContactInfo)
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
    #p2p_transfer_ResourceChange{
        payload =
            {got, #p2p_transfer_ResourceGot{
                sender = marshal(resource, Sender),
                receiver = marshal(resource, Receiver)
            }}
    };
marshal(risk_score, low) ->
    low;
marshal(risk_score, high) ->
    high;
marshal(risk_score, fatal) ->
    fatal;
marshal(route, #{provider_id := ProviderID}) ->
    #p2p_transfer_Route{
        provider_id = marshal(integer, ProviderID)
    };
marshal(session, {SessionID, started}) ->
    #p2p_transfer_SessionChange{
        id = marshal(id, SessionID),
        payload = {started, #p2p_transfer_SessionStarted{}}
    };
marshal(session, {SessionID, {finished, SessionResult}}) ->
    #p2p_transfer_SessionChange{
        id = marshal(id, SessionID),
        payload =
            {finished, #p2p_transfer_SessionFinished{
                result = marshal(session_result, SessionResult)
            }}
    };
marshal(session_state, Session) ->
    #p2p_transfer_SessionState{
        id = marshal(id, maps:get(id, Session)),
        result = maybe_marshal(session_result, maps:get(result, Session, undefined))
    };
marshal(session_result, success) ->
    {succeeded, #p2p_transfer_SessionSucceeded{}};
marshal(session_result, {failure, Failure}) ->
    {failed, #p2p_transfer_SessionFailed{failure = marshal(failure, Failure)}};
marshal(ctx, Ctx) ->
    maybe_marshal(context, Ctx);
marshal(T, V) ->
    ff_codec:marshal(T, V).

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) -> ff_codec:decoded_value().
unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];
unmarshal(repair_scenario, {add_events, #p2p_transfer_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events,
        genlib_map:compact(#{
            events => unmarshal({list, change}, Events),
            action => maybe_unmarshal(complex_action, Action)
        })};
unmarshal(timestamped_change, TimestampedChange) ->
    Timestamp = ff_codec:unmarshal(timestamp, TimestampedChange#p2p_transfer_TimestampedChange.occured_at),
    Change = unmarshal(change, TimestampedChange#p2p_transfer_TimestampedChange.change),
    {ev, Timestamp, Change};
unmarshal(change, {created, #p2p_transfer_CreatedChange{p2p_transfer = Transfer}}) ->
    {created, unmarshal(transfer, Transfer)};
unmarshal(change, {status_changed, #p2p_transfer_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};
unmarshal(
    change,
    {resource, #p2p_transfer_ResourceChange{
        payload = {got, #p2p_transfer_ResourceGot{sender = Sender, receiver = Receiver}}
    }}
) ->
    unmarshal(resource_got, {Sender, Receiver});
unmarshal(change, {risk_score, #p2p_transfer_RiskScoreChange{score = RiskScore}}) ->
    {risk_score_changed, unmarshal(risk_score, RiskScore)};
unmarshal(change, {route, #p2p_transfer_RouteChange{route = Route}}) ->
    {route_changed, unmarshal(route, Route)};
unmarshal(change, {transfer, #p2p_transfer_TransferChange{payload = TransferChange}}) ->
    {p_transfer, ff_p_transfer_codec:unmarshal(change, TransferChange)};
unmarshal(change, {session, #p2p_transfer_SessionChange{id = ID, payload = Payload}}) ->
    {session, unmarshal(session, {ID, Payload})};
unmarshal(change, {adjustment, Change}) ->
    Payload = ff_p2p_transfer_adjustment_codec:unmarshal(change, Change#p2p_transfer_AdjustmentChange.payload),
    {adjustment, #{
        id => unmarshal(id, Change#p2p_transfer_AdjustmentChange.id),
        payload => Payload
    }};
unmarshal(transfer, #p2p_transfer_P2PTransfer{
    id = ID,
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
    external_id = ExternalID,
    deadline = Deadline,
    metadata = Metadata
}) ->
    genlib_map:compact(#{
        version => 3,
        id => unmarshal(id, ID),
        status => unmarshal(status, Status),
        owner => unmarshal(id, Owner),
        body => unmarshal(cash, Body),
        sender => unmarshal(participant, Sender),
        receiver => unmarshal(participant, Receiver),
        domain_revision => unmarshal(domain_revision, DomainRevision),
        party_revision => unmarshal(domain_revision, PartyRevision),
        operation_timestamp => unmarshal(timestamp_ms, OperationTimestamp),
        created_at => unmarshal(timestamp_ms, CreatedAt),
        quote => maybe_unmarshal(quote_state, Quote),
        client_info => maybe_unmarshal(client_info, ClientInfo),
        external_id => maybe_unmarshal(id, ExternalID),
        deadline => maybe_unmarshal(timestamp_ms, Deadline),
        metadata => maybe_unmarshal(ctx, Metadata)
    });
unmarshal(quote_state, Quote) ->
    genlib_map:compact(#{
        fees => maybe_unmarshal(fees, Quote#p2p_transfer_QuoteState.fees),
        created_at => unmarshal(timestamp_ms, Quote#p2p_transfer_QuoteState.created_at),
        expires_on => unmarshal(timestamp_ms, Quote#p2p_transfer_QuoteState.expires_on),
        sender => unmarshal(resource_descriptor, Quote#p2p_transfer_QuoteState.sender),
        receiver => unmarshal(resource_descriptor, Quote#p2p_transfer_QuoteState.receiver)
    });
unmarshal(quote, Quote) ->
    genlib_map:compact(#{
        amount => unmarshal(cash, Quote#p2p_transfer_Quote.body),
        fees => maybe_unmarshal(fees, Quote#p2p_transfer_Quote.fees),
        created_at => unmarshal(timestamp_ms, Quote#p2p_transfer_Quote.created_at),
        expires_on => unmarshal(timestamp_ms, Quote#p2p_transfer_Quote.expires_on),
        identity_id => unmarshal(id, Quote#p2p_transfer_Quote.identity_id),
        party_revision => unmarshal(party_revision, Quote#p2p_transfer_Quote.party_revision),
        domain_revision => unmarshal(domain_revision, Quote#p2p_transfer_Quote.domain_revision),
        sender => unmarshal(compact_resource, Quote#p2p_transfer_Quote.sender),
        receiver => unmarshal(compact_resource, Quote#p2p_transfer_Quote.receiver)
    });
unmarshal(compact_resource, Resource) ->
    {bank_card, #{bank_card := BankCard}} = unmarshal(resource, Resource),
    {bank_card, #{
        token => maps:get(token, BankCard),
        bin_data_id => maps:get(bin_data_id, BankCard)
    }};
unmarshal(status, Status) ->
    ff_p2p_transfer_status_codec:unmarshal(status, Status);
unmarshal(resource_got, {Sender, Receiver}) ->
    {resource_got, unmarshal(resource, Sender), unmarshal(resource, Receiver)};
unmarshal(
    participant,
    {resource, #p2p_transfer_RawResource{
        resource = Resource,
        contact_info = ContactInfo
    }}
) ->
    {raw,
        genlib_map:compact(#{
            resource_params => unmarshal(resource, Resource),
            contact_info => unmarshal(contact_info, ContactInfo)
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
    #{
        version => 1,
        provider_id => unmarshal(integer, ProviderID)
    };
unmarshal(session, {SessionID, {started, #p2p_transfer_SessionStarted{}}}) ->
    {SessionID, started};
unmarshal(session, {SessionID, {finished, #p2p_transfer_SessionFinished{result = SessionResult}}}) ->
    {SessionID, {finished, unmarshal(session_result, SessionResult)}};
unmarshal(session_state, Session) ->
    genlib_map:compact(#{
        id => unmarshal(id, Session#p2p_transfer_SessionState.id),
        result => maybe_unmarshal(session_result, Session#p2p_transfer_SessionState.result)
    });
unmarshal(session_result, {succeeded, #p2p_transfer_SessionSucceeded{}}) ->
    success;
unmarshal(session_result, {failed, #p2p_transfer_SessionFailed{failure = Failure}}) ->
    {failure, unmarshal(failure, Failure)};
unmarshal(ctx, Ctx) ->
    maybe_unmarshal(context, Ctx);
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
        new_status => #{
            new_status => succeeded
        }
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

    Resource =
        {bank_card, #{
            bank_card => #{
                token => genlib:unique(),
                bin_data_id => {binary, genlib:unique()}
            }
        }},

    Participant =
        {raw, #{
            resource_params => Resource,
            contact_info => #{}
        }},

    Quote = #{
        fees => #{
            fees => #{
                surplus => {200, <<"RUB">>}
            }
        },
        created_at => ff_time:now(),
        expires_on => ff_time:now() + 1000,
        sender => {bank_card, nil},
        receiver => {bank_card, []}
    },

    P2PTransfer = #{
        version => 3,
        id => genlib:unique(),
        status => pending,
        owner => genlib:unique(),
        body => {123, <<"RUB">>},
        created_at => ff_time:now(),
        sender => Participant,
        receiver => Participant,
        quote => Quote,
        domain_revision => 123,
        party_revision => 321,
        operation_timestamp => ff_time:now(),
        external_id => genlib:unique(),
        deadline => ff_time:now(),
        metadata => #{<<"Hello">> => <<"World">>}
    },

    PTransfer = #{
        id => genlib:unique(),
        final_cash_flow => FinalCashFlow
    },

    Changes = [
        {created, P2PTransfer},
        {resource_got, Resource, Resource},
        {risk_score_changed, low},
        {route_changed, #{version => 1, provider_id => 1}},
        {p_transfer, {created, PTransfer}},
        {session, {genlib:unique(), started}},
        {status_changed, succeeded},
        {adjustment, #{id => genlib:unique(), payload => {created, Adjustment}}}
    ],

    Type = {struct, union, {ff_proto_p2p_transfer_thrift, 'Change'}},
    Binaries = [ff_proto_utils:serialize(Type, C) || C <- marshal({list, change}, Changes)],
    Decoded = [ff_proto_utils:deserialize(Type, B) || B <- Binaries],
    ?assertEqual(Changes, unmarshal({list, change}, Decoded)).

-spec p2p_timestamped_change_codec_test() -> _.
p2p_timestamped_change_codec_test() ->
    Resource =
        {bank_card, #{
            bank_card => #{
                token => genlib:unique(),
                bin_data_id => {binary, genlib:unique()}
            }
        }},

    Participant =
        {raw, #{
            resource_params => Resource,
            contact_info => #{}
        }},

    P2PTransfer = #{
        version => 3,
        id => genlib:unique(),
        status => pending,
        owner => genlib:unique(),
        body => {123, <<"RUB">>},
        created_at => ff_time:now(),
        sender => Participant,
        receiver => Participant,
        quote => #{
            created_at => ff_time:now(),
            expires_on => ff_time:now() + 1000,
            sender => {bank_card, nil},
            receiver => {bank_card, []}
        },
        domain_revision => 123,
        party_revision => 321,
        operation_timestamp => ff_time:now(),
        external_id => genlib:unique(),
        metadata => #{<<"Hello">> => <<"World">>}
    },
    Change = {created, P2PTransfer},
    TimestampedChange = {ev, machinery_time:now(), Change},
    Type = {struct, struct, {ff_proto_p2p_transfer_thrift, 'TimestampedChange'}},
    Binary = ff_proto_utils:serialize(Type, marshal(timestamped_change, TimestampedChange)),
    Decoded = ff_proto_utils:deserialize(Type, Binary),
    ?assertEqual(TimestampedChange, unmarshal(timestamped_change, Decoded)).

-endif.
