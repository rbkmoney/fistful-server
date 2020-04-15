-module(ff_w2w_transfer_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_w2w_transfer_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% Data transform

-define(to_session_event(SessionID, Payload),
    {session, #{id => SessionID, payload => Payload}}).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(event, {EventID, {ev, Timestamp, Change}}) ->
    #w2w_transfer_Event{
        event_id = ff_codec:marshal(event_id, EventID),
        occured_at = ff_codec:marshal(timestamp, Timestamp),
        change = marshal(change, Change)
    };

marshal(change, {created, W2WTransfer}) ->
    {created, #w2w_transfer_CreatedChange{w2w_transfer = marshal(w2w_transfer, W2WTransfer)}};
marshal(change, {status_changed, Status}) ->
    {status_changed, #w2w_transfer_StatusChange{status = ff_w2w_transfer_status_codec:marshal(status, Status)}};
marshal(change, {p_transfer, TransferChange}) ->
    {transfer, #w2w_transfer_TransferChange{payload = ff_p_transfer_codec:marshal(change, TransferChange)}};
marshal(change, {limit_check, Details}) ->
    {limit_check, #w2w_transfer_LimitCheckChange{details = ff_limit_check_codec:marshal(details, Details)}};
marshal(change, {adjustment, #{id := ID, payload := Payload}}) ->
    {adjustment, #w2w_transfer_AdjustmentChange{
        id = marshal(id, ID),
        payload = ff_w2w_transfer_adjustment_codec:marshal(change, Payload)
    }};

marshal(w2w_transfer, W2WTransfer) ->
    #w2w_transfer_W2WTransfer{
        id = marshal(id, w2w_transfer:id(W2WTransfer)),
        body = marshal(cash, w2w_transfer:body(W2WTransfer)),
        status = maybe_marshal(status, w2w_transfer:status(W2WTransfer)),
        wallet_to_id = marshal(id, w2w_transfer:wallet_to_id(W2WTransfer)),
        wallet_from_id = marshal(id, w2w_transfer:wallet_from_id(W2WTransfer)),
        external_id = maybe_marshal(id, w2w_transfer:external_id(W2WTransfer)),
        domain_revision = maybe_marshal(domain_revision, w2w_transfer:domain_revision(W2WTransfer)),
        party_revision = maybe_marshal(party_revision, w2w_transfer:party_revision(W2WTransfer)),
        created_at = maybe_marshal(timestamp_ms, w2w_transfer:created_at(W2WTransfer))
    };

marshal(status, Status) ->
    ff_w2w_transfer_status_codec:marshal(status, Status);

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #w2w_transfer_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, change}, Events),
        action => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(change, {created, #w2w_transfer_CreatedChange{w2w_transfer = W2WTransfer}}) ->
    {created, unmarshal(w2w_transfer, W2WTransfer)};
unmarshal(change, {status_changed, #w2w_transfer_StatusChange{status = W2WTransferStatus}}) ->
    {status_changed, unmarshal(status, W2WTransferStatus)};
unmarshal(change, {transfer, #w2w_transfer_TransferChange{payload = TransferChange}}) ->
    {p_transfer, ff_p_transfer_codec:unmarshal(change, TransferChange)};
unmarshal(change, {limit_check, #w2w_transfer_LimitCheckChange{details = Details}}) ->
    {limit_check, ff_limit_check_codec:unmarshal(details, Details)};
unmarshal(change, {adjustment, Change}) ->
    {revert, #{
        id => unmarshal(id, Change#w2w_transfer_AdjustmentChange.id),
        payload => ff_w2w_transfer_adjustment_codec:unmarshal(id, Change#w2w_transfer_AdjustmentChange.payload)
    }};

unmarshal(status, Status) ->
    ff_w2w_transfer_status_codec:unmarshal(status, Status);

unmarshal(w2w_transfer, W2WTransfer) ->
    #{
        id => unmarshal(id, W2WTransfer#w2w_transfer_W2WTransfer.id),
        body => unmarshal(cash, W2WTransfer#w2w_transfer_W2WTransfer.body),
        status => maybe_marshal(status, W2WTransfer#w2w_transfer_W2WTransfer.status),
        wallet_to_id => unmarshal(id, W2WTransfer#w2w_transfer_W2WTransfer.wallet_to_id),
        wallet_from_id => unmarshal(id, W2WTransfer#w2w_transfer_W2WTransfer.wallet_from_id),
        external_id => maybe_unmarshal(id, W2WTransfer#w2w_transfer_W2WTransfer.external_id),
        party_revision => maybe_unmarshal(party_revision, W2WTransfer#w2w_transfer_W2WTransfer.party_revision),
        domain_revision => maybe_unmarshal(domain_revision, W2WTransfer#w2w_transfer_W2WTransfer.domain_revision),
        created_at => maybe_unmarshal(timestamp_ms, W2WTransfer#w2w_transfer_W2WTransfer.created_at)
    };

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

-spec w2w_transfer_symmetry_test() -> _.
w2w_transfer_symmetry_test() ->
    Encoded = #w2w_transfer_W2WTransfer{
        body = #'Cash'{
            amount = 10101,
            currency = #'CurrencyRef'{ symbolic_code = <<"Banana Republic">> }
        },
        wallet_from_id = genlib:unique(),
        wallet_to_id = genlib:unique(),
        external_id = undefined,
        status = {pending, #w2w_status_Pending{}},
        id = genlib:unique(),
        domain_revision = 24500062,
        party_revision = 140028,
        created_at = <<"2025-01-01T00:00:00.001Z">>
    },
    ?assertEqual(Encoded, marshal(w2w_transfer, unmarshal(w2w_transfer, Encoded))).

-endif.
