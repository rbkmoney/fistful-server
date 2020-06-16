-module(ff_deposit_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_deposit_thrift.hrl").

-export([marshal_deposit_state/2]).
-export([marshal/2]).
-export([unmarshal/2]).

%% Data transform

-define(to_session_event(SessionID, Payload),
    {session, #{id => SessionID, payload => Payload}}).

-spec marshal_deposit_state(ff_deposit:deposit_state(), ff_entity_context:context()) ->
    ff_proto_deposit_thrift:'DepositState'().

marshal_deposit_state(DepositState, Context) ->
    CashFlow = ff_deposit:effective_final_cash_flow(DepositState),
    Reverts = ff_deposit:reverts(DepositState),
    Adjustments = ff_deposit:adjustments(DepositState),
    #deposit_DepositState{
        id = marshal(id, ff_deposit:id(DepositState)),
        body = marshal(cash, ff_deposit:body(DepositState)),
        status = maybe_marshal(status, ff_deposit:status(DepositState)),
        wallet_id = marshal(id, ff_deposit:wallet_id(DepositState)),
        source_id = marshal(id, ff_deposit:source_id(DepositState)),
        external_id = maybe_marshal(id, ff_deposit:external_id(DepositState)),
        domain_revision = maybe_marshal(domain_revision, ff_deposit:domain_revision(DepositState)),
        party_revision = maybe_marshal(party_revision, ff_deposit:party_revision(DepositState)),
        created_at = maybe_marshal(timestamp_ms, ff_deposit:created_at(DepositState)),
        effective_final_cash_flow = ff_cash_flow_codec:marshal(final_cash_flow, CashFlow),
        reverts = [ff_deposit_revert_codec:marshal(revert_state, R) || R <- Reverts],
        adjustments = [ff_deposit_adjustment_codec:marshal(adjustment_state, A) || A <- Adjustments],
        context = marshal(ctx, Context),
        metadata = marshal(ctx, ff_deposit:metadata(DepositState))
    }.

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(event, {EventID, {ev, Timestamp, Change}}) ->
    #deposit_Event{
        event_id = ff_codec:marshal(event_id, EventID),
        occured_at = ff_codec:marshal(timestamp, Timestamp),
        change = marshal(change, Change)
    };

marshal(timestamped_change, {ev, Timestamp, Change}) ->
    #deposit_TimestampedChange{
        change = marshal(change, Change),
        occured_at = ff_codec:marshal(timestamp, Timestamp)
    };

marshal(change, {created, Deposit}) ->
    {created, #deposit_CreatedChange{deposit = marshal(deposit, Deposit)}};
marshal(change, {status_changed, Status}) ->
    {status_changed, #deposit_StatusChange{status = ff_deposit_status_codec:marshal(status, Status)}};
marshal(change, {p_transfer, TransferChange}) ->
    {transfer, #deposit_TransferChange{payload = ff_p_transfer_codec:marshal(change, TransferChange)}};
marshal(change, {limit_check, Details}) ->
    {limit_check, #deposit_LimitCheckChange{details = ff_limit_check_codec:marshal(details, Details)}};
marshal(change, {revert, #{id := ID, payload := Payload}}) ->
    {revert, #deposit_RevertChange{
        id = marshal(id, ID),
        payload = ff_deposit_revert_codec:marshal(change, Payload)
    }};
marshal(change, {adjustment, #{id := ID, payload := Payload}}) ->
    {adjustment, #deposit_AdjustmentChange{
        id = marshal(id, ID),
        payload = ff_deposit_adjustment_codec:marshal(change, Payload)
    }};

marshal(deposit, Deposit) ->
    #deposit_Deposit{
        id = marshal(id, ff_deposit:id(Deposit)),
        body = marshal(cash, ff_deposit:body(Deposit)),
        status = maybe_marshal(status, ff_deposit:status(Deposit)),
        wallet_id = marshal(id, ff_deposit:wallet_id(Deposit)),
        source_id = marshal(id, ff_deposit:source_id(Deposit)),
        external_id = maybe_marshal(id, ff_deposit:external_id(Deposit)),
        domain_revision = maybe_marshal(domain_revision, ff_deposit:domain_revision(Deposit)),
        party_revision = maybe_marshal(party_revision, ff_deposit:party_revision(Deposit)),
        created_at = maybe_marshal(timestamp_ms, ff_deposit:created_at(Deposit)),
        metadata = maybe_marshal(ctx, ff_deposit:metadata(Deposit))
    };
marshal(deposit_params, DepositParams) ->
    #deposit_DepositParams{
        id = marshal(id, maps:get(id, DepositParams)),
        body = marshal(cash, maps:get(body, DepositParams)),
        wallet_id = marshal(id, maps:get(wallet_id, DepositParams)),
        source_id = marshal(id, maps:get(source_id, DepositParams)),
        external_id = maybe_marshal(id, maps:get(external_id, DepositParams, undefined)),
        metadata = maybe_marshal(ctx, maps:get(metadata, DepositParams, undefined))
    };

marshal(ctx, Ctx) ->
    maybe_marshal(context, Ctx);

marshal(status, Status) ->
    ff_deposit_status_codec:marshal(status, Status);

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #deposit_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, change}, Events),
        action => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(timestamped_change, TimestampedChange) ->
    Timestamp = ff_codec:unmarshal(timestamp, TimestampedChange#deposit_TimestampedChange.occured_at),
    Change = unmarshal(change, TimestampedChange#deposit_TimestampedChange.change),
    {ev, Timestamp, Change};

unmarshal(change, {created, #deposit_CreatedChange{deposit = Deposit}}) ->
    {created, unmarshal(deposit, Deposit)};
unmarshal(change, {status_changed, #deposit_StatusChange{status = DepositStatus}}) ->
    {status_changed, unmarshal(status, DepositStatus)};
unmarshal(change, {transfer, #deposit_TransferChange{payload = TransferChange}}) ->
    {p_transfer, ff_p_transfer_codec:unmarshal(change, TransferChange)};
unmarshal(change, {limit_check, #deposit_LimitCheckChange{details = Details}}) ->
    {limit_check, ff_limit_check_codec:unmarshal(details, Details)};
unmarshal(change, {revert, Change}) ->
    {revert, #{
        id => unmarshal(id, Change#deposit_RevertChange.id),
        payload => ff_deposit_revert_codec:unmarshal(id, Change#deposit_RevertChange.payload)
    }};
unmarshal(change, {adjustment, Change}) ->
    {revert, #{
        id => unmarshal(id, Change#deposit_AdjustmentChange.id),
        payload => ff_deposit_adjustment_codec:unmarshal(id, Change#deposit_AdjustmentChange.payload)
    }};

unmarshal(status, Status) ->
    ff_deposit_status_codec:unmarshal(status, Status);

unmarshal(deposit, Deposit) ->
    genlib_map:compact(#{
        version => 2,
        id => unmarshal(id, Deposit#deposit_Deposit.id),
        body => unmarshal(cash, Deposit#deposit_Deposit.body),
        status => maybe_unmarshal(status, Deposit#deposit_Deposit.status),
        params => genlib_map:compact(#{
            wallet_id => unmarshal(id, Deposit#deposit_Deposit.wallet_id),
            source_id => unmarshal(id, Deposit#deposit_Deposit.source_id)
        }),
        external_id => maybe_unmarshal(id, Deposit#deposit_Deposit.external_id),
        party_revision => maybe_unmarshal(party_revision, Deposit#deposit_Deposit.party_revision),
        domain_revision => maybe_unmarshal(domain_revision, Deposit#deposit_Deposit.domain_revision),
        created_at => maybe_unmarshal(timestamp_ms, Deposit#deposit_Deposit.created_at),
        metadata => maybe_unmarshal(ctx, Deposit#deposit_Deposit.metadata)
    });

unmarshal(deposit_params, DepositParams) ->
    genlib_map:compact(#{
        id => unmarshal(id, DepositParams#deposit_DepositParams.id),
        body => unmarshal(cash, DepositParams#deposit_DepositParams.body),
        wallet_id => unmarshal(id, DepositParams#deposit_DepositParams.wallet_id),
        source_id => unmarshal(id, DepositParams#deposit_DepositParams.source_id),
        metadata => maybe_unmarshal(ctx, DepositParams#deposit_DepositParams.metadata),
        external_id => maybe_unmarshal(id, DepositParams#deposit_DepositParams.external_id)
    });

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

-spec deposit_symmetry_test() -> _.
deposit_symmetry_test() ->
    Encoded = #deposit_Deposit{
        body = #'Cash'{
            amount = 10101,
            currency = #'CurrencyRef'{ symbolic_code = <<"Banana Republic">> }
        },
        source_id = genlib:unique(),
        wallet_id = genlib:unique(),
        external_id = undefined,
        status = {pending, #dep_status_Pending{}},
        id = genlib:unique(),
        domain_revision = 24500062,
        party_revision = 140028,
        created_at = <<"2025-01-01T00:00:00.001Z">>
    },
    ?assertEqual(Encoded, marshal(deposit, unmarshal(deposit, Encoded))).

-spec deposit_params_symmetry_test() -> _.
deposit_params_symmetry_test() ->
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Encoded = #deposit_DepositParams{
        body = #'Cash'{
            amount = 10101,
            currency = #'CurrencyRef'{ symbolic_code = <<"Banana Republic">> }
        },
        source_id = genlib:unique(),
        wallet_id = genlib:unique(),
        external_id = undefined,
        id = genlib:unique(),
        metadata = Metadata
    },
    ?assertEqual(Encoded, marshal(deposit_params, unmarshal(deposit_params, Encoded))).

-spec deposit_timestamped_change_codec_test() -> _.
deposit_timestamped_change_codec_test() ->
    Deposit = #{
        version => 2,
        id => genlib:unique(),
        status => pending,
        body => {123, <<"RUB">>},
        created_at => ff_time:now(),
        domain_revision => 123,
        party_revision => 321,
        params => #{
            wallet_id => genlib:unique(),
            source_id => genlib:unique()
        },
        external_id => genlib:unique()
    },
    Change = {created, Deposit},
    TimestampedChange = {ev, machinery_time:now(), Change},
    Type = {struct, struct, {ff_proto_deposit_thrift, 'TimestampedChange'}},
    Binary = ff_proto_utils:serialize(Type, marshal(timestamped_change, TimestampedChange)),
    Decoded = ff_proto_utils:deserialize(Type, Binary),
    ?assertEqual(TimestampedChange, unmarshal(timestamped_change, Decoded)).

-endif.
