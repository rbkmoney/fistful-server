-module(ff_deposit_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_deposit_thrift.hrl").

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

marshal(event, {EventID, _EventTimestamp, {ev, Timestamp, Change}}) ->
    #deposit_Event{
        event_id = ff_codec:marshal(event_id, EventID),
        occured_at = ff_codec:marshal(timestamp, Timestamp),
        change = marshal(change, Change)
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
        external_id = marshal(id, ff_deposit:external_id(Deposit))
    };
marshal(deposit_params, DepositParams) ->
    #deposit_DepositParams{
        id = marshal(id, maps:get(id, DepositParams)),
        body = marshal(cash, maps:get(body, DepositParams)),
        wallet_id = marshal(id, maps:get(wallet_id, DepositParams)),
        source_id = marshal(id, maps:get(source_id, DepositParams)),
        external_id = maybe_marshal(id, maps:get(external_id, DepositParams, undefined))
    };
marshal(deposit_state, DepositState) ->
    #{
        deposit := Deposit,
        context := Context
    } = DepositState,
    CashFlow = ff_deposit:effective_final_cash_flow(Deposit),
    Reverts = ff_deposit:reverts(Deposit),
    Adjustments = ff_deposit:adjustments(Deposit),
    #deposit_DepositState{
        deposit = marshal(deposit, Deposit),
        context = marshal(context, Context),
        effective_final_cash_flow = ff_cash_flow_codec:marshal(final_cash_flow, CashFlow),
        reverts = [ff_deposit_revert_codec:marshal(revert_state, R) || R <- Reverts],
        adjustments = [ff_deposit_adjustment_codec:marshal(adjustment_state, A) || A <- Adjustments]
    };

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
        actions => maybe_unmarshal(complex_action, Action)
    })};

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
    #{
        id => unmarshal(id, Deposit#deposit_Deposit.id),
        body => unmarshal(cash, Deposit#deposit_Deposit.body),
        status => maybe_marshal(status, Deposit#deposit_Deposit.status),
        params => genlib_map:compact(#{
            wallet_id => unmarshal(id, Deposit#deposit_Deposit.wallet_id),
            source_id => unmarshal(id, Deposit#deposit_Deposit.source_id),
            external_id => maybe_marshal(id, Deposit#deposit_Deposit.external_id)
        })
    };

unmarshal(deposit_params, DepositParams) ->
    genlib_map:compact(#{
        id => unmarshal(id, DepositParams#deposit_DepositParams.id),
        body => unmarshal(cash, DepositParams#deposit_DepositParams.body),
        wallet_id => unmarshal(id, DepositParams#deposit_DepositParams.wallet_id),
        source_id => unmarshal(id, DepositParams#deposit_DepositParams.source_id),
        external_id => maybe_marshal(id, DepositParams#deposit_DepositParams.external_id)
    });

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
        id = genlib:unique()
    },
    ?assertEqual(Encoded, marshal(deposit, unmarshal(deposit, Encoded))).

-spec deposit_params_symmetry_test() -> _.
deposit_params_symmetry_test() ->
    Encoded = #deposit_DepositParams{
        body = #'Cash'{
            amount = 10101,
            currency = #'CurrencyRef'{ symbolic_code = <<"Banana Republic">> }
        },
        source_id = genlib:unique(),
        wallet_id = genlib:unique(),
        external_id = undefined,
        id = genlib:unique()
    },
    ?assertEqual(Encoded, marshal(deposit_params, unmarshal(deposit_params, Encoded))).

-endif.
