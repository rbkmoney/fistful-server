-module(ff_withdrawal_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").

-export([unmarshal_quote_params/1]).

-export([unmarshal_withdrawal_params/1]).
-export([marshal_withdrawal_params/1]).

-export([marshal_withdrawal_state/2]).
-export([marshal_event/1]).

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec unmarshal_quote_params(ff_proto_withdrawal_thrift:'QuoteParams'()) ->
    ff_withdrawal:quote_params().

unmarshal_quote_params(Params) ->
    genlib_map:compact(#{
        wallet_id      => unmarshal(id, Params#wthd_QuoteParams.wallet_id),
        currency_from  => unmarshal(currency_ref, Params#wthd_QuoteParams.currency_from),
        currency_to    => unmarshal(currency_ref, Params#wthd_QuoteParams.currency_to),
        body           => unmarshal(cash, Params#wthd_QuoteParams.body),
        destination_id => maybe_unmarshal(id, Params#wthd_QuoteParams.destination_id),
        external_id    => maybe_unmarshal(id, Params#wthd_QuoteParams.external_id)
    }).

-spec marshal_withdrawal_params(ff_withdrawal:params()) ->
    ff_proto_withdrawal_thrift:'WithdrawalParams'().

marshal_withdrawal_params(Params) ->
    #wthd_WithdrawalParams{
        id             = marshal(id, maps:get(id, Params)),
        wallet_id      = marshal(id, maps:get(wallet_id, Params)),
        destination_id = marshal(id, maps:get(destination_id, Params)),
        body           = marshal(cash, maps:get(body, Params)),
        external_id    = maybe_marshal(id, maps:get(external_id, Params, undefined)),
        metadata       = maybe_marshal(ctx, maps:get(metadata, Params, undefined))
    }.

-spec unmarshal_withdrawal_params(ff_proto_withdrawal_thrift:'WithdrawalParams'()) ->
    ff_withdrawal:params().

unmarshal_withdrawal_params(Params) ->
    genlib_map:compact(#{
        id             => unmarshal(id, Params#wthd_WithdrawalParams.id),
        wallet_id      => unmarshal(id, Params#wthd_WithdrawalParams.wallet_id),
        destination_id => unmarshal(id, Params#wthd_WithdrawalParams.destination_id),
        body           => unmarshal(cash, Params#wthd_WithdrawalParams.body),
        quote          => maybe_unmarshal(quote, Params#wthd_WithdrawalParams.quote),
        external_id    => maybe_unmarshal(id, Params#wthd_WithdrawalParams.external_id),
        metadata       => maybe_unmarshal(ctx, Params#wthd_WithdrawalParams.metadata)
    }).

-spec marshal_withdrawal_state(ff_withdrawal:withdrawal_state(), ff_entity_context:context()) ->
    ff_proto_withdrawal_thrift:'WithdrawalState'().

marshal_withdrawal_state(WithdrawalState, Context) ->
    CashFlow = ff_withdrawal:effective_final_cash_flow(WithdrawalState),
    Adjustments = ff_withdrawal:adjustments(WithdrawalState),
    Sessions = ff_withdrawal:sessions(WithdrawalState),
    #wthd_WithdrawalState{
        id = marshal(id, ff_withdrawal:id(WithdrawalState)),
        body = marshal(cash, ff_withdrawal:body(WithdrawalState)),
        wallet_id = marshal(id, ff_withdrawal:wallet_id(WithdrawalState)),
        destination_id = marshal(id, ff_withdrawal:destination_id(WithdrawalState)),
        route = maybe_marshal(route, ff_withdrawal:route(WithdrawalState)),
        external_id = maybe_marshal(id, ff_withdrawal:external_id(WithdrawalState)),
        domain_revision = maybe_marshal(domain_revision, ff_withdrawal:domain_revision(WithdrawalState)),
        party_revision = maybe_marshal(party_revision, ff_withdrawal:party_revision(WithdrawalState)),
        created_at = maybe_marshal(timestamp_ms, ff_withdrawal:created_at(WithdrawalState)),
        status = maybe_marshal(status, ff_withdrawal:status(WithdrawalState)),
        sessions = [marshal(session_state, S) || S <- Sessions],
        effective_route = maybe_marshal(route, ff_withdrawal:route(WithdrawalState)),
        effective_final_cash_flow = ff_cash_flow_codec:marshal(final_cash_flow, CashFlow),
        adjustments = [ff_withdrawal_adjustment_codec:marshal(adjustment_state, A) || A <- Adjustments],
        context = marshal(ctx, Context),
        metadata = marshal(ctx, ff_withdrawal:metadata(WithdrawalState)),
        quote = maybe_marshal(quote_state, ff_withdrawal:quote(WithdrawalState))
    }.

-spec marshal_event(ff_withdrawal_machine:event()) ->
    ff_proto_withdrawal_thrift:'Event'().

marshal_event({EventID, {ev, Timestamp, Change}}) ->
    #wthd_Event{
        event_id = ff_codec:marshal(event_id, EventID),
        occured_at = ff_codec:marshal(timestamp, Timestamp),
        change = marshal(change, Change)
    }.

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(timestamped_change, {ev, Timestamp, Change}) ->
    #wthd_TimestampedChange{
        change = marshal(change, Change),
        occured_at = ff_codec:marshal(timestamp, Timestamp)
    };

marshal(change, {created, Withdrawal}) ->
    {created, #wthd_CreatedChange{withdrawal = marshal(withdrawal, Withdrawal)}};
marshal(change, {status_changed, Status}) ->
    {status_changed, #wthd_StatusChange{status = ff_withdrawal_status_codec:marshal(status, Status)}};
marshal(change, {p_transfer, TransferChange}) ->
    {transfer, #wthd_TransferChange{payload = ff_p_transfer_codec:marshal(change, TransferChange)}};
marshal(change, {session_started, SessionID}) ->
    {session, #wthd_SessionChange{id = SessionID, payload = marshal(session_event, started)}};
marshal(change, {session_finished, {SessionID, SessionResult}}) ->
    {session, #wthd_SessionChange{id = SessionID, payload = marshal(session_event, {finished, SessionResult})}};
marshal(change, {route_changed, Route}) ->
    {route, #wthd_RouteChange{route = marshal(route, Route)}};
marshal(change, {limit_check, Details}) ->
    {limit_check, #wthd_LimitCheckChange{details = ff_limit_check_codec:marshal(details, Details)}};
marshal(change, {resource_got, Resource}) ->
    {resource, {got, #wthd_ResourceGot{resource = marshal(resource, Resource)}}};
marshal(change, {adjustment, #{id := ID, payload := Payload}}) ->
    {adjustment, #wthd_AdjustmentChange{
        id = marshal(id, ID),
        payload = ff_withdrawal_adjustment_codec:marshal(change, Payload)
    }};

marshal(withdrawal, Withdrawal) ->
    #wthd_Withdrawal{
        id = marshal(id, ff_withdrawal:id(Withdrawal)),
        body = marshal(cash, ff_withdrawal:body(Withdrawal)),
        wallet_id = marshal(id, ff_withdrawal:wallet_id(Withdrawal)),
        destination_id = marshal(id, ff_withdrawal:destination_id(Withdrawal)),
        route = maybe_marshal(route, ff_withdrawal:route(Withdrawal)),
        external_id = maybe_marshal(id, ff_withdrawal:external_id(Withdrawal)),
        domain_revision = maybe_marshal(domain_revision, ff_withdrawal:domain_revision(Withdrawal)),
        party_revision = maybe_marshal(party_revision, ff_withdrawal:party_revision(Withdrawal)),
        created_at = maybe_marshal(timestamp_ms, ff_withdrawal:created_at(Withdrawal)),
        metadata = maybe_marshal(ctx, ff_withdrawal:metadata(Withdrawal)),
        quote = maybe_marshal(quote_state, ff_withdrawal:quote(Withdrawal))
    };

marshal(route, Route) ->
    #{
        version := 1,
        provider_id := ProviderID
    } = Route,
    #wthd_Route{
        provider_id = marshal(provider_id, ProviderID),
        terminal_id = maybe_marshal(terminal_id, genlib_map:get(terminal_id, Route)),
        provider_id_legacy = marshal(string, get_legacy_provider_id(Route))
    };

marshal(status, Status) ->
    ff_withdrawal_status_codec:marshal(status, Status);

marshal(session_event, started) ->
    {started, #wthd_SessionStarted{}};
marshal(session_event, {finished, Result}) ->
    {finished, #wthd_SessionFinished{result = marshal(session_result, Result)}};

marshal(session_result, success) ->
    {succeeded, #wthd_SessionSucceeded{}};
marshal(session_result, {success, TrxInfo}) ->
    {succeeded, #wthd_SessionSucceeded{trx_info = marshal(transaction_info, TrxInfo)}};
marshal(session_result, {failed, Failure}) ->
    {failed, #wthd_SessionFailed{failure = ff_codec:marshal(failure, Failure)}};

marshal(transaction_info, TrxInfo) ->
    ff_withdrawal_session_codec:marshal(transaction_info, TrxInfo);

marshal(session_state, Session) ->
    #wthd_SessionState{
        id = marshal(id, maps:get(id, Session)),
        result = maybe_marshal(session_result, maps:get(result, Session, undefined))
    };

marshal(quote_state, Quote) ->
    #wthd_QuoteState{
        cash_from  = marshal(cash, maps:get(cash_from, Quote)),
        cash_to    = marshal(cash, maps:get(cash_to, Quote)),
        created_at = maps:get(created_at, Quote), % already formatted
        expires_on = maps:get(expires_on, Quote),
        quote_data = maybe_marshal(msgpack, maps:get(quote_data, Quote, undefined)),
        route = maybe_marshal(route, maps:get(route, Quote, undefined)),
        resource = maybe_marshal(resource_descriptor, maps:get(resource_descriptor, Quote, undefined)),
        quote_data_legacy = marshal(ctx, #{})
    };
marshal(quote, Quote) ->
    #wthd_Quote{
        cash_from  = marshal(cash, maps:get(cash_from, Quote)),
        cash_to    = marshal(cash, maps:get(cash_to, Quote)),
        created_at = maps:get(created_at, Quote), % already formatted
        expires_on = maps:get(expires_on, Quote),
        quote_data = maybe_marshal(msgpack, genlib_map:get(quote_data, Quote)),
        route = maybe_marshal(route, genlib_map:get(route, Quote)),
        resource = maybe_marshal(resource_descriptor, genlib_map:get(resource_descriptor, Quote)),
        party_revision = maybe_marshal(party_revision, genlib_map:get(party_revision, Quote)),
        domain_revision = maybe_marshal(domain_revision, genlib_map:get(domain_revision, Quote)),
        operation_timestamp = maybe_marshal(timestamp_ms, genlib_map:get(operation_timestamp, Quote))
    };

marshal(ctx, Ctx) ->
    maybe_marshal(context, Ctx);

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(timestamped_change, TimestampedChange) ->
    Timestamp = ff_codec:unmarshal(timestamp, TimestampedChange#wthd_TimestampedChange.occured_at),
    Change = unmarshal(change, TimestampedChange#wthd_TimestampedChange.change),
    {ev, Timestamp, Change};

unmarshal(repair_scenario, {add_events, #wthd_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, change}, Events),
        action => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(change, {created, #wthd_CreatedChange{withdrawal = Withdrawal}}) ->
    {created, unmarshal(withdrawal, Withdrawal)};
unmarshal(change, {status_changed, #wthd_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};
unmarshal(change, {transfer, #wthd_TransferChange{payload = TransferChange}}) ->
    {p_transfer, ff_p_transfer_codec:unmarshal(change, TransferChange)};
unmarshal(change, {session, SessionChange}) ->
    unmarshal(session_event, SessionChange);
unmarshal(change, {route, #wthd_RouteChange{route = Route}}) ->
    {route_changed, unmarshal(route, Route)};
unmarshal(change, {limit_check, #wthd_LimitCheckChange{details = Details}}) ->
    {limit_check, ff_limit_check_codec:unmarshal(details, Details)};
unmarshal(change, {resource, {got, #wthd_ResourceGot{resource = Resource}}}) ->
    {resource_got, unmarshal(resource, Resource)};
unmarshal(change, {adjustment, Change}) ->
    {adjustment, #{
        id => unmarshal(id, Change#wthd_AdjustmentChange.id),
        payload => ff_withdrawal_adjustment_codec:unmarshal(change, Change#wthd_AdjustmentChange.payload)
    }};

unmarshal(withdrawal, Withdrawal = #wthd_Withdrawal{}) ->
    ff_withdrawal:gen(#{
        id => unmarshal(id, Withdrawal#wthd_Withdrawal.id),
        body => unmarshal(cash, Withdrawal#wthd_Withdrawal.body),
        params => genlib_map:compact(#{
            wallet_id => unmarshal(id, Withdrawal#wthd_Withdrawal.wallet_id),
            destination_id => unmarshal(id, Withdrawal#wthd_Withdrawal.destination_id),
            quote => maybe_unmarshal(quote_state, Withdrawal#wthd_Withdrawal.quote)
        }),
        route => maybe_unmarshal(route, Withdrawal#wthd_Withdrawal.route),
        external_id => maybe_unmarshal(id, Withdrawal#wthd_Withdrawal.external_id),
        domain_revision => maybe_unmarshal(domain_revision, Withdrawal#wthd_Withdrawal.domain_revision),
        party_revision => maybe_unmarshal(party_revision, Withdrawal#wthd_Withdrawal.party_revision),
        created_at => maybe_unmarshal(timestamp_ms, Withdrawal#wthd_Withdrawal.created_at),
        transfer_type => withdrawal,
        metadata => maybe_unmarshal(ctx, Withdrawal#wthd_Withdrawal.metadata)
    });

unmarshal(route, Route) ->
    genlib_map:compact(#{
        version => 1,
        provider_id => unmarshal(provider_id, Route#wthd_Route.provider_id),
        terminal_id => maybe_unmarshal(terminal_id, Route#wthd_Route.terminal_id),
        provider_id_legacy => maybe_unmarshal(string, Route#wthd_Route.provider_id_legacy)
    });

unmarshal(status, Status) ->
    ff_withdrawal_status_codec:unmarshal(status, Status);

unmarshal(session_event, #wthd_SessionChange{id = ID, payload = {started, #wthd_SessionStarted{}}}) ->
    {session_started, unmarshal(id, ID)};
unmarshal(session_event, #wthd_SessionChange{id = ID, payload = {finished, Finished}}) ->
    #wthd_SessionFinished{result = Result} = Finished,
    {session_finished, {unmarshal(id, ID), unmarshal(session_result, Result)}};

unmarshal(session_result, {succeeded, #wthd_SessionSucceeded{trx_info = undefined}}) ->
    success;
unmarshal(session_result, {succeeded, #wthd_SessionSucceeded{trx_info = TrxInfo}}) ->
    {success, unmarshal(transaction_info, TrxInfo)};
unmarshal(session_result, {failed, #wthd_SessionFailed{failure = Failure}}) ->
    {failed, ff_codec:unmarshal(failure, Failure)};

unmarshal(transaction_info, TrxInfo) ->
    ff_withdrawal_session_codec:unmarshal(transaction_info, TrxInfo);

unmarshal(session_state, Session) ->
    genlib_map:compact(#{
        id => unmarshal(id, Session#wthd_SessionState.id),
        result => maybe_unmarshal(session_result, Session#wthd_SessionState.result)
    });

unmarshal(quote_state, Quote) ->
    genlib_map:compact(#{
        cash_from => unmarshal(cash, Quote#wthd_QuoteState.cash_from),
        cash_to   => unmarshal(cash, Quote#wthd_QuoteState.cash_to),
        created_at => Quote#wthd_QuoteState.created_at,
        expires_on => Quote#wthd_QuoteState.expires_on,
        route => maybe_unmarshal(route, Quote#wthd_QuoteState.route),
        resource_descriptor => maybe_unmarshal(resource_descriptor, Quote#wthd_QuoteState.resource),
        quote_data => maybe_unmarshal(msgpack, Quote#wthd_QuoteState.quote_data)
    });

unmarshal(quote, Quote) ->
    genlib_map:compact(#{
        cash_from => unmarshal(cash, Quote#wthd_Quote.cash_from),
        cash_to   => unmarshal(cash, Quote#wthd_Quote.cash_to),
        created_at => Quote#wthd_Quote.created_at,
        expires_on => Quote#wthd_Quote.expires_on,
        route => maybe_unmarshal(route, Quote#wthd_Quote.route),
        resource_descriptor => maybe_unmarshal(resource_descriptor, Quote#wthd_Quote.resource),
        quote_data => maybe_unmarshal(msgpack, Quote#wthd_Quote.quote_data),
        domain_revision => maybe_unmarshal(domain_revision, Quote#wthd_Quote.domain_revision),
        party_revision => maybe_unmarshal(party_revision, Quote#wthd_Quote.party_revision),
        operation_timestamp => maybe_unmarshal(timestamp_ms, Quote#wthd_Quote.operation_timestamp)
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

get_legacy_provider_id(#{provider_id_legacy := Provider}) when is_binary(Provider) ->
    Provider;
get_legacy_provider_id(#{provider_id := Provider}) when is_integer(Provider) ->
    genlib:to_binary(Provider - 300).

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec withdrawal_symmetry_test() -> _.
withdrawal_symmetry_test() ->
    In = #wthd_Withdrawal{
        id = genlib:unique(),
        body = #'Cash'{
            amount = 10101,
            currency = #'CurrencyRef'{ symbolic_code = <<"Banana Republic">> }
        },
        wallet_id = genlib:unique(),
        destination_id = genlib:unique(),
        external_id = genlib:unique(),
        route = #wthd_Route{
            provider_id = 1,
            terminal_id = 7,
            provider_id_legacy = <<"mocketbank">>
        },
        domain_revision = 1,
        party_revision = 3,
        created_at = <<"2099-01-01T00:00:00.123Z">>
    },
    ?assertEqual(In, marshal(withdrawal, unmarshal(withdrawal, In))).

-spec withdrawal_params_symmetry_test() -> _.
withdrawal_params_symmetry_test() ->
    In = #wthd_WithdrawalParams{
        id = genlib:unique(),
        body = #'Cash'{
            amount = 10101,
            currency = #'CurrencyRef'{ symbolic_code = <<"Banana Republic">> }
        },
        wallet_id = genlib:unique(),
        destination_id = genlib:unique(),
        external_id = undefined
    },
    ?assertEqual(In, marshal_withdrawal_params(unmarshal_withdrawal_params(In))).

-spec quote_state_symmetry_test() -> _.
quote_state_symmetry_test() ->
    In = #wthd_QuoteState{
        cash_from  = #'Cash'{
            amount = 10101,
            currency = #'CurrencyRef'{ symbolic_code = <<"Banana Republic">> }
        },
        cash_to    = #'Cash'{
            amount = 20202,
            currency = #'CurrencyRef'{ symbolic_code = <<"Pineapple Empire">> }
        },
        created_at = genlib:unique(),
        expires_on = genlib:unique(),
        quote_data = {arr, [{bin, genlib:unique()}, {i, 5}, {nl, #msgp_Nil{}}]},
        route = #wthd_Route{
            provider_id = 1,
            terminal_id = 2,
            provider_id_legacy = <<>>
        },
        resource = {bank_card, #'ResourceDescriptorBankCard'{bin_data_id = {arr, [{bin, genlib:unique()}]}}},
        quote_data_legacy = #{}
    },
    ?assertEqual(In, marshal(quote_state, unmarshal(quote_state, In))).

-spec quote_symmetry_test() -> _.
quote_symmetry_test() ->
    In = #wthd_Quote{
        cash_from  = #'Cash'{
            amount = 10101,
            currency = #'CurrencyRef'{ symbolic_code = <<"Banana Republic">> }
        },
        cash_to    = #'Cash'{
            amount = 20202,
            currency = #'CurrencyRef'{ symbolic_code = <<"Pineapple Empire">> }
        },
        created_at = genlib:unique(),
        expires_on = genlib:unique(),
        quote_data = {arr, [{bin, genlib:unique()}, {i, 5}, {nl, #msgp_Nil{}}]},
        route = #wthd_Route{
            provider_id = 1,
            terminal_id = 2,
            provider_id_legacy = <<"drovider">>
        },
        resource = {bank_card, #'ResourceDescriptorBankCard'{bin_data_id = {arr, [{bin, genlib:unique()}]}}},
        domain_revision = 1,
        party_revision = 2,
        operation_timestamp = <<"2020-01-01T01:00:00Z">>
    },
    ?assertEqual(In, marshal(quote, unmarshal(quote, In))).


-spec marshal_session_result_test_() ->  _.
marshal_session_result_test_() ->

    TransactionInfo = #{
        id => <<"ID">>,
        extra => #{<<"Hello">> => <<"World">>}
    },

    Results = [
        {success, TransactionInfo},
        success
    ],

    Missed = [
        {succeeded, #wthd_SessionSucceeded{trx_info = marshal(transaction_info, TransactionInfo)}},
        {succeeded, #wthd_SessionSucceeded{}}
    ],

    Marshaled = marshal({list, session_result}, Results),

    [
        ?_assertEqual(Missed, Marshaled),
        ?_assertEqual(Results, unmarshal({list, session_result}, Missed))
    ].

-endif.
