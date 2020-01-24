-module(ff_withdrawal_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").

-export([unmarshal_withdrawal_params/1]).
-export([marshal_cash_range_error/1]).
-export([marshal_currency_invalid/1]).

-export([marshal_withdrawal/1]).
-export([unmarshal_withdrawal/1]).

-export([marshal_event/1]).

-export([marshal/2]).
-export([unmarshal/2]).

%% Data transform

-spec unmarshal_withdrawal_params(ff_proto_withdrawal_thrift:'WithdrawalParams'()) ->
    ff_withdrawal_handler:params().

unmarshal_withdrawal_params(Params) ->
    Body = Params#wthd_WithdrawalParams.body,
    #{
        wallet_id      => Params#wthd_WithdrawalParams.source,
        destination_id => Params#wthd_WithdrawalParams.destination,
        body           => ff_codec:unmarshal(cash, Body),
        external_id    => Params#wthd_WithdrawalParams.external_id
    }.

-spec marshal_currency_invalid({ff_currency:id(), ff_currency:id()}) ->
     ff_proto_fistful_thrift:'WithdrawalCurrencyInvalid'().

marshal_currency_invalid({WithdrawalCurrencyID, WalletCurrencyID}) ->
    #fistful_WithdrawalCurrencyInvalid{
        withdrawal_currency = ff_codec:marshal(currency_ref, WithdrawalCurrencyID),
        wallet_currency     = ff_codec:marshal(currency_ref, WalletCurrencyID)
    }.

-spec marshal_cash_range_error({ff_party:cash(), ff_party:cash_range()}) ->
    ff_proto_fistful_thrift:'WithdrawalCashAmountInvalid'().

marshal_cash_range_error({Cash, Range}) ->
    #fistful_WithdrawalCashAmountInvalid{
        cash  = ff_codec:marshal(cash, Cash),
        range = ff_codec:marshal(cash_range, Range)
    }.

%% API

-spec marshal_withdrawal(ff_withdrawal:withdrawal()) ->
    ff_proto_withdrawal_thrift:'Withdrawal'().

marshal_withdrawal(Withdrawal) ->
    #wthd_Withdrawal{
        body        = marshal(cash, ff_withdrawal:body(Withdrawal)),
        source      = marshal(id, ff_withdrawal:wallet_id(Withdrawal)),
        destination = marshal(id, ff_withdrawal:destination_id(Withdrawal)),
        external_id = maybe_marshal(id, ff_withdrawal:external_id(Withdrawal)),
        id          = marshal(id, ff_withdrawal:id(Withdrawal)),
        status      = maybe_marshal(status, ff_withdrawal:status(Withdrawal))
    }.

-spec unmarshal_withdrawal(ff_proto_withdrawal_thrift:'Withdrawal'()) ->
    ff_withdrawal:withdrawal().

unmarshal_withdrawal(#wthd_Withdrawal{
    body        = Body,
    source      = WalletID,
    destination = DestinationID,
    external_id = ExternalID,
    status      = Status,
    id          = ID
}) ->
    Params = genlib_map:compact(#{
        wallet_id      => unmarshal(id, WalletID),
        destination_id => unmarshal(id, DestinationID)
    }),
    Cash = unmarshal(cash, Body),
    TransferType = withdrawal,
    WithdrawalStatus = maybe_unmarshal(status, Status),
    ff_withdrawal:gen(#{
        id     => unmarshal(id, ID),
        body   => Cash,
        params => Params,
        status => WithdrawalStatus,
        external_id   => maybe_unmarshal(id, ExternalID),
        transfer_type => TransferType
    }).

-spec marshal_event({integer(), ff_machine:timestamped_event(ff_withdrawal:event())}) ->
    ff_proto_withdrawal_thrift:'Event'().

marshal_event({ID, {ev, Timestamp, Ev}}) ->
    #wthd_Event{
        event      = ff_codec:marshal(event_id, ID),
        occured_at = ff_codec:marshal(timestamp, Timestamp),
        change     = ff_withdrawal_codec:marshal(event, Ev)
    }.

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(event, {created, Withdrawal}) ->
    {created, #wthd_CreatedChange{withdrawal = marshal_withdrawal(Withdrawal)}};
marshal(event, {status_changed, Status}) ->
    {status_changed, #wthd_StatusChange{status = marshal(status, Status)}};
marshal(event, {p_transfer, TransferChange}) ->
    {transfer, #wthd_TransferChange{payload = ff_p_transfer_codec:marshal(event, TransferChange)}};
marshal(event, {session_started, SessionID}) ->
    {session, #wthd_SessionChange{id = SessionID, payload = marshal(session_event, started)}};
marshal(event, {session_finished, {SessionID, SessionResult}}) ->
    {session, #wthd_SessionChange{id = SessionID, payload = marshal(session_event, {finished, SessionResult})}};
marshal(event, {route_changed, Route}) ->
    {route, #wthd_RouteChange{route = marshal(route, Route)}};
marshal(event, {limit_check, Details}) ->
    {limit_check, #wthd_LimitCheckChange{details = ff_limit_check_codec:marshal(details, Details)}};
marshal(event, {resource_got, Resource}) ->
    {resource, {got, #wthd_ResourceGot{resource = marshal(resource, Resource)}}};

marshal(route, #{provider_id := ProviderID}) ->
    #wthd_Route{provider_id = marshal(provider_id, ProviderID)};

marshal(status, pending) ->
    {pending, #wthd_status_Pending{}};
marshal(status, succeeded) ->
    {succeeded, #wthd_status_Succeeded{}};
marshal(status, {failed, Failure}) ->
    {failed, #wthd_status_Failed{failure = marshal(failure, Failure)}};

marshal(session_event, started) ->
    {started, #wthd_SessionStarted{}};
marshal(session_event, {finished, Result}) ->
    {finished, #wthd_SessionFinished{result = marshal(session_result, Result)}};

marshal(session_result, {success, TrxInfo}) ->
    MarshaledTrxInfo = ff_withdrawal_session_codec:marshal(transaction_info, TrxInfo),
    {succeeded, #wthd_SessionSucceeded{trx_info = MarshaledTrxInfo}};
marshal(session_result, {failed, Failure}) ->
    {failed, #wthd_SessionFailed{failure = ff_codec:marshal(failure, Failure)}};

marshal(ctx, Ctx) ->
    marshal(context, Ctx);

marshal(provider_id, ProviderID) ->
    marshal(id, genlib:to_binary(ProviderID));

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #wthd_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, event}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(event, {created, #wthd_CreatedChange{withdrawal = Withdrawal}}) ->
    {created, unmarshal_withdrawal(Withdrawal)};
unmarshal(event, {status_changed, #wthd_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};
unmarshal(event, {transfer, #wthd_TransferChange{payload = TransferChange}}) ->
    {p_transfer, ff_p_transfer_codec:unmarshal(event, TransferChange)};
unmarshal(event, {session, SessionChange}) ->
    unmarshal(session_event, SessionChange);
unmarshal(event, {route, Route}) ->
    {route_changed, unmarshal(route, Route)};
unmarshal(event, {limit_check, #wthd_LimitCheckChange{details = Details}}) ->
    {limit_check, ff_limit_check_codec:unmarshal(details, Details)};
unmarshal(event, {resource, {got, #wthd_ResourceGot{resource = Resource}}}) ->
    {resource_got, unmarshal(resource, Resource)};

unmarshal(route, #wthd_Route{provider_id = ProviderID}) ->
    #{provider_id => unmarshal(provider_id, ProviderID)};

unmarshal(status, {pending, #wthd_status_Pending{}}) ->
    pending;
unmarshal(status, {succeeded, #wthd_status_Succeeded{}}) ->
    succeeded;
unmarshal(status, {failed, #wthd_status_Failed{failure = Failure}}) ->
    {failed, unmarshal(failure, Failure)};

unmarshal(provider_id, ProviderID) ->
    unmarshal(integer, erlang:binary_to_integer(ProviderID));

unmarshal(session_event, #wthd_SessionChange{id = ID, payload = {started, #wthd_SessionStarted{}}}) ->
    {session_started, unmarshal(id, ID)};
unmarshal(session_event, #wthd_SessionChange{id = ID, payload = {finished, Finished}}) ->
    #wthd_SessionFinished{result = Result} = Finished,
    {session_finished, {unmarshal(id, ID), unmarshal(session_result, Result)}};

unmarshal(session_result, {succeeded, #wthd_SessionSucceeded{trx_info = TrxInfo}}) ->
    {success, ff_withdrawal_session_codec:unmarshal(transaction_info, TrxInfo)};
unmarshal(session_result, {failed, #wthd_SessionFailed{failure = Failure}}) ->
    {failed, ff_codec:unmarshal(failure, Failure)};

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

-spec withdrawal_test() -> _.
withdrawal_test() ->
    WalletID    = genlib:unique(),
    Dest        = genlib:unique(),
    ExternalID  = genlib:unique(),

    In = #wthd_Withdrawal{
        body        = #'Cash'{
            amount = 10101,
            currency = #'CurrencyRef'{ symbolic_code = <<"Banana Republic">> }
        },
        source      = WalletID,
        destination = Dest,
        external_id = ExternalID,
        status      = {pending, #wthd_status_Pending{}},
        id          = genlib:unique()
    },
    ?assertEqual(In, marshal_withdrawal(unmarshal_withdrawal(In))).

-endif.
