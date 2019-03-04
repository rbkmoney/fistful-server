-module(ff_withdrawal_codec).

-behaviour(ff_codec).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_cashflow_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([decode_withdrawal_params/1]).
-export([encode_cash_range_error/1]).
-export([encode_currency_invalid/1]).

-export([marshal_withdrawal/1]).
-export([marshal_event/1]).

-export([marshal/2]).
-export([unmarshal/2]).

%% Data transform

final_account_to_final_cash_flow_account(#{
    account := #{id := AccountID},
    type := AccountType}
) ->
    #{account_type => AccountType, account_id => AccountID}.

-define(to_session_event(SessionID, Payload),
    {session, #{id => SessionID, payload => Payload}}).

%% DECODE

-spec decode_withdrawal_params(ff_proto_withdrawal_thrift:'WithdrawalParams'()) ->
    ff_withdrawal:params().

decode_withdrawal_params(Params) ->
    Body = Params#wthd_WithdrawalParams.body,
    #{
        wallet_id      => Params#wthd_WithdrawalParams.source,
        destination_id => Params#wthd_WithdrawalParams.destination,
        body           => ff_codec:unmarshal(cash, Body),
        external_id    => Params#wthd_WithdrawalParams.external_id
    }.

%% ENCODE

-spec encode_currency_invalid({ff_currency:id(), ff_currency:id()}) ->
     ff_proto_withdrawal_thrift:'WithdrawalCurrencyInvalid'().

encode_currency_invalid({WithdrawalCurrencyID, WalletCurrencyID}) ->
    #fistful_WithdrawalCurrencyInvalid{
        withdrawal_currency = ff_codec:marshal(currency_ref, WithdrawalCurrencyID),
        wallet_currency     = ff_codec:marshal(currency_ref, WalletCurrencyID)
    }.

-spec encode_cash_range_error({ff_party:cash(), ff_party:cash_range()}) ->
    ff_proto_withdrawal_thrift:'fistful_WithdrawalCashAmountInvalid'().

encode_cash_range_error({Cash, Range}) ->
    #fistful_WithdrawalCashAmountInvalid{
        cash  = ff_codec:marshal(cash, Cash),
        range = ff_codec:marshal(cash_range, Range)
    }.

%% API

-spec marshal_withdrawal({ff_withdrawal:withdrawal(), ff_withdrawal:ctx()}) ->
    ff_proto_withdrawal_thrift:'Withdrawal'().

marshal_withdrawal({Withdrawal, Ctx}) ->
    WalletID      = ff_withdrawal:wallet_id(Withdrawal),
    DestinationID = ff_withdrawal:destination_id(Withdrawal),
    ExternalID    = ff_withdrawal:external_id(Withdrawal),
    Cash          = ff_withdrawal:body(Withdrawal),
    ID            = ff_withdrawal:id(Withdrawal),
    Status        = ff_withdrawal:status(Withdrawal),
    #wthd_Withdrawal{
        body        = marshal(cash, Cash),
        source      = marshal(id, WalletID),
        destination = marshal(id, DestinationID),
        external_id = marshal(id, ExternalID),
        id          = maybe_marshal(id, ID),
        status      = maybe_marshal(withdrawal_status_changed, Status),
        context     = maybe_marshal(context, Ctx)
    }.

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
    {created, marshal_withdrawal({Withdrawal, undefined})};
marshal(event, {status_changed, WithdrawalStatus}) ->
    {status_changed, marshal(withdrawal_status_changed, WithdrawalStatus)};
marshal(event, {p_transfer, TransferChange}) ->
    {transfer, marshal(postings_transfer_change, TransferChange)};
marshal(event, {session_started, SessionID}) ->
    marshal(session, ?to_session_event(SessionID, started));
marshal(event, {session_finished, SessionID}) ->
    marshal(session, ?to_session_event(SessionID, finished));
marshal(session, {session, SessionChange}) ->
    {session, marshal(withdrawal_session_change, SessionChange)};
marshal(event, {route_changed, Route}) ->
    {route, marshal(withdrawal_route_changed, Route)};

marshal(withdrawal_status_changed, pending) ->
    {pending, #wthd_WithdrawalPending{}};
marshal(withdrawal_status_changed, succeeded) ->
    {succeeded, #wthd_WithdrawalSucceeded{}};
% TODO: Process failures propertly
% marshal(withdrawal_status_changed, {failed, #{
%         failure := Failure
%     }}) ->
%     {failed, #wthd_WithdrawalFailed{failure = marshal(failure, Failure)}};
marshal(withdrawal_status_changed, {failed, _}) ->
    {failed, #wthd_WithdrawalFailed{failure = marshal(failure, dummy)}};
marshal(failure, _) ->
    #wthd_Failure{};

marshal(postings_transfer_change, {created, Transfer}) ->
    {created, marshal(transfer, Transfer)}; % not ff_transfer :) see thrift
marshal(postings_transfer_change, {status_changed, TransferStatus}) ->
    {status_changed, marshal(transfer_status, TransferStatus)};
marshal(transfer, #{
        final_cash_flow := Cashflow
}) ->
    #wthd_Transfer{
        cashflow = marshal(final_cash_flow, Cashflow)
    };
marshal(final_cash_flow, #{
    postings := Postings
}) ->
    #cashflow_FinalCashFlow{
        postings = marshal({list, postings}, Postings)
    };
marshal(postings, Postings = #{
    sender := Source,
    receiver := Destination,
    volume := Cash
}) ->
    Details = maps:get(details, Postings, undefined),
    #cashflow_FinalCashFlowPosting{
        source      = marshal(final_cash_flow_account,
            final_account_to_final_cash_flow_account(Source)),
        destination = marshal(final_cash_flow_account,
            final_account_to_final_cash_flow_account(Destination)),
        volume      = marshal(cash, Cash),
        details     = marshal(string, Details)
    };
marshal(final_cash_flow_account, #{
    account_type   := AccountType,
    account_id     := AccountID
}) ->
    #cashflow_FinalCashFlowAccount{
        account_type   = marshal(account_type, AccountType),
        account_id     = marshal(id, AccountID)
    };

marshal(account_type, CashflowAccount) ->
    % Mapped to thrift type WalletCashFlowAccount as is
    CashflowAccount;
marshal(transfer_status, created) ->
    {created, #wthd_TransferCreated{}};
marshal(transfer_status, prepared) ->
    {prepared, #wthd_TransferPrepared{}};
marshal(transfer_status, committed) ->
    {committed, #wthd_TransferCommitted{}};
marshal(transfer_status, cancelled) ->
    {cancelled, #wthd_TransferCancelled{}};

marshal(withdrawal_session_change, #{
    id      := SessionID,
    payload := Payload
}) ->
    #wthd_SessionChange{
        id      = marshal(id, SessionID),
        payload = marshal(withdrawal_session_payload, Payload)
    };
marshal(withdrawal_session_payload, started) ->
    {started, #wthd_SessionStarted{}};
marshal(withdrawal_session_payload, finished) ->
    {finished, #wthd_SessionFinished{}};

marshal(withdrawal_route_changed, #{
    provider_id := ProviderID
}) ->
    #wthd_RouteChange{
        id = marshal(id, genlib:to_binary(ProviderID))
    };

marshal(context, Ctx) ->
    maybe_marshal(msgpack, Ctx);

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

unmarshal(event, {created, Withdrawal}) ->
    {created, unmarshal(withdrawal, Withdrawal)};
unmarshal(event, {status_changed, WithdrawalStatus}) ->
    {status_changed, unmarshal(withdrawal_status_changed, WithdrawalStatus)};
unmarshal(event, {transfer, TransferChange}) ->
    {p_transfer, unmarshal(postings_transfer_change, TransferChange)};
unmarshal(event, {session, #wthd_SessionChange{id = ID, payload = {started, #wthd_SessionStarted{}}}}) ->
    {session_started, unmarshal(id, ID)};
unmarshal(event, {session, #wthd_SessionChange{id = ID, payload = {finished, #wthd_SessionFinished{}}}}) ->
    {session_finished, unmarshal(id, ID)};
unmarshal(event, {route, Route}) ->
    {route_changed, unmarshal(withdrawal_route_changed, Route)};
unmarshal(event, {route_changed, Route}) ->
    {route, unmarshal(withdrawal_route_changed, Route)};

unmarshal(withdrawal, #wthd_Withdrawal{
    body = Cash,
    source = WalletID,
    destination = DestinationID,
    external_id = ExternalID
}) ->
    #{
        body => unmarshal(cash, Cash),
        params => genlib_map:compact(#{
            wallet_id => unmarshal(id, WalletID),
            destination_id => unmarshal(id, DestinationID),
            external_id => unmarshal(id, ExternalID)
        })
    };

unmarshal(withdrawal_status_changed, {pending, #wthd_WithdrawalPending{}}) ->
    pending;
unmarshal(withdrawal_status_changed, {succeeded, #wthd_WithdrawalSucceeded{}}) ->
    succeeded;
% TODO: Process failures propertly
% unmarshal(withdrawal_status_changed, {failed, #wthd_WithdrawalFailed{failure = Failure}}) ->
%     {failed, unmarshal(failure, Failure)};
unmarshal(withdrawal_status_changed, {failed, #wthd_WithdrawalFailed{failure = #wthd_Failure{}}}) ->
    {failed, #domain_Failure{code = <<"unknown">>}};

unmarshal(postings_transfer_change, {created, Transfer}) ->
    {created, unmarshal(transfer, Transfer)}; % not ff_transfer :) see thrift
unmarshal(postings_transfer_change, {status_changed, TransferStatus}) ->
    {status_changed, unmarshal(transfer_status, TransferStatus)};
unmarshal(transfer, #wthd_Transfer{
    cashflow = Cashflow
}) ->
    #{
        cashflow => unmarshal(final_cash_flow, Cashflow)
    };
unmarshal(final_cash_flow, #cashflow_FinalCashFlow{
    postings = Postings
}) ->
    #{
        postings => unmarshal({list, postings}, Postings)
    };
unmarshal(postings, #cashflow_FinalCashFlowPosting{
    source = Source,
    destination = Destination,
    volume = Cash,
    details = Details
}) ->
    genlib_map:compact(#{
        source      => unmarshal(final_cash_flow_account, Source),
        destination => unmarshal(final_cash_flow_account, Destination),
        volume      => unmarshal(cash, Cash),
        details     => unmarshal(string, Details)
    });
unmarshal(final_cash_flow_account, CashFlow = #cashflow_FinalCashFlowAccount{
    account_type = _AccountType,
    account_id   = _AccountID
}) ->
    % FIXME: Make protocol symmetric. final_cash_flow_account is unrecoverable from thrift now
    erlang:error({not_implemented, {unmarshal, final_cash_flow_account}}, [final_cash_flow_account, CashFlow]);

unmarshal(account_type, CashflowAccount) ->
    % Mapped to thrift type WalletCashFlowAccount as is
    CashflowAccount;
unmarshal(transfer_status, {created, #wthd_TransferCreated{}}) ->
    created;
unmarshal(transfer_status, {prepared, #wthd_TransferPrepared{}}) ->
    prepared;
unmarshal(transfer_status, {committed, #wthd_TransferCommitted{}}) ->
    committed;
unmarshal(transfer_status, {cancelled, #wthd_TransferCancelled{}}) ->
    cancelled;

unmarshal(withdrawal_route_changed, #wthd_RouteChange{
    id = ProviderID
}) ->
    #{
        provider_id => unmarshal(id, erlang:binary_to_integer(ProviderID))
    };

unmarshal(context, Ctx) ->
    maybe_unmarshal(msgpack, Ctx);

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals
maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

