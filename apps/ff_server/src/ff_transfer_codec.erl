-module(ff_transfer_codec).

-behaviour(ff_codec).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_cashflow_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

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

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(event, {created, Transfer}) ->
    {transfer_changed, {created, marshal(transfer, Transfer)}};
marshal(event, {status_changed, TransferStatus}) ->
    {status_changed, marshal(transfer_status_changed, TransferStatus)};
marshal(event, {route_changed, Route}) ->
    {route_changed, {created, {withdrawal, marshal(route, Route)}}};
marshal(event, {transaction, TransactionChange}) ->
    {transaction_changed, marshal(transaction_change, TransactionChange)};
marshal(event, {child_transfer, SignedEvent}) ->
    {child_transfer_changed, marshal(signed_event, SignedEvent)};

marshal(transfer, Transfer = #{
    transfer_type := TransferType,
    id := ID,
    body := Cash,
    params := Params
}) ->
    ExternalID = maps:get(external_id, Transfer, undefined),
    #transfer_TransferCreated{transfer = #transfer_Transfer{
        transfer_type   = marshal(transfer_type, TransferType),
        id              = marshal(id, ID),
        body            = marshal(cash, Cash),
        external_id     = marshal(id, ExternalID),
        params          = marshal(transfer_params, {TransferType, Params})
    }};

marshal(transfer_type, deposit) ->
    {deposit, #transfer_TransferDeposit{}};
marshal(transfer_type, withdrawal) ->
    {withdrawal, #transfer_TransferWithdrawal{}};

marshal(transfer_params, {deposit, #{
    wallet_id := WalletID,
    source_id := SourceID
}}) ->
    {deposit, #transfer_DepositParams{
        wallet_id = marshal(id, WalletID),
        source_id = marshal(id, SourceID)
    }};
marshal(transfer_params, {withdrawal, #{
    wallet_id       := WalletID,
    destination_id  := DestinationID
}}) ->
    {withdrawal, #transfer_WithdrawalParams{
        wallet_id       = marshal(id, WalletID),
        destination_id  = marshal(id, DestinationID)
    }};

marshal(transfer_status_changed, pending) ->
    {pending, #transfer_TransferPending{}};
marshal(transfer_status_changed, succeeded) ->
    {succeeded, #transfer_TransferSucceeded{}};
% marshal(transfer_status_changed, {failed, #{
%         failure := Failure
%     }}) ->
%     {failed, #transfer_TransferFailed{failure = marshal(failure, Failure)}};
marshal(transfer_status_changed, {failed, _}) ->
    {failed, #transfer_TransferFailed{failure = marshal(failure, dummy)}};
marshal(failure, _) ->
    #transfer_Failure{};

marshal(route, #{
    provider_id := ID
}) ->
    #transfer_RouteWithdrawal{
        %% TODO id maybe integer
        id = marshal(id, ID)
    };

marshal(transaction_change, {created, Transaction}) ->
    {created, marshal(transaction, Transaction)};
marshal(transaction_change, {status_changed, TransactionStatus}) ->
    {status_changed, marshal(transaction_status_changed, TransactionStatus)};
marshal(transaction_change, {p_transfer, PTransfer}) ->
    {postings_transfer_changed, marshal(postings_transfer_change, PTransfer)};
marshal(transaction_change, {session_started, SessionID}) ->
    marshal(session, ?to_session_event(SessionID, started));
marshal(transaction_change, {session_finished, SessionID}) ->
    marshal(session, ?to_session_event(SessionID, finished));
marshal(session, {session, SessionChange}) ->
    {session_changed, marshal(session_change, SessionChange)};

marshal(transaction, #{
    id              := ID,
    body            := Cash,
    session_data    := SessionData,
    final_cash_flow := FinalCashFlow
}) ->
    #transfer_TransactionCreated{transaction = #transfer_Transaction{
        id                  = marshal(id, ID),
        body                = marshal(cash, Cash),
        session_data        = marshal(session_data, SessionData),
        final_cash_flow     = marshal(final_cash_flow, FinalCashFlow)
    }};

marshal(session_data, {empty, _, _}) ->
    {empty, #transfer_SessionDataEmpty{}};
marshal(session_data, {withdrawal, Data, Params}) ->
    {withdrawal, #transfer_SessionDataWithdrawal{
        data = marshal(withdrawal_session_data, Data),
        params = marshal(withdrawal_session_params, Params)
    }};

marshal(withdrawal_session_data, #{
    id          := ID,
    cash        := Cash,
    %% TODO change identity to id of identity
    sender      := SenderID,
    receiver    := ReceiverID
}) ->
    #transfer_SessionWithdrawalData{
        id          = marshal(id, ID),
        cash        = marshal(cash, Cash),
        sender_id      = marshal(id, SenderID),
        receiver_id    = marshal(id, ReceiverID)
    };

marshal(withdrawal_session_params, #{
    destination_id     := DestinationID,
    provider_id        := ProviderID
}) ->
    #transfer_SessionWithdrawalParams{
        destination_id     = marshal(id, DestinationID),
        provider_id        = marshal(id, ProviderID)
    };

marshal(transaction_status_changed, pending) ->
    {pending, #transfer_TransactionPending{}};
marshal(transaction_status_changed, succeeded) ->
    {succeeded, #transfer_TransactionSucceeded{}};
% marshal(transaction_status_changed, {failed, #{
%         failure := Failure
%     }}) ->
%     {failed, #transfer_TransactionFailed{failure = marshal(failure, Failure)}};
marshal(transaction_status_changed, {failed, _}) ->
    {failed, #transfer_TransactionFailed{failure = marshal(failure, dummy)}};

marshal(postings_transfer_change, {created, PTransfer}) ->
    {created, marshal(postings_transfer, PTransfer)};
marshal(postings_transfer_change, {status_changed, PTransferStatus}) ->
    {status_changed, marshal(postings_transfer_status, PTransferStatus)};
marshal(postings_transfer, #{
        final_cash_flow := Cashflow
}) ->
    #transfer_PostingsTransferCreated{posting_transfer = #transfer_PostingsTransfer{
        cashflow = marshal(final_cash_flow, Cashflow)
    }};
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
            %% TODO привести в соответствие с внутренним представлением
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
marshal(postings_transfer_status, created) ->
    {created, #transfer_PostingsTransferStatusCreated{}};
marshal(postings_transfer_status, prepared) ->
    {prepared, #transfer_PostingsTransferStatusPrepared{}};
marshal(postings_transfer_status, committed) ->
    {committed, #transfer_PostingsTransferStatusCommitted{}};
marshal(postings_transfer_status, cancelled) ->
    {cancelled, #transfer_PostingsTransferStatusCancelled{}};

marshal(session_change, #{
    id      := SessionID,
    payload := Payload
}) ->
    #transfer_SessionChange{
        id      = marshal(id, SessionID),
        payload = marshal(session_payload, Payload)
    };
marshal(withdrawal_session_payload, started) ->
    {started, #transfer_SessionStarted{}};
marshal(withdrawal_session_payload, finished) ->
    {finished, #transfer_SessionFinished{}};

marshal(signed_event, #{
    type    := TransferType,
    id      := ID,
    event   := Event,
    parent  := Parent
}) ->
    #transfer_ChildTransferChange{
        type    = marshal(transfer_type, TransferType),
        id      = marshal(id, ID),
        changes = [marshal(event, Event)],
        parent  = marshal(transfer_parent, Parent)
    };

marshal(transfer_parent, {TransferType, ID}) ->
    #transfer_TransferParent{
        type    = marshal(transfer_type, TransferType),
        id      = marshal(id, ID)
    };

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #transfer_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, event}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(event, {transfer_changed, TransferChange}) ->
    unmarshal(transfer_change, TransferChange);
unmarshal(event, {status_changed, TransferStatus}) ->
    {status_changed, unmarshal(status_changed, TransferStatus)};
unmarshal(event, {route_changed, RouteChange}) ->
    {route_changed, unmarshal(route_change, RouteChange)};
unmarshal(event, {transaction_changed, TransactionChange}) ->
    {transaction, unmarshal(transaction_change, TransactionChange)};

unmarshal(event, {child_transfer_changed, ChildTransfer}) ->
    {child_transfer, unmarshal(child_transfer_change, ChildTransfer)};

unmarshal(transfer_change, #transfer_TransferCreated{transfer =  Transfer}) ->
    {created, unmarshal(transfer, Transfer)};

unmarshal(transfer, #transfer_Transfer{
    transfer_type   = TransferType,
    id              = ID,
    body            = Cash,
    external_id     = ExternalID,
    params          = Params
}) ->
    #{
        transfer_type => unmarshal(transfer_type, TransferType),
        id            => unmarshal(id, ID),
        body          => unmarshal(cash, Cash),
        external_id   => unmarshal(id, ExternalID),
        params        => unmarshal(transfer_params, {TransferType, Params})
    };

unmarshal(transfer_type, {deposit, #transfer_TransferDeposit{}}) ->
    deposit;
unmarshal(transfer_type, {withdrawal, #transfer_TransferWithdrawal{}}) ->
    withdrawal;

unmarshal(transfer_params, {deposit, #transfer_DepositParams{
    wallet_id = WalletID,
    source_id = SourceID
}}) ->
    #{
        wallet_id => unmarshal(id, WalletID),
        source_id => unmarshal(id, SourceID)
    };
unmarshal(transfer_params, {withdrawal, #transfer_WithdrawalParams{
    wallet_id = WalletID,
    destination_id = DestinationID
}}) ->
    #{
        wallet_id       => unmarshal(id, WalletID),
        destination_id  => unmarshal(id, DestinationID)
    };

unmarshal(status_changed, {pending, #transfer_TransferPending{}}) ->
    pending;
unmarshal(status_changed, {succeeded, #transfer_TransferSucceeded{}}) ->
    succeeded;
% TODO: Process failures propertly
% unmarshal(status_changed, {failed, #transfer_TransferFailed{failure = Failure}}) ->
%     {failed, unmarshal(failure, Failure)};
unmarshal(status_changed, {failed, #transfer_TransferFailed{failure = #transfer_Failure{}}}) ->
    {failed, #domain_Failure{code = <<"unknown">>}};

unmarshal(route_change, {created, RouteCreated}) ->
    unmarshal(route_created, RouteCreated);

unmarshal(route_created, {withdrawal, WithdrawalRouteCreated}) ->
    unmarshal(withdrawal_route_created, WithdrawalRouteCreated);

unmarshal(withdrawal_route_created, #transfer_RouteWithdrawal{id = ProviderID}) ->
    #{
        %% TODO id maybe integer
        provider_id => unmarshal(id, ProviderID)
    };

unmarshal(transaction_change, {created, #transfer_TransactionCreated{transaction = Transaction}}) ->
    unmarshal(transaction, Transaction);
unmarshal(transaction_change, {status_changed, TransactionStatus}) ->
    {status_changed, unmarshal(transaction_status_changed, TransactionStatus)};

unmarshal(transaction_change, {postings_transfer_changed, PTransferChange}) ->
    {p_transfer, unmarshal(postings_transfer_change, PTransferChange)};
unmarshal(transaction_change, {session, #transfer_SessionChange{
    id = ID,
    payload = {started, #transfer_SessionStarted{}}
}}) ->
    {session_started, unmarshal(id, ID)};
unmarshal(transaction_change, {session, #transfer_SessionChange{
    id = ID,
    payload = {finished, #transfer_SessionFinished{}}
}}) ->
    {session_finished, unmarshal(id, ID)};

unmarshal(transaction, #transfer_Transaction{
    id                  = ID,
    body                = Cash,
    session_data        = SessionData,
    final_cash_flow     = FinalCashFlow
}) ->
    #{
        id              => unmarshal(id, ID),
        body            => unmarshal(cash, Cash),
        session_data    => unmarshal(session_data, SessionData),
        final_cash_flow => unmarshal(final_cash_flow, FinalCashFlow)
    };

unmarshal(session_data, #transfer_SessionDataEmpty{}) ->
    {empty, undefined, undefined};
unmarshal(session_data, #transfer_SessionDataWithdrawal{
    data        = SessionData,
    params      = SessionParams
}) ->
    {
        withdrawal,
        unmarshal(withdrawal_session_data, SessionData),
        unmarshal(withdrawal_session_params, SessionParams)
    };

unmarshal(withdrawal_session_data, #transfer_SessionWithdrawalData{
    id              = ID,
    cash            = Cash,
    sender_id       = SenderID,
    receiver_id     = ReceiverID
}) ->
    #{
        id              => unmarshal(id, ID),
        cash            => unmarshal(cash, Cash),
        sender_id       => unmarshal(id, SenderID),
        receiver_id     => unmarshal(id, ReceiverID)
    };

unmarshal(withdrawal_session_params, #transfer_SessionWithdrawalParams{
    destination_id      = DestinationID,
    provider_id         = ProviderID
}) ->
    #{
        destination_id  => unmarshal(id, DestinationID),
        provider_id     => unmarshal(id, ProviderID)
    };

unmarshal(transaction_status_changed, {pending, #transfer_TransactionPending{}}) ->
    pending;
unmarshal(transaction_status_changed, {succeeded, #transfer_TransactionSucceeded{}}) ->
    succeeded;
% TODO: Process failures propertly
% unmarshal(transaction_status_changed, {failed, #transfer_TransactionFailed{failure = Failure}}) ->
%     {failed, unmarshal(failure, Failure)};
unmarshal(transaction_status_changed, {failed, #transfer_TransactionFailed{failure = #transfer_Failure{}}}) ->
    {failed, #domain_Failure{code = <<"unknown">>}};

unmarshal(postings_transfer_change, {created, Transfer}) ->
    {created, unmarshal(transfer, Transfer)}; % not ff_transfer :) see thrift
unmarshal(postings_transfer_change, {status_changed, TransferStatus}) ->
    {status_changed, unmarshal(postings_transfer_status, TransferStatus)};

unmarshal(transfer, #transfer_PostingsTransfer{
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
        details     => maybe_unmarshal(string, Details)
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

unmarshal(postings_transfer_status, {created, #transfer_PostingsTransferStatusCreated{}}) ->
    created;
unmarshal(postings_transfer_status, {prepared, #transfer_PostingsTransferStatusPrepared{}}) ->
    prepared;
unmarshal(postings_transfer_status, {committed, #transfer_PostingsTransferStatusCommitted{}}) ->
    committed;
unmarshal(postings_transfer_status, {cancelled, #transfer_PostingsTransferStatusCancelled{}}) ->
    cancelled;

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).
