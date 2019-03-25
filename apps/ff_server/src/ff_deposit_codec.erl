-module(ff_deposit_codec).

-behaviour(ff_codec).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_deposit_thrift.hrl").
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

marshal(event, {created, Deposit}) ->
    {created, marshal(deposit, Deposit)};
marshal(event, {status_changed, DepositStatus}) ->
    {status_changed, marshal(deposit_status_changed, DepositStatus)};
marshal(event, {p_transfer, TransferChange}) ->
    {transfer, marshal(postings_transfer_change, TransferChange)};
marshal(event, {reposit, RepositChange}) ->
    {reposit, marshal(reposit_change, RepositChange)};

marshal(deposit, #{
    body := Cash,
    params := Params
}) ->
    WalletID = maps:get(wallet_id, Params),
    SourceID = maps:get(source_id, Params),
    ExternalID = maps:get(external_id, Params, undefined),
    #deposit_Deposit{
        body = marshal(cash, Cash),
        wallet = marshal(id, WalletID),
        source = marshal(id, SourceID),
        external_id = marshal(id, ExternalID)
    };

marshal(deposit_status_changed, pending) ->
    {pending, #deposit_DepositPending{}};
marshal(deposit_status_changed, succeeded) ->
    {succeeded, #deposit_DepositSucceeded{}};
% marshal(deposit_status_changed, {failed, #{
%         failure := Failure
%     }}) ->
%     {failed, #deposit_DepositFailed{failure = marshal(failure, Failure)}};
marshal(deposit_status_changed, {failed, _}) ->
    {failed, #deposit_DepositFailed{failure = marshal(failure, dummy)}};
marshal(deposit_status_changed, {reverted, _}) ->
    {reverted, #deposit_DepositReverted{}};
marshal(failure, _) ->
    #deposit_Failure{};

marshal(postings_transfer_change, {created, Transfer}) ->
    {created, marshal(transfer, Transfer)}; % not ff_transfer :) see thrift
marshal(postings_transfer_change, {status_changed, TransferStatus}) ->
    {status_changed, marshal(transfer_status, TransferStatus)};
marshal(transfer, #{
        final_cash_flow := Cashflow
}) ->
    #deposit_Transfer{
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
    {created, #deposit_TransferCreated{}};
marshal(transfer_status, prepared) ->
    {prepared, #deposit_TransferPrepared{}};
marshal(transfer_status, committed) ->
    {committed, #deposit_TransferCommitted{}};
marshal(transfer_status, cancelled) ->
    {cancelled, #deposit_TransferCancelled{}};

marshal(reposit_change, {created, Reposit}) ->
    {created, marshal(reposit, Reposit)};
marshal(reposit_change, {status_changed, RepositStatus}) ->
    {status_changed, marshal(reposit_status, RepositStatus)};

marshal(reposit, Params = #{
    deposit_id      := DepositID,
    source_id       := SourceID,
    wallet_id       := WalletID,
    body            := Body,
    create_at       := CreatedAt
}) ->
    Reason = maps:get(reason, Params, undefined),
    #deposit_Reposit{
        deposit     = marshal(id, DepositID),
        source      = marshal(id, WalletID),
        destination = marshal(id, SourceID),
        body        = marshal(cash, Body),
        created_at  = marshal(string, CreatedAt),
        reason      = marshal(string, Reason)
    };

marshal(reposit_status, pending) ->
    {pending, #deposit_RepositPending{}};
marshal(reposit_status, succeeded) ->
    {succeeded, #deposit_RepositSucceeded{}};
% marshal(reposit_status, {failed, #{
%         failure := Failure
%     }}) ->
%     {failed, #deposit_RepositFailed{failure = marshal(failure, Failure)}};
marshal(reposit_status, {failed, _}) ->
    {failed, #deposit_RepositFailed{failure = marshal(failure, dummy)}};

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #deposit_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, event}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(event, {created, Deposit}) ->
    {created, unmarshal(deposit, Deposit)};
unmarshal(event, {status_changed, DepositStatus}) ->
    {status_changed, unmarshal(deposit_status_changed, DepositStatus)};
unmarshal(event, {transfer, TransferChange}) ->
    {p_transfer, unmarshal(postings_transfer_change, TransferChange)};
unmarshal(event, {reposit, RepositChange}) ->
    {reposit, unmarshal(reposit_change, RepositChange)};

unmarshal(deposit, #deposit_Deposit{
    body = Cash,
    wallet = WalletID,
    source = SourceID,
    external_id = ExternalID
}) ->
    #{
        body => unmarshal(cash, Cash),
        params => #{
            wallet_id => unmarshal(id, WalletID),
            source_id => unmarshal(id, SourceID),
            external_id => unmarshal(id, ExternalID)
        }
    };

unmarshal(deposit_status_changed, {pending, #deposit_DepositPending{}}) ->
    pending;
unmarshal(deposit_status_changed, {succeeded, #deposit_DepositSucceeded{}}) ->
    succeeded;
% TODO: Process failures propertly
% unmarshal(deposit_status_changed, {failed, #deposit_DepositFailed{failure = Failure}}) ->
%     {failed, unmarshal(failure, Failure)};
unmarshal(deposit_status_changed, {failed, #deposit_DepositFailed{failure = #deposit_Failure{}}}) ->
    {failed, #domain_Failure{code = <<"unknown">>}};
unmarshal(deposit_status_changed, {reverted, #deposit_DepositReverted{}}) ->
    {reverted, <<"unknown">>};

unmarshal(postings_transfer_change, {created, Transfer}) ->
    {created, unmarshal(transfer, Transfer)}; % not ff_transfer :) see thrift
unmarshal(postings_transfer_change, {status_changed, TransferStatus}) ->
    {status_changed, unmarshal(transfer_status, TransferStatus)};

unmarshal(transfer, #deposit_Transfer{
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

unmarshal(transfer_status, {created, #deposit_TransferCreated{}}) ->
    created;
unmarshal(transfer_status, {prepared, #deposit_TransferPrepared{}}) ->
    prepared;
unmarshal(transfer_status, {committed, #deposit_TransferCommitted{}}) ->
    committed;
unmarshal(transfer_status, {cancelled, #deposit_TransferCancelled{}}) ->
    cancelled;

unmarshal(reposit_change, {created, Reposit}) ->
    {created, unmarshal(reposit, Reposit)};
unmarshal(reposit_change, {status_changed, RepositStatus}) ->
    {status_changed, unmarshal(reposit_status, RepositStatus)};

unmarshal(reposit, #deposit_Reposit{
    deposit     = DepositID,
    source      = WalletID,
    destination = SourceID,
    body        = Body,
    created_at  = CreatedAt,
    reason      = Reason
}) ->
    genlib_map:compact(#{
        deposit_id      => unmarshal(id, DepositID),
        source_id       => unmarshal(id, SourceID),
        wallet_id       => unmarshal(id, WalletID),
        body            => unmarshal(cash, Body),
        create_at       => unmarshal(string, CreatedAt),
        reason          => unmarshal(string, Reason)
    });

unmarshal(reposit_status_changed, {pending, #deposit_RepositPending{}}) ->
    pending;
unmarshal(reposit_status_changed, {succeeded, #deposit_RepositSucceeded{}}) ->
    succeeded;
% TODO: Process failures propertly
% unmarshal(reposit_status_changed, {failed, #deposit_RepositFailed{failure = Failure}}) ->
%     {failed, unmarshal(failure, Failure)};
unmarshal(reposit_status_changed, {failed, #deposit_RepositFailed{failure = #deposit_Failure{}}}) ->
    {failed, #domain_Failure{code = <<"unknown">>}};

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).
