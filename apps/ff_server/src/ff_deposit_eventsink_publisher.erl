-module(ff_deposit_eventsink_publisher).

-behaviour(ff_eventsink_publisher).

-export([publish_events/1]).

-include_lib("fistful_proto/include/ff_proto_deposit_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_cashflow_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type event() :: ff_eventsink_publisher:event(ff_deposit:event()).
-type sinkevent() :: ff_eventsink_publisher:sinkevent(ff_proto_deposit_thrift:'SinkEvent'()).


%% Data transform

-define(transaction_body_to_cash(Amount, SymCode),
    #{amount => Amount, currency => #{symbolic_code => SymCode}}).

final_account_to_final_cash_flow_account(#{
    account := #{id := AccountID},
    type := AccountType}) ->
    #{account_type => AccountType, account_id => AccountID}.

-define(to_session_event(SessionID, Payload),
    {session, #{id => SessionID, payload => Payload}}).

%%
%% Internals
%%

-spec publish_events(list(event())) ->
    list(sinkevent()).

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

-spec publish_event(event()) ->
    sinkevent().

publish_event(#{
    id          := ID,
    source_id   := SourceID,
    event       := {
        EventID,
        Dt,
        {ev, EventDt, Payload}
    }
}) ->
    #deposit_SinkEvent{
        id            = marshal(event_id, ID),
        created_at    = marshal(timestamp, Dt),
        source        = marshal(id, SourceID),
        payload       = #deposit_Event{
            sequence   = marshal(event_id, EventID),
            occured_at = marshal(timestamp, EventDt),
            changes    = [marshal(event, ff_transfer:maybe_migrate(Payload, deposit))]
        }
    }.

%%

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(event, {created, Deposit}) ->
    {created, marshal(deposit, Deposit)};
marshal(event, {status_changed, DepositStatus}) ->
    {status_changed, marshal(deposit_status_changed, DepositStatus)};
marshal(event, {p_transfer, TransferChange}) ->
    {transfer, marshal(postings_transfer_change, TransferChange)};

marshal(deposit, #{
        body := {Amount, SymCode},
        params := Params
}) ->
    WalletID = maps:get(wallet_id, Params),
    SourceID = maps:get(source_id, Params),
    #deposit_Deposit{
        body = marshal(cash, ?transaction_body_to_cash(Amount, SymCode)),
        wallet = marshal(id, WalletID),
        source = marshal(id, SourceID)
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
        volume := {Amount, SymCode}
}) ->
    Details = maps:get(details, Postings, undefined),
    #cashflow_FinalCashFlowPosting{
        source      = marshal(final_cash_flow_account,
            final_account_to_final_cash_flow_account(Source)),
        destination = marshal(final_cash_flow_account,
            final_account_to_final_cash_flow_account(Destination)),
        volume      = marshal(cash, ?transaction_body_to_cash(Amount, SymCode)),
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

marshal(T, V) ->
    ff_eventsink_publisher:marshal(T, V).
