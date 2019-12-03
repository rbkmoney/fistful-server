-module(ff_p_transfer_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_cashflow_thrift.hrl").

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
    {created, #transfer_CreatedChange{transfer = marshal(transfer, Transfer)}};
marshal(event, {status_changed, Status}) ->
    {status_changed, #transfer_StatusChange{status = marshal(status, Status)}};
marshal(event, {clock_updated, Clock}) ->
    {clock_updated, #transfer_ClockChange{clock = marshal(clock, Clock)}};

marshal(transfer, #{final_cash_flow := Cashflow}) ->
    #transfer_Transfer{
        cashflow = marshal(final_cash_flow, Cashflow)
    };
marshal(final_cash_flow, #{postings := Postings}) ->
    #cashflow_FinalCashFlow{
        postings = marshal({list, postings}, Postings)
    };
marshal(postings, Posting) ->
    #{
        sender := Sender,
        receiver := Receiver,
        volume := Cash
    } = Posting,
    Details = maps:get(details, Posting, undefined),
    SenderAccount = final_account_to_final_cash_flow_account(Sender),
    ReceiverAccount = final_account_to_final_cash_flow_account(Receiver),
    #cashflow_FinalCashFlowPosting{
        source      = marshal(final_cash_flow_account, SenderAccount),
        destination = marshal(final_cash_flow_account, ReceiverAccount),
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

marshal(status, created) ->
    {created, #transfer_Created{}};
marshal(status, prepared) ->
    {prepared, #transfer_Prepared{}};
marshal(status, committed) ->
    {committed, #transfer_Committed{}};
marshal(status, cancelled) ->
    {cancelled, #transfer_Cancelled{}};

marshal(clock, Clock) ->
    ff_clock:marshal(transfer, Clock);

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(event, {created, #transfer_CreatedChange{transfer = Transfer}}) ->
    {created, unmarshal(transfer, Transfer)};
unmarshal(event, {status_changed, #transfer_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};
unmarshal(event, {clock_updated, #transfer_ClockChange{clock = Clock}}) ->
    {clock_updated, unmarshal(clock, Clock)};

unmarshal(transfer, #transfer_Transfer{
    cashflow = Cashflow
}) ->
    #{
        final_cash_flow => unmarshal(final_cash_flow, Cashflow)
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

unmarshal(status, {created, #transfer_Created{}}) ->
    created;
unmarshal(status, {prepared, #transfer_Prepared{}}) ->
    prepared;
unmarshal(status, {committed, #transfer_Committed{}}) ->
    committed;
unmarshal(status, {cancelled, #transfer_Cancelled{}}) ->
    cancelled;

unmarshal(clock, Clock) ->
    ff_clock:unmarshal(transfer, Clock);

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).
