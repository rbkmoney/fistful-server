-module(ff_p_transfer_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_cashflow_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(change, {created, Transfer}) ->
    {created, #transfer_CreatedChange{transfer = marshal(transfer, Transfer)}};
marshal(change, {status_changed, Status}) ->
    {status_changed, #transfer_StatusChange{status = marshal(status, Status)}};
marshal(change, {clock_updated, Clock}) ->
    {clock_updated, #transfer_ClockChange{clock = marshal(clock, Clock)}};

marshal(transfer, #{final_cash_flow := Cashflow}) ->
    #transfer_Transfer{
        cashflow = ff_cash_flow_codec:marshal(final_cash_flow, Cashflow)
    };

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

unmarshal(change, {created, #transfer_CreatedChange{transfer = Transfer}}) ->
    {created, unmarshal(transfer, Transfer)};
unmarshal(change, {status_changed, #transfer_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};
unmarshal(change, {clock_updated, #transfer_ClockChange{clock = Clock}}) ->
    {clock_updated, unmarshal(clock, Clock)};

unmarshal(transfer, #transfer_Transfer{cashflow = Cashflow}) ->
    #{
        cashflow => ff_cash_flow_codec:unmarshal(final_cash_flow, Cashflow)
    };

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
