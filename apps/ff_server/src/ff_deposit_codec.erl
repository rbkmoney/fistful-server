-module(ff_deposit_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_deposit_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_deposit_status_thrift.hrl").

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

marshal(event, {created, Deposit}) ->
    {created, #deposit_CreatedChange{deposit = marshal(deposit, Deposit)}};
marshal(event, {status_changed, Status}) ->
    {status_changed, #deposit_StatusChange{status = marshal(status, Status)}};
marshal(event, {p_transfer, TransferChange}) ->
    {transfer, #deposit_TransferChange{payload = ff_p_transfer_codec:marshal(event, TransferChange)}};
marshal(event, {limit_check, Details}) ->
    {limit_check, #deposit_LimitCheckChange{details = ff_limit_check_codec:marshal(details, Details)}};

marshal(deposit, Deposit) ->
    #deposit_Deposit{
        id = marshal(id, ff_deposit:id(Deposit)),
        body = marshal(cash, ff_deposit:body(Deposit)),
        status = maybe_marshal(status, ff_deposit:status(Deposit)),
        wallet = marshal(id, ff_deposit:wallet_id(Deposit)),
        source = marshal(id, ff_deposit:source_id(Deposit)),
        external_id = marshal(id, ff_deposit:external_id(Deposit))
    };

marshal(status, pending) ->
    {pending, #dep_status_Pending{}};
marshal(status, succeeded) ->
    {succeeded, #dep_status_Succeeded{}};
marshal(status, {failed, Failure}) ->
    {failed, #dep_status_Failed{failure = marshal(failure, Failure)}};

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

unmarshal(event, {created, #deposit_CreatedChange{deposit = Deposit}}) ->
    {created, unmarshal(deposit, Deposit)};
unmarshal(event, {status_changed, #deposit_StatusChange{status = DepositStatus}}) ->
    {status_changed, unmarshal(status, DepositStatus)};
unmarshal(event, {transfer, #deposit_TransferChange{payload = TransferChange}}) ->
    {p_transfer, ff_p_transfer_codec:unmarshal(event, TransferChange)};
unmarshal(event, {limit_check, #deposit_LimitCheckChange{details = Details}}) ->
    {limit_check, ff_limit_check_codec:unmarshal(details, Details)};

unmarshal(deposit, #deposit_Deposit{
    id = ID,
    body = Cash,
    wallet = WalletID,
    source = SourceID,
    external_id = ExternalID
}) ->
    #{
        id => unmarshal(id, ID),
        body => unmarshal(cash, Cash),
        params => #{
            wallet_id => unmarshal(id, WalletID),
            source_id => unmarshal(id, SourceID),
            external_id => unmarshal(id, ExternalID)
        }
    };

unmarshal(status, {pending, #dep_status_Pending{}}) ->
    pending;
unmarshal(status, {succeeded, #dep_status_Succeeded{}}) ->
    succeeded;
unmarshal(status, {failed, #dep_status_Failed{failure = Failure}}) ->
    {failed, unmarshal(failure, Failure)};

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
