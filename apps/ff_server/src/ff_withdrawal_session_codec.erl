-module(ff_withdrawal_session_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_withdrawal_session_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().
marshal(event, {created, Session}) ->
    {created, marshal(session, Session)};
marshal(event, {next_state, AdapterState}) ->
    {next_state, marshal(msgpack_value, AdapterState)};
marshal(event, {finished, SessionResult}) ->
    {finished, marshal(session_result, SessionResult)};

marshal(session, #{
    id := SessionID,
    status := SessionStatus,
    withdrawal := Withdrawal,
    provider := ProviderID
}) ->
    #wthd_session_Session{
        id = marshal(id, SessionID),
        status = marshal(session_status, SessionStatus),
        withdrawal = marshal(withdrawal, Withdrawal),
        provider = marshal(id, genlib:to_binary(ProviderID))
    };

marshal(session_status, active) ->
    {active, #wthd_session_SessionActive{}};
marshal(session_status, {finished, Result}) ->
    {
        finished,
        #wthd_session_SessionFinished{status = marshal(session_finished_status, Result)}
    };
marshal(session_finished_status, success) ->
    {success, #wthd_session_SessionFinishedSuccess{}};
marshal(session_finished_status, failed) ->
    {failed, #wthd_session_SessionFinishedFailed{}};

marshal(withdrawal, Params = #{
    id := WithdrawalID,
    resource := Resource,
    cash := Cash
}) ->
    SenderIdentity = maps:get(sender, Params, undefined),
    ReceiverIdentity = maps:get(receiver, Params, undefined),
    #wthd_session_Withdrawal{
        id = marshal(id, WithdrawalID),
        destination_resource = marshal(resource, Resource),
        cash = marshal(cash, Cash),
        sender   = ff_identity_codec:marshal_identity(SenderIdentity),
        receiver = ff_identity_codec:marshal_identity(ReceiverIdentity)
    };

marshal(msgpack_value, V) ->
    marshal_msgpack(V);

marshal(session_result, {success, TransactionInfo}) ->
    {success, #wthd_session_SessionResultSuccess{
        trx_info = marshal(transaction_info, TransactionInfo)
    }};
marshal(session_result, {failed, Failure}) ->
    {failed, #wthd_session_SessionResultFailed{
        failure = ff_codec:marshal(failure, Failure)
    }};

marshal(T, V) ->
    ff_codec:marshal(T, V).

marshal_msgpack(nil)                  -> {nl, #msgp_Nil{}};
marshal_msgpack(V) when is_boolean(V) -> {b, V};
marshal_msgpack(V) when is_integer(V) -> {i, V};
marshal_msgpack(V) when is_float(V)   -> V;
marshal_msgpack(V) when is_binary(V)  -> {str, V}; % Assuming well-formed UTF-8 bytestring.
marshal_msgpack({binary, V}) when is_binary(V) ->
    {bin, V};
marshal_msgpack(V) when is_list(V) ->
    {arr, [marshal_msgpack(ListItem) || ListItem <- V]};
marshal_msgpack(V) when is_map(V) ->
    {obj, maps:fold(fun(Key, Value, Map) -> Map#{marshal_msgpack(Key) => marshal_msgpack(Value)} end, #{}, V)};
marshal_msgpack(undefined) ->
    undefined.

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #wthd_session_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, event}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};
unmarshal(repair_scenario, {set_session_result, #wthd_session_SetResultRepair{result = Result}}) ->
    {set_session_result, unmarshal(session_result, Result)};

unmarshal(event, {created, Session}) ->
    {created, unmarshal(session, Session)};
unmarshal(event, {next_state, AdapterState}) ->
    {next_state, unmarshal(msgpack_value, AdapterState)};
unmarshal(event, {finished, SessionResult}) ->
    {finished, unmarshal(session_result, SessionResult)};

unmarshal(session, #wthd_session_Session{
    id = SessionID,
    status = SessionStatus,
    withdrawal = Withdrawal,
    provider = ProviderID
}) ->
    #{
        id => unmarshal(id, SessionID),
        status => unmarshal(session_status, SessionStatus),
        withdrawal => unmarshal(withdrawal, Withdrawal),
        provider => unmarshal(id, erlang:binary_to_integer(ProviderID))
    };

unmarshal(session_status, {active, #wthd_session_SessionActive{}}) ->
    active;
unmarshal(session_status, {finished, #wthd_session_SessionFinished{status = Result}}) ->
    {finished, unmarshal(session_finished_status, Result)};
unmarshal(session_finished_status, {success, #wthd_session_SessionFinishedSuccess{}}) ->
    success;
unmarshal(session_finished_status, {failed, #wthd_session_SessionFinishedFailed{}}) ->
    failed;

unmarshal(withdrawal, #wthd_session_Withdrawal{
    id = WithdrawalID,
    destination_resource = Resource,
    cash = Cash,
    sender = SenderIdentity,
    receiver = ReceiverIdentity
}) ->
    genlib_map:compact(#{
        id => unmarshal(id, WithdrawalID),
        resource => unmarshal(resource, Resource),
        cash => unmarshal(cash, Cash),
        sender => ff_identity_codec:unmarshal(identity, SenderIdentity),
        receiver => ff_identity_codec:unmarshal(identity, ReceiverIdentity)
    });

unmarshal(msgpack_value, V) ->
    unmarshal_msgpack(V);

unmarshal(session_result, {success, #wthd_session_SessionResultSuccess{trx_info = Trx}}) ->
    {success, unmarshal(transaction_info, Trx)};
unmarshal(session_result, {failed, #wthd_session_SessionResultFailed{failure = Failure}}) ->
    {failed, ff_codec:unmarshal(failure, Failure)};

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

unmarshal_msgpack({nl,  #msgp_Nil{}})        -> nil;
unmarshal_msgpack({b,   V}) when is_boolean(V) -> V;
unmarshal_msgpack({i,   V}) when is_integer(V) -> V;
unmarshal_msgpack({flt, V}) when is_float(V)   -> V;
unmarshal_msgpack({str, V}) when is_binary(V)  -> V; % Assuming well-formed UTF-8 bytestring.
unmarshal_msgpack({bin, V}) when is_binary(V)  -> {binary, V};
unmarshal_msgpack({arr, V}) when is_list(V)    -> [unmarshal_msgpack(ListItem) || ListItem <- V];
unmarshal_msgpack({obj, V}) when is_map(V)     ->
    maps:fold(fun(Key, Value, Map) -> Map#{unmarshal_msgpack(Key) => unmarshal_msgpack(Value)} end, #{}, V);
unmarshal_msgpack(undefined) ->
    undefined.

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).
