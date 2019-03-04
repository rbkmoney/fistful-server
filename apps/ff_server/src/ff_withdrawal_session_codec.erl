-module(ff_withdrawal_session_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_withdrawal_session_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

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
    destination := Destination,
    cash := Cash
}) ->
    SenderIdentity = maps:get(sender, Params, undefined),
    ReceiverIdentity = maps:get(receiver, Params, undefined),
    #wthd_session_Withdrawal{
        id = marshal(id, WithdrawalID),
        destination = ff_destination_codec:marshal_destination(Destination),
        cash = marshal(cash, Cash),
        sender = ff_identity_codec:marshal(identity, SenderIdentity),
        receiver = ff_identity_codec:marshal(identity, ReceiverIdentity)
    };

marshal(msgpack_value, V) ->
    marshal_dmsl(V);

marshal(session_result, {success, TransactionInfo}) ->
    {success, #wthd_session_SessionResultSuccess{
        trx_info = marshal(transaction_info, TransactionInfo)
    }};
% TODO change all dmsl types to fistfull types
marshal(transaction_info, #domain_TransactionInfo{
    id = TransactionID,
    timestamp = Timestamp,
    extra = Extra
}) ->
    #'TransactionInfo'{
        id = marshal(id, TransactionID),
        timestamp = marshal(timestamp, Timestamp),
        extra = Extra
    };

marshal(session_result, {failed, Failure}) ->
    {failed, #wthd_session_SessionResultFailed{
        failure = marshal(failure, Failure)
    }};

marshal(failure, #domain_Failure{
    code = Code,
    reason = Reason,
    sub = SubFailure
}) ->
    #'Failure'{
        code = marshal(string, Code),
        reason = marshal(string, Reason),
        sub = marshal(sub_failure, SubFailure)
    };
marshal(sub_failure, #domain_SubFailure{
    code = Code,
    sub = SubFailure
}) ->
    #'SubFailure'{
        code = marshal(string, Code),
        sub = marshal(sub_failure, SubFailure)
    };

marshal(T, V) ->
    ff_codec:marshal(T, V).

% Convert msgpack from dmsl to fistful proto
marshal_dmsl({nl, #msgpack_Nil{}}) ->
    {nl, #msgp_Nil{}};
marshal_dmsl({arr, List}) when is_list(List) ->
    {arr, [marshal_dmsl(V) || V <- List]};
marshal_dmsl({obj, Map}) when is_map(Map) ->
    {obj, maps:fold(
        fun (K, V, Acc) ->
            NewK = marshal_dmsl(K),
            NewV = marshal_dmsl(V),
            Acc#{NewK => NewV}
        end,
        #{},
        Map
    )};
marshal_dmsl(Other) ->
    Other.


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
    destination = Destination,
    cash = Cash,
    sender = SenderIdentity,
    receiver = ReceiverIdentity
}) ->
    genlib_map:compact(#{
        id => unmarshal(id, WithdrawalID),
        destination => ff_destination_codec:unmarshal_destination(Destination),
        cash => unmarshal(cash, Cash),
        sender => ff_identity_codec:unmarshal(identity, SenderIdentity),
        receiver => ff_identity_codec:unmarshal(identity, ReceiverIdentity)
    });

unmarshal(msgpack_value, V) ->
    unmarshal_dmsl(V);

unmarshal(session_result, {success, #wthd_session_SessionResultSuccess{trx_info = Trx}}) ->
    {success, unmarshal(transaction_info, Trx)};
% TODO change all dmsl types to fistfull types
unmarshal(transaction_info, #'TransactionInfo'{
    id = TransactionID,
    timestamp = Timestamp,
    extra = Extra
}) ->
    #domain_TransactionInfo{
        id = unmarshal(id, TransactionID),
        timestamp = maybe_unmarshal(timestamp, Timestamp),
        extra = Extra
    };

unmarshal(session_result, {failed, #wthd_session_SessionResultFailed{failure = Failure}}) ->
    {failed, unmarshal(failure, Failure)};

unmarshal(failure, #'Failure'{
    code = Code,
    reason = Reason,
    sub = SubFailure
}) ->
    #domain_Failure{
        code = unmarshal(string, Code),
        reason = maybe_unmarshal(string, Reason),
        sub = maybe_unmarshal(sub_failure, SubFailure)
    };
unmarshal(sub_failure, #'SubFailure'{
    code = Code,
    sub = SubFailure
}) ->
    #domain_SubFailure{
        code = unmarshal(string, Code),
        sub = maybe_unmarshal(sub_failure, SubFailure)
    };

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

% Convert msgpack from fistful proto to dmsl
unmarshal_dmsl({nl, #msgp_Nil{}}) ->
    {nl, #msgpack_Nil{}};
unmarshal_dmsl({arr, List}) when is_list(List) ->
    {arr, [unmarshal_dmsl(V) || V <- List]};
unmarshal_dmsl({obj, Map}) when is_map(Map) ->
    {obj, maps:fold(
        fun (K, V, Acc) ->
            NewK = unmarshal_dmsl(K),
            NewV = unmarshal_dmsl(V),
            Acc#{NewK => NewV}
        end,
        #{},
        Map
    )};
unmarshal_dmsl(Other) ->
    Other.

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).
