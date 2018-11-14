-module(ff_withdrawal_session_eventsink_publisher).

-behaviour(ff_eventsink_publisher).

-export([publish_events/1]).

-include_lib("fistful_proto/include/ff_proto_withdrawal_session_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type event() :: ff_eventsink_publisher:event(ff_withdrawal_session_machine:ev()).
-type sinkevent() :: ff_eventsink_publisher:sinkevent(
    ff_proto_withdrawal_session_thrift:'SinkEvent'()
).

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
    #wthd_session_SinkEvent{
        id            = marshal(event_id, ID),
        created_at    = marshal(timestamp, Dt),
        source        = marshal(id, SourceID),
        payload       = #wthd_session_Event{
            sequence   = marshal(event_id, EventID),
            occured_at = marshal(timestamp, EventDt),
            changes    = [marshal(event, ff_transfer:maybe_migrate(Payload))]
        }
    }.
%%

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(event, {created, Session}) ->
    {created, marshal(session, Session)};
marshal(event, {next_state, AdapterState}) ->
    {next_state, marshal(adapter_state, AdapterState)}; % handle in base marshal
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
        provider = marshal(id, ProviderID)
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
    #wthd_session_withdrawal{
        id = marshal(id, WithdrawalID),
        destination = ff_destination_eventsink_publisher:marshal(destination, Destination),
        cash = marshal(cash, Cash),
        sender = ff_identity_eventsink_publisher:marshal(identity, SenderIdentity),
        receiver = ff_identity_eventsink_publisher:marshal(identity, ReceiverIdentity)
    };

marshal(session_result, {success, TransactionInfo}) ->
    {success, #wthd_session_SessionResultSuccess{
        trx_info = marshal(transaction_info, TransactionInfo)
    }};
marshal(session_result, {failed, Failure}) ->
    {success, #wthd_session_SessionResultFailed{
        failure = marshal(failure, Failure)
    }};

marshal(T, V) ->
    ff_eventsink_publisher:marshal(T, V).
