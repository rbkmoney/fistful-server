-module(ff_withdrawal_session_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_withdrawal_session_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

-export([marshal_state/3]).

-export([marshal/2]).
-export([unmarshal/2]).

%% API
-spec marshal_state(ff_withdrawal_session:session_state(), ff_withdrawal_session:id(), ff_entity_context:context()) ->
    ff_proto_withdrawal_session_thrift:'SessionState'().

marshal_state(State, ID, Context) ->
    #wthd_session_SessionState{
        id = marshal(id, ID),
        status = marshal(session_status, ff_withdrawal_session:status(State)),
        withdrawal = marshal(withdrawal, ff_withdrawal_session:withdrawal(State)),
        route = marshal(route, ff_withdrawal_session:route(State)),
        context = marshal(ctx, Context)
    }.

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(timestamped_change, {ev, Timestamp, Change}) ->
    #wthd_session_TimestampedChange{
        change = marshal(change, Change),
        occured_at = ff_codec:marshal(timestamp, Timestamp)
    };

marshal(change, {created, Session}) ->
    {created, marshal(session, Session)};
marshal(change, {next_state, AdapterState}) ->
    {next_state, marshal(msgpack, AdapterState)};
marshal(change, {finished, SessionResult}) ->
    {finished, marshal(session_result, SessionResult)};
marshal(change, {callback, CallbackChange}) ->
    {callback, marshal(callback_change, CallbackChange)};

marshal(session, Session) ->
    #{
        id := SessionID,
        status := SessionStatus,
        withdrawal := Withdrawal,
        route := Route
    } = Session,
    #wthd_session_Session{
        id = marshal(id, SessionID),
        status = marshal(session_status, SessionStatus),
        withdrawal = marshal(withdrawal, Withdrawal),
        route = marshal(route, Route),
        provider_legacy = marshal(string, get_legacy_provider_id(Session))
    };

marshal(session_status, active) ->
    {active, #wthd_session_SessionActive{}};
marshal(session_status, {finished, Result}) ->
    {
        finished,
        #wthd_session_SessionFinished{status = marshal(session_finished_status, Result)}
    };
marshal(session_finished_status, {success, _}) ->
    {success, #wthd_session_SessionFinishedSuccess{}};
marshal(session_finished_status, {failed, Failure}) ->
    {failed, #wthd_session_SessionFinishedFailed{failure = marshal(failure, Failure)}};

marshal(withdrawal, Params = #{
    id := WithdrawalID,
    resource := Resource,
    cash := Cash
}) ->
    SenderIdentity = maps:get(sender, Params, undefined),
    ReceiverIdentity = maps:get(receiver, Params, undefined),
    SessionID = maps:get(session_id, Params, undefined),
    Quote = maps:get(quote, Params, undefined),
    #wthd_session_Withdrawal{
        id = marshal(id, WithdrawalID),
        destination_resource = marshal(resource, Resource),
        cash = marshal(cash, Cash),
        sender   = marshal(identity, SenderIdentity),
        receiver = marshal(identity, ReceiverIdentity),
        session_id = maybe_marshal(id, SessionID),
        quote = maybe_marshal(quote, Quote)
    };

marshal(identity, Identity = #{id := ID}) ->
    #wthd_session_Identity{
        identity_id = marshal(id, ID),
        effective_challenge = maybe_marshal(challenge, maps:get(effective_challenge, Identity, undefined))
    };

marshal(challenge, #{id := ID, proofs := Proofs}) ->
    #wthd_session_Challenge{
        id = maybe_marshal(id, ID),
        proofs = maybe_marshal({list, proof}, Proofs)
    };

marshal(proof, {Type, Token}) ->
    #wthd_session_ChallengeProof{
        type = Type,
        token = Token
    };

marshal(route, Route) ->
    #wthd_session_Route{
        provider_id = marshal(provider_id, maps:get(provider_id, Route)),
        terminal_id = maybe_marshal(terminal_id, genlib_map:get(terminal_id, Route))
    };

marshal(quote, #{
    cash_from := CashFrom,
    cash_to := CashTo,
    created_at := CreatedAt,
    expires_on := ExpiresOn,
    quote_data := Data
}) ->
    #wthd_session_Quote{
        cash_from = marshal(cash, CashFrom),
        cash_to = marshal(cash, CashTo),
        created_at = CreatedAt,
        expires_on = ExpiresOn,
        quote_data = maybe_marshal(msgpack, Data),
        quote_data_legacy = marshal(ctx, #{})
    };

marshal(ctx, Ctx) ->
    maybe_marshal(context, Ctx);

marshal(session_result, {success, TransactionInfo}) ->
    {success, #wthd_session_SessionResultSuccess{
        trx_info = marshal(transaction_info, TransactionInfo)
    }};
marshal(session_result, {failed, Failure}) ->
    {failed, #wthd_session_SessionResultFailed{
        failure = ff_codec:marshal(failure, Failure)
    }};

marshal(callback_change, #{tag := Tag, payload := Payload}) ->
    #wthd_session_CallbackChange{
        tag = marshal(string, Tag),
        payload = marshal(callback_event, Payload)
    };

marshal(callback_event, {created, Callback}) ->
    {created, #wthd_session_CallbackCreatedChange{callback = marshal(callback, Callback)}};
marshal(callback_event, {status_changed, Status}) ->
    {status_changed, #wthd_session_CallbackStatusChange{status = marshal(callback_status, Status)}};
marshal(callback_event, {finished, #{payload := Response}}) ->
    {finished, #wthd_session_CallbackResultChange{payload = Response}};

marshal(callback, #{tag := Tag}) ->
    #wthd_session_Callback{tag = marshal(string, Tag)};

marshal(callback_status, pending) ->
    {pending, #wthd_session_CallbackStatusPending{}};
marshal(callback_status, succeeded) ->
    {succeeded, #wthd_session_CallbackStatusSucceeded{}};

marshal(T, V) ->
    ff_codec:marshal(T, V).

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(timestamped_change, TimestampedChange) ->
    Timestamp = ff_codec:unmarshal(timestamp, TimestampedChange#wthd_session_TimestampedChange.occured_at),
    Change = unmarshal(change, TimestampedChange#wthd_session_TimestampedChange.change),
    {ev, Timestamp, Change};

unmarshal(repair_scenario, {add_events, #wthd_session_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, change}, Events),
        action => maybe_unmarshal(complex_action, Action)
    })};
unmarshal(repair_scenario, {set_session_result, #wthd_session_SetResultRepair{result = Result}}) ->
    {set_session_result, unmarshal(session_result, Result)};

unmarshal(change, {created, Session}) ->
    {created, unmarshal(session, Session)};
unmarshal(change, {next_state, AdapterState}) ->
    {next_state, unmarshal(msgpack, AdapterState)};
unmarshal(change, {finished, SessionResult}) ->
    {finished, unmarshal(session_result, SessionResult)};
unmarshal(change, {callback, #wthd_session_CallbackChange{tag = Tag, payload = Payload}}) ->
    {callback, #{
        tag => unmarshal(string, Tag),
        payload => unmarshal(callback_event, Payload)
    }};

unmarshal(session, #wthd_session_Session{
    id = SessionID,
    status = SessionStatus,
    withdrawal = Withdrawal,
    route = Route0
}) ->
    Route1 = unmarshal(route, Route0),
    genlib_map:compact(#{
        version => 3,
        id => unmarshal(id, SessionID),
        status => unmarshal(session_status, SessionStatus),
        withdrawal => unmarshal(withdrawal, Withdrawal),
        route => Route1,
        provider_legacy => get_legacy_provider_id(#{route => Route1})
    });

unmarshal(session_status, {active, #wthd_session_SessionActive{}}) ->
    active;
unmarshal(session_status, {finished, #wthd_session_SessionFinished{status = Result}}) ->
    {finished, unmarshal(session_finished_status, Result)};
unmarshal(session_finished_status, {success, #wthd_session_SessionFinishedSuccess{}}) ->
    %% need to fix proto to fill this with trx info mb?
    {success, #{id => <<"default id">>, extra => <<"default extra">>}};
unmarshal(session_finished_status, {failed, #wthd_session_SessionFinishedFailed{failure = Failure}}) ->
    {failed, unmarshal(failure, Failure)};

unmarshal(withdrawal, #wthd_session_Withdrawal{
    id = WithdrawalID,
    destination_resource = Resource,
    cash = Cash,
    sender = SenderIdentity,
    receiver = ReceiverIdentity,
    session_id = SessionID,
    quote = Quote
}) ->
    genlib_map:compact(#{
        id => unmarshal(id, WithdrawalID),
        resource => unmarshal(resource, Resource),
        cash => unmarshal(cash, Cash),
        sender => unmarshal(identity, SenderIdentity),
        receiver => unmarshal(identity, ReceiverIdentity),
        session_id => maybe_unmarshal(id, SessionID),
        quote => maybe_unmarshal(quote, Quote)
    });

unmarshal(identity, #wthd_session_Identity{
    identity_id = ID,
    effective_challenge = EffectiveChallenge
}) ->
    genlib_map:compact(#{
        id => unmarshal(id, ID),
        effective_challenge => maybe_unmarshal(challenge, EffectiveChallenge)
    });

unmarshal(challenge, #wthd_session_Challenge{
    id = ID,
    proofs = Proofs
}) ->
    #{
        id => maybe_unmarshal(id, ID),
        proofs => maybe_unmarshal({list, proof}, Proofs)
    };

unmarshal(proof, #wthd_session_ChallengeProof{
    type = Type,
    token = Token
}) ->
    {Type, Token};

unmarshal(route, Route) ->
    genlib_map:compact(#{
        provider_id => unmarshal(provider_id, Route#wthd_session_Route.provider_id),
        terminal_id => maybe_unmarshal(terminal_id, Route#wthd_session_Route.terminal_id)
    });

unmarshal(quote, #wthd_session_Quote{
    cash_from = CashFrom,
    cash_to = CashTo,
    created_at = CreatedAt,
    expires_on = ExpiresOn,
    quote_data = Data
}) ->
    genlib_map:compact(#{
        cash_from => unmarshal(cash, CashFrom),
        cash_to => unmarshal(cash, CashTo),
        created_at => CreatedAt,
        expires_on => ExpiresOn,
        quote_data => maybe_unmarshal(msgpack, Data)
    });

unmarshal(session_result, {success, #wthd_session_SessionResultSuccess{trx_info = Trx}}) ->
    {success, unmarshal(transaction_info, Trx)};
unmarshal(session_result, {failed, #wthd_session_SessionResultFailed{failure = Failure}}) ->
    {failed, ff_codec:unmarshal(failure, Failure)};

unmarshal(callback_event, {created, #wthd_session_CallbackCreatedChange{callback = Callback}}) ->
    {created, unmarshal(callback, Callback)};
unmarshal(callback_event, {finished, #wthd_session_CallbackResultChange{payload = Response}}) ->
    {finished, #{payload => Response}};
unmarshal(callback_event, {status_changed, #wthd_session_CallbackStatusChange{status = Status}}) ->
    {status_changed, unmarshal(callback_status, Status)};

unmarshal(callback, #wthd_session_Callback{tag = Tag}) ->
    #{tag => unmarshal(string, Tag)};

unmarshal(callback_status, {pending, #wthd_session_CallbackStatusPending{}}) ->
    pending;
unmarshal(callback_status, {succeeded, #wthd_session_CallbackStatusSucceeded{}}) ->
    succeeded;

unmarshal(ctx, Ctx) ->
    maybe_unmarshal(context, Ctx);

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

get_legacy_provider_id(#{provider_legacy := Provider}) when is_binary(Provider) ->
    Provider;
get_legacy_provider_id(#{route := #{provider_id := Provider}}) when is_integer(Provider) ->
    genlib:to_binary(Provider - 300).
