-module(ff_p2p_session_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_p2p_session_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(change, {created, Session}) ->
    {created, #p2p_session_CreatedChange{session = marshal(session, Session)}};
marshal(change, {next_state, AdapterState}) ->
    {adapter_state, #p2p_session_AdapterStateChange{state = AdapterState}};
marshal(change, {transaction_bound, TransactionInfo}) ->
    {transaction_bound, #p2p_session_TransactionBoundChange{trx_info = marshal(transaction_info, TransactionInfo)}};
marshal(change, {finished, SessionResult}) ->
    {finished, #p2p_session_ResultChange{result = marshal(session_result, SessionResult)}};
marshal(change, {callback, CallbackChange}) ->
    {callback, marshal(callback_change, CallbackChange)};
marshal(change, {user_interaction, UserInteractionChange}) ->
    {ui, marshal(user_interaction_change, UserInteractionChange)};

marshal(session, #{
    id := ID,
    status := Status,
    transfer_params := TransferParams,
    provider_id := ProviderID,
    domain_revision := DomainRevision,
    party_revision := PartyRevision
}) ->
    #p2p_session_Session{
        id = marshal(id, ID),
        status = marshal(status, Status),
        p2p_transfer = marshal(p2p_transfer, TransferParams),
        provider = marshal(integer, ProviderID),
        party_revision = marshal(party_revision, PartyRevision),
        domain_revision = marshal(domain_revision, DomainRevision)
    };

marshal(status, active) ->
    {active, #p2p_session_SessionActive{}};
marshal(status, {finished, SessionResult}) ->
    {finished, #p2p_session_SessionFinished{result = marshal(session_result, SessionResult)}};

marshal(session_result, success) ->
    {success, #p2p_session_ResultSuccess{}};
marshal(session_result, {failure, Failure}) ->
    {failed, #p2p_session_ResultFailed{failure = marshal(failure, Failure)}};

marshal(p2p_transfer, Transfer = #{
    id := ID,
    body := Body,
    sender := Sender,
    receiver := Receiver
}) ->
    Deadline = maps:get(deadline, Transfer, undefined),
    #p2p_session_P2PTransfer{
        id = marshal(id, ID),
        sender = marshal(resource, Sender),
        receiver = marshal(resource, Receiver),
        cash = marshal(cash, Body),
        deadline = maybe_marshal(deadline, Deadline)
    };

marshal(deadline, Deadline) ->
    ff_time:to_rfc3339(Deadline);

marshal(callback_change, #{tag := Tag, payload := Payload}) ->
    #p2p_session_CallbackChange{
        tag = marshal(string, Tag),
        payload = marshal(callback_event, Payload)
    };

marshal(callback_event, {created, Callback}) ->
    {created, #p2p_session_CallbackCreatedChange{callback = marshal(callback, Callback)}};
marshal(callback_event, {finished, #{payload := Response}}) ->
    {finished, #p2p_session_CallbackResultChange{payload = Response}};
marshal(callback_event, {status_changed, Status}) ->
    {status_changed, #p2p_session_CallbackStatusChange{status = marshal(callback_status, Status)}};

marshal(callback, #{tag := Tag}) ->
    #p2p_session_Callback{tag = marshal(string, Tag)};

marshal(callback_status, pending) ->
    {pending, #p2p_session_CallbackStatusPending{}};
marshal(callback_status, succeeded) ->
    {succeeded, #p2p_session_CallbackStatusSucceeded{}};

marshal(user_interaction_change, #{id := ID, payload := Payload}) ->
    #p2p_session_UserInteractionChange{
        id = marshal(id, ID),
        payload = marshal(user_interaction_event, Payload)
    };

marshal(user_interaction_event, {created, UserInteraction}) ->
    {created, #p2p_session_UserInteractionCreatedChange{ui = marshal(user_interaction, UserInteraction)}};
marshal(user_interaction_event, {status_changed, Status}) ->
    {status_changed, #p2p_session_UserInteractionStatusChange{
        status = marshal(user_interaction_status, Status)
    }};

marshal(user_interaction, #{id := ID, content := Content}) ->
    #p2p_session_UserInteraction{
        id = marshal(id, ID),
        user_interaction = marshal(user_interaction_content, Content)
    };

marshal(user_interaction_content, {redirect, #{content := Redirect}}) ->
    {redirect, marshal(redirect, Redirect)};

marshal(redirect, {get, URI}) ->
    {get_request, #ui_BrowserGetRequest{uri = URI}};
marshal(redirect, {post, URI, Form}) ->
    {post_request, #ui_BrowserPostRequest{uri = URI, form = marshal(form, Form)}};

marshal(form, Form) when is_map(Form) ->
    maps:fold(fun(Key, Value, Map) ->
        Map#{marshal(string, Key) => marshal(string, Value)} end,
        #{},
        Form
    );

marshal(user_interaction_status, pending) ->
    {pending, #p2p_session_UserInteractionStatusPending{}};
marshal(user_interaction_status, finished) ->
    {finished, #p2p_session_UserInteractionStatusFinished{}};

marshal(T, V) ->
    ff_codec:marshal(T, V).

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #p2p_session_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, change}, Events),
        action => maybe_unmarshal(complex_action, Action)
    })};
unmarshal(repair_scenario, {set_session_result, #p2p_session_SetResultRepair{result = Result}}) ->
    {set_session_result, unmarshal(session_result, Result)};

unmarshal(change, {created, #p2p_session_CreatedChange{session = Session}}) ->
    {created, unmarshal(session, Session)};
unmarshal(change, {adapter_state, #p2p_session_AdapterStateChange{state = AdapterState}}) ->
    {next_state, AdapterState};
unmarshal(change, {transaction_bound, #p2p_session_TransactionBoundChange{trx_info = TransactionInfo}}) ->
    {transaction_bound, unmarshal(transaction_info, TransactionInfo)};
unmarshal(change, {finished, #p2p_session_ResultChange{result = SessionResult}}) ->
    {finished, unmarshal(session_result, SessionResult)};
unmarshal(change, {callback, #p2p_session_CallbackChange{tag = Tag, payload = Payload}}) ->
    {callback, #{
        tag => unmarshal(string, Tag),
        payload => unmarshal(callback_event, Payload)
    }};
unmarshal(change, {ui, #p2p_session_UserInteractionChange{id = ID, payload = Payload}}) ->
    {user_interaction, #{
        id => unmarshal(id, ID),
        payload => unmarshal(user_interaction_event, Payload)
    }};

unmarshal(session, #p2p_session_Session{
    id = ID,
    status = Status,
    p2p_transfer = P2PTransfer,
    provider = ProviderID,
    party_revision = PartyRevision,
    domain_revision = DomainRevision
}) ->
    #{
        id => unmarshal(id, ID),
        status => unmarshal(status, Status),
        transfer_params => unmarshal(p2p_transfer, P2PTransfer),
        provider_id => unmarshal(integer, ProviderID),
        party_revision => unmarshal(party_revision, PartyRevision),
        domain_revision => unmarshal(domain_revision, DomainRevision)
    };

unmarshal(status, {active, #p2p_session_SessionActive{}}) ->
    active;
unmarshal(status, {finished, #p2p_session_SessionFinished{result = SessionResult}}) ->
    {finished, unmarshal(session_result, SessionResult)};

unmarshal(session_result, {success, #p2p_session_ResultSuccess{}}) ->
    success;
unmarshal(session_result, {failed, #p2p_session_ResultFailed{failure = Failure}}) ->
    {failure, unmarshal(failure, Failure)};

unmarshal(p2p_transfer, #p2p_session_P2PTransfer{
    id = ID,
    sender = Sender,
    receiver = Receiver,
    cash = Body,
    deadline = Deadline
}) ->
    genlib_map:compact(#{
        id => unmarshal(id, ID),
        sender => unmarshal(resource, Sender),
        receiver => unmarshal(resource, Receiver),
        body => unmarshal(cash, Body),
        deadline => maybe_unmarshal(deadline, Deadline)
    });

unmarshal(deadline, Deadline) ->
    ff_time:from_rfc3339(Deadline);

unmarshal(callback_event, {created, #p2p_session_CallbackCreatedChange{callback = Callback}}) ->
    {created, unmarshal(callback, Callback)};
unmarshal(callback_event, {finished, #p2p_session_CallbackResultChange{payload = Response}}) ->
    {finished, #{payload => Response}};
unmarshal(callback_event, {status_changed, #p2p_session_CallbackStatusChange{status = Status}}) ->
    {status_changed, unmarshal(callback_status, Status)};

unmarshal(callback, #p2p_session_Callback{tag = Tag}) ->
    #{tag => unmarshal(string, Tag)};

unmarshal(callback_status, {pending, #p2p_session_CallbackStatusPending{}}) ->
    pending;
unmarshal(callback_status, {succeeded, #p2p_session_CallbackStatusSucceeded{}}) ->
    succeeded;

unmarshal(user_interaction_event, {created, #p2p_session_UserInteractionCreatedChange{
    ui = UserInteraction
}}) ->
    {created, unmarshal(user_interaction, UserInteraction)};
unmarshal(user_interaction_event, {status_changed, #p2p_session_UserInteractionStatusChange{
    status = Status
}}) ->
    {status_changed, unmarshal(user_interaction_status, Status)};

unmarshal(user_interaction, #p2p_session_UserInteraction{
    id = ID,
    user_interaction = Content
}) ->
    #{
        id => unmarshal(id, ID),
        content => unmarshal(user_interaction_content, Content)
    };

unmarshal(user_interaction_content, {redirect, Redirect}) ->
    {redirect, #{
        content => unmarshal(redirect, Redirect)
    }};

unmarshal(redirect, {get_request, #ui_BrowserGetRequest{uri = URI}}) ->
    {get, URI};
unmarshal(redirect, {post_request, #ui_BrowserPostRequest{uri = URI, form = Form}}) ->
    {post, URI, unmarshal(form, Form)};

unmarshal(form, Form) when is_map(Form) ->
    maps:fold(fun(Key, Value, Map) ->
        Map#{unmarshal(string, Key) => unmarshal(string, Value)} end,
        #{},
        Form
    );

unmarshal(user_interaction_status, {pending, #p2p_session_UserInteractionStatusPending{}}) ->
    pending;
unmarshal(user_interaction_status, {finished, #p2p_session_UserInteractionStatusFinished{}}) ->
    finished;

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


%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec p2p_session_codec_test() -> _.
p2p_session_codec_test() ->
    UserInteraction = #{
        id => genlib:unique(),
        content => {redirect, #{
            content => {get, <<"URI">>}
        }}
    },

    Callback = #{tag => <<"Tag">>},

    TransactionInfo = #{
        id => genlib:unique(),
        extra => <<"Extra">>
    },

    Resource = {bank_card, #{bank_card => #{
        token => <<"token">>,
        payment_system => visa
    }}},

    TransferParams = #{
        id => genlib:unique(),
        body => {123, <<"RUB">>},
        sender => Resource,
        receiver => Resource,
        deadline => ff_time:now()
    },

    Session = #{
        id => genlib:unique(),
        status => active,
        transfer_params => TransferParams,
        provider_id => 1,
        party_revision => 123,
        domain_revision => 321
    },

    Changes = [
        {created, Session},
        {next_state, <<"test state">>},
        {transaction_bound, TransactionInfo},
        {finished, success},
        {callback, #{tag => <<"Tag">>, payload => {created, Callback}}},
        {user_interaction, #{id => genlib:unique(), payload => {created, UserInteraction}}}
    ],
    Marshaled = marshal({list, change}, Changes),
    ?assertEqual(Changes, unmarshal({list, change}, Marshaled)).

-endif.
