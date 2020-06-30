-module(p2p_session_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("shumpune_proto/include/shumpune_shumpune_thrift.hrl").

%% Common test API

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests

-export([user_interaction_ok_test/1]).
-export([wrong_callback_tag_test/1]).
-export([callback_ok_test/1]).
-export([create_deadline_fail_test/1]).
-export([create_fail_test/1]).
-export([create_ok_test/1]).
-export([unknown_test/1]).

%% Internal types

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

%% Macro helpers

-define(CALLBACK(Tag, Payload), #p2p_adapter_Callback{tag = Tag, payload = Payload}).

-define(PROCESS_CALLBACK_SUCCESS(Payload), {succeeded, #p2p_adapter_ProcessCallbackSucceeded{
    response = #p2p_adapter_CallbackResponse{
        payload = Payload
    }
}}).

-define(PROCESS_CALLBACK_FINISHED(AdapterState), {finished, #p2p_adapter_ProcessCallbackFinished{
    response = #p2p_adapter_Context{
        session = #p2p_adapter_Session{
            state = AdapterState
        }
    }
}}).

%% API

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, default}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            user_interaction_ok_test,
            wrong_callback_tag_test,
            callback_ok_test,
            create_deadline_fail_test,
            create_fail_test,
            create_ok_test,
            unknown_test
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    ct_helper:makeup_cfg([
        ct_helper:test_case_name(init),
        ct_payment_system:setup()
    ], C).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C).

%%

-spec init_per_group(group_name(), config()) -> config().
init_per_group(_, C) ->
    C.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_, _) ->
    ok.
%%

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

%% Tests

-spec user_interaction_ok_test(config()) -> test_return().
user_interaction_ok_test(C) ->
    Cash = {101, <<"RUB">>},
    #{
        session_id := SessionID,
        transfer_params := TransferParams,
        session_params := SessionParams
    } = prepare_standard_environment(<<"token_interaction_">>, Cash, C),
    ok = p2p_session_machine:create(SessionID, TransferParams, SessionParams),
    ?assertMatch(<<"user_sleep">>, await_p2p_session_adapter_state(SessionID, <<"user_sleep">>)),
    ?assertMatch({finished, success}, await_final_p2p_session_status(SessionID)),
    ?assertMatch(<<"user_sleep_finished">>, await_p2p_session_adapter_state(SessionID, <<"user_sleep_finished">>)).

-spec callback_ok_test(config()) -> test_return().
callback_ok_test(C) ->
    Cash = {999, <<"RUB">>},
    #{
        session_id := SessionID,
        transfer_params := TransferParams,
        session_params := SessionParams,
        token := Token
    } = prepare_standard_environment(<<"token_callback_">>, Cash, C),
    ok = p2p_session_machine:create(SessionID, TransferParams, SessionParams),
    Callback = ?CALLBACK(Token, <<"payload">>),
    ?assertMatch(<<"simple_sleep">>, await_p2p_session_adapter_state(SessionID, <<"simple_sleep">>)),
    ?assertMatch({ok, ?PROCESS_CALLBACK_SUCCESS(<<"simple_payload">>)}, call_host(Callback)),
    ?assertMatch(<<"simple_callback">>, get_p2p_session_adapter_state(SessionID)),
    ?assertMatch({finished, success}, await_final_p2p_session_status(SessionID)),
    ?assertMatch(<<"sleep_finished">>, await_p2p_session_adapter_state(SessionID, <<"sleep_finished">>)).

-spec wrong_callback_tag_test(config()) -> test_return().
wrong_callback_tag_test(C) ->
    Cash = {99, <<"RUB">>},
    #{
        session_id := SessionID,
        transfer_params := TransferParams,
        session_params := SessionParams
    } = prepare_standard_environment(<<"token_wrong_">>, Cash, C),
    ok = p2p_session_machine:create(SessionID, TransferParams, SessionParams),
    WrongCallback = ?CALLBACK(<<"WRONG">>, <<"payload">>),
    State0 = <<"wrong">>,
    State1 = <<"wrong_finished">>,
    ?assertMatch(State0, await_p2p_session_adapter_state(SessionID, State0)),
    ?assertMatch({exception, #p2p_adapter_SessionNotFound{}}, call_host(WrongCallback)),
    ?assertMatch(State1, await_p2p_session_adapter_state(SessionID, State1)),
    ?assertMatch({exception, #p2p_adapter_SessionNotFound{}}, call_host(WrongCallback)).

-spec create_deadline_fail_test(config()) -> test_return().
create_deadline_fail_test(C) ->
    Cash = {1001, <<"RUB">>},
    #{
        session_id := SessionID,
        transfer_params := TransferParams,
        session_params := SessionParams
    } = prepare_standard_environment(Cash, C),
    ok = p2p_session_machine:create(SessionID, TransferParams#{deadline => 0}, SessionParams),
    Failure = #{
        code => <<"authorization_failed">>,
        reason => <<"{deadline_reached,0}">>,
        sub => #{
            code => <<"deadline_reached">>
        }
    },
    ?assertMatch({finished, {failure, Failure}}, await_final_p2p_session_status(SessionID)).

-spec create_fail_test(config()) -> test_return().
create_fail_test(C) ->
    Cash = {1001, <<"RUB">>},
    #{
        session_id := SessionID,
        transfer_params := TransferParams,
        session_params := SessionParams
    } = prepare_standard_environment(Cash, C),
    ok = p2p_session_machine:create(SessionID, TransferParams, SessionParams),
    ?assertEqual({finished, {failure, #{code => <<"test_failure">>}}}, await_final_p2p_session_status(SessionID)).

-spec create_ok_test(config()) -> test_return().
create_ok_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        session_id := SessionID,
        transfer_params := TransferParams,
        session_params := SessionParams
    } = prepare_standard_environment(Cash, C),
    ok = p2p_session_machine:create(SessionID, TransferParams, SessionParams),
    ?assertEqual({finished, success}, await_final_p2p_session_status(SessionID)).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    SessionID = <<"unknown_p2p_session">>,
    Result = p2p_session_machine:get(SessionID),
    ?assertMatch({error, {unknown_p2p_session, SessionID}}, Result).

%% Utils

prepare_standard_environment(TransferCash, C) ->
    prepare_standard_environment(undefined, TransferCash, C).

prepare_standard_environment(TokenPrefix, TransferCash, C) ->
    Token = case TokenPrefix of
        undefined ->
            undefined;
        _ ->
            TokenRandomised = generate_id(),
            <<TokenPrefix/binary, TokenRandomised/binary>>
    end,
    PartyID = create_party(C),
    ResourceSender = create_resource_raw(Token, C),
    ResourceReceiver = create_resource_raw(Token, C),
    SessionID = generate_id(),
    TransferParams = #{
        id => <<"p2p_transfer_id">>,
        sender => prepare_resource(ResourceSender),
        receiver => prepare_resource(ResourceReceiver),
        body => TransferCash
    },
    DomainRevision = ff_domain_config:head(),
    {ok, PartyRevision} = ff_party:get_revision(PartyID),
    SessionParams = #{
        route => #{
            provider_id => 1
        },
        domain_revision => DomainRevision,
        party_revision => PartyRevision
    },
    #{
        session_id => SessionID,
        transfer_params => TransferParams,
        session_params => SessionParams,
        token => Token
    }.

prepare_resource(#{token := Token} = RawBankCard) ->
    {ok, BinData} = ff_bin_data:get(Token, undefined),
    KeyList = [payment_system, bank_name, iso_country_code, card_type],
    ExtendData = maps:with(KeyList, BinData),
    {bank_card, #{
        bank_card => maps:merge(RawBankCard, ExtendData#{bin_data_id => ff_bin_data:id(BinData)}),
        auth_data => {session, #{
            session_id => <<"ID">>
        }}
    }}.

get_p2p_session(SessionID) ->
    {ok, Machine} = p2p_session_machine:get(SessionID),
    p2p_session_machine:session(Machine).

get_p2p_session_status(SessionID) ->
    p2p_session:status(get_p2p_session(SessionID)).

await_p2p_session_adapter_state(SessionID, State) ->
    Poller = fun() -> get_p2p_session_adapter_state(SessionID) end,
    Retry = genlib_retry:linear(15, 1000),
    ct_helper:await(State, Poller, Retry).

get_p2p_session_adapter_state(SessionID) ->
    Session = get_p2p_session(SessionID),
    p2p_session:adapter_state(Session).

await_final_p2p_session_status(SessionID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            Session = get_p2p_session(SessionID),
            case p2p_session:is_finished(Session) of
                false ->
                    {not_finished, Session};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(15, 1000)
    ),
    get_p2p_session_status(SessionID).

generate_id() ->
    ff_id:generate_snowflake_id().

create_resource_raw(Token, C) ->
    StoreSource = ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    case Token of
        undefined ->
            StoreSource;
        Token ->
            StoreSource#{token => Token}
    end.

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

call_host(Callback) ->
    Service  = {dmsl_p2p_adapter_thrift, 'P2PAdapterHost'},
    Function = 'ProcessCallback',
    Args     = [Callback],
    Request  = {Service, Function, Args},
    ff_woody_client:call(ff_p2p_adapter_host, Request).
