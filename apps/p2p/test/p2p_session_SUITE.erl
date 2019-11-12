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
-export([create_fail_test/1]).
-export([create_ok_test/1]).
-export([unknown_test/1]).

%% Internal types

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

%% Macro helpers

-define(final_balance(Cash), {
    element(1, Cash),
    {
        {inclusive, element(1, Cash)}, {inclusive, element(1, Cash)}
    },
    element(2, Cash)
}).
-define(final_balance(Amount, Currency), ?final_balance({Amount, Currency})).

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
    TokenPrefix = <<"token_interaction_">>,
    TokenRandomised = generate_id(),
    Token = <<TokenPrefix/binary, TokenRandomised/binary>>,
    #{
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = prepare_standard_environment(Cash, Token, C),
    P2PSessionID = generate_id(),
    P2PProviderID = 1,
    P2PSessionParams = #{
        id => <<"p2p_transfer_id">>,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash
    },
    ok = p2p_session_machine:create(P2PSessionID, P2PSessionParams, #{provider_id => P2PProviderID}),
    Callback = ?CALLBACK(Token, <<"payload">>),
    ?assertMatch(<<"user_sleep">>, await_p2p_session_adapter_state(P2PSessionID, <<"user_sleep">>)),
    ?assertMatch({ok, ?PROCESS_CALLBACK_SUCCESS(<<"user_payload">>)}, call_host(Callback)),
    ?assertMatch(<<"user_callback">>, get_p2p_session_adapter_state(P2PSessionID)),
    ?assertMatch({finished, success}, await_final_p2p_session_status(P2PSessionID)),
    ?assertMatch(<<"user_sleep_finished">>, await_p2p_session_adapter_state(P2PSessionID, <<"user_sleep_finished">>)),
    ?assertMatch({ok, ?PROCESS_CALLBACK_FINISHED(<<"user_sleep_finished">>)}, call_host(Callback)).

-spec callback_ok_test(config()) -> test_return().
callback_ok_test(C) ->
    Cash = {999, <<"RUB">>},
    TokenPrefix = <<"token_callback_">>,
    TokenRandomised = generate_id(),
    Token = <<TokenPrefix/binary, TokenRandomised/binary>>,
    #{
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = prepare_standard_environment(Cash, Token, C),
    P2PSessionID = generate_id(),
    P2PProviderID = 1,
    P2PSessionParams = #{
        id => <<"p2p_transfer_id">>,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash
    },
    ok = p2p_session_machine:create(P2PSessionID, P2PSessionParams, #{provider_id => P2PProviderID}),
    Callback = ?CALLBACK(Token, <<"payload">>),
    ?assertMatch(<<"simple_sleep">>, await_p2p_session_adapter_state(P2PSessionID, <<"simple_sleep">>)),
    ?assertMatch({ok, ?PROCESS_CALLBACK_SUCCESS(<<"simple_payload">>)}, call_host(Callback)),
    ?assertMatch(<<"simple_callback">>, get_p2p_session_adapter_state(P2PSessionID)),
    ?assertMatch({finished, success}, await_final_p2p_session_status(P2PSessionID)),
    ?assertMatch(<<"sleep_finished">>, await_p2p_session_adapter_state(P2PSessionID, <<"sleep_finished">>)),
    ?assertMatch({ok, ?PROCESS_CALLBACK_FINISHED(<<"sleep_finished">>)}, call_host(Callback)).

-spec wrong_callback_tag_test(config()) -> test_return().
wrong_callback_tag_test(C) ->
    Cash = {99, <<"RUB">>},
    TokenPrefix = <<"token_wrong_">>,
    TokenRandomised = generate_id(),
    Token = <<TokenPrefix/binary, TokenRandomised/binary>>,
    #{
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = prepare_standard_environment(Cash, Token, C),
    P2PSessionID = generate_id(),
    P2PProviderID = 1,
    P2PSessionParams = #{
        id => <<"p2p_transfer_id">>,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash
    },
    ok = p2p_session_machine:create(P2PSessionID, P2PSessionParams, #{provider_id => P2PProviderID}),
    WrongCallback = ?CALLBACK(<<"WRONG">>, <<"payload">>),
    State0 = <<"wrong">>,
    State1 = <<"wrong_finished">>,
    ?assertMatch(State0, await_p2p_session_adapter_state(P2PSessionID, State0)),
    ?assertMatch({exception, #p2p_adapter_SessionNotFound{}}, call_host(WrongCallback)),
    ?assertMatch(State1, await_p2p_session_adapter_state(P2PSessionID, State1)),
    ?assertMatch({finished, success}, await_final_p2p_session_status(P2PSessionID)),
    ?assertMatch({exception, #p2p_adapter_SessionNotFound{}}, call_host(WrongCallback)).

-spec create_fail_test(config()) -> test_return().
create_fail_test(C) ->
    Cash = {1001, <<"RUB">>},
    #{
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = prepare_standard_environment(Cash, C),
    P2PSessionID = generate_id(),
    P2PProviderID = 1,
    P2PSessionParams = #{
        id => <<"p2p_transfer_id">>,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash
    },
    ok = p2p_session_machine:create(P2PSessionID, P2PSessionParams, #{provider_id => P2PProviderID}),
    ?assertEqual({finished, {failure, #{code => <<"test_failure">>}}}, await_final_p2p_session_status(P2PSessionID)).

-spec create_ok_test(config()) -> test_return().
create_ok_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = prepare_standard_environment(Cash, C),
    P2PSessionID = generate_id(),
    P2PProviderID = 1,
    P2PSessionParams = #{
        id => <<"p2p_transfer_id">>,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash
    },
    ok = p2p_session_machine:create(P2PSessionID, P2PSessionParams, #{provider_id => P2PProviderID}),
    ?assertEqual({finished, success}, await_final_p2p_session_status(P2PSessionID)).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    P2PSessionID = <<"unknown_p2p_session">>,
    Result = p2p_session_machine:get(P2PSessionID),
    ?assertMatch({error, {unknown_p2p_session, P2PSessionID}}, Result).

%% Utils

prepare_standard_environment(P2PTransferCash, C) ->
    prepare_standard_environment(P2PTransferCash, undefined, C).

prepare_standard_environment(_P2PTransferCash, Token, C) ->
    ResourceSender = create_resource_raw(Token, C),
    ResourceReceiver = create_resource_raw(Token, C),
    #{
        sender => make_resource_full(ResourceSender),
        receiver => make_resource_full(ResourceReceiver)
    }.

make_resource_full(#{token := Token} = RawBankCard) ->
    {ok, BinData} = ff_bin_data:get(Token, undefined),
    KeyList = [payment_system, bank_name, iso_country_code, card_type],
    ExtendData = maps:with(KeyList, BinData),
    {raw_full, maps:merge(RawBankCard, ExtendData#{bin_data_id => ff_bin_data:id(BinData)})}.

get_p2p_session(P2PSessionID) ->
    {ok, Machine} = p2p_session_machine:get(P2PSessionID),
    p2p_session_machine:session(Machine).

get_p2p_session_status(P2PSessionID) ->
    p2p_session:status(get_p2p_session(P2PSessionID)).

await_p2p_session_adapter_state(P2PSessionID, State) ->
    Poller = fun() -> get_p2p_session_adapter_state(P2PSessionID) end,
    Retry = genlib_retry:linear(15, 1000),
    ct_helper:await(State, Poller, Retry).

get_p2p_session_adapter_state(P2PSessionID) ->
    P2PSession = get_p2p_session(P2PSessionID),
    p2p_session:adapter_state(P2PSession).

await_final_p2p_session_status(P2PSessionID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            P2PSession = get_p2p_session(P2PSessionID),
            case p2p_session:is_finished(P2PSession) of
                false ->
                    {not_finished, P2PSession};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(15, 1000)
    ),
    get_p2p_session_status(P2PSessionID).

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

call_host(Callback) ->
    Service  = {dmsl_p2p_adapter_thrift, 'P2PAdapterHost'},
    Function = 'ProcessCallback',
    Args     = [Callback],
    Request  = {Service, Function, Args},
    ff_woody_client:call(p2p_adapter_host, Request).
