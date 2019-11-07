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
-export([callback_ok_test/1]).
-export([fail_ok_test/1]).
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
            % user_interaction_ok_test,
            callback_ok_test
            % fail_ok_test,
            % create_ok_test,
            % unknown_test
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
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = prepare_standard_environment(Cash, C),
    P2PSessionID = generate_id(),
    P2PProviderID = 1,
    P2PSessionParams = #{
        id => <<"p2p_transfer_id">>,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        cash => Cash
    },
    ok = p2p_session_machine:create(P2PSessionID, P2PSessionParams, #{provider_id => P2PProviderID}),
    %% TODO call process callback here
    ok = timer:sleep(3000),
    ?assertEqual({finished, success}, await_final_p2p_session_status(P2PSessionID)),
    P2PSession = get_p2p_session(P2PSessionID),
    ?assertEqual(<<"user_sleep_finished">>, p2p_session:adapter_state(P2PSession)).

-spec callback_ok_test(config()) -> test_return().
callback_ok_test(C) ->
    Cash = {999, <<"RUB">>},
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
        cash => Cash
    },
    ok = p2p_session_machine:create(P2PSessionID, P2PSessionParams, #{provider_id => P2PProviderID}),
    %% TODO call process callback here
    WrongTagCallback = ?CALLBACK(<<"tag">>, <<"payload">>),
    Callback         = ?CALLBACK(<<"simple_tag">>, <<"payload">>),
    ?assertMatch({ok, {succeeded, _}}, call_host(Callback)),
    ok = timer:sleep(1000),
    ?assertEqual({exception, #p2p_adapter_SessionNotFound{}}, call_host(WrongTagCallback)),
    % ok = timer:sleep(1000),
    ok = timer:sleep(3000),
    _WrongTagResult = call_host(WrongTagCallback),
    ?assertMatch({ok, {finished, _}}, call_host(Callback)),
    ?assertEqual({finished, success}, await_final_p2p_session_status(P2PSessionID)),
    P2PSession = get_p2p_session(P2PSessionID),
    ?assertEqual(<<"sleep_finished">>, p2p_session:adapter_state(P2PSession)).

-spec fail_ok_test(config()) -> test_return().
fail_ok_test(C) ->
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
        cash => Cash
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
        cash => Cash
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
        genlib_retry:linear(10, 1000)
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
    Client   = ff_woody_client:new(<<"http://fistful-server:8022/v1/p2p_adapter_host">>),
    ff_woody_client:call(Client, Request).
