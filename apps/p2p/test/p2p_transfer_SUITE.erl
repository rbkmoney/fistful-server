-module(p2p_transfer_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
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
-export([session_user_interaction_ok_test/1]).
-export([session_callback_ok_test/1]).
-export([session_create_deadline_fail_test/1]).
-export([session_create_fail_test/1]).

-export([create_ok_with_inspector_fail_test/1]).
-export([route_not_found_fail_test/1]).
-export([create_cashlimit_validation_error_test/1]).
-export([create_currency_validation_error_test/1]).
-export([create_sender_resource_notfound_test/1]).
-export([create_receiver_resource_notfound_test/1]).
-export([create_ok_test/1]).
-export([balance_check_ok_test/1]).
-export([preserve_revisions_test/1]).
-export([unknown_test/1]).
-export([fees_passed/1]).

-export([consume_eventsinks/1]).

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

%% API

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, default},
        {group, balance},
        {group, eventsink}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            session_user_interaction_ok_test,
            session_callback_ok_test,
            session_create_deadline_fail_test,
            session_create_fail_test,
            create_ok_with_inspector_fail_test,
            route_not_found_fail_test,
            create_cashlimit_validation_error_test,
            create_currency_validation_error_test,
            create_sender_resource_notfound_test,
            create_receiver_resource_notfound_test,
            create_ok_test,
            preserve_revisions_test,
            unknown_test,
            fees_passed
        ]},
        {balance, [], [
            balance_check_ok_test
        ]},
        {eventsink, [], [
            consume_eventsinks
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

-spec balance_check_ok_test(config()) -> test_return().
balance_check_ok_test(C) ->
    Amount = 100,
    Currency = <<"RUB">>,
    Cash = {Amount, Currency},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, C),
    {ok, #domain_SystemAccountSet{accounts = Accounts}} =
        ff_domain_config:object({system_account_set, #domain_SystemAccountSetRef{id = 1}}),
    #domain_SystemAccount{
        settlement = Settlement,
        subagent = Subagent
    } = maps:get(#domain_CurrencyRef{symbolic_code = Currency}, Accounts),
    {SettlementAmountOnStart, _, _} = get_account_balance(Settlement, Currency),
    {SubagentAmountOnStart, _, _} = get_account_balance(Subagent, Currency),
    P2PTransferID = generate_id(),
    ClientInfo = #{
        ip_address => <<"some ip_address">>,
        fingerprint => <<"some fingerprint">>
    },
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash,
        client_info => ClientInfo,
        external_id => P2PTransferID
    },
    ok = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    ?assertEqual(succeeded, await_final_p2p_transfer_status(P2PTransferID)),
    SettlementBalanceOnEnd = get_account_balance(Settlement, Currency),
    SubagentBalanceOnEnd = get_account_balance(Subagent, Currency),
    SubagentEndCash = {SubagentAmountOnStart + 10, Currency},
    SettlementEndCash = {SettlementAmountOnStart - 15, Currency},
    ?assertEqual(?final_balance(SubagentEndCash), SubagentBalanceOnEnd),
    ?assertEqual(?final_balance(SettlementEndCash), SettlementBalanceOnEnd).

-spec session_user_interaction_ok_test(config()) -> test_return().
session_user_interaction_ok_test(C) ->
    Cash = {101, <<"RUB">>},
    Prefix = <<"token_interaction_">>,
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, {with_prefix, Prefix}, C),
    P2PTransferID = generate_id(),
    ClientInfo = #{
        ip_address => <<"some ip_address">>,
        fingerprint => <<"some fingerprint">>
    },
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash,
        client_info => ClientInfo,
        external_id => P2PTransferID
    },
    ok = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    ?assertMatch(<<"user_sleep">>, await_p2p_session_adapter_state(P2PTransferID, <<"user_sleep">>)),
    ?assertEqual(succeeded, await_final_p2p_transfer_status(P2PTransferID)).

-spec session_callback_ok_test(config()) -> test_return().
session_callback_ok_test(C) ->
    Cash = {999, <<"RUB">>},
    Prefix = <<"token_callback_">>,
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, {with_prefix, Prefix}, C),
    P2PTransferID = generate_id(),
    ClientInfo = #{
        ip_address => <<"some ip_address">>,
        fingerprint => <<"some fingerprint">>
    },
    {raw, #{resource_params := {bank_card, #{token := Token}}}} = ResourceSender,
    Callback = ?CALLBACK(Token, <<"payload">>),
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash,
        client_info => ClientInfo,
        external_id => P2PTransferID
    },
    ok = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    ?assertMatch(<<"simple_sleep">>, await_p2p_session_adapter_state(P2PTransferID, <<"simple_sleep">>)),
    ?assertMatch({ok, ?PROCESS_CALLBACK_SUCCESS(<<"simple_payload">>)}, call_host(Callback)),
    ?assertEqual(succeeded, await_final_p2p_transfer_status(P2PTransferID)).

-spec session_create_deadline_fail_test(config()) -> test_return().
session_create_deadline_fail_test(C) ->
    Cash = {199, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, C),
    P2PTransferID = generate_id(),
    ClientInfo = #{
        ip_address => <<"some ip_address">>,
        fingerprint => <<"some fingerprint">>
    },
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash,
        client_info => ClientInfo,
        external_id => P2PTransferID
    },
    Failure = #{
        code => <<"authorization_failed">>,
        reason => <<"{deadline_reached,0}">>,
        sub => #{
            code => <<"deadline_reached">>
        }
    },
    ok = p2p_transfer_machine:create(P2PTransferParams#{deadline => 0}, ff_entity_context:new()),
    ?assertEqual({failed, Failure}, await_final_p2p_transfer_status(P2PTransferID)).

-spec session_create_fail_test(config()) -> test_return().
session_create_fail_test(C) ->
    Cash = {1001, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, C),
    P2PTransferID = generate_id(),
    ClientInfo = #{
        ip_address => <<"some ip_address">>,
        fingerprint => <<"some fingerprint">>
    },
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash,
        client_info => ClientInfo,
        external_id => P2PTransferID
    },
    ok = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    ?assertEqual({failed, #{code => <<"test_failure">>}}, await_final_p2p_transfer_status(P2PTransferID)).

-spec create_ok_with_inspector_fail_test(config()) -> test_return().
create_ok_with_inspector_fail_test(C) ->
    Cash = {199, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, C),
    P2PTransferID = generate_id(),
    ClientInfo = #{
        ip_address => <<"some ip_address">>,
        fingerprint => <<"some fingerprint">>
    },
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash,
        client_info => ClientInfo,
        external_id => P2PTransferID
    },
    ok = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    ?assertEqual(succeeded, await_final_p2p_transfer_status(P2PTransferID)).

-spec route_not_found_fail_test(config()) -> test_return().
route_not_found_fail_test(C) ->
    Cash = {100, <<"USD">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, C),
    P2PTransferID = generate_id(),
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash
    },
    ok = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    Result = await_final_p2p_transfer_status(P2PTransferID),
    ?assertMatch({failed, #{code := <<"no_route_found">>}}, Result).

-spec create_cashlimit_validation_error_test(config()) -> test_return().
create_cashlimit_validation_error_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, C),
    P2PTransferID = generate_id(),
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => {20000000, <<"RUB">>},
        external_id => P2PTransferID
    },
    Result = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    CashRange = {{inclusive, {0, <<"RUB">>}}, {exclusive, {10000001, <<"RUB">>}}},
    Details = {terms_violation, {cash_range, {{20000000, <<"RUB">>}, CashRange}}},
    ?assertMatch({error, {terms, Details}}, Result).

-spec create_currency_validation_error_test(config()) -> test_return().
create_currency_validation_error_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, C),
    P2PTransferID = generate_id(),
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => {100, <<"EUR">>},
        external_id => P2PTransferID
    },
    Result = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    Details = {
        #domain_CurrencyRef{symbolic_code = <<"EUR">>},
        [
            #domain_CurrencyRef{symbolic_code = <<"RUB">>},
            #domain_CurrencyRef{symbolic_code = <<"USD">>}
        ]
    },
    ?assertMatch({error, {terms, {terms_violation, {not_allowed_currency, Details}}}}, Result).

-spec create_sender_resource_notfound_test(config()) -> test_return().
create_sender_resource_notfound_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, {missing, sender}, C),
    P2PTransferID = generate_id(),
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash,
        external_id => P2PTransferID
    },
    Result = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    ?assertMatch({error, {sender, {bin_data, not_found}}}, Result).

-spec create_receiver_resource_notfound_test(config()) -> test_return().
create_receiver_resource_notfound_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, {missing, receiver}, C),
    P2PTransferID = generate_id(),
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash,
        external_id => P2PTransferID
    },
    Result = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    ?assertMatch({error, {receiver, {bin_data, not_found}}}, Result).

-spec create_ok_test(config()) -> test_return().
create_ok_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, C),
    P2PTransferID = generate_id(),
    ClientInfo = #{
        ip_address => <<"some ip_address">>,
        fingerprint => <<"some fingerprint">>
    },
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash,
        client_info => ClientInfo,
        external_id => P2PTransferID
    },
    ok = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    ?assertEqual(succeeded, await_final_p2p_transfer_status(P2PTransferID)),
    P2PTransfer = get_p2p_transfer(P2PTransferID),
    ?assertEqual(IdentityID, p2p_transfer:owner(P2PTransfer)),
    ?assertEqual(ResourceSender, p2p_transfer:sender(P2PTransfer)),
    ?assertEqual(ResourceReceiver, p2p_transfer:receiver(P2PTransfer)),
    ?assertEqual(Cash, p2p_transfer:body(P2PTransfer)),
    ?assertEqual(ClientInfo, p2p_transfer:client_info(P2PTransfer)),
    ?assertEqual(P2PTransferID, p2p_transfer:external_id(P2PTransfer)).

-spec preserve_revisions_test(config()) -> test_return().
preserve_revisions_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, C),
    P2PTransferID = generate_id(),
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash,
        external_id => P2PTransferID
    },
    ok = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    P2PTransfer = get_p2p_transfer(P2PTransferID),
    ?assertNotEqual(undefined, p2p_transfer:domain_revision(P2PTransfer)),
    ?assertNotEqual(undefined, p2p_transfer:party_revision(P2PTransfer)),
    ?assertNotEqual(undefined, p2p_transfer:created_at(P2PTransfer)).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    P2PTransferID = <<"unknown_p2p_transfer">>,
    Result = p2p_transfer_machine:get(P2PTransferID),
    ?assertMatch({error, {unknown_p2p_transfer, P2PTransferID}}, Result).

-spec fees_passed(config()) -> test_return().
fees_passed(C) ->
    Cash = {1002, <<"RUB">>}, % see p2p_ct_provider_handler:handle_function_/4
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = p2p_tests_utils:prepare_standard_environment(Cash, C),
    P2PTransferID = generate_id(),
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash
    },
    ok = p2p_transfer_machine:create(P2PTransferParams, ff_entity_context:new()),
    ?assertEqual(succeeded, await_final_p2p_transfer_status(P2PTransferID)),
    P2PTransfer = get_p2p_transfer(P2PTransferID),
    ?assertEqual(IdentityID, p2p_transfer:owner(P2PTransfer)),
    ?assertEqual(ResourceSender, p2p_transfer:sender(P2PTransfer)),
    ?assertEqual(ResourceReceiver, p2p_transfer:receiver(P2PTransfer)),
    ?assertEqual(Cash, p2p_transfer:body(P2PTransfer)).

-spec consume_eventsinks(config()) -> test_return().
consume_eventsinks(_) ->
    EventSinks = [
          p2p_transfer_event_sink,
          p2p_session_event_sink
    ],
    [_Events = ct_eventsink:consume(1000, Sink) || Sink <- EventSinks].

%% Utils

get_p2p_transfer(P2PTransferID) ->
    {ok, Machine} = p2p_transfer_machine:get(P2PTransferID),
    p2p_transfer_machine:p2p_transfer(Machine).

get_p2p_transfer_status(P2PTransferID) ->
    p2p_transfer:status(get_p2p_transfer(P2PTransferID)).

await_final_p2p_transfer_status(P2PTransferID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            {ok, Machine} = p2p_transfer_machine:get(P2PTransferID),
            P2PTransfer = p2p_transfer_machine:p2p_transfer(Machine),
            case p2p_transfer:is_finished(P2PTransfer) of
                false ->
                    {not_finished, P2PTransfer};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(10, 1000)
    ),
    get_p2p_transfer_status(P2PTransferID).

generate_id() ->
    ff_id:generate_snowflake_id().

get_account_balance(AccountID, Currency) ->
    {ok, {Amounts, Currency}} = ff_transaction:balance(
        #{
            currency => Currency,
            accounter_account_id => AccountID
        },
        ff_clock:latest_clock()
    ),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

await_p2p_session_adapter_state(P2PTransferID, State) ->
    State = ct_helper:await(
        State,
        fun () ->
            {ok, Machine} = p2p_transfer_machine:get(P2PTransferID),
            P2PTransfer = p2p_transfer_machine:p2p_transfer(Machine),
            case maps:get(session, P2PTransfer, undefined) of
                undefined ->
                    undefined;
                #{id := SessionID} ->
                    get_p2p_session_adapter_state(SessionID)
            end
        end,
        genlib_retry:linear(10, 1000)
    ).

get_p2p_session(SessionID) ->
    {ok, Machine} = p2p_session_machine:get(SessionID),
    p2p_session_machine:session(Machine).

get_p2p_session_adapter_state(SessionID) ->
    Session = get_p2p_session(SessionID),
    p2p_session:adapter_state(Session).

call_host(Callback) ->
    Service  = {dmsl_p2p_adapter_thrift, 'P2PAdapterHost'},
    Function = 'ProcessCallback',
    Args     = [Callback],
    Request  = {Service, Function, Args},
    ff_woody_client:call(ff_p2p_adapter_host, Request).
