-module(ff_withdrawal_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_withdrawal_session_thrift.hrl").
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
-export([session_fail_test/1]).
-export([session_repair_test/1]).
-export([quote_fail_test/1]).
-export([route_not_found_fail_test/1]).
-export([provider_operations_forbidden_fail_test/1]).
-export([misconfigured_terminal_fail_test/1]).
-export([limit_check_fail_test/1]).
-export([create_cashlimit_validation_error_test/1]).
-export([create_wallet_currency_validation_error_test/1]).
-export([create_identity_providers_mismatch_error_test/1]).
-export([create_destination_currency_validation_error_test/1]).
-export([create_currency_validation_error_test/1]).
-export([create_destination_resource_notfound_test/1]).
-export([create_destination_notfound_test/1]).
-export([create_wallet_notfound_test/1]).
-export([create_ok_test/1]).
-export([quota_ok_test/1]).
-export([crypto_quota_ok_test/1]).
-export([preserve_revisions_test/1]).
-export([use_quote_revisions_test/1]).
-export([unknown_test/1]).
-export([provider_callback_test/1]).

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

%% API

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, default},
        {group, non_parallel}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            session_fail_test,
            session_repair_test,
            quote_fail_test,
            route_not_found_fail_test,
            provider_operations_forbidden_fail_test,
            misconfigured_terminal_fail_test,
            limit_check_fail_test,
            create_cashlimit_validation_error_test,
            create_wallet_currency_validation_error_test,
            create_destination_currency_validation_error_test,
            create_currency_validation_error_test,
            create_identity_providers_mismatch_error_test,
            create_destination_resource_notfound_test,
            create_destination_notfound_test,
            create_wallet_notfound_test,
            create_ok_test,
            quota_ok_test,
            crypto_quota_ok_test,
            preserve_revisions_test,
            unknown_test,
            provider_callback_test
        ]},
        {non_parallel, [sequence], [
            use_quote_revisions_test
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

-spec session_fail_test(config()) -> test_return().
session_fail_test(C) ->
    Party = create_party(C),
    Currency = <<"RUB">>,
    WithdrawalCash = {100, Currency},
    IdentityID = create_person_identity(Party, C, <<"quote-owner">>),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, Currency, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    DestinationID = create_destination(IdentityID, undefined, C),
    ok = set_wallet_balance(WithdrawalCash, WalletID),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => WithdrawalCash,
        quote => #{
            cash_from   => {4240, <<"RUB">>},
            cash_to     => {2120, <<"USD">>},
            created_at  => <<"2016-03-22T06:12:27Z">>,
            expires_on  => <<"2016-03-22T06:12:27Z">>,
            route       => ff_withdrawal_routing:make_route(3, 1),
            quote_data  => #{<<"test">> => <<"error">>}
        }
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    Result = await_final_withdrawal_status(WithdrawalID),
    ?assertMatch({failed, #{code := <<"test_error">>}}, Result),
    ?assertEqual(?final_balance(WithdrawalCash), get_wallet_balance(WalletID)).

-spec quote_fail_test(config()) -> test_return().
quote_fail_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        quote => #{
            cash_from   => {4240, <<"RUB">>},
            cash_to     => {2120, <<"USD">>},
            created_at  => <<"2016-03-22T06:12:27Z">>,
            expires_on  => <<"2016-03-22T06:12:27Z">>,
            route       => ff_withdrawal_routing:make_route(10, 10),
            quote_data  => #{<<"test">> => <<"test">>}
        }
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    Result = await_final_withdrawal_status(WithdrawalID),
    ?assertMatch({failed, #{code := <<"unknown">>}}, Result),
    ?assertEqual(?final_balance(Cash), get_wallet_balance(WalletID)).

-spec route_not_found_fail_test(config()) -> test_return().
route_not_found_fail_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, <<"USD_COUNTRY">>, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    Result = await_final_withdrawal_status(WithdrawalID),
    ?assertMatch({failed, #{code := <<"no_route_found">>}}, Result).


-spec provider_operations_forbidden_fail_test(config()) -> test_return().
provider_operations_forbidden_fail_test(C) ->
    Cash = {123123, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    Result = await_final_withdrawal_status(WithdrawalID),
    ?assertMatch({failed, #{code := <<"no_route_found">>}}, Result).

-spec misconfigured_terminal_fail_test(config()) -> test_return().
misconfigured_terminal_fail_test(C) ->
    Cash = {3500000, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    Result = await_final_withdrawal_status(WithdrawalID),
    ?assertMatch({failed, #{code := <<"no_route_found">>}}, Result).

-spec limit_check_fail_test(config()) -> test_return().
limit_check_fail_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => {200, <<"RUB">>}
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    Result = await_final_withdrawal_status(WithdrawalID),
    ?assertMatch({failed, #{
        code := <<"account_limit_exceeded">>,
        sub := #{
            code := <<"amount">>
        }
    }}, Result),
    ?assertEqual(?final_balance(Cash), get_wallet_balance(WalletID)).

-spec create_cashlimit_validation_error_test(config()) -> test_return().
create_cashlimit_validation_error_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => {20000000, <<"RUB">>}
    },
    Result = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    CashRange = {{inclusive, {0, <<"RUB">>}}, {exclusive, {10000001, <<"RUB">>}}},
    Details = {terms_violation, {cash_range, {{20000000, <<"RUB">>}, CashRange}}},
    ?assertMatch({error, {terms, Details}}, Result).

-spec create_wallet_currency_validation_error_test(config()) -> test_return().
create_wallet_currency_validation_error_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        destination_id := DestinationID,
        identity_id := IdentityID
    } = prepare_standard_environment(Cash, C),
    WalletID = create_wallet(IdentityID, <<"USD wallet">>, <<"USD">>, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => {100, <<"RUB">>}
    },
    Result = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertMatch({error, {inconsistent_currency, {<<"RUB">>, <<"USD">>, <<"RUB">>}}}, Result).

-spec create_destination_currency_validation_error_test(config()) -> test_return().
create_destination_currency_validation_error_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, <<"USD_CURRENCY">>, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => {100, <<"RUB">>}
    },
    Result = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertMatch({error, {inconsistent_currency, {<<"RUB">>, <<"RUB">>, <<"USD">>}}}, Result).

-spec create_currency_validation_error_test(config()) -> test_return().
create_currency_validation_error_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => {100, <<"EUR">>}
    },
    Result = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    Details = {
        #domain_CurrencyRef{symbolic_code = <<"EUR">>},
        [
            #domain_CurrencyRef{symbolic_code = <<"RUB">>},
            #domain_CurrencyRef{symbolic_code = <<"USD">>}
        ]
    },
    ?assertMatch({error, {terms, {terms_violation, {not_allowed_currency, Details}}}}, Result).

-spec create_identity_providers_mismatch_error_test(config()) -> test_return().

create_identity_providers_mismatch_error_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    Party = create_party(C),
    IdentityID = create_identity(Party, <<"good-two">>, <<"person">>, C),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, <<"RUB">>, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => {100, <<"RUB">>}
    },
    Result = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertMatch({error, {identity_providers_mismatch, {<<"good-two">>, <<"good-one">>}}}, Result).

-spec create_destination_resource_notfound_test(config()) -> test_return().
create_destination_resource_notfound_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, <<"TEST_NOTFOUND">>, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash
    },
    Result = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertMatch({error, {destination_resource, {bin_data, not_found}}}, Result).

-spec create_destination_notfound_test(config()) -> test_return().
create_destination_notfound_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        wallet_id := WalletID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => <<"unknown_destination">>,
        wallet_id => WalletID,
        body => Cash
    },
    Result = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertMatch({error, {destination, notfound}}, Result).

-spec create_wallet_notfound_test(config()) -> test_return().
create_wallet_notfound_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => <<"unknown_wallet">>,
        body => Cash
    },
    Result = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertMatch({error, {wallet, notfound}}, Result).

-spec create_ok_test(config()) -> test_return().
create_ok_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        external_id => WithdrawalID
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertEqual(succeeded, await_final_withdrawal_status(WithdrawalID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
    Withdrawal = get_withdrawal(WithdrawalID),
    ?assertEqual(WalletID, ff_withdrawal:wallet_id(Withdrawal)),
    ?assertEqual(DestinationID, ff_withdrawal:destination_id(Withdrawal)),
    ?assertEqual(Cash, ff_withdrawal:body(Withdrawal)),
    ?assertEqual(WithdrawalID, ff_withdrawal:external_id(Withdrawal)).

-spec quota_ok_test(config()) -> test_return().
quota_ok_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        quote => #{
            cash_from   => Cash,
            cash_to     => {2120, <<"USD">>},
            created_at  => <<"2016-03-22T06:12:27Z">>,
            expires_on  => <<"2016-03-22T06:12:27Z">>,
            route       => ff_withdrawal_routing:make_route(1, 1),
            quote_data  => #{<<"test">> => <<"test">>}
        }
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertEqual(succeeded, await_final_withdrawal_status(WithdrawalID)).

-spec crypto_quota_ok_test(config()) -> test_return().
crypto_quota_ok_test(C) ->
    Currency = <<"RUB">>,
    Cash = {100, Currency},
    Party = create_party(C),
    IdentityID = create_person_identity(Party, C, <<"quote-owner">>),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, Currency, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    DestinationID = create_crypto_destination(IdentityID, C),
    Params = #{
        wallet_id      => WalletID,
        currency_from  => <<"RUB">>,
        currency_to    => <<"BTC">>,
        body           => Cash,
        destination_id => DestinationID
    },
    {ok, _Quote} = ff_withdrawal:get_quote(Params).

-spec preserve_revisions_test(config()) -> test_return().
preserve_revisions_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        external_id => WithdrawalID
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    Withdrawal = get_withdrawal(WithdrawalID),
    ?assertNotEqual(undefined, ff_withdrawal:domain_revision(Withdrawal)),
    ?assertNotEqual(undefined, ff_withdrawal:party_revision(Withdrawal)),
    ?assertNotEqual(undefined, ff_withdrawal:created_at(Withdrawal)).

-spec use_quote_revisions_test(config()) -> test_return().
use_quote_revisions_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        party_id := PartyID,
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    Time = ff_time:now(),
    DomainRevision = ff_domain_config:head(),
    {ok, PartyRevision} = ff_party:get_revision(PartyID),
    ok = ct_domain_config:bump_revision(),
    ok = make_dummy_party_change(PartyID),
    ?assertNotEqual(DomainRevision, ff_domain_config:head()),
    ?assertNotEqual({ok, PartyRevision}, ff_party:get_revision(PartyID)),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        quote => #{
            cash_from => Cash,
            cash_to => {2120, <<"USD">>},
            created_at => <<"2016-03-22T06:12:27Z">>,
            expires_on => <<"2016-03-22T06:12:27Z">>,
            domain_revision => DomainRevision,
            party_revision => PartyRevision,
            operation_timestamp => Time,
            route => ff_withdrawal_routing:make_route(1, 1),
            quote_data => #{<<"test">> => <<"test">>}
        }
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    Withdrawal = get_withdrawal(WithdrawalID),
    ?assertEqual(DomainRevision, ff_withdrawal:domain_revision(Withdrawal)),
    ?assertEqual(PartyRevision, ff_withdrawal:party_revision(Withdrawal)),
    ?assertEqual(succeeded, await_final_withdrawal_status(WithdrawalID)).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    WithdrawalID = <<"unknown_withdrawal">>,
    Result = ff_withdrawal_machine:get(WithdrawalID),
    ?assertMatch({error, {unknown_withdrawal, WithdrawalID}}, Result).

-spec provider_callback_test(config()) -> test_return().
provider_callback_test(C) ->
    Currency = <<"RUB">>,
    Cash = {700700, Currency},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        external_id => WithdrawalID
    },
    CallbackTag = <<"cb_", WithdrawalID/binary>>,
    CallbackPayload = <<"super_secret">>,
    Callback = #{
        tag => CallbackTag,
        payload => CallbackPayload
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertEqual(pending, await_session_processing_status(WithdrawalID, pending)),
    SessionID = get_session_id(WithdrawalID),
    ?assertEqual(<<"callback_processing">>, await_session_adapter_state(SessionID, <<"callback_processing">>)),
    ?assertMatch(#{id := <<"SleepyID">>, extra := #{}}, get_session_transaction_info(SessionID)),
    ?assertEqual({ok, #{payload => CallbackPayload}}, call_process_callback(Callback)),
    ?assertEqual(<<"callback_finished">>, await_session_adapter_state(SessionID, <<"callback_finished">>)),
    ?assertMatch(#{id := <<"SleepyID">>, extra := #{}}, get_session_transaction_info(SessionID)),
    ?assertEqual(succeeded, await_final_withdrawal_status(WithdrawalID)),
    ?assertEqual({ok, #{payload => CallbackPayload}}, call_process_callback(Callback)),
    % Wait ff_ct_sleepy_provider timeout
    timer:sleep(5000),
    % Check that session is still alive
    ?assertEqual({ok, #{payload => CallbackPayload}}, call_process_callback(Callback)).

-spec session_repair_test(config()) -> test_return().
session_repair_test(C) ->
    Currency = <<"RUB">>,
    Cash = {700700, Currency},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        quote => #{
            cash_from   => {700700, <<"RUB">>},
            cash_to     => {700700, <<"RUB">>},
            created_at  => <<"2016-03-22T06:12:27Z">>,
            expires_on  => <<"2016-03-22T06:12:27Z">>,
            route       => ff_withdrawal_routing:make_route(11, 1),
            quote_data  => #{<<"test">> => <<"fatal">>}
        }
    },
    Callback = #{
        tag => <<"cb_", WithdrawalID/binary>>,
        payload => <<"super_secret">>
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertEqual(pending, await_session_processing_status(WithdrawalID, pending)),
    SessionID = get_session_id(WithdrawalID),
    ?assertEqual(<<"callback_processing">>, await_session_adapter_state(SessionID, <<"callback_processing">>)),
    ?assertError({failed, _, _}, call_process_callback(Callback)),
    timer:sleep(3000),
    ?assertEqual(pending, await_session_processing_status(WithdrawalID, pending)),
    ok = repair_withdrawal_session(WithdrawalID),
    ?assertEqual(succeeded, await_final_withdrawal_status(WithdrawalID)).

%% Utils

prepare_standard_environment(WithdrawalCash, C) ->
    prepare_standard_environment(WithdrawalCash, undefined, C).

prepare_standard_environment({_Amount, Currency} = WithdrawalCash, Token, C) ->
    Party = create_party(C),
    IdentityID = create_person_identity(Party, C),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, Currency, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    DestinationID = create_destination(IdentityID, Token, C),
    ok = set_wallet_balance(WithdrawalCash, WalletID),
    #{
        identity_id => IdentityID,
        party_id => Party,
        wallet_id => WalletID,
        destination_id => DestinationID
    }.

get_withdrawal(WithdrawalID) ->
    {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
    ff_withdrawal_machine:withdrawal(Machine).

get_withdrawal_status(WithdrawalID) ->
    Withdrawal = get_withdrawal(WithdrawalID),
    maps:get(status, Withdrawal).

await_session_processing_status(WithdrawalID, Status) ->
    Poller = fun() -> get_session_processing_status(WithdrawalID) end,
    Retry = genlib_retry:linear(20, 1000),
    ct_helper:await(Status, Poller, Retry).

get_session_processing_status(WithdrawalID) ->
    Withdrawal = get_withdrawal(WithdrawalID),
    ff_withdrawal:get_current_session_status(Withdrawal).

get_session(SessionID) ->
    {ok, Machine} = ff_withdrawal_session_machine:get(SessionID),
    ff_withdrawal_session_machine:session(Machine).

await_session_adapter_state(SessionID, State) ->
    Poller = fun() -> get_session_adapter_state(SessionID) end,
    Retry = genlib_retry:linear(20, 1000),
    ct_helper:await(State, Poller, Retry).

get_session_adapter_state(SessionID) ->
    Session = get_session(SessionID),
    ff_withdrawal_session:adapter_state(Session).

get_session_id(WithdrawalID) ->
    Withdrawal = get_withdrawal(WithdrawalID),
    Session = ff_withdrawal:get_current_session(Withdrawal),
    ff_withdrawal_session:id(Session).

await_final_withdrawal_status(WithdrawalID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
            Withdrawal = ff_withdrawal_machine:withdrawal(Machine),
            case ff_withdrawal:is_finished(Withdrawal) of
                false ->
                    {not_finished, Withdrawal};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(20, 1000)
    ),
    get_withdrawal_status(WithdrawalID).

get_session_transaction_info(SessionID) ->
    Session = get_session(SessionID),
    ff_withdrawal_session:transaction_info(Session).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C) ->
    create_person_identity(Party, C, <<"good-one">>).

create_person_identity(Party, C, ProviderID) ->
    create_identity(Party, ProviderID, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, C) ->
    create_identity(Party, <<"Identity Name">>, ProviderID, ClassID, C).

create_identity(Party, Name, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, name => Name, party => Party, provider => ProviderID, class => ClassID},
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
    ),
    ID.

create_wallet(IdentityID, Name, Currency, _C) ->
    ID = genlib:unique(),
    ok = ff_wallet_machine:create(
        #{id => ID, identity => IdentityID, name => Name, currency => Currency},
        ff_entity_context:new()
    ),
    ID.

await_wallet_balance({Amount, Currency}, ID) ->
    Balance = {Amount, {{inclusive, Amount}, {inclusive, Amount}}, Currency},
    Balance = ct_helper:await(
        Balance,
        fun () -> get_wallet_balance(ID) end,
        genlib_retry:linear(3, 500)
    ),
    ok.

get_wallet_balance(ID) ->
    {ok, Machine} = ff_wallet_machine:get(ID),
    get_account_balance(ff_wallet:account(ff_wallet_machine:wallet(Machine))).

get_account_balance(Account) ->
    {ok, {Amounts, Currency}} = ff_transaction:balance(
        Account,
        ff_clock:latest_clock()
    ),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

generate_id() ->
    ff_id:generate_snowflake_id().

create_destination(IID, <<"USD_CURRENCY">>, C) ->
    create_destination(IID, <<"USD">>, undefined, C);
create_destination(IID, Token, C) ->
    create_destination(IID, <<"RUB">>, Token, C).

create_destination(IID, Currency, Token, C) ->
    ID = generate_id(),
    StoreSource = ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    NewStoreResource = case Token of
        undefined ->
            StoreSource;
        Token ->
            StoreSource#{token => Token}
        end,
    Resource = {bank_card, #{bank_card => NewStoreResource}},
    Params = #{id => ID, identity => IID, name => <<"XDesination">>, currency => Currency, resource => Resource},
    ok = ff_destination_machine:create(Params, ff_entity_context:new()),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, Machine} = ff_destination_machine:get(ID),
            Destination = ff_destination_machine:destination(Machine),
            ff_destination:status(Destination)
        end
    ),
    ID.

create_crypto_destination(IID, _C) ->
    ID = generate_id(),
    Resource = {crypto_wallet, #{crypto_wallet => #{
        id => <<"a30e277c07400c9940628828949efd48">>,
        currency => {litecoin, #{}}
    }}},
    Params = #{id => ID, identity => IID, name => <<"CryptoDestination">>, currency => <<"RUB">>, resource => Resource},
    ok = ff_destination_machine:create(Params, ff_entity_context:new()),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, Machine} = ff_destination_machine:get(ID),
            Destination = ff_destination_machine:destination(Machine),
            ff_destination:status(Destination)
        end
    ),
    ID.

set_wallet_balance({Amount, Currency}, ID) ->
    TransactionID = generate_id(),
    {ok, Machine} = ff_wallet_machine:get(ID),
    Account = ff_wallet:account(ff_wallet_machine:wallet(Machine)),
    AccounterID = ff_account:accounter_account_id(Account),
    {CurrentAmount, _, Currency} = get_account_balance(Account),
    {ok, AnotherAccounterID} = create_account(Currency),
    Postings = [{AnotherAccounterID, AccounterID, {Amount - CurrentAmount, Currency}}],
    {ok, _} = ff_transaction:prepare(TransactionID, Postings),
    {ok, _} = ff_transaction:commit(TransactionID, Postings),
    ok.

create_account(CurrencyCode) ->
    Description = <<"ff_test">>,
    case call_accounter('CreateAccount', [construct_account_prototype(CurrencyCode, Description)]) of
        {ok, Result} ->
            {ok, Result};
        {exception, Exception} ->
            {error, {exception, Exception}}
    end.

construct_account_prototype(CurrencyCode, Description) ->
    #shumpune_AccountPrototype{
        currency_sym_code = CurrencyCode,
        description = Description
    }.

call_accounter(Function, Args) ->
    Service = {shumpune_shumpune_thrift, 'Accounter'},
    ff_woody_client:call(accounter, {Service, Function, Args}, woody_context:new()).


make_dummy_party_change(PartyID) ->
    {ok, _ContractID} = ff_party:create_contract(PartyID, #{
        payinst           => #domain_PaymentInstitutionRef{id = 1},
        contract_template => #domain_ContractTemplateRef{id = 1},
        contractor_level  => full
    }),
    ok.

call_process_callback(Callback) ->
    ff_withdrawal_session_machine:process_callback(Callback).

repair_withdrawal_session(WithdrawalID) ->
   SessionID = get_session_id(WithdrawalID),
   {ok, ok} = call_session_repair(SessionID, {set_session_result, #wthd_session_SetResultRepair{
       result = {success, #wthd_session_SessionResultSuccess{
           trx_info = #'TransactionInfo'{
               id = SessionID,
               extra = #{}
           }
       }}
   }}),
   ok.

call_session_repair(SessionID, Scenario) ->
   Service = {ff_proto_withdrawal_session_thrift, 'Repairer'},
   Request = {Service, 'Repair', [SessionID, Scenario]},
   Client  = ff_woody_client:new(#{
       url           => <<"http://localhost:8022/v1/repair/withdrawal/session">>,
       event_handler => scoper_woody_event_handler
   }),
   ff_woody_client:call(Client, Request).
