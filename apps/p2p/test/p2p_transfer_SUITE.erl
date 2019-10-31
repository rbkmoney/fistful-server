-module(p2p_transfer_SUITE).

-include_lib("stdlib/include/assert.hrl").
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
-export([route_not_found_fail_test/1]).
-export([create_cashlimit_validation_error_test/1]).
-export([create_currency_validation_error_test/1]).
-export([create_sender_resource_notfound_test/1]).
-export([create_ok_test/1]).
-export([preserve_revisions_test/1]).
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
            route_not_found_fail_test,
            create_cashlimit_validation_error_test,
            create_currency_validation_error_test,
            create_sender_resource_notfound_test,
            create_ok_test,
            preserve_revisions_test,
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

-spec route_not_found_fail_test(config()) -> test_return().
route_not_found_fail_test(C) ->
    Cash = {100, <<"USD">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = prepare_standard_environment(Cash, C),
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
    } = prepare_standard_environment(Cash, C),
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
    } = prepare_standard_environment(Cash, C),
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
        <<"EUR">>,
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
    } = prepare_standard_environment(Cash, <<"TEST_NOTFOUND">>, C),
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
    ?assertMatch({error, {resource_full, {sender, {bin_data, not_found}}}}, Result).

-spec create_ok_test(config()) -> test_return().
create_ok_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = prepare_standard_environment(Cash, C),
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
    ?assertEqual(succeeded, await_final_p2p_transfer_status(P2PTransferID)),
    P2PTransfer = get_p2p_transfer(P2PTransferID),
    ?assertEqual(IdentityID, p2p_transfer:identity_id(P2PTransfer)),
    % ?assertEqual(ResourceSender, p2p_transfer:resource_full(P2PTransfer, sender)),
    % ?assertEqual(ResourceReceiver, p2p_transfer:resource_full(P2PTransfer, receiver)),
    ?assertEqual(Cash, p2p_transfer:body(P2PTransfer)),
    ?assertEqual(P2PTransferID, p2p_transfer:external_id(P2PTransfer)).

-spec preserve_revisions_test(config()) -> test_return().
preserve_revisions_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = prepare_standard_environment(Cash, C),
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

%% Utils

prepare_standard_environment(P2PTransferCash, C) ->
    prepare_standard_environment(P2PTransferCash, undefined, C).

prepare_standard_environment(_P2PTransferCash, Token, C) ->
    PartyID = create_party(C),
    IdentityID = create_person_identity(PartyID, C, <<"quote-owner">>),
    ResourceSender = create_resource_raw(Token, C),
    ResourceReceiver = create_resource_raw(Token, C),
    #{
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        party_id => PartyID
    }.

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

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C, ProviderID) ->
    create_identity(Party, ProviderID, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        ID,
        #{party => Party, provider => ProviderID, class => ClassID},
        ff_entity_context:new()
    ),
    ID.

generate_id() ->
    ff_id:generate_snowflake_id().

create_resource_raw(Token, C) ->
    StoreSource = ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    NewStoreResource = case Token of
        undefined ->
            StoreSource;
        Token ->
            StoreSource#{token => Token}
        end,
    {raw, #{resource => NewStoreResource}}.
