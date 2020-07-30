-module(p2p_quote_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_accounter_thrift.hrl").
%% Common test API

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_fee_ok_test/1]).
-export([visa_to_nspkmir_not_allow_test/1]).

%% Internal types

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

%% API

-spec all() -> [test_case_name() | {group, group_name()}].
all() -> [
        get_fee_ok_test,
        visa_to_nspkmir_not_allow_test
].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [].

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
    ok.

-spec get_fee_ok_test(config()) -> test_return().
get_fee_ok_test(C) ->
    Cash = {22500, <<"RUB">>},
    #{
        identity_id := Identity,
        sender := CardSender
    } = prepare_standard_environment(C),
    Sender = {bank_card, #{bank_card => CardSender}},
    {ok, Quote} = p2p_quote:get_quote(Cash, Identity, Sender, Sender),
    Fees = p2p_quote:fees(Quote),
    Fee = ff_fees_final:surplus(Fees),
    ?assertEqual({146, <<"RUB">>}, Fee).

-spec visa_to_nspkmir_not_allow_test(config()) -> test_return().
visa_to_nspkmir_not_allow_test(C) ->
    Cash = {22500, <<"RUB">>},
    #{bin := Bin, masked_pan := Pan} = ct_cardstore:bank_card(<<"2204399999000900">>, {12, 2025}, C),
    #{
        identity_id := Identity,
        sender := CardSender
    } = prepare_standard_environment(C),
    Sender = {bank_card, #{bank_card => CardSender}},
    Receiver = {bank_card, #{bank_card => #{
        bin => Bin,
        masked_pan => Pan,
        token => <<"NSPK MIR">>
    }}},
    Result = p2p_quote:get_quote(Cash, Identity, Sender, Receiver),
    ?assertEqual({error, {terms, {terms_violation, p2p_forbidden}}}, Result).

%% Utils

prepare_standard_environment(C) ->
    Party = create_party(C),
    IdentityID = create_person_identity(Party, C),
    CardSender = ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    #{
        identity_id => IdentityID,
        party_id => Party,
        sender => CardSender
    }.

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C) ->
    create_person_identity(Party, C, <<"good-one">>).

create_person_identity(Party, C, ProviderID) ->
    create_identity(Party, ProviderID, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, party => Party, provider => ProviderID, class => ClassID},
        ff_entity_context:new()
    ),
    ID.
