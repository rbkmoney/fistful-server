-module(ff_wallet_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_error_not_found/1]).
-export([create_ok/1]).
-export([create_error_id_exists/1]).
-export([create_error_identity_not_found/1]).
-export([create_error_currency_not_found/1]).
-export([create_error_party_blocked/1]).
-export([create_error_party_suspended/1]).
-export([create_error_terms_not_allowed_currency/1]).

-spec get_error_not_found(config()) -> test_return().
-spec create_ok(config()) -> test_return().
-spec create_error_id_exists(config()) -> test_return().
-spec create_error_identity_not_found(config()) -> test_return().
-spec create_error_currency_not_found(config()) -> test_return().
-spec create_error_party_blocked(config()) -> test_return().
-spec create_error_party_suspended(config()) -> test_return().
-spec create_error_terms_not_allowed_currency(config()) -> test_return().

%%

-import(ff_pipeline, [unwrap/1]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        get_error_not_found,
        create_ok,
        create_error_id_exists,
        create_error_identity_not_found,
        create_error_currency_not_found,
        create_error_party_blocked,
        create_error_party_suspended,
        create_error_terms_not_allowed_currency
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    ct_helper:makeup_cfg(
        [
            ct_helper:test_case_name(init),
            ct_payment_system:setup()
        ],
        C
    ).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C),
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

%%

get_error_not_found(_C) ->
    ?assertMatch({error, notfound}, ff_wallet_machine:get(genlib:unique())).

create_ok(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    IdentityID = create_identity(Party, C),
    WalletParams = construct_wallet_params(IdentityID),
    CreateResult = ff_wallet_machine:create(WalletParams#{id => ID}, ff_entity_context:new()),
    Wallet = ff_wallet_machine:wallet(unwrap(ff_wallet_machine:get(ID))),
    Accessibility = unwrap(ff_wallet:is_accessible(Wallet)),
    Account = ff_wallet:account(Wallet),
    {Amount, <<"RUB">>} = unwrap(ff_transaction:balance(Account, ff_clock:latest_clock())),
    CurrentAmount = ff_indef:current(Amount),
    ?assertMatch(ok, CreateResult),
    ?assertMatch(accessible, Accessibility),
    ?assertMatch(0, CurrentAmount).

create_error_id_exists(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    IdentityID = create_identity(Party, C),
    WalletParams = construct_wallet_params(IdentityID),
    CreateResult0 = ff_wallet_machine:create(WalletParams#{id => ID}, ff_entity_context:new()),
    CreateResult1 = ff_wallet_machine:create(WalletParams#{id => ID}, ff_entity_context:new()),
    ?assertMatch(ok, CreateResult0),
    ?assertMatch({error, exists}, CreateResult1).

create_error_identity_not_found(_C) ->
    ID = genlib:unique(),
    WalletParams = construct_wallet_params(genlib:unique()),
    CreateResult = ff_wallet_machine:create(WalletParams#{id => ID}, ff_entity_context:new()),
    ?assertMatch({error, {identity, notfound}}, CreateResult).

create_error_currency_not_found(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    IdentityID = create_identity(Party, C),
    WalletParams = construct_wallet_params(IdentityID, <<"EOS">>),
    CreateResult = ff_wallet_machine:create(WalletParams#{id => ID}, ff_entity_context:new()),
    ?assertMatch({error, {currency, notfound}}, CreateResult).

create_error_party_blocked(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    IdentityID = create_identity(Party, C),
    ok = block_party(Party, C),
    WalletParams = construct_wallet_params(IdentityID),
    CreateResult = ff_wallet_machine:create(WalletParams#{id => ID}, ff_entity_context:new()),
    ?assertMatch({error, {party, {inaccessible, blocked}}}, CreateResult).

create_error_party_suspended(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    IdentityID = create_identity(Party, C),
    ok = suspend_party(Party, C),
    WalletParams = construct_wallet_params(IdentityID),
    CreateResult = ff_wallet_machine:create(WalletParams#{id => ID}, ff_entity_context:new()),
    ?assertMatch({error, {party, {inaccessible, suspended}}}, CreateResult).

create_error_terms_not_allowed_currency(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    IdentityID = create_identity(Party, C),
    WalletParams = construct_wallet_params(IdentityID, <<"EUR">>),
    CreateResult = ff_wallet_machine:create(WalletParams#{id => ID}, ff_entity_context:new()),
    ExpectedError =
        {terms,
            {terms_violation,
                {not_allowed_currency,
                    {
                        #domain_CurrencyRef{symbolic_code = <<"EUR">>},
                        [
                            #domain_CurrencyRef{symbolic_code = <<"RUB">>},
                            #domain_CurrencyRef{symbolic_code = <<"USD">>}
                        ]
                    }}}},
    ?assertMatch({error, ExpectedError}, CreateResult).

%%

-include_lib("ff_cth/include/ct_domain.hrl").

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    Name = <<"Identity Name">>,
    ok = ff_identity_machine:create(
        #{
            id => ID,
            name => Name,
            party => Party,
            provider => ProviderID,
            class => ClassID
        },
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
    ),
    ID.

construct_wallet_params(IdentityID) ->
    #{
        identity => IdentityID,
        name => <<"HAHA YES">>,
        currency => <<"RUB">>
    }.

construct_wallet_params(IdentityID, Currency) ->
    #{
        identity => IdentityID,
        name => <<"HAHA YES">>,
        currency => Currency
    }.

construct_userinfo() ->
    #payproc_UserInfo{id = <<"fistful">>, type = construct_usertype()}.

construct_usertype() ->
    {service_user, #payproc_ServiceUser{}}.

suspend_party(Party, C) ->
    Service = {dmsl_payment_processing_thrift, 'PartyManagement'},
    Args = {construct_userinfo(), Party},
    Request = {Service, 'Suspend', Args},
    _ = ff_woody_client:call(partymgmt, Request, ct_helper:get_woody_ctx(C)),
    ok.

block_party(Party, C) ->
    Service = {dmsl_payment_processing_thrift, 'PartyManagement'},
    Args = {construct_userinfo(), Party, <<"BECAUSE">>},
    Request = {Service, 'Block', Args},
    _ = ff_woody_client:call(partymgmt, Request, ct_helper:get_woody_ctx(C)),
    ok.
