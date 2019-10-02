-module(ff_deposit_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

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

-export([limit_check_fail_test/1]).
-export([create_bad_amount_test/1]).
-export([create_currency_validation_error_test/1]).
-export([create_source_notfound_test/1]).
-export([create_wallet_notfound_test/1]).
-export([create_ok_test/1]).
-export([unknown_test/1]).

%% Internal types

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

%% Macro helpers

-define(final_balance(Amount, Currency), {Amount, {{inclusive, Amount}, {inclusive, Amount}}, Currency}).

%% API

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            limit_check_fail_test,
            create_bad_amount_test,
            create_currency_validation_error_test,
            create_source_notfound_test,
            create_wallet_notfound_test,
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

-spec limit_check_fail_test(config()) -> test_return().
limit_check_fail_test(C) ->
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment(<<"RUB">>, C),
    DepositID = generate_id(),
    DepositParams = #{
        id            => DepositID,
        body          => {20000000, <<"RUB">>},
        source_id     => SourceID,
        wallet_id     => WalletID,
        external_id   => generate_id()
    },
    ok = ff_deposit_machine:create(DepositParams, ff_entity_context:new()),
    Result = await_final_deposit_status(DepositID),
    ?assertMatch({failed, #{
        code := <<"account_limit_exceeded">>,
        sub := #{
            code := <<"amount">>
        }
    }}, Result),
    ok = await_wallet_balance({0, <<"RUB">>}, WalletID).

-spec create_bad_amount_test(config()) -> test_return().
create_bad_amount_test(C) ->
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment(<<"RUB">>, C),
    DepositID = generate_id(),
    DepositParams = #{
        id            => DepositID,
        body          => {0, <<"RUB">>},
        source_id     => SourceID,
        wallet_id     => WalletID,
        external_id   => generate_id()
    },
    Result = ff_deposit_machine:create(DepositParams, ff_entity_context:new()),
    ?assertMatch({error, {bad_deposit_amount, 0}}, Result).

-spec create_currency_validation_error_test(config()) -> test_return().
create_currency_validation_error_test(C) ->
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment(<<"RUB">>, C),
    DepositID = generate_id(),
    DepositParams = #{
        id            => DepositID,
        body          => {5000, <<"EUR">>},
        source_id     => SourceID,
        wallet_id     => WalletID,
        external_id   => generate_id()
    },
    Result = ff_deposit_machine:create(DepositParams, ff_entity_context:new()),
    Details = {
        <<"EUR">>,
        [
            #domain_CurrencyRef{symbolic_code = <<"RUB">>},
            #domain_CurrencyRef{symbolic_code = <<"USD">>}
        ]
    },
    ?assertMatch({error, {terms_violation, {not_allowed_currency, Details}}}, Result).

-spec create_source_notfound_test(config()) -> test_return().
create_source_notfound_test(C) ->
    #{
        wallet_id := WalletID
    } = prepare_standard_environment(<<"RUB">>, C),
    DepositID = generate_id(),
    DepositParams = #{
        id            => DepositID,
        body          => {5000, <<"RUB">>},
        source_id     => <<"unknown_source">>,
        wallet_id     => WalletID,
        external_id   => generate_id()
    },
    Result = ff_deposit_machine:create(DepositParams, ff_entity_context:new()),
    ?assertMatch({error, {source, notfound}}, Result).

-spec create_wallet_notfound_test(config()) -> test_return().
create_wallet_notfound_test(C) ->
    #{
        source_id := SourceID
    } = prepare_standard_environment(<<"RUB">>, C),
    DepositID = generate_id(),
    DepositParams = #{
        id            => DepositID,
        body          => {5000, <<"RUB">>},
        source_id     => SourceID,
        wallet_id     => <<"unknown_wallet">>,
        external_id   => generate_id()
    },
    Result = ff_deposit_machine:create(DepositParams, ff_entity_context:new()),
    ?assertMatch({error, {wallet, notfound}}, Result).

-spec create_ok_test(config()) -> test_return().
create_ok_test(C) ->
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment(<<"RUB">>, C),
    DepositID = generate_id(),
    DepositCash = {5000, <<"RUB">>},
    DepositParams = #{
        id            => DepositID,
        body          => DepositCash,
        source_id     => SourceID,
        wallet_id     => WalletID,
        external_id   => DepositID
    },
    ok = ff_deposit_machine:create(DepositParams, ff_entity_context:new()),
    succeeded = await_final_deposit_status(DepositID),
    ok = await_wallet_balance(DepositCash, WalletID),
    Deposit = get_deposit(DepositID),
    DepositCash = ff_deposit:body(Deposit),
    WalletID = ff_deposit:wallet_id(Deposit),
    SourceID = ff_deposit:source_id(Deposit),
    DepositID = ff_deposit:external_id(Deposit).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    DepositID = <<"unknown_deposit">>,
    Result = ff_deposit_machine:get(DepositID),
    ?assertMatch({error, {unknown_deposit, DepositID}}, Result).

%% Utils

prepare_standard_environment(Currency, C) ->
    Party = create_party(C),
    IdentityID = create_person_identity(Party, C),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    SourceID = create_source(IdentityID, C),
    #{
        identity_id => IdentityID,
        party_id => Party,
        wallet_id => WalletID,
        source_id => SourceID
    }.

get_deposit(DepositID) ->
    {ok, Machine} = ff_deposit_machine:get(DepositID),
    ff_deposit_machine:deposit(Machine).

get_deposit_status(DepositID) ->
    ff_deposit:status(get_deposit(DepositID)).

await_final_deposit_status(DepositID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            {ok, Machine} = ff_deposit_machine:get(DepositID),
            Deposit = ff_deposit_machine:deposit(Machine),
            case ff_deposit:is_finished(Deposit) of
                false ->
                    {not_finished, Deposit};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    get_deposit_status(DepositID).

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
        ID,
        #{party => Party, provider => ProviderID, class => ClassID},
        ff_entity_context:new()
    ),
    ID.

create_wallet(IdentityID, Name, Currency, _C) ->
    ID = genlib:unique(),
    ok = ff_wallet_machine:create(
        ID,
        #{identity => IdentityID, name => Name, currency => Currency},
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
    {ok, {Amounts, Currency}} = ff_transaction:balance(ff_account:accounter_account_id(Account)),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

generate_id() ->
    ff_id:generate_snowflake_id().

create_source(IID, _C) ->
    ID = generate_id(),
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    Params = #{identity => IID, name => <<"XSource">>, currency => <<"RUB">>, resource => SrcResource},
    ok = ff_source:create(ID, Params, ff_entity_context:new()),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, SrcM} = ff_source:get_machine(ID),
            ff_source:status(ff_source:get(SrcM))
        end
    ),
    ID.
