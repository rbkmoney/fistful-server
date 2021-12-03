-module(ff_deposit_revert_SUITE).

-include_lib("ff_cth/include/ct_domain.hrl").
-include_lib("stdlib/include/assert.hrl").
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

-export([revert_ok_test/1]).
-export([multiple_reverts_ok_test/1]).
-export([multiple_parallel_reverts_ok_test/1]).
-export([idempotency_test/1]).
-export([optional_fields_test/1]).
-export([insufficient_deposit_amount_test/1]).
-export([insufficient_amount_multiple_reverts_test/1]).
-export([invalid_revert_amount_test/1]).
-export([inconsistent_revert_currency_test/1]).
-export([wallet_limit_check_fail_test/1]).
-export([multiple_parallel_reverts_limit_fail_test/1]).
-export([unknown_deposit_test/1]).

%% Internal types

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

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
            revert_ok_test,
            multiple_reverts_ok_test,
            multiple_parallel_reverts_ok_test,
            idempotency_test,
            optional_fields_test,
            insufficient_deposit_amount_test,
            insufficient_amount_multiple_reverts_test,
            invalid_revert_amount_test,
            inconsistent_revert_currency_test,
            wallet_limit_check_fail_test,
            multiple_parallel_reverts_limit_fail_test,
            unknown_deposit_test
        ]}
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

-spec revert_ok_test(config()) -> test_return().
revert_ok_test(C) ->
    #{
        party_id := PartyID,
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({10000, <<"RUB">>}, C),
    RevertID = process_revert(DepositID, #{
        body => {5000, <<"RUB">>}
    }),
    ?assertEqual(?final_balance(5000, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-5000, <<"RUB">>), get_source_balance(SourceID)),
    Revert = get_revert(RevertID, DepositID),
    ?assertEqual(undefined, ff_deposit_revert:reason(Revert)),
    ?assertEqual(undefined, ff_deposit_revert:external_id(Revert)),
    ?assertEqual({5000, <<"RUB">>}, ff_deposit_revert:body(Revert)),
    ?assertEqual(SourceID, ff_deposit_revert:source_id(Revert)),
    ?assertEqual(WalletID, ff_deposit_revert:wallet_id(Revert)),
    DomainRevision = ff_domain_config:head(),
    ?assertEqual(DomainRevision, ff_deposit_revert:domain_revision(Revert)),
    {ok, PartyRevision} = ff_party:get_revision(PartyID),
    ?assertEqual(PartyRevision, ff_deposit_revert:party_revision(Revert)).

-spec multiple_reverts_ok_test(config()) -> test_return().
multiple_reverts_ok_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID
    } = prepare_standard_environment({10000, <<"RUB">>}, C),
    _ = process_revert(DepositID, #{body => {1000, <<"RUB">>}}),
    ?assertEqual(?final_balance(9000, <<"RUB">>), get_wallet_balance(WalletID)),
    _ = process_revert(DepositID, #{body => {1000, <<"RUB">>}}),
    ?assertEqual(?final_balance(8000, <<"RUB">>), get_wallet_balance(WalletID)),
    _ = process_revert(DepositID, #{body => {1000, <<"RUB">>}}),
    ?assertEqual(?final_balance(7000, <<"RUB">>), get_wallet_balance(WalletID)).

-spec multiple_parallel_reverts_ok_test(config()) -> test_return().
multiple_parallel_reverts_ok_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({10000, <<"RUB">>}, C),
    _ = genlib_pmap:map(
        fun(_) ->
            ok = ct_helper:set_context(C),
            process_revert(DepositID, #{body => {1000, <<"RUB">>}})
        end,
        lists:seq(1, 10)
    ),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_source_balance(SourceID)).

-spec idempotency_test(config()) -> test_return().
idempotency_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({10000, <<"RUB">>}, C),
    RevertID = generate_id(),
    Params = #{
        id => RevertID,
        body => {5000, <<"RUB">>}
    },
    ok = ff_deposit_machine:start_revert(DepositID, Params),
    ok = ff_deposit_machine:start_revert(DepositID, Params),
    RevertID = process_revert(DepositID, Params),
    ok = ff_deposit_machine:start_revert(DepositID, Params),
    RevertID = process_revert(DepositID, Params),
    ?assertEqual(1, erlang:length(get_reverts(DepositID))),
    ?assertEqual(?final_balance(5000, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-5000, <<"RUB">>), get_source_balance(SourceID)).

-spec optional_fields_test(config()) -> test_return().
optional_fields_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({10000, <<"RUB">>}, C),

    RevertID = process_revert(DepositID, #{
        body => {5000, <<"RUB">>},
        reason => <<"Why not">>,
        external_id => <<"001">>
    }),
    ?assertEqual(?final_balance(5000, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-5000, <<"RUB">>), get_source_balance(SourceID)),

    Revert = get_revert(RevertID, DepositID),
    ?assertEqual(<<"Why not">>, ff_deposit_revert:reason(Revert)),
    ?assertEqual(<<"001">>, ff_deposit_revert:external_id(Revert)),
    ?assertEqual({5000, <<"RUB">>}, ff_deposit_revert:body(Revert)),
    ?assertEqual(SourceID, ff_deposit_revert:source_id(Revert)),
    ?assertEqual(WalletID, ff_deposit_revert:wallet_id(Revert)).

-spec insufficient_deposit_amount_test(config()) -> test_return().
insufficient_deposit_amount_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    RevertID = generate_id(),
    Result = ff_deposit_machine:start_revert(DepositID, #{
        id => RevertID,
        body => {5000, <<"RUB">>}
    }),
    ?assertMatch({error, {insufficient_deposit_amount, {{5000, <<"RUB">>}, {100, <<"RUB">>}}}}, Result).

-spec insufficient_amount_multiple_reverts_test(config()) -> test_return().
insufficient_amount_multiple_reverts_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    _ = process_revert(DepositID, #{body => {90, <<"RUB">>}}),
    RevertID = generate_id(),
    Result = ff_deposit_machine:start_revert(DepositID, #{
        id => RevertID,
        body => {11, <<"RUB">>}
    }),
    ?assertMatch({error, {insufficient_deposit_amount, {{11, <<"RUB">>}, {10, <<"RUB">>}}}}, Result).

-spec invalid_revert_amount_test(config()) -> test_return().
invalid_revert_amount_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    _ = process_revert(DepositID, #{body => {1, <<"RUB">>}}),
    RevertID = generate_id(),
    Result = ff_deposit_machine:start_revert(DepositID, #{
        id => RevertID,
        body => {0, <<"RUB">>}
    }),
    ?assertMatch({error, {invalid_revert_amount, {0, <<"RUB">>}}}, Result).

-spec inconsistent_revert_currency_test(config()) -> test_return().
inconsistent_revert_currency_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    RevertID = generate_id(),
    Result = ff_deposit_machine:start_revert(DepositID, #{
        id => RevertID,
        body => {10, <<"USD">>}
    }),
    ?assertMatch({error, {inconsistent_revert_currency, {<<"USD">>, <<"RUB">>}}}, Result).

-spec wallet_limit_check_fail_test(config()) -> test_return().
wallet_limit_check_fail_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({1000, <<"RUB">>}, C),
    ok = set_wallet_balance({900, <<"RUB">>}, WalletID),
    ?assertEqual(?final_balance(900, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-1000, <<"RUB">>), get_source_balance(SourceID)),
    RevertID = generate_id(),
    ok = ff_deposit_machine:start_revert(DepositID, #{
        id => RevertID,
        body => {1000, <<"RUB">>}
    }),
    Status = await_final_revert_status(RevertID, DepositID),
    ?assertEqual(?final_balance(900, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-1000, <<"RUB">>), get_source_balance(SourceID)),
    ?assertMatch({failed, _}, Status),
    {failed, Failure} = Status,
    ?assertMatch(#{code := <<"unknown">>}, Failure).

-spec multiple_parallel_reverts_limit_fail_test(config()) -> test_return().
multiple_parallel_reverts_limit_fail_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({10000, <<"RUB">>}, C),
    Lack = 1000,
    ok = set_wallet_balance({10000 - Lack, <<"RUB">>}, WalletID),
    _ = genlib_pmap:map(
        fun(_) ->
            ok = ct_helper:set_context(C),
            RevertID = generate_id(),
            ok = ff_deposit_machine:start_revert(DepositID, #{
                id => RevertID,
                body => {1000, <<"RUB">>}
            }),
            _ = await_final_revert_status(RevertID, DepositID)
        end,
        lists:seq(1, 10)
    ),
    ?final_balance(WalletBalance, <<"RUB">>) = get_wallet_balance(WalletID),
    ?final_balance(SourceBalance, <<"RUB">>) = get_source_balance(SourceID),
    ?assertEqual(-WalletBalance, SourceBalance + Lack),
    ?assert(WalletBalance >= 0).

-spec unknown_deposit_test(config()) -> test_return().
unknown_deposit_test(_C) ->
    DepositID = <<"unknown_deposit">>,
    Result = ff_deposit_machine:start_revert(DepositID, #{
        id => generate_id(),
        body => {1000, <<"RUB">>}
    }),
    ?assertMatch({error, {unknown_deposit, DepositID}}, Result).

%% Utils

prepare_standard_environment({_Amount, Currency} = Cash, C) ->
    Party = create_party(C),
    IdentityID = create_identity(Party, C),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    SourceID = create_source(IdentityID, C),
    DepositID = process_deposit(#{
        source_id => SourceID,
        wallet_id => WalletID,
        body => Cash
    }),
    #{
        identity_id => IdentityID,
        party_id => Party,
        wallet_id => WalletID,
        source_id => SourceID,
        deposit_id => DepositID
    }.

process_deposit(DepositParams) ->
    DepositID = generate_id(),
    ok = ff_deposit_machine:create(
        DepositParams#{id => DepositID},
        ff_entity_context:new()
    ),
    succeeded = ct_helper:await(
        succeeded,
        fun() ->
            {ok, Machine} = ff_deposit_machine:get(DepositID),
            ff_deposit:status(ff_deposit_machine:deposit(Machine))
        end,
        genlib_retry:linear(15, 1000)
    ),
    DepositID.

process_revert(DepositID, RevertParams0) ->
    RevertParams1 = maps:merge(#{id => generate_id()}, RevertParams0),
    #{id := RevertID} = RevertParams1,
    ok = ff_deposit_machine:start_revert(DepositID, RevertParams1),
    succeeded = await_final_revert_status(RevertID, DepositID),
    RevertID.

await_final_revert_status(RevertID, DepositID) ->
    finished = ct_helper:await(
        finished,
        fun() ->
            {ok, Machine} = ff_deposit_machine:get(DepositID),
            Deposit = ff_deposit_machine:deposit(Machine),
            {ok, Revert} = ff_deposit:find_revert(RevertID, Deposit),
            case ff_deposit_revert:is_finished(Revert) of
                false ->
                    {not_finished, Deposit};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    {ok, Machine} = ff_deposit_machine:get(DepositID),
    Deposit = ff_deposit_machine:deposit(Machine),
    {ok, Revert} = ff_deposit:find_revert(RevertID, Deposit),
    ff_deposit_revert:status(Revert).

get_revert(RevertID, DepositID) ->
    {ok, Machine} = ff_deposit_machine:get(DepositID),
    Desposit = ff_deposit_machine:deposit(Machine),
    {ok, Revert} = ff_deposit:find_revert(RevertID, Desposit),
    Revert.

get_reverts(DepositID) ->
    {ok, Machine} = ff_deposit_machine:get(DepositID),
    Desposit = ff_deposit_machine:deposit(Machine),
    ff_deposit:reverts(Desposit).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_identity(Party, C) ->
    create_identity(Party, ?IDENTITY_PROVIDER_NAME1, C).

create_identity(Party, ProviderID, C) ->
    create_identity(Party, <<"Identity Name">>, ProviderID, C).

create_identity(Party, Name, ProviderID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, name => Name, party => Party, provider => ProviderID},
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
        fun() -> get_wallet_balance(ID) end,
        genlib_retry:linear(3, 500)
    ),
    ok.

get_wallet_balance(ID) ->
    {ok, Machine} = ff_wallet_machine:get(ID),
    get_account_balance(ff_wallet:account(ff_wallet_machine:wallet(Machine))).

get_source_balance(ID) ->
    {ok, Machine} = ff_source_machine:get(ID),
    Source = ff_source_machine:source(Machine),
    get_account_balance(ff_source:account(Source)).

set_wallet_balance({Amount, Currency}, ID) ->
    TransactionID = generate_id(),
    {ok, Machine} = ff_wallet_machine:get(ID),
    Account = ff_wallet:account(ff_wallet_machine:wallet(Machine)),
    AccounterID = ff_account:accounter_account_id(Account),
    {CurrentAmount, _, Currency} = get_account_balance(Account),
    {ok, AnotherAccounterID} = create_account(Currency),
    Postings = [{AccounterID, AnotherAccounterID, {CurrentAmount - Amount, Currency}}],
    {ok, _} = ff_transaction:prepare(TransactionID, Postings),
    {ok, _} = ff_transaction:commit(TransactionID, Postings),
    ok.

get_account_balance(Account) ->
    {ok, {Amounts, Currency}} = ff_transaction:balance(
        Account,
        ff_clock:latest_clock()
    ),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

generate_id() ->
    ff_id:generate_snowflake_id().

create_source(IID, _C) ->
    ID = generate_id(),
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    Params = #{id => ID, identity => IID, name => <<"XSource">>, currency => <<"RUB">>, resource => SrcResource},
    ok = ff_source_machine:create(Params, ff_entity_context:new()),
    authorized = ct_helper:await(
        authorized,
        fun() ->
            {ok, SrcM} = ff_source_machine:get(ID),
            Source = ff_source_machine:source(SrcM),
            ff_source:status(Source)
        end
    ),
    ID.

create_account(CurrencyCode) ->
    Description = <<"ff_test">>,
    case call_accounter('CreateAccount', {construct_account_prototype(CurrencyCode, Description)}) of
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
