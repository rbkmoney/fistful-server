-module(w2w_transfer_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
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

-export([limit_check_fail_test/1]).
-export([create_bad_amount_test/1]).
-export([create_currency_validation_error_test/1]).
-export([create_wallet_from_notfound_test/1]).
-export([create_wallet_to_notfound_test/1]).
-export([preserve_revisions_test/1]).
-export([create_ok_test/1]).
-export([unknown_test/1]).

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
            limit_check_fail_test,
            create_bad_amount_test,
            create_currency_validation_error_test,
            create_wallet_from_notfound_test,
            create_wallet_to_notfound_test,
            preserve_revisions_test,
            create_ok_test,
            unknown_test
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

-spec limit_check_fail_test(config()) -> test_return().
limit_check_fail_test(C) ->
    #{
        wallet_from_id := WalletFromID,
        wallet_to_id := WalletToID
    } = prepare_standard_environment(<<"RUB">>, C),
    W2WTransferID = generate_id(),
    W2WTransferCash = {50001, <<"RUB">>},
    W2WTransferParams = #{
        id => W2WTransferID,
        body => W2WTransferCash,
        wallet_from_id => WalletFromID,
        wallet_to_id => WalletToID,
        external_id => W2WTransferID
    },
    ?assertEqual(?final_balance(50000, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ok = w2w_transfer_machine:create(W2WTransferParams, ff_entity_context:new()),
    Result = await_final_w2w_transfer_status(W2WTransferID),
    ?assertMatch(
        {failed, #{
            code := <<"account_limit_exceeded">>,
            sub := #{
                code := <<"amount">>
            }
        }},
        Result
    ),
    ?assertEqual(?final_balance(50000, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletToID)).

-spec create_bad_amount_test(config()) -> test_return().
create_bad_amount_test(C) ->
    #{
        wallet_from_id := WalletFromID,
        wallet_to_id := WalletToID
    } = prepare_standard_environment(<<"RUB">>, C),
    W2WTransferID = generate_id(),
    W2WTransferCash = {0, <<"RUB">>},
    W2WTransferParams = #{
        id => W2WTransferID,
        body => W2WTransferCash,
        wallet_from_id => WalletFromID,
        wallet_to_id => WalletToID,
        external_id => W2WTransferID
    },
    Result = w2w_transfer_machine:create(W2WTransferParams, ff_entity_context:new()),
    ?assertMatch({error, {terms, {bad_w2w_transfer_amount, {0, <<"RUB">>}}}}, Result).

-spec create_currency_validation_error_test(config()) -> test_return().
create_currency_validation_error_test(C) ->
    #{
        wallet_from_id := WalletFromID,
        wallet_to_id := WalletToID
    } = prepare_standard_environment(<<"RUB">>, C),
    W2WTransferID = generate_id(),
    W2WTransferCash = {5000, <<"EUR">>},
    W2WTransferParams = #{
        id => W2WTransferID,
        body => W2WTransferCash,
        wallet_from_id => WalletFromID,
        wallet_to_id => WalletToID,
        external_id => W2WTransferID
    },
    Result = w2w_transfer_machine:create(W2WTransferParams, ff_entity_context:new()),
    Details = {
        #domain_CurrencyRef{symbolic_code = <<"EUR">>},
        [
            #domain_CurrencyRef{symbolic_code = <<"RUB">>},
            #domain_CurrencyRef{symbolic_code = <<"USD">>}
        ]
    },
    ?assertMatch({error, {terms, {terms_violation, {not_allowed_currency, Details}}}}, Result).

-spec create_wallet_from_notfound_test(config()) -> test_return().
create_wallet_from_notfound_test(C) ->
    #{
        wallet_to_id := WalletToID
    } = prepare_standard_environment(<<"RUB">>, C),
    W2WTransferID = generate_id(),
    W2WTransferCash = {5000, <<"RUB">>},
    W2WTransferParams = #{
        id => W2WTransferID,
        body => W2WTransferCash,
        wallet_from_id => <<"unknown_wallet_from">>,
        wallet_to_id => WalletToID,
        external_id => W2WTransferID
    },
    Result = w2w_transfer_machine:create(W2WTransferParams, ff_entity_context:new()),
    ?assertMatch({error, {wallet_from, notfound}}, Result).

-spec create_wallet_to_notfound_test(config()) -> test_return().
create_wallet_to_notfound_test(C) ->
    #{
        wallet_from_id := WalletFromID
    } = prepare_standard_environment(<<"RUB">>, C),
    W2WTransferID = generate_id(),
    W2WTransferCash = {5000, <<"RUB">>},
    W2WTransferParams = #{
        id => W2WTransferID,
        body => W2WTransferCash,
        wallet_from_id => WalletFromID,
        wallet_to_id => <<"unknown_wallet_to">>,
        external_id => W2WTransferID
    },
    Result = w2w_transfer_machine:create(W2WTransferParams, ff_entity_context:new()),
    ?assertMatch({error, {wallet_to, notfound}}, Result).

-spec preserve_revisions_test(config()) -> test_return().
preserve_revisions_test(C) ->
    #{
        wallet_from_id := WalletFromID,
        wallet_to_id := WalletToID
    } = prepare_standard_environment(<<"RUB">>, C),
    W2WTransferID = generate_id(),
    W2WTransferCash = {5000, <<"RUB">>},
    W2WTransferParams = #{
        id => W2WTransferID,
        body => W2WTransferCash,
        wallet_from_id => WalletFromID,
        wallet_to_id => WalletToID,
        external_id => W2WTransferID
    },
    ok = w2w_transfer_machine:create(W2WTransferParams, ff_entity_context:new()),
    W2WTransfer = get_w2w_transfer(W2WTransferID),
    ?assertNotEqual(undefined, w2w_transfer:domain_revision(W2WTransfer)),
    ?assertNotEqual(undefined, w2w_transfer:party_revision(W2WTransfer)),
    ?assertNotEqual(undefined, w2w_transfer:created_at(W2WTransfer)).

-spec create_ok_test(config()) -> test_return().
create_ok_test(C) ->
    #{
        wallet_from_id := WalletFromID,
        wallet_to_id := WalletToID
    } = prepare_standard_environment(<<"RUB">>, C),
    W2WTransferID = generate_id(),
    W2WTransferCash = {50000, <<"RUB">>},
    W2WTransferParams = #{
        id => W2WTransferID,
        body => W2WTransferCash,
        wallet_from_id => WalletFromID,
        wallet_to_id => WalletToID,
        external_id => W2WTransferID
    },
    ?assertEqual(?final_balance(50000, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletToID)),
    ok = w2w_transfer_machine:create(W2WTransferParams, ff_entity_context:new()),
    succeeded = await_final_w2w_transfer_status(W2WTransferID),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(50000, <<"RUB">>), get_wallet_balance(WalletToID)),
    W2WTransfer = get_w2w_transfer(W2WTransferID),
    W2WTransferCash = w2w_transfer:body(W2WTransfer),
    WalletFromID = w2w_transfer:wallet_from_id(W2WTransfer),
    WalletToID = w2w_transfer:wallet_to_id(W2WTransfer),
    W2WTransferID = w2w_transfer:external_id(W2WTransfer).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    W2WTransferID = <<"unknown_w2w_transfer">>,
    Result = w2w_transfer_machine:get(W2WTransferID),
    ?assertMatch({error, {unknown_w2w_transfer, W2WTransferID}}, Result).

%% Utils

prepare_standard_environment(Currency, C) ->
    PartyID = create_party(C),
    IdentityID = create_person_identity(PartyID, C),
    WalletFromID = create_wallet(IdentityID, <<"My wallet from">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, Currency}, WalletFromID),
    WalletToID = create_wallet(IdentityID, <<"My wallet to">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, Currency}, WalletToID),
    ok = set_wallet_balance({50000, <<"RUB">>}, WalletFromID),
    #{
        identity_id => IdentityID,
        party_id => PartyID,
        wallet_from_id => WalletFromID,
        wallet_to_id => WalletToID
    }.

get_w2w_transfer(W2WTransferID) ->
    {ok, Machine} = w2w_transfer_machine:get(W2WTransferID),
    w2w_transfer_machine:w2w_transfer(Machine).

get_w2w_transfer_status(W2WTransferID) ->
    w2w_transfer:status(get_w2w_transfer(W2WTransferID)).

await_final_w2w_transfer_status(W2WTransferID) ->
    finished = ct_helper:await(
        finished,
        fun() ->
            {ok, Machine} = w2w_transfer_machine:get(W2WTransferID),
            W2WTransfer = w2w_transfer_machine:w2w_transfer(Machine),
            case w2w_transfer:is_finished(W2WTransfer) of
                false ->
                    {not_finished, W2WTransfer};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    get_w2w_transfer_status(W2WTransferID).

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
        fun() -> get_wallet_balance(ID) end,
        genlib_retry:linear(3, 500)
    ),
    ok.

get_wallet_balance(ID) ->
    {ok, Machine} = ff_wallet_machine:get(ID),
    get_account_balance(ff_wallet:account(ff_wallet_machine:wallet(Machine))).

%% NOTE: This function can flap tests after switch to shumpune
%% because of potentially wrong Clock. In common case it should be passed
%% from caller after applying changes to account balance.
%% This will work fine with shumway because it return LatestClock on any
%% balance changes, therefore it will broke tests with shumpune
%% because of proper clocks.
get_account_balance(Account) ->
    {ok, {Amounts, Currency}} = ff_transaction:balance(
        Account,
        ff_clock:latest_clock()
    ),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

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

generate_id() ->
    ff_id:generate_snowflake_id().

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
