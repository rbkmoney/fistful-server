-module(ff_deposit_revert_adjustment_SUITE).

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

%% Tests

-export([adjustment_can_change_status_to_failed_test/1]).
-export([adjustment_can_change_failure_test/1]).
-export([adjustment_can_change_status_to_succeeded_test/1]).
-export([adjustment_can_not_change_status_to_pending_test/1]).
-export([adjustment_can_not_change_status_to_same/1]).
-export([adjustment_sequence_test/1]).
-export([adjustment_idempotency_test/1]).
-export([no_parallel_adjustments_test/1]).
-export([no_pending_revert_adjustments_test/1]).
-export([unknown_deposit_test/1]).
-export([unknown_revert_test/1]).

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
            adjustment_can_change_status_to_failed_test,
            adjustment_can_change_failure_test,
            adjustment_can_change_status_to_succeeded_test,
            adjustment_can_not_change_status_to_pending_test,
            adjustment_can_not_change_status_to_same,
            adjustment_sequence_test,
            adjustment_idempotency_test,
            no_parallel_adjustments_test,
            no_pending_revert_adjustments_test,
            unknown_deposit_test,
            unknown_revert_test
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
    ok = ff_woody_ctx:set(ct_helper:get_woody_ctx(C1)),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, _C) ->
    ok = ff_woody_ctx:unset().

%% Tests

-spec adjustment_can_change_status_to_failed_test(config()) -> test_return().
adjustment_can_change_status_to_failed_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID,
        revert_id := RevertID
    } = prepare_standard_environment({100, <<"RUB">>}, {50, <<"RUB">>}, C),
    ?assertEqual(?final_balance(50, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-50, <<"RUB">>), get_source_balance(SourceID)),
    Failure = #{code => <<"test">>},
    AdjustmentID = process_adjustment(DepositID, RevertID, #{
        change => {change_status, {failed, Failure}},
        external_id => <<"true_unique_id">>
    }),
    ?assertMatch(succeeded, get_adjustment_status(DepositID, RevertID, AdjustmentID)),
    ExternalID = ff_adjustment:external_id(get_adjustment(DepositID, RevertID, AdjustmentID)),
    ?assertEqual(<<"true_unique_id">>, ExternalID),
    ?assertEqual({failed, Failure},  get_revert_status(DepositID, RevertID)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-100, <<"RUB">>), get_source_balance(SourceID)),
    _ = process_revert(DepositID, #{body   => {100, <<"RUB">>}}).

-spec adjustment_can_change_failure_test(config()) -> test_return().
adjustment_can_change_failure_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID,
        revert_id := RevertID
    } = prepare_standard_environment({100, <<"RUB">>}, {50, <<"RUB">>}, C),
    ?assertEqual(?final_balance(50, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-50, <<"RUB">>), get_source_balance(SourceID)),
    Failure1 = #{code => <<"one">>},
    _ = process_adjustment(DepositID, RevertID, #{
        change => {change_status, {failed, Failure1}}
    }),
    ?assertEqual({failed, Failure1},  get_revert_status(DepositID, RevertID)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-100, <<"RUB">>), get_source_balance(SourceID)),
    Failure2 = #{code => <<"two">>},
    _ = process_adjustment(DepositID, RevertID, #{
        change => {change_status, {failed, Failure2}}
    }),
    ?assertEqual({failed, Failure2},  get_revert_status(DepositID, RevertID)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-100, <<"RUB">>), get_source_balance(SourceID)).

-spec adjustment_can_change_status_to_succeeded_test(config()) -> test_return().
adjustment_can_change_status_to_succeeded_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({100, <<"RUB">>}, {50, <<"RUB">>}, C),
    ok = set_wallet_balance({40, <<"RUB">>}, WalletID),
    ?assertEqual(?final_balance(40, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-50, <<"RUB">>), get_source_balance(SourceID)),
    RevertID = generate_id(),
    ok = ff_deposit_machine:start_revert(DepositID, #{
        id => RevertID,
        body => {50, <<"RUB">>}
    }),
    ?assertMatch({failed, _}, await_final_revert_status(DepositID, RevertID)),
    AdjustmentID = process_adjustment(DepositID, RevertID, #{
        change => {change_status, succeeded}
    }),
    ?assertMatch(succeeded, get_adjustment_status(DepositID, RevertID, AdjustmentID)),
    ?assertMatch(succeeded, get_revert_status(DepositID, RevertID)),
    ?assertEqual(?final_balance(-10, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_source_balance(SourceID)).

-spec adjustment_can_not_change_status_to_pending_test(config()) -> test_return().
adjustment_can_not_change_status_to_pending_test(C) ->
    #{
        deposit_id := DepositID,
        revert_id := RevertID
    } = prepare_standard_environment({100, <<"RUB">>}, {50, <<"RUB">>}, C),
    Result = ff_deposit_machine:start_revert_adjustment(DepositID, RevertID, #{
        id => generate_id(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {invalid_status_change, {unavailable_status, pending}}}, Result).

-spec adjustment_can_not_change_status_to_same(config()) -> test_return().
adjustment_can_not_change_status_to_same(C) ->
    #{
        deposit_id := DepositID,
        revert_id := RevertID
    } = prepare_standard_environment({100, <<"RUB">>}, {50, <<"RUB">>}, C),
    Result = ff_deposit_machine:start_revert_adjustment(DepositID, RevertID, #{
        id => generate_id(),
        change => {change_status, succeeded}
    }),
    ?assertMatch({error, {invalid_status_change, {already_has_status, succeeded}}}, Result).

-spec adjustment_sequence_test(config()) -> test_return().
adjustment_sequence_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID,
        revert_id := RevertID
    } = prepare_standard_environment({100, <<"RUB">>}, {50, <<"RUB">>}, C),
    ?assertEqual(?final_balance(50, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-50, <<"RUB">>), get_source_balance(SourceID)),
    MakeFailed = fun() ->
        _ = process_adjustment(DepositID, RevertID, #{
            change => {change_status, {failed, #{code => <<"test">>}}}
        }),
        ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
        ?assertEqual(?final_balance(-100, <<"RUB">>), get_source_balance(SourceID))
    end,
    MakeSucceeded = fun() ->
        _ = process_adjustment(DepositID, RevertID, #{
            change => {change_status, succeeded}
        }),
        ?assertEqual(?final_balance(50, <<"RUB">>), get_wallet_balance(WalletID)),
        ?assertEqual(?final_balance(-50, <<"RUB">>), get_source_balance(SourceID))
    end,
    MakeFailed(),
    MakeSucceeded(),
    MakeFailed(),
    MakeSucceeded(),
    MakeFailed().

-spec adjustment_idempotency_test(config()) -> test_return().
adjustment_idempotency_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID,
        revert_id := RevertID
    } = prepare_standard_environment({100, <<"RUB">>}, {50, <<"RUB">>}, C),
    ?assertEqual(?final_balance(50, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-50, <<"RUB">>), get_source_balance(SourceID)),
    Params = #{
        id => generate_id(),
        change => {change_status, {failed, #{code => <<"test">>}}}
    },
    _ = process_adjustment(DepositID, RevertID, Params),
    _ = process_adjustment(DepositID, RevertID, Params),
    _ = process_adjustment(DepositID, RevertID, Params),
    _ = process_adjustment(DepositID, RevertID, Params),
    Revert = get_revert(DepositID, RevertID),
    ?assertMatch([_], ff_deposit_revert:adjustments(Revert)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-100, <<"RUB">>), get_source_balance(SourceID)).

-spec no_parallel_adjustments_test(config()) -> test_return().
no_parallel_adjustments_test(C) ->
    #{
        deposit_id := DepositID,
        revert_id := RevertID
    } = prepare_standard_environment({100, <<"RUB">>}, {50, <<"RUB">>}, C),
    Revert0 = get_revert(DepositID, RevertID),
    AdjustmentID0 = generate_id(),
    Params0 = #{
        id => AdjustmentID0,
        change => {change_status, {failed, #{code => <<"test">>}}}
    },
    {ok, {_, Events0}} = ff_deposit_revert:start_adjustment(Params0, Revert0),
    Revert1 = lists:foldl(fun ff_deposit_revert:apply_event/2, Revert0, Events0),
    Params1 = #{
        id => generate_id(),
        change => {change_status, succeeded}
    },
    Result = ff_deposit_revert:start_adjustment(Params1, Revert1),
    ?assertMatch({error, {another_adjustment_in_progress, AdjustmentID0}}, Result).

-spec no_pending_revert_adjustments_test(config()) -> test_return().
no_pending_revert_adjustments_test(C) ->
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({100, <<"RUB">>}, {50, <<"RUB">>}, C),
    {ok, {_, Events0}} = ff_deposit_revert:create(#{
        id => generate_id(),
        wallet_id => WalletID,
        source_id => SourceID,
        body => {50, <<"RUB">>}
    }),
    Revert1 = lists:foldl(fun ff_deposit_revert:apply_event/2, undefined, Events0),
    Params1 = #{
        id => generate_id(),
        change => {change_status, succeeded}
    },
    Result = ff_deposit_revert:start_adjustment(Params1, Revert1),
    ?assertMatch({error, {invalid_revert_status, pending}}, Result).

-spec unknown_deposit_test(config()) -> test_return().
unknown_deposit_test(_C) ->
    DepositID = <<"unknown_deposit">>,
    RevertID = <<"unknown_revert">>,
    Result = ff_deposit_machine:start_revert_adjustment(DepositID, RevertID, #{
        id => generate_id(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {unknown_deposit, DepositID}}, Result).

-spec unknown_revert_test(config()) -> test_return().
unknown_revert_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment({100, <<"RUB">>}, {50, <<"RUB">>}, C),
    RevertID = <<"unknown_revert">>,
    Result = ff_deposit_machine:start_revert_adjustment(DepositID, RevertID, #{
        id => generate_id(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {unknown_revert, RevertID}}, Result).

%% Utils

prepare_standard_environment({_Amount, Currency} = DepositCash, RevertCash, C) ->
    Party = create_party(C),
    IdentityID = create_person_identity(Party, C),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    SourceID = create_source(IdentityID, C),
    DepositID = process_deposit(#{
        source_id => SourceID,
        wallet_id => WalletID,
        body => DepositCash
    }),
    RevertID = process_revert(DepositID, #{
        body => RevertCash
    }),
    #{
        identity_id => IdentityID,
        party_id => Party,
        wallet_id => WalletID,
        source_id => SourceID,
        deposit_id => DepositID,
        revert_id => RevertID
    }.

get_deposit(DepositID) ->
    {ok, Machine} = ff_deposit_machine:get(DepositID),
    ff_deposit_machine:deposit(Machine).

get_revert(DepositID, RevertID) ->
    Deposit = get_deposit(DepositID),
    {ok, Revert} = ff_deposit:find_revert(RevertID, Deposit),
    Revert.

get_adjustment(DepositID, RevertID, AdjustmentID) ->
    Revert = get_revert(DepositID, RevertID),
    {ok, Adjustment} = ff_deposit_revert:find_adjustment(AdjustmentID, Revert),
    Adjustment.

process_deposit(DepositParams) ->
    DepositID = generate_id(),
    ok = ff_deposit_machine:create(DepositParams#{id => DepositID}, ff_ctx:new()),
    succeeded = ct_helper:await(
        succeeded,
        fun () ->
            ff_deposit:status(get_deposit(DepositID))
        end,
        genlib_retry:linear(15, 1000)
    ),
    DepositID.

process_revert(DepositID, RevertParams0) ->
    RevertParams1 = maps:merge(#{id => generate_id()}, RevertParams0),
    #{id := RevertID} = RevertParams1,
    ok = ff_deposit_machine:start_revert(DepositID, RevertParams1),
    succeeded = await_final_revert_status(DepositID, RevertID),
    RevertID.

process_adjustment(DepositID, RevertID, AdjustmentParams0) ->
    AdjustmentParams1 = maps:merge(#{id => generate_id()}, AdjustmentParams0),
    #{id := AdjustmentID} = AdjustmentParams1,
    ok = ff_deposit_machine:start_revert_adjustment(DepositID, RevertID, AdjustmentParams1),
    succeeded = await_final_adjustment_status(DepositID, RevertID, AdjustmentID),
    AdjustmentID.

get_revert_status(DepositID, RevertID) ->
    ff_deposit_revert:status(get_revert(DepositID, RevertID)).

get_adjustment_status(DepositID, RevertID, AdjustmentID) ->
    ff_adjustment:status(get_adjustment(DepositID, RevertID, AdjustmentID)).

await_final_revert_status(DepositID, RevertID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
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
    ff_deposit_revert:status(get_revert(DepositID, RevertID)).

await_final_adjustment_status(DepositID, RevertID, AdjustmentID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            {ok, Machine} = ff_deposit_machine:get(DepositID),
            Deposit = ff_deposit_machine:deposit(Machine),
            {ok, Revert} = ff_deposit:find_revert(RevertID, Deposit),
            {ok, Adjustment} = ff_deposit_revert:find_adjustment(AdjustmentID, Revert),
            case ff_adjustment:is_finished(Adjustment) of
                false ->
                    {not_finished, Revert};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    ff_adjustment:status(get_adjustment(DepositID, RevertID, AdjustmentID)).

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
        ff_ctx:new()
    ),
    ID.

create_wallet(IdentityID, Name, Currency, _C) ->
    ID = genlib:unique(),
    ok = ff_wallet_machine:create(
        ID,
        #{identity => IdentityID, name => Name, currency => Currency},
        ff_ctx:new()
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

get_source_balance(ID) ->
    {ok, Machine} = ff_source:get_machine(ID),
    get_account_balance(ff_source:account(ff_source:get(Machine))).

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
    {ok, {Amounts, Currency}} = ff_transaction:balance(ff_account:accounter_account_id(Account)),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

generate_id() ->
    ff_id:generate_snowflake_id().

create_source(IID, _C) ->
    ID = generate_id(),
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    Params = #{identity => IID, name => <<"XSource">>, currency => <<"RUB">>, resource => SrcResource},
    ok = ff_source:create(ID, Params, ff_ctx:new()),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, SrcM} = ff_source:get_machine(ID),
            ff_source:status(ff_source:get(SrcM))
        end
    ),
    ID.

create_account(CurrencyCode) ->
    Description = <<"ff_test">>,
    case call_accounter('CreateAccount', [construct_account_prototype(CurrencyCode, Description)]) of
        {ok, Result} ->
            {ok, Result};
        {exception, Exception} ->
            {error, {exception, Exception}}
    end.

construct_account_prototype(CurrencyCode, Description) ->
    #accounter_AccountPrototype{
        currency_sym_code = CurrencyCode,
        description = Description
    }.

call_accounter(Function, Args) ->
    Service = {dmsl_accounter_thrift, 'Accounter'},
    ff_woody_client:call(accounter, {Service, Function, Args}, woody_context:new()).
