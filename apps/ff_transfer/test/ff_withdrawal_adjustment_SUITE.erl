-module(ff_withdrawal_adjustment_SUITE).

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
-export([no_pending_withdrawal_adjustments_test/1]).
-export([unknown_withdrawal_test/1]).

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
            no_pending_withdrawal_adjustments_test,
            unknown_withdrawal_test
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

-spec adjustment_can_change_status_to_failed_test(config()) -> test_return().
adjustment_can_change_status_to_failed_test(C) ->
    #{
        withdrawal_id := WithdrawalID,
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(80, <<"RUB">>), get_destination_balance(DestinationID)),
    Failure = #{code => <<"test">>},
    AdjustmentID = process_adjustment(WithdrawalID, #{
        change => {change_status, {failed, Failure}},
        external_id => <<"true_unique_id">>
    }),
    ?assertMatch(succeeded, get_adjustment_status(WithdrawalID, AdjustmentID)),
    ExternalID = ff_adjustment:external_id(get_adjustment(WithdrawalID, AdjustmentID)),
    ?assertEqual(<<"true_unique_id">>, ExternalID),
    ?assertEqual({failed, Failure},  get_withdrawal_status(WithdrawalID)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_destination_balance(DestinationID)).

-spec adjustment_can_change_failure_test(config()) -> test_return().
adjustment_can_change_failure_test(C) ->
    #{
        withdrawal_id := WithdrawalID,
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(80, <<"RUB">>), get_destination_balance(DestinationID)),
    Failure1 = #{code => <<"one">>},
    _ = process_adjustment(WithdrawalID, #{
        change => {change_status, {failed, Failure1}}
    }),
    ?assertEqual({failed, Failure1},  get_withdrawal_status(WithdrawalID)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_destination_balance(DestinationID)),
    Failure2 = #{code => <<"two">>},
    _ = process_adjustment(WithdrawalID, #{
        change => {change_status, {failed, Failure2}}
    }),
    ?assertEqual({failed, Failure2},  get_withdrawal_status(WithdrawalID)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_destination_balance(DestinationID)).

-spec adjustment_can_change_status_to_succeeded_test(config()) -> test_return().
adjustment_can_change_status_to_succeeded_test(C) ->
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(80, <<"RUB">>), get_destination_balance(DestinationID)),
    WithdrawalID = generate_id(),
    Params = #{
        id => WithdrawalID,
        wallet_id => WalletID,
        destination_id => DestinationID,
        body => {1000, <<"RUB">>}
    },
    ok = ff_withdrawal_machine:create(Params, ff_entity_context:new()),
    ?assertMatch({failed, _}, await_final_withdrawal_status(WithdrawalID)),
    AdjustmentID = process_adjustment(WithdrawalID, #{
        change => {change_status, succeeded}
    }),
    ?assertMatch(succeeded, get_adjustment_status(WithdrawalID, AdjustmentID)),
    ?assertMatch(succeeded, get_withdrawal_status(WithdrawalID)),
    ?assertEqual(?final_balance(-1000, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(880, <<"RUB">>), get_destination_balance(DestinationID)).

-spec adjustment_can_not_change_status_to_pending_test(config()) -> test_return().
adjustment_can_not_change_status_to_pending_test(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Result = ff_withdrawal_machine:start_adjustment(WithdrawalID, #{
        id => generate_id(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {invalid_status_change, {unavailable_status, pending}}}, Result).

-spec adjustment_can_not_change_status_to_same(config()) -> test_return().
adjustment_can_not_change_status_to_same(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Result = ff_withdrawal_machine:start_adjustment(WithdrawalID, #{
        id => generate_id(),
        change => {change_status, succeeded}
    }),
    ?assertMatch({error, {invalid_status_change, {already_has_status, succeeded}}}, Result).

-spec adjustment_sequence_test(config()) -> test_return().
adjustment_sequence_test(C) ->
    #{
        withdrawal_id := WithdrawalID,
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(80, <<"RUB">>), get_destination_balance(DestinationID)),
    MakeFailed = fun() ->
        _ = process_adjustment(WithdrawalID, #{
            change => {change_status, {failed, #{code => <<"test">>}}}
        }),
        ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
        ?assertEqual(?final_balance(0, <<"RUB">>), get_destination_balance(DestinationID))
    end,
    MakeSucceeded = fun() ->
        _ = process_adjustment(WithdrawalID, #{
            change => {change_status, succeeded}
        }),
        ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
        ?assertEqual(?final_balance(80, <<"RUB">>), get_destination_balance(DestinationID))
    end,
    MakeFailed(),
    MakeSucceeded(),
    MakeFailed(),
    MakeSucceeded(),
    MakeFailed().

-spec adjustment_idempotency_test(config()) -> test_return().
adjustment_idempotency_test(C) ->
    #{
        withdrawal_id := WithdrawalID,
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(80, <<"RUB">>), get_destination_balance(DestinationID)),
    Params = #{
        id => generate_id(),
        change => {change_status, {failed, #{code => <<"test">>}}}
    },
    _ = process_adjustment(WithdrawalID, Params),
    _ = process_adjustment(WithdrawalID, Params),
    _ = process_adjustment(WithdrawalID, Params),
    _ = process_adjustment(WithdrawalID, Params),
    Withdrawal = get_withdrawal(WithdrawalID),
    ?assertMatch([_], ff_withdrawal:adjustments(Withdrawal)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_destination_balance(DestinationID)).

-spec no_parallel_adjustments_test(config()) -> test_return().
no_parallel_adjustments_test(C) ->
    #{
        withdrawal_id := WithdrawalID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Withdrawal0 = get_withdrawal(WithdrawalID),
    AdjustmentID0 = generate_id(),
    Params0 = #{
        id => AdjustmentID0,
        change => {change_status, {failed, #{code => <<"test">>}}}
    },
    {ok, {_, Events0}} = ff_withdrawal:start_adjustment(Params0, Withdrawal0),
    Withdrawal1 = lists:foldl(fun ff_withdrawal:apply_event/2, Withdrawal0, Events0),
    Params1 = #{
        id => generate_id(),
        change => {change_status, succeeded}
    },
    Result = ff_withdrawal:start_adjustment(Params1, Withdrawal1),
    ?assertMatch({error, {another_adjustment_in_progress, AdjustmentID0}}, Result).

-spec no_pending_withdrawal_adjustments_test(config()) -> test_return().
no_pending_withdrawal_adjustments_test(C) ->
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    {ok, Events0} = ff_withdrawal:create(#{
        id => generate_id(),
        wallet_id => WalletID,
        destination_id => DestinationID,
        body => {100, <<"RUB">>}
    }),
    Withdrawal1 = lists:foldl(fun ff_withdrawal:apply_event/2, undefined, Events0),
    Params1 = #{
        id => generate_id(),
        change => {change_status, succeeded}
    },
    Result = ff_withdrawal:start_adjustment(Params1, Withdrawal1),
    ?assertMatch({error, {invalid_withdrawal_status, pending}}, Result).

-spec unknown_withdrawal_test(config()) -> test_return().
unknown_withdrawal_test(_C) ->
    WithdrawalID = <<"unknown_withdrawal">>,
    Result = ff_withdrawal_machine:start_adjustment(WithdrawalID, #{
        id => generate_id(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {unknown_withdrawal, WithdrawalID}}, Result).

%% Utils

prepare_standard_environment({_Amount, Currency} = WithdrawalCash, C) ->
    Party = create_party(C),
    IdentityID = create_person_identity(Party, C),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    DestinationID = create_destination(IdentityID, C),
    ok = set_wallet_balance(WithdrawalCash, WalletID),
    WithdrawalID = process_withdrawal(#{
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => WithdrawalCash
    }),
    #{
        identity_id => IdentityID,
        party_id => Party,
        wallet_id => WalletID,
        destination_id => DestinationID,
        withdrawal_id => WithdrawalID
    }.

get_withdrawal(WithdrawalID) ->
    {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
    ff_withdrawal_machine:withdrawal(Machine).

get_adjustment(WithdrawalID, AdjustmentID) ->
    Withdrawal = get_withdrawal(WithdrawalID),
    {ok, Adjustment} = ff_withdrawal:find_adjustment(AdjustmentID, Withdrawal),
    Adjustment.

process_withdrawal(WithdrawalParams) ->
    WithdrawalID = generate_id(),
    ok = ff_withdrawal_machine:create(WithdrawalParams#{id => WithdrawalID}, ff_entity_context:new()),
    succeeded = await_final_withdrawal_status(WithdrawalID),
    WithdrawalID.

process_adjustment(WithdrawalID, AdjustmentParams0) ->
    AdjustmentParams1 = maps:merge(#{id => generate_id()}, AdjustmentParams0),
    #{id := AdjustmentID} = AdjustmentParams1,
    ok = ff_withdrawal_machine:start_adjustment(WithdrawalID, AdjustmentParams1),
    succeeded = await_final_adjustment_status(WithdrawalID, AdjustmentID),
    AdjustmentID.

get_withdrawal_status(WithdrawalID) ->
    ff_withdrawal:status(get_withdrawal(WithdrawalID)).

get_adjustment_status(WithdrawalID, AdjustmentID) ->
    ff_adjustment:status(get_adjustment(WithdrawalID, AdjustmentID)).

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
        genlib_retry:linear(90, 1000)
    ),
    get_withdrawal_status(WithdrawalID).

await_final_adjustment_status(WithdrawalID, AdjustmentID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
            Withdrawal = ff_withdrawal_machine:withdrawal(Machine),
            {ok, Adjustment} = ff_withdrawal:find_adjustment(AdjustmentID, Withdrawal),
            case ff_adjustment:is_finished(Adjustment) of
                false ->
                    {not_finished, Withdrawal};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    get_adjustment_status(WithdrawalID, AdjustmentID).

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

get_destination_balance(ID) ->
    {ok, Machine} = ff_destination:get_machine(ID),
    get_account_balance(ff_destination:account(ff_destination:get(Machine))).

get_account_balance(Account) ->
    {ok, {Amounts, Currency}} = ff_transaction:balance(ff_account:accounter_account_id(Account)),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

generate_id() ->
    ff_id:generate_snowflake_id().

create_destination(IID, C) ->
    ID = generate_id(),
    Resource = {bank_card, ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C)},
    Params = #{identity => IID, name => <<"XDesination">>, currency => <<"RUB">>, resource => Resource},
    ok = ff_destination:create(ID, Params, ff_entity_context:new()),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, Machine} = ff_destination:get_machine(ID),
            ff_destination:status(ff_destination:get(Machine))
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
    #accounter_AccountPrototype{
        currency_sym_code = CurrencyCode,
        description = Description
    }.

call_accounter(Function, Args) ->
    Service = {dmsl_accounter_thrift, 'Accounter'},
    ff_woody_client:call(accounter, {Service, Function, Args}, woody_context:new()).
