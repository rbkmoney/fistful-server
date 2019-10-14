-module(ff_deposit_adjustment_SUITE).

-include_lib("stdlib/include/assert.hrl").

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
-export([no_pending_deposit_adjustments_test/1]).
-export([unknown_deposit_test/1]).

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
            no_pending_deposit_adjustments_test,
            unknown_deposit_test
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
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-100, <<"RUB">>), get_source_balance(SourceID)),
    Failure = #{code => <<"test">>},
    AdjustmentID = process_adjustment(DepositID, #{
        change => {change_status, {failed, Failure}},
        external_id => <<"true_unique_id">>
    }),
    ?assertMatch(succeeded, get_adjustment_status(DepositID, AdjustmentID)),
    ExternalID = ff_adjustment:external_id(get_adjustment(DepositID, AdjustmentID)),
    ?assertEqual(<<"true_unique_id">>, ExternalID),
    ?assertEqual({failed, Failure},  get_deposit_status(DepositID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_source_balance(SourceID)).

-spec adjustment_can_change_failure_test(config()) -> test_return().
adjustment_can_change_failure_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-100, <<"RUB">>), get_source_balance(SourceID)),
    Failure1 = #{code => <<"one">>},
    _ = process_adjustment(DepositID, #{
        change => {change_status, {failed, Failure1}}
    }),
    ?assertEqual({failed, Failure1},  get_deposit_status(DepositID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_source_balance(SourceID)),
    Failure2 = #{code => <<"two">>},
    _ = process_adjustment(DepositID, #{
        change => {change_status, {failed, Failure2}}
    }),
    ?assertEqual({failed, Failure2},  get_deposit_status(DepositID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_source_balance(SourceID)).

-spec adjustment_can_change_status_to_succeeded_test(config()) -> test_return().
adjustment_can_change_status_to_succeeded_test(C) ->
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({5000000, <<"RUB">>}, C),
    ?assertEqual(?final_balance(5000000, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-5000000, <<"RUB">>), get_source_balance(SourceID)),
    DepositID = generate_id(),
    Params = #{
        id => DepositID,
        wallet_id => WalletID,
        source_id => SourceID,
        body => {100, <<"RUB">>}
    },
    ok = ff_deposit_machine:create(Params, ff_entity_context:new()),
    ?assertMatch({failed, _}, await_final_deposit_status(DepositID)),
    AdjustmentID = process_adjustment(DepositID, #{
        change => {change_status, succeeded}
    }),
    ?assertMatch(succeeded, get_adjustment_status(DepositID, AdjustmentID)),
    ?assertMatch(succeeded, get_deposit_status(DepositID)),
    ?assertEqual(?final_balance(5000100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-5000100, <<"RUB">>), get_source_balance(SourceID)).

-spec adjustment_can_not_change_status_to_pending_test(config()) -> test_return().
adjustment_can_not_change_status_to_pending_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Result = ff_deposit_machine:start_adjustment(DepositID, #{
        id => generate_id(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {invalid_status_change, {unavailable_status, pending}}}, Result).

-spec adjustment_can_not_change_status_to_same(config()) -> test_return().
adjustment_can_not_change_status_to_same(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Result = ff_deposit_machine:start_adjustment(DepositID, #{
        id => generate_id(),
        change => {change_status, succeeded}
    }),
    ?assertMatch({error, {invalid_status_change, {already_has_status, succeeded}}}, Result).

-spec adjustment_sequence_test(config()) -> test_return().
adjustment_sequence_test(C) ->
    #{
        deposit_id := DepositID,
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-100, <<"RUB">>), get_source_balance(SourceID)),
    MakeFailed = fun() ->
        _ = process_adjustment(DepositID, #{
            change => {change_status, {failed, #{code => <<"test">>}}}
        }),
        ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
        ?assertEqual(?final_balance(0, <<"RUB">>), get_source_balance(SourceID))
    end,
    MakeSucceeded = fun() ->
        _ = process_adjustment(DepositID, #{
            change => {change_status, succeeded}
        }),
        ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
        ?assertEqual(?final_balance(-100, <<"RUB">>), get_source_balance(SourceID))
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
        source_id := SourceID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(-100, <<"RUB">>), get_source_balance(SourceID)),
    Params = #{
        id => generate_id(),
        change => {change_status, {failed, #{code => <<"test">>}}}
    },
    _ = process_adjustment(DepositID, Params),
    _ = process_adjustment(DepositID, Params),
    _ = process_adjustment(DepositID, Params),
    _ = process_adjustment(DepositID, Params),
    Deposit = get_deposit(DepositID),
    ?assertMatch([_], ff_deposit:adjustments(Deposit)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_source_balance(SourceID)).

-spec no_parallel_adjustments_test(config()) -> test_return().
no_parallel_adjustments_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Deposit0 = get_deposit(DepositID),
    AdjustmentID0 = generate_id(),
    Params0 = #{
        id => AdjustmentID0,
        change => {change_status, {failed, #{code => <<"test">>}}}
    },
    {ok, {_, Events0}} = ff_deposit:start_adjustment(Params0, Deposit0),
    Deposit1 = lists:foldl(fun ff_deposit:apply_event/2, Deposit0, Events0),
    Params1 = #{
        id => generate_id(),
        change => {change_status, succeeded}
    },
    Result = ff_deposit:start_adjustment(Params1, Deposit1),
    ?assertMatch({error, {another_adjustment_in_progress, AdjustmentID0}}, Result).

-spec no_pending_deposit_adjustments_test(config()) -> test_return().
no_pending_deposit_adjustments_test(C) ->
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    {ok, Events0} = ff_deposit:create(#{
        id => generate_id(),
        wallet_id => WalletID,
        source_id => SourceID,
        body => {100, <<"RUB">>}
    }),
    Deposit1 = lists:foldl(fun ff_deposit:apply_event/2, undefined, Events0),
    Params1 = #{
        id => generate_id(),
        change => {change_status, succeeded}
    },
    Result = ff_deposit:start_adjustment(Params1, Deposit1),
    ?assertMatch({error, {invalid_deposit_status, pending}}, Result).

-spec unknown_deposit_test(config()) -> test_return().
unknown_deposit_test(_C) ->
    DepositID = <<"unknown_deposit">>,
    Result = ff_deposit_machine:start_adjustment(DepositID, #{
        id => generate_id(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {unknown_deposit, DepositID}}, Result).

%% Utils

prepare_standard_environment({_Amount, Currency} = DepositCash, C) ->
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
    #{
        identity_id => IdentityID,
        party_id => Party,
        wallet_id => WalletID,
        source_id => SourceID,
        deposit_id => DepositID
    }.

get_deposit(DepositID) ->
    {ok, Machine} = ff_deposit_machine:get(DepositID),
    ff_deposit_machine:deposit(Machine).

get_adjustment(DepositID, AdjustmentID) ->
    Deposit = get_deposit(DepositID),
    {ok, Adjustment} = ff_deposit:find_adjustment(AdjustmentID, Deposit),
    Adjustment.

process_deposit(DepositParams) ->
    DepositID = generate_id(),
    ok = ff_deposit_machine:create(DepositParams#{id => DepositID}, ff_entity_context:new()),
    succeeded = await_final_deposit_status(DepositID),
    DepositID.

process_adjustment(DepositID, AdjustmentParams0) ->
    AdjustmentParams1 = maps:merge(#{id => generate_id()}, AdjustmentParams0),
    #{id := AdjustmentID} = AdjustmentParams1,
    ok = ff_deposit_machine:start_adjustment(DepositID, AdjustmentParams1),
    succeeded = await_final_adjustment_status(DepositID, AdjustmentID),
    AdjustmentID.

get_deposit_status(DepositID) ->
    ff_deposit:status(get_deposit(DepositID)).

get_adjustment_status(DepositID, AdjustmentID) ->
    ff_adjustment:status(get_adjustment(DepositID, AdjustmentID)).

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

await_final_adjustment_status(DepositID, AdjustmentID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            {ok, Machine} = ff_deposit_machine:get(DepositID),
            Deposit = ff_deposit_machine:deposit(Machine),
            {ok, Adjustment} = ff_deposit:find_adjustment(AdjustmentID, Deposit),
            case ff_adjustment:is_finished(Adjustment) of
                false ->
                    {not_finished, Deposit};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    get_adjustment_status(DepositID, AdjustmentID).

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

get_source_balance(ID) ->
    {ok, Machine} = ff_source:get_machine(ID),
    get_account_balance(ff_source:account(ff_source:get(Machine))).

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
