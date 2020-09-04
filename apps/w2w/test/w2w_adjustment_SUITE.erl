-module(w2w_adjustment_SUITE).

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

-export([adjustment_can_change_status_to_failed_test/1]).
-export([adjustment_can_change_failure_test/1]).
-export([adjustment_can_change_status_to_succeeded_test/1]).
-export([adjustment_can_not_change_status_to_pending_test/1]).
-export([adjustment_can_not_change_status_to_same/1]).
-export([adjustment_sequence_test/1]).
-export([adjustment_idempotency_test/1]).
-export([no_parallel_adjustments_test/1]).
-export([no_pending_w2w_transfer_adjustments_test/1]).
-export([unknown_w2w_transfer_test/1]).
-export([consume_eventsinks/1]).

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
    [{group, default}, {group, eventsink}].

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
            no_pending_w2w_transfer_adjustments_test,
            unknown_w2w_transfer_test
        ]},
        {eventsink, [], [
            consume_eventsinks
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
        w2w_transfer_id := W2WTransferID,
        wallet_to_id := WalletToID,
        wallet_from_id := WalletFromID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletToID)),
    Failure = #{code => <<"test">>},
    AdjustmentID = process_adjustment(W2WTransferID, #{
        change => {change_status, {failed, Failure}},
        external_id => <<"true_unique_id">>
    }),
    ?assertMatch(succeeded, get_adjustment_status(W2WTransferID, AdjustmentID)),
    ExternalID = ff_adjustment:external_id(get_adjustment(W2WTransferID, AdjustmentID)),
    ?assertEqual(<<"true_unique_id">>, ExternalID),
    ?assertEqual({failed, Failure},  get_w2w_transfer_status(W2WTransferID)),
    assert_adjustment_same_revisions(W2WTransferID, AdjustmentID),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletToID)).

-spec adjustment_can_change_failure_test(config()) -> test_return().
adjustment_can_change_failure_test(C) ->
    #{
        w2w_transfer_id := W2WTransferID,
        wallet_to_id := WalletToID,
        wallet_from_id := WalletFromID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletToID)),
    Failure1 = #{code => <<"one">>},
    AdjustmentID1 = process_adjustment(W2WTransferID, #{
        change => {change_status, {failed, Failure1}}
    }),
    ?assertEqual({failed, Failure1},  get_w2w_transfer_status(W2WTransferID)),
    assert_adjustment_same_revisions(W2WTransferID, AdjustmentID1),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletToID)),
    Failure2 = #{code => <<"two">>},
    AdjustmentID2 = process_adjustment(W2WTransferID, #{
        change => {change_status, {failed, Failure2}}
    }),
    ?assertEqual({failed, Failure2},  get_w2w_transfer_status(W2WTransferID)),
    assert_adjustment_same_revisions(W2WTransferID, AdjustmentID2),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletToID)).

-spec adjustment_can_change_status_to_succeeded_test(config()) -> test_return().
adjustment_can_change_status_to_succeeded_test(C) ->
    #{
        wallet_to_id := WalletToID,
        wallet_from_id := WalletFromID
    } = prepare_standard_environment({50000, <<"RUB">>}, C),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(50000, <<"RUB">>), get_wallet_balance(WalletToID)),
    W2WTransferID = generate_id(),
    Params = #{
        id => W2WTransferID,
        wallet_to_id => WalletToID,
        wallet_from_id => WalletFromID,
        body => {100, <<"RUB">>}
    },
    ok = w2w_transfer_machine:create(Params, ff_entity_context:new()),
    ?assertMatch({failed, _}, await_final_w2w_transfer_status(W2WTransferID)),
    AdjustmentID = process_adjustment(W2WTransferID, #{
        change => {change_status, succeeded}
    }),
    ?assertMatch(succeeded, get_adjustment_status(W2WTransferID, AdjustmentID)),
    ?assertMatch(succeeded, get_w2w_transfer_status(W2WTransferID)),
    assert_adjustment_same_revisions(W2WTransferID, AdjustmentID),
    ?assertEqual(?final_balance(-100, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(50100, <<"RUB">>), get_wallet_balance(WalletToID)).

-spec adjustment_can_not_change_status_to_pending_test(config()) -> test_return().
adjustment_can_not_change_status_to_pending_test(C) ->
    #{
        w2w_transfer_id := W2WTransferID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Result = w2w_transfer_machine:start_adjustment(W2WTransferID, #{
        id => generate_id(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {invalid_status_change, {unavailable_status, pending}}}, Result).

-spec adjustment_can_not_change_status_to_same(config()) -> test_return().
adjustment_can_not_change_status_to_same(C) ->
    #{
        w2w_transfer_id := W2WTransferID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Result = w2w_transfer_machine:start_adjustment(W2WTransferID, #{
        id => generate_id(),
        change => {change_status, succeeded}
    }),
    ?assertMatch({error, {invalid_status_change, {already_has_status, succeeded}}}, Result).

-spec adjustment_sequence_test(config()) -> test_return().
adjustment_sequence_test(C) ->
    #{
        w2w_transfer_id := W2WTransferID,
        wallet_to_id := WalletToID,
        wallet_from_id := WalletFromID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletToID)),
    MakeFailed = fun() ->
        _ = process_adjustment(W2WTransferID, #{
            change => {change_status, {failed, #{code => <<"test">>}}}
        }),
        ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletFromID)),
        ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletToID))
    end,
    MakeSucceeded = fun() ->
        _ = process_adjustment(W2WTransferID, #{
            change => {change_status, succeeded}
        }),
        ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletFromID)),
        ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletToID))
    end,
    MakeFailed(),
    MakeSucceeded(),
    MakeFailed(),
    MakeSucceeded(),
    MakeFailed().

-spec adjustment_idempotency_test(config()) -> test_return().
adjustment_idempotency_test(C) ->
    #{
        w2w_transfer_id := W2WTransferID,
        wallet_to_id := WalletToID,
        wallet_from_id := WalletFromID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletToID)),
    Params = #{
        id => generate_id(),
        change => {change_status, {failed, #{code => <<"test">>}}}
    },
    _ = process_adjustment(W2WTransferID, Params),
    _ = process_adjustment(W2WTransferID, Params),
    _ = process_adjustment(W2WTransferID, Params),
    _ = process_adjustment(W2WTransferID, Params),
    W2WTransfer = get_w2w_transfer(W2WTransferID),
    ?assertMatch([_], w2w_transfer:adjustments(W2WTransfer)),
    ?assertEqual(?final_balance(100, <<"RUB">>), get_wallet_balance(WalletFromID)),
    ?assertEqual(?final_balance(0, <<"RUB">>), get_wallet_balance(WalletToID)).

-spec no_parallel_adjustments_test(config()) -> test_return().
no_parallel_adjustments_test(C) ->
    #{
        w2w_transfer_id := W2WTransferID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    W2WTransfer0 = get_w2w_transfer(W2WTransferID),
    AdjustmentID0 = generate_id(),
    Params0 = #{
        id => AdjustmentID0,
        change => {change_status, {failed, #{code => <<"test">>}}}
    },
    {ok, {_, Events0}} = w2w_transfer:start_adjustment(Params0, W2WTransfer0),
    W2WTransfer1 = lists:foldl(fun w2w_transfer:apply_event/2, W2WTransfer0, Events0),
    Params1 = #{
        id => generate_id(),
        change => {change_status, succeeded}
    },
    Result = w2w_transfer:start_adjustment(Params1, W2WTransfer1),
    ?assertMatch({error, {another_adjustment_in_progress, AdjustmentID0}}, Result).

-spec no_pending_w2w_transfer_adjustments_test(config()) -> test_return().
no_pending_w2w_transfer_adjustments_test(C) ->
    #{
        wallet_to_id := WalletToID,
        wallet_from_id := WalletFromID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    {ok, Events0} = w2w_transfer:create(#{
        id => generate_id(),
        wallet_to_id => WalletToID,
        wallet_from_id => WalletFromID,
        body => {100, <<"RUB">>}
    }),
    W2WTransfer1 = lists:foldl(fun w2w_transfer:apply_event/2, undefined, Events0),
    Params1 = #{
        id => generate_id(),
        change => {change_status, succeeded}
    },
    Result = w2w_transfer:start_adjustment(Params1, W2WTransfer1),
    ?assertMatch({error, {invalid_w2w_transfer_status, pending}}, Result).

-spec unknown_w2w_transfer_test(config()) -> test_return().
unknown_w2w_transfer_test(_C) ->
    W2WTransferID = <<"unknown_w2w_transfer">>,
    Result = w2w_transfer_machine:start_adjustment(W2WTransferID, #{
        id => generate_id(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {unknown_w2w_transfer, W2WTransferID}}, Result).

-spec consume_eventsinks(config()) -> test_return().

consume_eventsinks(_) ->
    EventSinks = [w2w_transfer_event_sink],
    [_Events = ct_eventsink:consume(1000, Sink) || Sink <- EventSinks].

%% Utils

prepare_standard_environment({_Amount, Currency} = Cash, C) ->
    PartyID = create_party(C),
    IdentityID = create_person_identity(PartyID, C),
    WalletFromID = create_wallet(IdentityID, <<"My wallet from">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, Currency}, WalletFromID),
    WalletToID = create_wallet(IdentityID, <<"My wallet to">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, Currency}, WalletToID),
    ok = set_wallet_balance(Cash, WalletFromID),
    W2WTransferID = process_w2w_transfer(#{
        wallet_from_id => WalletFromID,
        wallet_to_id => WalletToID,
        body => Cash
    }),
    #{
        identity_id => IdentityID,
        party_id => PartyID,
        wallet_from_id => WalletFromID,
        wallet_to_id => WalletToID,
        w2w_transfer_id => W2WTransferID
    }.

get_w2w_transfer(W2WTransferID) ->
    {ok, Machine} = w2w_transfer_machine:get(W2WTransferID),
    w2w_transfer_machine:w2w_transfer(Machine).

get_adjustment(W2WTransferID, AdjustmentID) ->
    W2WTransfer = get_w2w_transfer(W2WTransferID),
    {ok, Adjustment} = w2w_transfer:find_adjustment(AdjustmentID, W2WTransfer),
    Adjustment.

process_w2w_transfer(W2WTransferParams) ->
    W2WTransferID = generate_id(),
    ok = w2w_transfer_machine:create(W2WTransferParams#{id => W2WTransferID}, ff_entity_context:new()),
    succeeded = await_final_w2w_transfer_status(W2WTransferID),
    W2WTransferID.

process_adjustment(W2WTransferID, AdjustmentParams0) ->
    AdjustmentParams1 = maps:merge(#{id => generate_id()}, AdjustmentParams0),
    #{id := AdjustmentID} = AdjustmentParams1,
    ok = w2w_transfer_machine:start_adjustment(W2WTransferID, AdjustmentParams1),
    succeeded = await_final_adjustment_status(W2WTransferID, AdjustmentID),
    AdjustmentID.

get_w2w_transfer_status(W2WTransferID) ->
    w2w_transfer:status(get_w2w_transfer(W2WTransferID)).

get_adjustment_status(W2WTransferID, AdjustmentID) ->
    ff_adjustment:status(get_adjustment(W2WTransferID, AdjustmentID)).

await_final_w2w_transfer_status(W2WTransferID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
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

await_final_adjustment_status(W2WTransferID, AdjustmentID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            {ok, Machine} = w2w_transfer_machine:get(W2WTransferID),
            W2WTransfer = w2w_transfer_machine:w2w_transfer(Machine),
            {ok, Adjustment} = w2w_transfer:find_adjustment(AdjustmentID, W2WTransfer),
            case ff_adjustment:is_finished(Adjustment) of
                false ->
                    {not_finished, W2WTransfer};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    get_adjustment_status(W2WTransferID, AdjustmentID).

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
        ff_entity_context:new()
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

assert_adjustment_same_revisions(W2WTransferID, AdjustmentID) ->
    Adjustment = get_adjustment(W2WTransferID, AdjustmentID),
    W2WTransfer = get_w2w_transfer(W2WTransferID),
    ?assertEqual(w2w_transfer:domain_revision(W2WTransfer), ff_adjustment:domain_revision(Adjustment)),
    ?assertEqual(w2w_transfer:party_revision(W2WTransfer), ff_adjustment:party_revision(Adjustment)),
    ?assertEqual(w2w_transfer:created_at(W2WTransfer), ff_adjustment:operation_timestamp(Adjustment)),
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
