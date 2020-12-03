-module(ff_w2w_transfer_handler_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/ff_proto_w2w_transfer_thrift.hrl").
-include_lib("shumpune_proto/include/shumpune_shumpune_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([create_adjustment_ok_test/1]).
-export([get_w2w_transfer_context_ok_test/1]).
-export([check_balance_w2w_transfer_ok_test/1]).
-export([get_w2w_transfer_ok_test/1]).
-export([create_w2w_transfer_ok_test/1]).
-export([unknown_test/1]).

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            create_adjustment_ok_test,
            get_w2w_transfer_context_ok_test,
            check_balance_w2w_transfer_ok_test,
            get_w2w_transfer_ok_test,
            create_w2w_transfer_ok_test,
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

-spec create_adjustment_ok_test(config()) -> test_return().
create_adjustment_ok_test(C) ->
    Cash = make_cash({100, <<"RUB">>}),
    #{
        w2w_transfer_id := ID
    } = prepare_standard_environment(Cash, C),
    AdjustmentID = generate_id(),
    ExternalID = generate_id(),
    Params = #w2w_adj_AdjustmentParams{
        id = AdjustmentID,
        change =
            {change_status, #w2w_adj_ChangeStatusRequest{
                new_status = {failed, #w2w_status_Failed{failure = #'Failure'{code = <<"Ooops">>}}}
            }},
        external_id = ExternalID
    },
    {ok, AdjustmentState} = call_w2w('CreateAdjustment', [ID, Params]),
    ExpectedAdjustment = get_adjustment(ID, AdjustmentID),

    ?assertEqual(AdjustmentID, AdjustmentState#w2w_adj_AdjustmentState.id),
    ?assertEqual(ExternalID, AdjustmentState#w2w_adj_AdjustmentState.external_id),
    ?assertEqual(
        ff_adjustment:created_at(ExpectedAdjustment),
        ff_codec:unmarshal(timestamp_ms, AdjustmentState#w2w_adj_AdjustmentState.created_at)
    ),
    ?assertEqual(
        ff_adjustment:domain_revision(ExpectedAdjustment),
        AdjustmentState#w2w_adj_AdjustmentState.domain_revision
    ),
    ?assertEqual(
        ff_adjustment:party_revision(ExpectedAdjustment),
        AdjustmentState#w2w_adj_AdjustmentState.party_revision
    ),
    ?assertEqual(
        ff_w2w_transfer_adjustment_codec:marshal(changes_plan, ff_adjustment:changes_plan(ExpectedAdjustment)),
        AdjustmentState#w2w_adj_AdjustmentState.changes_plan
    ).

-spec get_w2w_transfer_context_ok_test(config()) -> test_return().
get_w2w_transfer_context_ok_test(C) ->
    Cash = make_cash({100, <<"RUB">>}),
    #{
        w2w_transfer_id := ID,
        context := Ctx
    } = prepare_standard_environment(Cash, C),
    {ok, Context} = call_w2w('GetContext', [ID]),
    ?assertEqual(Ctx, Context).

-spec check_balance_w2w_transfer_ok_test(config()) -> test_return().
check_balance_w2w_transfer_ok_test(C) ->
    Cash = make_cash({100, <<"RUB">>}),
    #{
        w2w_transfer_id := ID,
        wallet_to_id := WalletID2
    } = prepare_standard_environment(Cash, C),
    {ok, _W2WTransferState} = call_w2w('Get', [ID, #'EventRange'{}]),
    ok = await_wallet_balance({200, <<"RUB">>}, WalletID2).

-spec get_w2w_transfer_ok_test(config()) -> test_return().
get_w2w_transfer_ok_test(C) ->
    Cash = make_cash({100, <<"RUB">>}),
    #{
        w2w_transfer_id := ID
    } = prepare_standard_environment(Cash, C),
    {ok, W2WTransferState} = call_w2w('Get', [ID, #'EventRange'{}]),
    ?assertEqual(ID, W2WTransferState#w2w_transfer_W2WTransferState.id).

-spec create_w2w_transfer_ok_test(config()) -> test_return().
create_w2w_transfer_ok_test(C) ->
    Cash = make_cash({100, <<"RUB">>}),
    #{
        wallet_from_id := WalletID1,
        wallet_to_id := WalletID2,
        context := Ctx,
        metadata := Metadata
    } = prepare_standard_environment(Cash, C),
    W2WTransferID = generate_id(),
    ExternalID = generate_id(),
    Params = #w2w_transfer_W2WTransferParams{
        id = W2WTransferID,
        body = Cash,
        wallet_from_id = WalletID1,
        wallet_to_id = WalletID2,
        external_id = ExternalID,
        metadata = Metadata
    },
    {ok, W2WTransferState} = call_w2w('Create', [Params, Ctx]),

    Expected = get_w2w_transfer(W2WTransferID),
    ?assertEqual(W2WTransferID, W2WTransferState#w2w_transfer_W2WTransferState.id),
    ?assertEqual(WalletID1, W2WTransferState#w2w_transfer_W2WTransferState.wallet_from_id),
    ?assertEqual(WalletID2, W2WTransferState#w2w_transfer_W2WTransferState.wallet_to_id),
    ?assertEqual(ExternalID, W2WTransferState#w2w_transfer_W2WTransferState.external_id),
    ?assertEqual(
        w2w_transfer:domain_revision(Expected),
        W2WTransferState#w2w_transfer_W2WTransferState.domain_revision
    ),
    ?assertEqual(
        w2w_transfer:party_revision(Expected),
        W2WTransferState#w2w_transfer_W2WTransferState.party_revision
    ),
    ?assertEqual(
        w2w_transfer:created_at(Expected),
        ff_codec:unmarshal(timestamp_ms, W2WTransferState#w2w_transfer_W2WTransferState.created_at)
    ).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    ID = <<"unknown_id">>,
    Result = call_w2w('Get', [ID, #'EventRange'{}]),
    ExpectedError = #fistful_W2WNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

%%  Internals

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
        genlib_retry:linear(10, 1000)
    ),
    get_w2w_status(W2WTransferID).

get_w2w(ID) ->
    {ok, Machine} = w2w_transfer_machine:get(ID),
    w2w_transfer_machine:w2w_transfer(Machine).

get_w2w_status(ID) ->
    w2w_transfer:status(get_w2w(ID)).

get_adjustment(ID, AdjustmentID) ->
    {ok, Adjustment} = w2w_transfer:find_adjustment(AdjustmentID, get_w2w(ID)),
    Adjustment.

make_cash({Amount, Currency}) ->
    #'Cash'{
        amount = Amount,
        currency = #'CurrencyRef'{symbolic_code = Currency}
    }.

call_w2w(Fun, Args) ->
    ServiceName = w2w_transfer_management,
    Service = ff_services:get_service(ServiceName),
    Request = {Service, Fun, Args},
    Client = ff_woody_client:new(#{
        url => "http://localhost:8022" ++ ff_services:get_service_path(ServiceName)
    }),
    ff_woody_client:call(Client, Request).

prepare_standard_environment(Body, C) ->
    #'Cash'{
        amount = Amount,
        currency = #'CurrencyRef'{symbolic_code = Currency}
    } = Body,
    Party = create_party(C),
    IdentityID = create_person_identity(Party, C),
    WalletID1 = create_wallet(IdentityID, <<"My wallet 1">>, Currency, C),
    WalletID2 = create_wallet(IdentityID, <<"My wallet 2">>, Currency, C),
    ok = await_wallet_balance({0, Currency}, WalletID1),
    ok = await_wallet_balance({0, Currency}, WalletID2),
    ok = set_wallet_balance({Amount, Currency}, WalletID1),
    ok = set_wallet_balance({Amount, Currency}, WalletID2),
    W2WTransferID = generate_id(),
    ExternalID = generate_id(),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Metadata = ff_entity_context_codec:marshal(#{<<"key">> => <<"value">>}),
    Params = #w2w_transfer_W2WTransferParams{
        id = W2WTransferID,
        body = Body,
        wallet_from_id = WalletID1,
        wallet_to_id = WalletID2,
        external_id = ExternalID,
        metadata = Metadata
    },
    {ok, _State} = call_w2w('Create', [Params, Ctx]),
    succeeded = await_final_w2w_transfer_status(W2WTransferID),
    #{
        identity_id => IdentityID,
        party_id => Party,
        w2w_transfer_id => W2WTransferID,
        wallet_from_id => WalletID1,
        wallet_to_id => WalletID2,
        context => Ctx,
        metadata => Metadata
    }.

get_w2w_transfer(W2WTransferID) ->
    {ok, Machine} = w2w_transfer_machine:get(W2WTransferID),
    w2w_transfer_machine:w2w_transfer(Machine).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
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

create_person_identity(Party, C) ->
    create_person_identity(Party, C, <<"quote-owner">>).

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

generate_id() ->
    ff_id:generate_snowflake_id().
