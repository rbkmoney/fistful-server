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
    CardSender   = ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    #{
        wallet_id := WalletID
    } = prepare_standard_environment(Cash, C),
    {ok, Machine} = ff_wallet_machine:get(WalletID),
    Wallet = ff_wallet_machine:wallet(Machine),
    Identity = ff_wallet:identity(Wallet),
    Sender = {bank_card, CardSender},
    {ok, {Fee, CashVolume, _}} = p2p_quote:get_fee_quote(Cash, Identity, Sender, Sender),
    ?assertEqual({share, {{65, 10000}, operation_amount, default}}, CashVolume),
    ?assertEqual({146, <<"RUB">>}, Fee).

-spec visa_to_nspkmir_not_allow_test(config()) -> test_return().
visa_to_nspkmir_not_allow_test(C) ->
    Cash = {22500, <<"RUB">>},
    CardSender   = ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    #{bin := Bin, masked_pan := Pan} = ct_cardstore:bank_card(<<"2204399999000900">>, {12, 2025}, C),
    #{
        wallet_id := WalletID
    } = prepare_standard_environment(Cash, C),
    {ok, Machine} = ff_wallet_machine:get(WalletID),
    Wallet = ff_wallet_machine:wallet(Machine),
    Identity = ff_wallet:identity(Wallet),
    Sender = {bank_card, CardSender},
    Receiver = {bank_card, #{
        bin => Bin,
        masked_pan => Pan,
        token => <<"NSPK MIR">>
    }},
    Result = p2p_quote:get_fee_quote(Cash, Identity, Sender, Receiver),
    ?assertEqual({error, {p2p_tool, not_allow}}, Result).

%% Utils

prepare_standard_environment(Cash, C) ->
    prepare_standard_environment(Cash, undefined, C).

prepare_standard_environment({_Amount, Currency} =Cash, Token, C) ->
    Party = create_party(C),
    IdentityID = create_person_identity(Party, C),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, Currency, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    DestinationID = create_destination(IdentityID, Token, C),
    ok = set_wallet_balance(Cash, WalletID),
    #{
        identity_id => IdentityID,
        party_id => Party,
        wallet_id => WalletID,
        destination_id => DestinationID
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
    {ok, {Amounts, Currency}} = ff_transaction:balance(
        Account,
        ff_clock:latest_clock()
    ),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

generate_id() ->
    ff_id:generate_snowflake_id().

create_destination(IID, <<"USD_CURRENCY">>, C) ->
    create_destination(IID, <<"USD">>, undefined, C);
create_destination(IID, Token, C) ->
    create_destination(IID, <<"RUB">>, Token, C).

create_destination(IID, Currency, Token, C) ->
    ID = generate_id(),
    StoreSource = ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    NewStoreResource = case Token of
        undefined ->
            StoreSource;
        Token ->
            StoreSource#{token => Token}
        end,
    Resource = {bank_card, NewStoreResource},
    Params = #{identity => IID, name => <<"XDesination">>, currency => Currency, resource => Resource},
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
