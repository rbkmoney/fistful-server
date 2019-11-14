-module(p2p_tests_utils).

-include_lib("damsel/include/dmsl_accounter_thrift.hrl").

%% API
-export([
    prepare_standard_environment/2,
    prepare_standard_environment/3
]).

-type config() :: ct_helper:config().
-type cash() :: ff_cash:cash().
-type token() :: binary().
-type prepared_ids() :: #{
    identity_id => ff_identity:id(),
    party_id => ff_party:id(),
    wallet_id => ff_wallet:id(),
    destination_id => ff_destination:id()
}.

-spec prepare_standard_environment(cash(), config()) -> prepared_ids().

prepare_standard_environment(Cash, C) ->
    prepare_standard_environment(Cash, undefined, C).

-spec prepare_standard_environment(cash(), token(), config()) -> prepared_ids().

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

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.
