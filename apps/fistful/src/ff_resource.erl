-module(ff_resource).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type resource() ::
    {bank_card, resource_bank_card()}
    | {crypto_wallet, resource_crypto_wallet()}
    | {digital_wallet, resource_digital_wallet()}.

-type resource_bank_card() :: #{
    bank_card := bank_card(),
    auth_data => bank_card_auth_data()
}.

-type resource_crypto_wallet() :: #{
    crypto_wallet := crypto_wallet()
}.

-type resource_digital_wallet() :: #{
    digital_wallet := digital_wallet()
}.

-type resource_params() ::
    {bank_card, resource_bank_card_params()}
    | {crypto_wallet, resource_crypto_wallet_params()}
    | {digital_wallet, resource_digital_wallet_params()}.

-type resource_bank_card_params() :: #{
    bank_card := bank_card_params(),
    auth_data => bank_card_auth_data()
}.

-type bank_card_params() :: #{
    token := binary(),
    bin => binary(),
    masked_pan => binary(),
    cardholder_name => cardholder_name(),
    exp_date => exp_date()
}.

-type resource_crypto_wallet_params() :: #{
    crypto_wallet := crypto_wallet_params()
}.

-type crypto_wallet_params() :: #{
    id := binary(),
    currency := crypto_currency()
}.

-type resource_digital_wallet_params() :: #{
    digital_wallet := digital_wallet_params()
}.

-type digital_wallet_params() :: #{
    id := binary(),
    data := digital_wallet_data()
}.

-opaque bank_card() :: #{
    token := token(),
    bin => bin(),
    payment_system => payment_system(),
    payment_system_deprecated => payment_system_deprecated(),
    masked_pan => masked_pan(),
    bank_name => bank_name(),
    iso_country_code => iso_country_code(),
    card_type => card_type(),
    cardholder_name => binary(),
    exp_date => exp_date(),
    bin_data_id => bin_data_id(),
    category => binary()
}.

-type token() :: binary().
-type bin() :: binary().
-type payment_system() :: #{
    id := binary()
}.
-type payment_system_deprecated() :: ff_bin_data:payment_system_deprecated().
-type masked_pan() :: binary().
-type bank_name() :: binary().
-type iso_country_code() :: ff_bin_data:iso_country_code().
-type card_type() :: charge_card | credit | debit | credit_or_debit.
-type cardholder_name() :: binary().
-type exp_date() :: {month(), year()}.
-type month() :: integer().
-type year() :: integer().
-type bin_data() :: ff_bin_data:bin_data().
-type bin_data_id() :: ff_bin_data:bin_data_id().
-type bin_data_error() :: ff_bin_data:bin_data_error().
-type category() :: binary().

-type bank_card_creation_data() :: {resource_descriptor, resource_descriptor() | undefined} | {bin_data, bin_data()}.
-type resource_descriptor() :: {bank_card, bin_data_id()}.

-type bank_card_auth_data() ::
    {session, session_auth_data()}.

-type session_auth_data() :: #{
    session_id := binary()
}.

-type crypto_wallet() :: #{
    id := binary(),
    currency := crypto_currency()
}.

-type crypto_currency() ::
    {bitcoin, #{}}
    | {bitcoin_cash, #{}}
    | {litecoin, #{}}
    | {ethereum, #{}}
    | {zcash, #{}}
    | {usdt, #{}}
    | {ripple, #{tag => binary()}}.

-type digital_wallet() :: #{
    id := binary(),
    data := digital_wallet_data()
}.

-type digital_wallet_data() ::
    {webmoney, #{}}.

-export_type([resource_descriptor/0]).
-export_type([resource/0]).
-export_type([resource_params/0]).
-export_type([bank_card/0]).
-export_type([crypto_wallet/0]).
-export_type([digital_wallet/0]).

-export_type([token/0]).
-export_type([bin/0]).
-export_type([payment_system/0]).
-export_type([payment_system_deprecated/0]).
-export_type([masked_pan/0]).
-export_type([bank_name/0]).
-export_type([iso_country_code/0]).
-export_type([category/0]).
-export_type([card_type/0]).

-export([get_bin_data/2]).
-export([create_resource/1]).
-export([create_resource/2]).
-export([create_bank_card/2]).
-export([complete_resource/2]).
-export([bin/1]).
-export([bin_data_id/1]).
-export([token/1]).
-export([masked_pan/1]).
-export([payment_system/1]).
-export([payment_system_deprecated/1]).
-export([country_code/1]).
-export([category/1]).
-export([bank_name/1]).
-export([exp_date/1]).
-export([cardholder_name/1]).
-export([resource_descriptor/1]).

%% Pipeline

-spec token(bank_card()) -> token().
token(#{token := Token}) ->
    Token.

-spec bin(bank_card()) -> bin().
bin(BankCard) ->
    maps:get(bin, BankCard, undefined).

-spec bin_data_id(bank_card()) -> bin_data_id().
bin_data_id(#{bin_data_id := BinDataID}) ->
    BinDataID.

-spec masked_pan(bank_card()) -> masked_pan().
masked_pan(BankCard) ->
    maps:get(masked_pan, BankCard, undefined).

-spec payment_system(bank_card()) -> payment_system().
payment_system(BankCard) ->
    maps:get(payment_system, BankCard, undefined).

-spec payment_system_deprecated(bank_card()) -> payment_system_deprecated().
payment_system_deprecated(BankCard) ->
    maps:get(payment_system_deprecated, BankCard, undefined).

-spec country_code(bank_card()) -> iso_country_code().
country_code(BankCard) ->
    maps:get(iso_country_code, BankCard, undefined).

-spec category(bank_card()) -> category().
category(BankCard) ->
    maps:get(category, BankCard, undefined).

-spec bank_name(bank_card()) -> bank_name().
bank_name(BankCard) ->
    maps:get(bank_name, BankCard, undefined).

-spec exp_date(bank_card()) -> exp_date().
exp_date(BankCard) ->
    maps:get(exp_date, BankCard, undefined).

-spec cardholder_name(bank_card()) -> cardholder_name().
cardholder_name(BankCard) ->
    maps:get(cardholder_name, BankCard, undefined).

-spec resource_descriptor(resource() | undefined) -> resource_descriptor() | undefined.
resource_descriptor({bank_card, #{bank_card := #{bin_data_id := ID}}}) ->
    {bank_card, ID};
resource_descriptor(_) ->
    undefined.

-spec get_bin_data(resource_params(), resource_descriptor() | undefined) ->
    {ok, bin_data()}
    | {error, bin_data_error()}.
get_bin_data({bank_card, #{bank_card := #{token := Token}}}, ResourceDescriptor) ->
    get_bin_data_(Token, ResourceDescriptor).

get_bin_data_(Token, undefined) ->
    ff_bin_data:get(Token, undefined);
get_bin_data_(Token, {bank_card, ResourceID}) ->
    ff_bin_data:get(Token, ResourceID).

-spec create_resource(resource_params()) ->
    {ok, resource()}
    | {error, {bin_data, bin_data_error()}}.
create_resource({bank_card, ResourceBankCardParams}) ->
    create_bank_card(ResourceBankCardParams, {resource_descriptor, undefined});
create_resource({crypto_wallet, ResourceCryptoWalletParams}) ->
    create_crypto_wallet(ResourceCryptoWalletParams);
create_resource({digital_wallet, ResourceDigitalWalletParams}) ->
    create_digital_wallet(ResourceDigitalWalletParams).

-spec create_resource(resource_params(), resource_descriptor() | undefined) ->
    {ok, resource()}
    | {error, {bin_data, bin_data_error()}}.
create_resource({bank_card, ResourceBankCardParams}, ResourceDescriptor) ->
    create_bank_card(ResourceBankCardParams, {resource_descriptor, ResourceDescriptor});
create_resource({crypto_wallet, ResourceCryptoWalletParams}, _ResourceDescriptor) ->
    create_crypto_wallet(ResourceCryptoWalletParams);
create_resource({digital_wallet, ResourceDigitalWalletParams}, _ResourceDescriptor) ->
    create_digital_wallet(ResourceDigitalWalletParams).

-spec create_bank_card(resource_bank_card_params(), bank_card_creation_data()) ->
    {ok, resource()}
    | {error, {bin_data, bin_data_error()}}.
create_bank_card(
    #{
        bank_card := #{token := Token}
    } = ResourceBankCardParams,
    {resource_descriptor, ResourceDescriptor}
) ->
    case get_bin_data_(Token, ResourceDescriptor) of
        {ok, BinData} ->
            create_bank_card(ResourceBankCardParams, {bin_data, BinData});
        {error, Error} ->
            {error, {bin_data, Error}}
    end;
create_bank_card(
    #{
        bank_card := BankCardParams
    } = ResourceBankCardParams,
    {bin_data, BinData}
) ->
    KeyList = [payment_system_deprecated, bank_name, iso_country_code, card_type, category],
    ExtendData = maps:with(KeyList, BinData),
    {ok,
        {bank_card,
            genlib_map:compact(#{
                bank_card => maps:merge(BankCardParams, ExtendData#{bin_data_id => ff_bin_data:id(BinData)}),
                auth_data => maps:get(auth_data, ResourceBankCardParams, undefined)
            })}}.

-spec create_crypto_wallet(resource_crypto_wallet_params()) -> {ok, resource()}.
create_crypto_wallet(#{
    crypto_wallet := #{
        id := ID,
        currency := Currency
    }
}) ->
    {ok,
        {crypto_wallet, #{
            crypto_wallet => #{
                id => ID,
                currency => Currency
            }
        }}}.

-spec create_digital_wallet(resource_digital_wallet_params()) -> {ok, resource()}.
create_digital_wallet(#{
    digital_wallet := #{
        id := ID,
        data := Data
    }
}) ->
    {ok,
        {digital_wallet, #{
            digital_wallet => #{
                id => ID,
                data => Data
            }
        }}}.

-spec complete_resource(resource(), payment_system()) -> resource().
complete_resource({bank_card, ResourceBankCard}, undefined) ->
    {bank_card, ResourceBankCard};
complete_resource({bank_card, #{bank_card := BankCard} = ResourceBankCard}, PaymentSystem) ->
    {bank_card, ResourceBankCard#{
        bank_card => BankCard#{
            payment_system => PaymentSystem
        }
    }};
complete_resource({crypto_wallet, _} = Resource, _) ->
    Resource;
complete_resource({digital_wallet, _} = Resource, _) ->
    Resource.
