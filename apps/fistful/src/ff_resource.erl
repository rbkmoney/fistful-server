-module(ff_resource).

-type bin_data_id() :: ff_bin_data:bin_data_id().

-opaque bank_card() :: #{
    token := token(),
    bin => bin(),
    payment_system := payment_system(),
    masked_pan => masked_pan(),
    bank_name => bank_name(),
    iso_country_code => iso_country_code(),
    card_type => card_type(),
    cardholder_name => binary(),
    exp_date => exp_date(),
    bin_data_id => bin_data_id(),
    category => binary()
}.

-type resource_bank_card_params() :: #{
    bank_card := bank_card_params(),
    auth_data => bank_card_auth_data()
}.

-type resource_crypto_wallet_params() :: #{
    crypto_wallet := crypto_wallet_params()
}.

-type bank_card_params() :: #{
    token := binary(),
    bin => binary(),
    masked_pan => binary(),
    cardholder_name => cardholder_name(),
    exp_date => exp_date()
}.

-type cardholder_name() :: binary().
-type month() :: integer().
-type year() :: integer().
-type exp_date() :: {month(), year()}.
-type bank_card_auth_data() ::
    {session, session_auth_data()}.

-type session_auth_data() :: #{
    session_id := binary()
}.

-type crypto_wallet_params() :: #{
    id := binary(),
    currency := crypto_currency()
}.

-type resource_descriptor() :: {bank_card, bin_data_id()}.
-type resource_params() :: {bank_card,  resource_bank_card_params()} |
                           {crypto_wallet, resource_crypto_wallet_params()}.
-type resource() :: {bank_card, resource_bank_card()} |
                    {crypto_wallet, resource_crypto_wallet()}.

-type resource_bank_card() :: #{
    bank_card := bank_card(),
    auth_data => bank_card_auth_data()
}.

-type resource_crypto_wallet() :: #{
    crypto_wallet := crypto_wallet()
}.

-type crypto_wallet() :: #{
    id       := binary(),
    currency := crypto_currency()
}.

-type crypto_currency()
    :: {bitcoin,      #{}}
     | {bitcoin_cash, #{}}
     | {litecoin,     #{}}
     | {ethereum,     #{}}
     | {zcash,        #{}}
     | {usdt,         #{}}
     | {ripple,       #{tag => binary()}}
     .

-type token() :: binary().
-type bin() :: binary().
-type payment_system() :: ff_bin_data:payment_system().
-type masked_pan() :: binary().
-type bank_name() :: binary().
-type iso_country_code() :: ff_bin_data:iso_country_code().
-type card_type() :: charge_card | credit | debit | credit_or_debit.
-type category() :: binary().

-export_type([resource/0]).
-export_type([resource_descriptor/0]).
-export_type([resource_params/0]).
-export_type([bank_card/0]).
-export_type([crypto_wallet/0]).

-export_type([token/0]).
-export_type([bin/0]).
-export_type([payment_system/0]).
-export_type([masked_pan/0]).
-export_type([bank_name/0]).
-export_type([iso_country_code/0]).
-export_type([category/0]).
-export_type([card_type/0]).

-export([create_resource/1]).
-export([create_resource/2]).
-export([bin/1]).
-export([bin_data_id/1]).
-export([token/1]).
-export([masked_pan/1]).
-export([payment_system/1]).
-export([country_code/1]).
-export([category/1]).
-export([bank_name/1]).
-export([exp_date/1]).
-export([cardholder_name/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/2]).

-spec token(bank_card()) ->
    token().
token(#{token := Token}) ->
    Token.

-spec bin(bank_card()) ->
    bin().
bin(BankCard) ->
    maps:get(bin, BankCard, undefined).

-spec bin_data_id(bank_card()) ->
    bin_data_id().
bin_data_id(#{bin_data_id := BinDataID}) ->
    BinDataID.


-spec masked_pan(bank_card()) ->
    masked_pan().
masked_pan(BankCard) ->
    maps:get(masked_pan, BankCard, undefined).

-spec payment_system(bank_card()) ->
    payment_system().
payment_system(#{payment_system := PaymentSystem}) ->
    PaymentSystem.

-spec country_code(bank_card()) ->
    iso_country_code().
country_code(BankCard) ->
    maps:get(iso_country_code, BankCard, undefined).

-spec category(bank_card()) ->
    category().
category(BankCard) ->
    maps:get(category, BankCard, undefined).

-spec bank_name(bank_card()) ->
    bank_name().
bank_name(BankCard) ->
    maps:get(bank_name, BankCard, undefined).

-spec exp_date(bank_card()) ->
    exp_date().
exp_date(BankCard) ->
    maps:get(exp_date, BankCard, undefined).

-spec cardholder_name(bank_card()) ->
    cardholder_name().
cardholder_name(BankCard) ->
    maps:get(cardholder_name, BankCard, undefined).

-spec create_resource(resource_params()) ->
    {ok, resource()} |
    {error, {bin_data, not_found}}.

create_resource(Resource) ->
    create_resource(Resource, undefined).

-spec create_resource(resource_params(), resource_descriptor() | undefined) ->
    {ok, resource()} |
    {error, {bin_data, not_found}}.

create_resource({bank_card, #{bank_card := #{token := Token} = BankCardParams} = Params}, ResourceID) ->
    do(fun() ->
        BinData = unwrap(bin_data, get_bin_data(Token, ResourceID)),
        KeyList = [payment_system, bank_name, iso_country_code, card_type, category],
        ExtendData = maps:with(KeyList, BinData),
        {bank_card, genlib_map:compact(#{
            bank_card => maps:merge(BankCardParams, ExtendData#{bin_data_id => ff_bin_data:id(BinData)}),
            auth_data => maps:get(auth_data, Params, undefined)
        })}
    end);
create_resource({crypto_wallet, #{crypto_wallet := #{
    id := ID,
    currency := Currency
}}}, _ResourceID) ->
    {ok, {crypto_wallet, #{
        crypto_wallet => #{
            id => ID,
            currency => Currency
        }
    }}}.

get_bin_data(Token, undefined) ->
    ff_bin_data:get(Token, undefined);
get_bin_data(Token, {bank_card, ResourceID}) ->
    ff_bin_data:get(Token, ResourceID).
