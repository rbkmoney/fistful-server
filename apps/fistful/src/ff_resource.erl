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
    bin_data_id => bin_data_id()
}.

-type bank_card_params() :: #{
    token := binary(),
    bin => binary(),
    masked_pan => binary()
}.

-type crypto_wallet_params() :: #{
    id := binary(),
    currency := atom(),
    tag => binary()
}.

-type resource_id() :: {bank_card, bin_data_id()}.
-type resource_params() :: {bank_card,  bank_card_params()} |
                           {crypto_wallet, crypto_wallet_params()}.
-type resource() :: {bank_card, bank_card()} |
                    {crypto_wallet, crypto_wallet()}.
-type crypto_wallet() :: crypto_wallet_params().

-type token() :: binary().
-type bin() :: binary().
-type payment_system() :: ff_bin_data:payment_system().
-type masked_pan() :: binary().
-type bank_name() :: binary().
-type iso_country_code() :: ff_bin_data:iso_country_code().
-type card_type() :: charge_card | credit | debit | credit_or_debit.

-export_type([resource/0]).
-export_type([resource_id/0]).
-export_type([resource_params/0]).
-export_type([bank_card/0]).
-export_type([crypto_wallet/0]).

-export_type([token/0]).
-export_type([bin/0]).
-export_type([payment_system/0]).
-export_type([masked_pan/0]).
-export_type([bank_name/0]).
-export_type([iso_country_code/0]).
-export_type([card_type/0]).

-export([create_resource/1]).
-export([create_resource/2]).
-export([bin/1]).
-export([bin_data_id/1]).
-export([token/1]).
-export([masked_pan/1]).
-export([payment_system/1]).
-export([country_code/1]).
-export([bank_name/1]).

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

-spec bank_name(bank_card()) ->
    bank_name().
bank_name(BankCard) ->
    maps:get(bank_name, BankCard, undefined).

-spec create_resource(resource()) ->
    {ok, resource()} |
    {error, {bin_data, not_found}}.

create_resource(Resource) ->
    create_resource(Resource, undefined).

-spec create_resource(resource_params(), resource_id() | undefined) ->
    {ok, resource()} |
    {error, {bin_data, not_found}}.

create_resource({bank_card, #{token := Token} = BankCard}, undefined) ->
    do(fun() ->
        BinData = unwrap(bin_data, ff_bin_data:get(Token, undefined)),
        KeyList = [payment_system, bank_name, iso_country_code, card_type],
        ExtendData = maps:with(KeyList, BinData),
        {bank_card, maps:merge(BankCard, ExtendData#{bin_data_id => ff_bin_data:id(BinData)})}
    end);
create_resource({bank_card, #{token := Token} = BankCard}, {bank_card, ResourceID}) ->
    do(fun() ->
        BinData = unwrap(bin_data, ff_bin_data:get(Token, ResourceID)),
        KeyList = [payment_system, bank_name, iso_country_code, card_type],
        ExtendData = maps:with(KeyList, BinData),
        {bank_card, maps:merge(BankCard, ExtendData#{bin_data_id => ff_bin_data:id(BinData)})}
    end);
create_resource({crypto_wallet, CryptoWallet}, _ResourceID) ->
    {ok, CryptoWallet}.
