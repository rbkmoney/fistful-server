-module(ff_resource).

-type bin_data_id() :: ff_bin_data:bin_data_id().

-opaque bank_card() :: #{
    token := binary(),
    bin => binary(),
    payment_system := atom(), % TODO
    masked_pan => binary(),
    bank_name => binary(),
    iso_country_code => atom(),
    card_type => charge_card | credit | debit | credit_or_debit,
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

-type contact_info() :: #{
    phone_number => binary(),
    email => binary()
}.

-type disposable_resource_params() :: #{
    params := resource_params(),
    contact_info => contact_info()
}.

-type disposable_resource() :: #{
    resource := resource(),
    contact_info => contact_info()
}.

-export_type([resource/0]).
-export_type([resource_id/0]).
-export_type([resource_params/0]).
-export_type([bank_card/0]).
-export_type([crypto_wallet/0]).
-export_type([disposable_resource/0]).
-export_type([disposable_resource_params/0]).
-export_type([contact_info/0]).

-export([create_resource/1]).
-export([create_resource/2]).
-export([create_disposable_resource/1]).
-export([bin/1]).
-export([bin_data_id/1]).
-export([token/1]).
-export([masked_pan/1]).
-export([payment_system/1]).
-export([country_code/1]).
-export([bank_name/1]).
-export([resource/1]).
-export([contact_info/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

-spec token(bank_card()) ->
    binary().
token(#{token := Token}) ->
    Token.

-spec bin(bank_card()) ->
    binary().
bin(BankCard) ->
    maps:get(bin, BankCard, undefined).

-spec bin_data_id(bank_card()) ->
    binary().
bin_data_id(#{bin_data_id := BinDataID}) ->
    BinDataID.


-spec masked_pan(bank_card()) ->
    binary().
masked_pan(BankCard) ->
    maps:get(masked_pan, BankCard, undefined).

-spec payment_system(bank_card()) ->
    atom().
payment_system(#{payment_system := PaymentSystem}) ->
    PaymentSystem.

-spec country_code(bank_card()) ->
    atom().
country_code(BankCard) ->
    maps:get(iso_country_code, BankCard, undefined).

-spec bank_name(bank_card()) ->
    binary().
bank_name(BankCard) ->
    maps:get(bank_name, BankCard, undefined).

-spec resource(disposable_resource()) ->
    resource().
resource(Resource) ->
    maps:get(resource, Resource).

-spec contact_info(disposable_resource()) ->
    contact_info() | undefined.
contact_info(Resource) ->
    maps:get(contact_info, Resource, undefined).

-spec create_resource(resource()) ->
    {ok, resource()} |
    {error, {bin_data, not_found}}.

create_resource(Resource) ->
    create_resource(Resource, undefined).

-spec create_resource(resource_params(), resource_id() | undefined) ->
    {ok, resource()} |
    {error, {bin_data, not_found}}.

create_resource({bank_card, #{token := Token} = BankCard}, {bank_card, ResourceID}) ->
    do(fun() ->
        BinData = unwrap(bin_data, ff_bin_data:get(Token, ResourceID)),
        KeyList = [payment_system, bank_name, iso_country_code, card_type],
        ExtendData = maps:with(KeyList, BinData),
        {bank_card, maps:merge(BankCard, ExtendData#{bin_data_id => ff_bin_data:id(BinData)})}
    end);
create_resource({crypto_wallet, CryptoWallet}, _ResourceID) ->
    {ok, CryptoWallet}.

-spec create_disposable_resource(disposable_resource_params()) ->
    {ok, disposable_resource()} |
    {error, {bin_data, not_found}}.

create_disposable_resource(Data = #{params := Params}) ->
    do(fun() ->
        ContactInfo = maps:get(contact_info, Data, undefined),
        Resource = unwrap(create_resource(Params)),
        genlib_map:compact(#{
            resource => Resource,
            contact_info => ContactInfo
        })
    end).
