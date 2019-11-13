-module(ff_resource).

-opaque bank_card() :: #{
    token               := binary(),
    bin                 => binary(),
    payment_system      := atom(), % TODO
    masked_pan          => binary(),
    bank_name           => binary(),
    iso_country_code    => atom(),
    card_type           => charge_card | credit | debit | credit_or_debit,
    bin_data_id         := ff_bin_data:bin_data_id()
}.

-type bank_card_resource() :: #{
    token      := binary(),
    bin        => binary(),
    masked_pan => binary()
}.
-type crypto_wallet_resource() :: #{
    id       := binary(),
    currency := atom(),
    tag      => binary()
}.

-type resource()        :: {bank_card,  bank_card_resource()} |
                           {crypto_wallet, crypto_wallet_resource()}.
-type resource_full()   :: {bank_card, bank_card()} |
                           {crypto_wallet, crypto_wallet()}.
-type crypto_wallet()   :: crypto_wallet_resource().

-opaque disposable_resource() :: #{tool := resource_full()}.

-export_type([resource/0]).
-export_type([bank_card/0]).
-export_type([crypto_wallet/0]).
-export_type([disposable_resource/0]).

-export([resource_full/1]).
-export([resource_full/2]).
-export([create_disposable_resource/1]).
-export([disposable_resource_tool/1]).
-export([bin/1]).
-export([bin_data_id/1]).
-export([token/1]).
-export([masked_pan/1]).
-export([payment_system/1]).
-export([country_code/1]).
-export([bank_name/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

-spec disposable_resource_tool(disposable_resource()) ->
    resource_full().

disposable_resource_tool(#{tool := ResourceFull}) ->
    ResourceFull.

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


-spec resource_full(resource()) ->
    {ok, resource_full()} |
    {error, {bin_data, not_found}}.

resource_full(Resource) ->
    resource_full(Resource, undefined).

-spec resource_full(resource(), binary() | undefined) ->
    {ok, resource_full()} |
    {error, {bin_data, not_found}}.

resource_full({bank_card, #{token := Token} = BankCard}, ResourceID) ->
    do(fun() ->
        BinData = unwrap(bin_data, ff_bin_data:get(Token, ResourceID)),
        KeyList = [payment_system, bank_name, iso_country_code, card_type],
        ExtendData = maps:with(KeyList, BinData),
        {bank_card, maps:merge(BankCard, ExtendData#{bin_data_id => ff_bin_data:id(BinData)})}
    end);
resource_full({crypto_wallet, CryptoWallet}, _ResourceID) ->
    {ok, CryptoWallet};
resource_full(Other, _) ->
    error({resource_full, {not_impl, Other}}).

-spec create_disposable_resource(resource()) ->
    {ok, disposable_resource()} |
    {error, {bin_data, not_found}}.

create_disposable_resource({bank_card, _} = Resource) ->
    do(fun() ->
        ResourceFull = unwrap(resource_full(Resource)),
        #{
            tool => ResourceFull
        }
    end);
create_disposable_resource(Resource) ->
    error({create_disposable_resource, {not_impl, Resource}}).
