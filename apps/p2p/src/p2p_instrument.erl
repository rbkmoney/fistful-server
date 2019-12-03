-module(p2p_instrument).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type bank_card_details() :: #{
    token               := binary(),
    payment_system      := atom(),
    bin                 := binary(),
    masked_pan          := binary(),
    bank_name           => binary(),
    iso_country_code    => atom(),
    card_type           => charge_card | credit | debit | credit_or_debit,
    bin_data_id         => ff_bin_data:bin_data_id()
}.

-type bank_card() :: #{
    token          := binary(),
    bin            => binary(),
    masked_pan     => binary()
}.

-type instrument_type() :: bank_card.

-opaque instrument() :: {bank_card, bank_card_details()}.

-export_type([instrument/0]).

-export([create/1]).
-export([construct_payment_tool/1]).
-export([token/1]).
-export([country_code/1]).
-export([bin_data_id/1]).
%% Pipeline

-import(ff_pipeline, [do/1, unwrap/2]).

%% Accessories

-spec token(instrument()) -> binary().

token({bank_card, #{token := Token}}) ->
    Token.

-spec bin(instrument()) -> binary().

bin({bank_card, #{bin := Bin}}) ->
    Bin.

-spec masked_pan(instrument()) -> binary().

masked_pan({bank_card, #{masked_pan := MaskedPan}}) ->
    MaskedPan.

-spec payment_system(instrument()) -> atom().

payment_system({bank_card, #{payment_system := PaymentSystem}}) ->
    PaymentSystem.

-spec country_code(instrument()) -> atom() | undefined.

country_code({bank_card, BankCardDetails}) ->
    maps:get(iso_country_code, BankCardDetails, undefined).

-spec bank_name(instrument()) -> binary() | undefined.

bank_name({bank_card, BankCardDetails}) ->
    maps:get(bank_card, BankCardDetails, undefined).

-spec bin_data_id(instrument()) -> binary() | undefined.

bin_data_id({bank_card, BankCardDetails}) ->
    maps:get(bin_data_id, BankCardDetails, undefined).
%% API

-spec create({instrument_type(), bank_card()}) ->
    {ok, instrument()} |
    {error, {bin_data, not_found}}.

create({bank_card, #{token := Token, bin := Bin, masked_pan := MaskedPan}}) ->
    do(fun() ->
        BinData = unwrap(bin_data, ff_bin_data:get(Token, undefined)),
        KeyList = [payment_system, bank_name, iso_country_code, card_type],
        ExtendData = maps:with(KeyList, BinData),
        BankCardDetails = ExtendData#{
            token       => Token,
            bin         => Bin,
            masked_pan  => MaskedPan,
            bin_data_id => ff_bin_data:id(BinData)
        },
        {bank_card, BankCardDetails}
    end);
create(_UnknownInstrumentType) ->
    not_impl.


-spec construct_payment_tool(instrument()) ->
    dmsl_domain_thrift:'PaymentTool'().

construct_payment_tool({bank_card, _} = ResourceBankCard) ->
    {bank_card, #domain_BankCard{
        token           = token(ResourceBankCard),
        bin             = bin(ResourceBankCard),
        masked_pan      = masked_pan(ResourceBankCard),
        payment_system  = payment_system(ResourceBankCard),
        issuer_country  = country_code(ResourceBankCard),
        bank_name       = bank_name(ResourceBankCard)
    }};
construct_payment_tool(_UnknownInstrument) ->
    not_impl.
