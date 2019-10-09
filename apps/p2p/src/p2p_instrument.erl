-module(p2p_instrument).

-type instrument_type() :: {bank_card, bank_card()}.
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

-opaque bank_card() :: #{
    token      := binary(),
    bin        => binary(),
    masked_pan => binary()
}.

-opaque instrument() :: #{
    type    := instrument_type(),
    token   := binary(),
    details := bank_card_details() | undefined
}.

-export_type([bank_card/0]).
-export_type([instrument/0]).

-export([create/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/2]).

%% API

-spec create(instrument_type()) ->
    {ok, instrument()} |
    {error, {bin_data, not_found}}.

create({bank_card, #{token := Token, bin := Bin, masked_pan := MaskedPan}}) ->
    do(fun() ->
        BinData = unwrap(bin_data, ff_bin_data:get(Token, undefined)),
        KeyList = [payment_system, bank_name, iso_country_code, card_type],
        ExtendData = maps:with(KeyList, BinData),
        BankCardDetails = ExtendData#{
            bin         => Bin,
            masked_pan  => MaskedPan,
            bin_data_id => ff_bin_data:id(BinData)
        },
        {ok, #{
            type    => bank_card,
            token   => Token,
            details => BankCardDetails
        }}
    end);
create(_UnknownInstrumentType) ->
    not_impl.
