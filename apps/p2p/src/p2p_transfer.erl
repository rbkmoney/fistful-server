-module(p2p_transfer).

-type token_error() :: {bank_card_type, _}
                    |  {bank_card_country, _}.

-export([
    valid_instruments/2
]).


%% Pipeline

-import(ff_pipeline, [do/1, unwrap/2, valid/2]).

-spec valid_instruments(any(), any()) ->
    ok |
    {error,
        {sender_instrument, token_error()} |
        {receiver_instrument, token_error()}
    }.

valid_instruments(Sender, Receiver) ->
    do(fun() ->
        ok = unwrap(sender_instrument, valid_token(Sender)),
        ok = unwrap(receiver_instrument, valid_token(Receiver))
    end).

%% Validation

valid_token(Instrument) ->
    do(fun() ->
        ok = unwrap(bank_card_type, valid(bank_card, p2p_instrument:type(Instrument))),
        ok = unwrap(bank_card_country, valid(rus, p2p_instrument:country_code(Instrument)))
    end).
