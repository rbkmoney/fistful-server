-module(ff_adapter_withdrawal_codec).

-include_lib("damsel/include/dmsl_withdrawals_provider_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(ff_codec).

-export([marshal/2]).
-export([unmarshal/2]).

%% @TODO: Not symmetrical right now

%%

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal(adapter_state, undefined) ->
    {nl, #msgpack_Nil{}};
marshal(adapter_state, ASt) ->
    marshal_msgpack(ASt);

marshal(body, {Amount, CurrencyID}) ->
    {ok, Currency} = ff_currency:get(CurrencyID),
    DomainCurrency = marshal(currency, Currency),
    #wthadpt_Cash{amount = Amount, currency = DomainCurrency};

marshal(callback, #{
    tag := Tag,
    payload := Payload
}) ->
    #wthadpt_Callback{
        tag     = Tag,
        payload = Payload
    };

marshal(callback_result, #{
    intent     := Intent,
    response   := Response,
    next_state := NextState
})->
    #wthadpt_CallbackResult{
        intent     = marshal(intent, Intent),
        response   = marshal(callback_response, Response),
        next_state = marshal(adapter_state, NextState)
    };

marshal(callback_response, #{payload := Payload}) ->
    #wthadpt_CallbackResponse{payload = Payload};

marshal(currency, #{
    name := Name,
    symcode := Symcode,
    numcode := Numcode,
    exponent := Exponent
}) ->
    #domain_Currency{
        name = Name,
        symbolic_code = Symcode,
        numeric_code = Numcode,
        exponent = Exponent
    };

marshal(challenge_documents, Challenge) ->
    lists:foldl(fun try_encode_proof_document/2, [], maps:get(proofs, Challenge, []));

marshal(exp_date, {Month, Year}) ->
    #domain_BankCardExpDate{
        month = Month,
        year = Year
    };

marshal(identity, Identity) ->
    % TODO: Add real contact fields
    #wthdm_Identity{
        id        = maps:get(id, Identity),
        documents = marshal(identity_documents, Identity),
        contact   = [{phone_number, <<"9876543210">>}]
    };

marshal(identity_documents, Identity) ->
    case maps:get(effective_challenge, Identity, undefined) of
        undefined ->
            [];
        Challenge ->
            marshal(challenge_documents, Challenge)
    end;

marshal(intent, {finish, {success, TrxInfo}}) ->
    {finish, #wthadpt_FinishIntent{
        status = {success, #wthadpt_Success{
            trx_info = ff_dmsl_codec:marshal(transaction_info, TrxInfo)
        }}
    }};
marshal(intent, {finish, {failed, Failure}}) ->
    {finish, #wthadpt_FinishIntent{
        status = {failure, ff_dmsl_codec:marshal(failure, Failure)}
    }};
marshal(intent, {sleep, #{timer := Timer, tag := Tag}}) ->
    {sleep, #wthadpt_SleepIntent{timer = Timer, callback_tag = Tag}};

marshal(process_callback_result, {succeeded, CallbackResponse}) ->
    {succeeded, #wthadpt_ProcessCallbackSucceeded{
        response = marshal(callback_response, CallbackResponse)
    }};
marshal(process_callback_result, {finished, #{
    withdrawal := Withdrawal,
    state := AdapterState,
    opts := Options
}}) ->
    {finished, #wthadpt_ProcessCallbackFinished{
        withdrawal = marshal(withdrawal, Withdrawal),
        state = marshal(adapter_state, AdapterState),
        opts = Options
    }};

marshal(quote_params, #{
    currency_from := CurrencyIDFrom,
    currency_to := CurrencyIDTo,
    body := Body
} = Params) ->
    ExternalID = maps:get(external_id, Params, undefined),
    {ok, CurrencyFrom} = ff_currency:get(CurrencyIDFrom),
    {ok, CurrencyTo} = ff_currency:get(CurrencyIDTo),
    #wthadpt_GetQuoteParams{
        idempotency_id = ExternalID,
        currency_from = marshal(currency, CurrencyFrom),
        currency_to = marshal(currency, CurrencyTo),
        exchange_cash = marshal(body, Body)
    };

marshal(quote, #{
    cash_from  := CashFrom,
    cash_to    := CashTo,
    created_at := CreatedAt,
    expires_on := ExpiresOn,
    quote_data := QuoteData
}) ->
    #wthadpt_Quote{
        cash_from  = marshal(body, CashFrom),
        cash_to    = marshal(body, CashTo),
        created_at = CreatedAt,
        expires_on = ExpiresOn,
        quote_data = marshal_msgpack(QuoteData)
    };

marshal(resource,
    {bank_card, #{bank_card := #{
        token          := Token,
        payment_system := PaymentSystem,
        bin            := BIN,
        masked_pan     := LastDigits
    } = BankCard}}
) ->
    CardHolderName = genlib_map:get(cardholder_name, BankCard),
    ExpDate = genlib_map:get(exp_date, BankCard),
    {bank_card, #domain_BankCard{
        token           = Token,
        payment_system  = PaymentSystem,
        bin             = BIN,
        last_digits     = LastDigits,
        cardholder_name = CardHolderName,
        exp_date        = maybe_marshal(exp_date, ExpDate)
    }};

marshal(resource,
    {crypto_wallet, #{crypto_wallet := #{
        id       := CryptoWalletID,
        currency := {Currency, Data}
    }}}
) ->
    {crypto_wallet, #domain_CryptoWallet{
        id              = CryptoWalletID,
        crypto_currency = Currency,
        destination_tag = maps:get(tag, Data, undefined)
    }};

marshal(withdrawal, #{
    id := ID,
    cash := Cash,
    resource := Resource,
    sender := Sender,
    receiver := Receiver
} = Withdrawal) ->
    SesID = maps:get(session_id, Withdrawal, undefined),
    #wthadpt_Withdrawal{
        id = ID,
        session_id = SesID,
        body = marshal(body, Cash),
        destination = marshal(resource, Resource),
        sender = maybe_marshal(identity, Sender),
        receiver = maybe_marshal(identity, Receiver),
        quote = maybe_marshal(quote, maps:get(quote, Withdrawal, undefined))
    };

marshal(T, V) ->
    ff_codec:marshal(T, V).

try_encode_proof_document({rus_domestic_passport, Token}, Acc) ->
    [{rus_domestic_passport, #wthdm_RUSDomesticPassport{token = Token}} | Acc];
try_encode_proof_document(_, Acc) ->
    Acc.

%%

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal(adapter_state, ASt) ->
    unmarshal_msgpack(ASt);

unmarshal(body, #wthadpt_Cash{
    amount = Amount,
    currency = DomainCurrency
}) ->
    CurrencyID = ff_currency:id(unmarshal(currency, DomainCurrency)),
    {Amount, CurrencyID};

unmarshal(callback, #wthadpt_Callback{
    tag     = Tag,
    payload = Payload
}) ->
    #{tag => Tag, payload => Payload};

unmarshal(callback_result, #wthadpt_CallbackResult{
    intent     = Intent,
    next_state = NextState,
    response   = Response
}) ->
    genlib_map:compact(#{
        intent           => unmarshal(intent, Intent),
        response         => unmarshal(callback_response, Response),
        next_state       => unmarshal(adapter_state, NextState)
    });

unmarshal(callback_response, #wthadpt_CallbackResponse{payload = Payload}) ->
    #{payload => Payload};

unmarshal(currency, #domain_Currency{
    name = Name,
    symbolic_code = Symcode,
    numeric_code = Numcode,
    exponent = Exponent
}) ->
    #{
        id => Symcode,
        name => Name,
        symcode => Symcode,
        numcode => Numcode,
        exponent => Exponent
    };

unmarshal(challenge_documents, _NotImplemented) ->
    erlang:error(not_implemented); %@TODO

unmarshal(exp_date, #domain_BankCardExpDate{
    month = Month,
    year = Year
}) ->
    {Month, Year};

unmarshal(identity, _NotImplemented) ->
    erlang:error(not_implemented); %@TODO

unmarshal(identity_documents, _NotImplemented) ->
    erlang:error(not_implemented); %@TODO

unmarshal(intent, {finish, #wthadpt_FinishIntent{status = {success, #wthadpt_Success{trx_info = TrxInfo}}}}) ->
    {finish, {success, ff_dmsl_codec:unmarshal(transaction_info, TrxInfo)}};
unmarshal(intent, {finish, #wthadpt_FinishIntent{status = {failure, Failure}}}) ->
    {finish, {failed, ff_dmsl_codec:unmarshal(failure, Failure)}};
unmarshal(intent, {sleep, #wthadpt_SleepIntent{timer = Timer, callback_tag = Tag}}) ->
    {sleep, genlib_map:compact(#{timer => Timer, tag => Tag})};

unmarshal(process_callback_result, _NotImplemented) ->
    erlang:error(not_implemented); %@TODO

unmarshal(quote_params, _NotImplemented) ->
    erlang:error(not_implemented); %@TODO

unmarshal(quote, #wthadpt_Quote{
    cash_from = CashFrom,
    cash_to = CashTo,
    created_at = CreatedAt,
    expires_on = ExpiresOn,
    quote_data = QuoteData
}) ->
    #{
        cash_from => unmarshal(body, CashFrom),
        cash_to => unmarshal(body, CashTo),
        created_at => CreatedAt,
        expires_on => ExpiresOn,
        quote_data => unmarshal_msgpack(QuoteData)
    };

unmarshal(resource, _NotImplemented) ->
    erlang:error(not_implemented); %@TODO

unmarshal(withdrawal, _NotImplemented) ->
    erlang:error(not_implemented); %@TODO

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%%

maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).

marshal_msgpack(nil)                  -> {nl, #msgpack_Nil{}};
marshal_msgpack(V) when is_boolean(V) -> {b, V};
marshal_msgpack(V) when is_integer(V) -> {i, V};
marshal_msgpack(V) when is_float(V)   -> V;
marshal_msgpack(V) when is_binary(V)  -> {str, V}; % Assuming well-formed UTF-8 bytestring.
marshal_msgpack({binary, V}) when is_binary(V) ->
    {bin, V};
marshal_msgpack(V) when is_list(V) ->
    {arr, [marshal_msgpack(ListItem) || ListItem <- V]};
marshal_msgpack(V) when is_map(V) ->
    {obj, maps:fold(fun(Key, Value, Map) -> Map#{marshal_msgpack(Key) => marshal_msgpack(Value)} end, #{}, V)}.

unmarshal_msgpack({nl, #msgpack_Nil{}})        -> nil;
unmarshal_msgpack({b,   V}) when is_boolean(V) -> V;
unmarshal_msgpack({i,   V}) when is_integer(V) -> V;
unmarshal_msgpack({flt, V}) when is_float(V)   -> V;
unmarshal_msgpack({str, V}) when is_binary(V)  -> V; % Assuming well-formed UTF-8 bytestring.
unmarshal_msgpack({bin, V}) when is_binary(V)  -> {binary, V};
unmarshal_msgpack({arr, V}) when is_list(V)    -> [unmarshal_msgpack(ListItem) || ListItem <- V];
unmarshal_msgpack({obj, V}) when is_map(V)     ->
    maps:fold(fun(Key, Value, Map) -> Map#{unmarshal_msgpack(Key) => unmarshal_msgpack(Value)} end, #{}, V).
