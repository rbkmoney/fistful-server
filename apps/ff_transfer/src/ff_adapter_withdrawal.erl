%%% Client for adapter for withdrawal provider
-module(ff_adapter_withdrawal).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

%% API

-export([process_withdrawal/4]).
-export([get_quote/3]).

%%
%% Internal types
%%

-type id()          :: machinery:id().
-type identity_id() :: id().

-type resource()    :: ff_destination:resource_full().
-type identity()    :: ff_identity:identity().
-type cash()        :: ff_transaction:body().
-type exp_date()    :: ff_destination:exp_date().

-type withdrawal() :: #{
    id          => binary(),
    resource    => resource(),
    cash        => cash(),
    sender      => identity() | undefined,
    receiver    => identity() | undefined,
    quote       => quote()
}.

-type quote_params() :: #{
    external_id => binary(),
    currency_from := ff_currency:id(),
    currency_to := ff_currency:id(),
    body := cash()
}.

-type quote() :: quote(quote_data()).

-type quote(T) :: #{
    cash_from   := cash(),
    cash_to     := cash(),
    created_at  := binary(),
    expires_on  := binary(),
    quote_data  := T
}.

-type quote_data()     :: %% as stolen from `machinery_msgpack`
    nil                |
    boolean()          |
    integer()          |
    float()            |
    binary()           | %% string
    {binary, binary()} | %% binary
    [quote_data()]     |
    #{quote_data() => quote_data()}.

-type adapter()               :: ff_adapter:adapter().
-type intent()                :: {finish, status()} | {sleep, timer()}.
-type status()                :: {success, transaction_info()} | {failure, failure()}.
-type timer()                 :: dmsl_base_thrift:'Timer'().
-type transaction_info()      :: ff_adapter:transaction_info().
-type failure()               :: ff_adapter:failure().

-type adapter_state()         :: ff_adapter:state().
-type process_result()        ::
    {ok, intent(), adapter_state()} |
    {ok, intent()}.

-type domain_withdrawal()     :: dmsl_withdrawals_provider_adapter_thrift:'Withdrawal'().
-type domain_cash()           :: dmsl_withdrawals_provider_adapter_thrift:'Cash'().
-type domain_currency()       :: dmsl_domain_thrift:'Currency'().
-type domain_destination()    :: dmsl_withdrawals_provider_adapter_thrift:'Destination'().
-type domain_identity()       :: dmsl_withdrawals_provider_adapter_thrift:'Identity'().
-type domain_internal_state() :: dmsl_withdrawals_provider_adapter_thrift:'InternalState'().
-type domain_exp_date()       :: dmsl_domain_thrift:'BankCardExpDate'().

-type domain_quote_params()  :: dmsl_withdrawals_provider_adapter_thrift:'GetQuoteParams'().

-export_type([withdrawal/0]).
-export_type([failure/0]).
-export_type([transaction_info/0]).
-export_type([quote/0]).
-export_type([quote/1]).
-export_type([quote_params/0]).
-export_type([quote_data/0]).

%%
%% API
%%

-spec process_withdrawal(Adapter, Withdrawal, ASt, AOpt) ->
    process_result() when
        Adapter :: adapter(),
        Withdrawal :: withdrawal(),
        ASt :: adapter_state(),
        AOpt :: map().

process_withdrawal(Adapter, Withdrawal, ASt, AOpt) ->
    DomainWithdrawal = encode_withdrawal(Withdrawal),
    {ok, Result} = call(Adapter, 'ProcessWithdrawal', [DomainWithdrawal, encode_adapter_state(ASt), AOpt]),
    decode_result(Result).

-spec get_quote(adapter(), quote_params(), map()) ->
    {ok, quote()}.

get_quote(Adapter, Params, AOpt) ->
    QuoteParams = encode_quote_params(Params),
    {ok, Result} = call(Adapter, 'GetQuote', [QuoteParams, AOpt]),
    decode_result(Result).

%%
%% Internals
%%

call(Adapter, Function, Args) ->
    Request = {{dmsl_withdrawals_provider_adapter_thrift, 'Adapter'}, Function, Args},
    ff_woody_client:call(Adapter, Request).

%% Encoders

-spec encode_quote_params(Params) -> domain_quote_params() when
    Params :: quote_params().
encode_quote_params(Params) ->
    #{
        currency_from := CurrencyIDFrom,
        currency_to := CurrencyIDTo,
        body := Body
    } = Params,
    ExternalID = maps:get(external_id, Params, undefined),
    {ok, CurrencyFrom} = ff_currency:get(CurrencyIDFrom),
    {ok, CurrencyTo} = ff_currency:get(CurrencyIDTo),
    #wthadpt_GetQuoteParams{
        idempotency_id = ExternalID,
        currency_from = encode_currency(CurrencyFrom),
        currency_to = encode_currency(CurrencyTo),
        exchange_cash = encode_body(Body)
    }.

-spec encode_withdrawal(Withdrawal) -> domain_withdrawal() when
    Withdrawal :: withdrawal().
encode_withdrawal(Withdrawal) ->
    #{
        id := ID,
        cash := Cash,
        resource := Resource,
        sender := Sender,
        receiver := Receiver
    } = Withdrawal,
    #wthadpt_Withdrawal{
        id = ID,
        body = encode_body(Cash),
        destination = encode_resource(Resource),
        sender = encode_identity(Sender),
        receiver = encode_identity(Receiver),
        quote = encode_quote(maps:get(quote, Withdrawal, undefined))
    }.

-spec encode_quote(quote() | undefined) -> domain_withdrawal() | undefined.
encode_quote(undefined) ->
    undefined;
encode_quote(Quote) ->
    #{
        cash_from  := CashFrom,
        cash_to    := CashTo,
        created_at := CreatedAt,
        expires_on := ExpiresOn,
        quote_data := QuoteData
    } = Quote,
    #wthadpt_Quote{
        cash_from  = encode_body(CashFrom),
        cash_to    = encode_body(CashTo),
        created_at = CreatedAt,
        expires_on = ExpiresOn,
        quote_data = encode_msgpack(QuoteData)
    }.

-spec encode_body(cash()) -> domain_cash().
encode_body({Amount, CurrencyID}) ->
    {ok, Currency} = ff_currency:get(CurrencyID),
    DomainCurrency = encode_currency(Currency),
    #wthadpt_Cash{amount = Amount, currency = DomainCurrency}.

-spec encode_currency(ff_currency:currency()) -> domain_currency().
encode_currency(#{
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
    }.

-spec encode_resource(resource()) -> domain_destination().
encode_resource(
    {bank_card, #{
        token          := Token,
        payment_system := PaymentSystem,
        bin            := BIN,
        masked_pan     := MaskedPan
    } = BankCard}
) ->
    CardHolderName = genlib_map:get(cardholder_name, BankCard),
    ExpDate = genlib_map:get(exp_date, BankCard),
    {bank_card, #domain_BankCard{
        token           = Token,
        payment_system  = PaymentSystem,
        bin             = BIN,
        masked_pan      = MaskedPan,
        cardholder_name = CardHolderName,
        exp_date        = encode_exp_date(ExpDate)
    }};
encode_resource(
    {crypto_wallet, #{
        id       := CryptoWalletID,
        currency := {Currency, Data}
    }}
) ->
    {crypto_wallet, #domain_CryptoWallet{
        id              = CryptoWalletID,
        crypto_currency = Currency,
        destination_tag = maps:get(tag, Data, undefined)
    }}.

-spec encode_exp_date
    (exp_date()) -> domain_exp_date();
    (undefined) -> undefined.
encode_exp_date(undefined) ->
    undefined;
encode_exp_date({Month, Year}) ->
    #domain_BankCardExpDate{
        month = Month,
        year = Year
    }.

-spec encode_identity
    (identity_id()) -> domain_identity();
    (undefined) -> undefined.
encode_identity(undefined) ->
    undefined;
encode_identity(Identity) ->
    % TODO: Add real contact fields
    #wthdm_Identity{
        id        = ff_identity:id(Identity),
        documents = encode_identity_documents(Identity),
        contact   = [{phone_number, <<"9876543210">>}]
    }.

encode_identity_documents(Identity) ->
    case ff_identity:effective_challenge(Identity) of
        {ok, ChallengeID} ->
            {ok, Challenge} = ff_identity:challenge(ChallengeID, Identity),
            encode_challenge_documents(Challenge);
        {error, notfound} ->
            []
    end.

encode_challenge_documents(Challenge) ->
    lists:foldl(fun try_encode_proof_document/2, [], ff_identity_challenge:proofs(Challenge)).

try_encode_proof_document({rus_domestic_passport, Token}, Acc) ->
    [{rus_domestic_passport, #wthdm_RUSDomesticPassport{token = Token}} | Acc];
try_encode_proof_document(_, Acc) ->
    Acc.

-spec encode_adapter_state(adapter_state()) -> domain_internal_state().
encode_adapter_state(undefined) ->
    {nl, #msgpack_Nil{}};
encode_adapter_state(ASt) ->
    encode_msgpack(ASt).

encode_msgpack(nil)                  -> {nl, #msgpack_Nil{}};
encode_msgpack(V) when is_boolean(V) -> {b, V};
encode_msgpack(V) when is_integer(V) -> {i, V};
encode_msgpack(V) when is_float(V)   -> V;
encode_msgpack(V) when is_binary(V)  -> {str, V}; % Assuming well-formed UTF-8 bytestring.
encode_msgpack({binary, V}) when is_binary(V) ->
    {bin, V};
encode_msgpack(V) when is_list(V) ->
    {arr, [encode_msgpack(ListItem) || ListItem <- V]};
encode_msgpack(V) when is_map(V) ->
    {obj, maps:fold(fun(Key, Value, Map) -> Map#{encode_msgpack(Key) => encode_msgpack(Value)} end, #{}, V)}.

%%

-spec decode_result
    (dmsl_withdrawals_provider_adapter_thrift:'ProcessResult'()) -> process_result();
    (dmsl_withdrawals_provider_adapter_thrift:'Quote'()) -> {ok, quote()}.

decode_result(#wthadpt_ProcessResult{intent = Intent, next_state = undefined}) ->
    {ok, decode_intent(Intent)};
decode_result(#wthadpt_ProcessResult{intent = Intent, next_state = NextState}) ->
    {ok, decode_intent(Intent), decode_adapter_state(NextState)};
decode_result(#wthadpt_Quote{} = Quote) ->
    {ok, decode_quote(Quote)}.

%% Decoders

-spec decode_adapter_state(domain_internal_state()) -> adapter_state().
decode_adapter_state(ASt) ->
    decode_msgpack(ASt).

-spec decode_intent(dmsl_withdrawals_provider_adapter_thrift:'Intent'()) -> intent().
decode_intent({finish, #wthadpt_FinishIntent{status = {success, #wthadpt_Success{trx_info = TrxInfo}}}}) ->
    {finish, {success, ff_dmsl_codec:unmarshal(transaction_info, TrxInfo)}};
decode_intent({finish, #wthadpt_FinishIntent{status = {failure, Failure}}}) ->
    {finish, {failed, ff_dmsl_codec:unmarshal(failure, Failure)}};
decode_intent({sleep, #wthadpt_SleepIntent{timer = Timer}}) ->
    {sleep, Timer}.

decode_quote(#wthadpt_Quote{
    cash_from = CashFrom,
    cash_to = CashTo,
    created_at = CreatedAt,
    expires_on = ExpiresOn,
    quote_data = QuoteData
}) ->
    #{
        cash_from => decode_body(CashFrom),
        cash_to => decode_body(CashTo),
        created_at => CreatedAt,
        expires_on => ExpiresOn,
        quote_data => decode_msgpack(QuoteData)
    }.

-spec decode_body(domain_cash()) -> cash().
decode_body(#wthadpt_Cash{
    amount = Amount,
    currency = DomainCurrency
}) ->
    CurrencyID = ff_currency:id(decode_currency(DomainCurrency)),
    {Amount, CurrencyID}.

-spec decode_currency(domain_currency()) -> ff_currency:currency().
decode_currency(#domain_Currency{
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
    }.

decode_msgpack({nl, #msgpack_Nil{}})        -> nil;
decode_msgpack({b,   V}) when is_boolean(V) -> V;
decode_msgpack({i,   V}) when is_integer(V) -> V;
decode_msgpack({flt, V}) when is_float(V)   -> V;
decode_msgpack({str, V}) when is_binary(V)  -> V; % Assuming well-formed UTF-8 bytestring.
decode_msgpack({bin, V}) when is_binary(V)  -> {binary, V};
decode_msgpack({arr, V}) when is_list(V)    -> [decode_msgpack(ListItem) || ListItem <- V];
decode_msgpack({obj, V}) when is_map(V)     ->
    maps:fold(fun(Key, Value, Map) -> Map#{decode_msgpack(Key) => decode_msgpack(Value)} end, #{}, V).
