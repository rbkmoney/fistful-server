-module(ff_codec).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_repairer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_account_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_msgpack_thrift.hrl").

-export([unmarshal/2]).
-export([unmarshal/3]).

-export([marshal/2]).
-export([marshal/3]).

%% Types

-type type_name() :: atom() | {list, atom()}.
-type codec() :: module().

-type encoded_value() :: encoded_value(any()).
-type encoded_value(T) :: T.

-type decoded_value() :: decoded_value(any()).
-type decoded_value(T) :: T.

-export_type([codec/0]).
-export_type([type_name/0]).
-export_type([encoded_value/0]).
-export_type([encoded_value/1]).
-export_type([decoded_value/0]).
-export_type([decoded_value/1]).

%% Callbacks

-callback unmarshal(type_name(), encoded_value()) ->
    decoded_value().
-callback marshal(type_name(), decoded_value()) ->
    encoded_value().

%% API

-spec unmarshal(codec(), type_name(), encoded_value()) ->
    decoded_value().
unmarshal(Codec, Type, Value) ->
    Codec:unmarshal(Type, Value).

-spec marshal(codec(), type_name(), decoded_value()) ->
    encoded_value().
marshal(Codec, Type, Value) ->
    Codec:marshal(Type, Value).

%% Generic codec

-spec marshal(type_name(), decoded_value()) ->
    encoded_value().
marshal(id, V) ->
    marshal(string, V);
marshal(event_id, V) ->
    marshal(integer, V);

marshal(blocked, V) ->
    marshal(bool, V);

marshal(account_change, {created, Account}) ->
    {created, marshal(account, Account)};
marshal(account, #{
    id                   := ID,
    identity             := IdentityID,
    currency             := CurrencyID,
    accounter_account_id := AAID
}) ->
    #'account_Account'{
        id = marshal(id, ID),
        identity = marshal(id, IdentityID),
        currency = marshal(currency_ref, CurrencyID),
        accounter_account_id = marshal(event_id, AAID)
    };

marshal(resource, {bank_card, BankCard = #{token := Token}}) ->
    Bin = maps:get(bin, BankCard, undefined),
    PaymentSystem = maps:get(payment_system, BankCard, undefined),
    MaskedPan = maps:get(masked_pan, BankCard, undefined),
    BankName = maps:get(bank_name, BankCard, undefined),
    IsoCountryCode = maps:get(iso_country_code, BankCard, undefined),
    CardType = maps:get(card_type, BankCard, undefined),
    {bank_card, #'BankCard'{
        token = marshal(string, Token),
        bin = marshal(string, Bin),
        masked_pan = marshal(string, MaskedPan),
        bank_name = marshal(string, BankName),
        payment_system = PaymentSystem,
        issuer_country = IsoCountryCode,
        card_type = CardType
    }};
marshal(resource, {crypto_wallet, CryptoWallet = #{id := ID, currency := Currency}}) ->
    {crypto_wallet, #'CryptoWallet'{
        id       = marshal(string, ID),
        currency = Currency,
        data = marshal(crypto_data, CryptoWallet)
    }};

marshal(crypto_data, #{
    currency := bitcoin
}) ->
    {bitcoin, #'CryptoDataBitcoin'{}};
marshal(crypto_data, #{
    currency := litecoin
}) ->
    {litecoin, #'CryptoDataLitecoin'{}};
marshal(crypto_data, #{
    currency := bitcoin_cash
}) ->
    {bitcoin_cash, #'CryptoDataBitcoinCash'{}};
marshal(crypto_data, #{
    currency := ripple
} = Data) ->
    {ripple, #'CryptoDataRipple'{
        tag = maybe_marshal(string, maps:get(tag, Data, undefined))
    }};
marshal(crypto_data, #{
    currency := ethereum
}) ->
    {ethereum, #'CryptoDataEthereum'{}};
marshal(crypto_data, #{
    currency := zcash
}) ->
    {zcash, #'CryptoDataZcash'{}};

marshal(cash, {Amount, CurrencyRef}) ->
    #'Cash'{
        amount   = marshal(amount, Amount),
        currency = marshal(currency_ref, CurrencyRef)
    };
marshal(cash_range, {{BoundLower, CashLower}, {BoundUpper, CashUpper}}) ->
    #'CashRange'{
        lower = {BoundLower, marshal(cash, CashLower)},
        upper = {BoundUpper, marshal(cash, CashUpper)}
    };
marshal(currency_ref, CurrencyID) when is_binary(CurrencyID) ->
    #'CurrencyRef'{
        symbolic_code = CurrencyID
    };
marshal(amount, V) ->
    marshal(integer, V);

marshal(failure, Failure) ->
    #'Failure'{
        code = marshal(string, ff_failure:code(Failure)),
        reason = maybe_marshal(string, ff_failure:reason(Failure)),
        sub = maybe_marshal(sub_failure, ff_failure:sub_failure(Failure))
    };
marshal(sub_failure, Failure) ->
    #'SubFailure'{
        code = marshal(string, ff_failure:code(Failure)),
        sub = maybe_marshal(sub_failure, ff_failure:sub_failure(Failure))
    };

marshal(timestamp, {{Date, Time}, USec} = V) ->
    case rfc3339:format({Date, Time, USec, 0}) of
        {ok, R} when is_binary(R) ->
            R;
        Error ->
            error({bad_timestamp, Error}, [timestamp, V])
    end;
marshal(string, V) when is_binary(V) ->
    V;
marshal(integer, V) when is_integer(V) ->
    V;
marshal(bool, V) when is_boolean(V) ->
    V;
marshal(context, V) when is_map(V) ->
    ff_entity_context_codec:marshal(V);

% Catch this up in thrift validation
marshal(_, Other) ->
    Other.

-spec unmarshal(type_name(), encoded_value()) ->
    decoded_value().
unmarshal(id, V) ->
    unmarshal(string, V);
unmarshal(event_id, V) ->
    unmarshal(integer, V);

unmarshal(complex_action, #ff_repairer_ComplexAction{
    timer = TimerAction,
    remove = RemoveAction
}) ->
    unmarshal(timer_action, TimerAction) ++ unmarshal(remove_action, RemoveAction);
unmarshal(timer_action, undefined) ->
    [];
unmarshal(timer_action, {set_timer, SetTimer}) ->
    [{set_timer, unmarshal(set_timer_action, SetTimer)}];
unmarshal(timer_action, {unset_timer, #ff_repairer_UnsetTimerAction{}}) ->
    [unset_timer];
unmarshal(remove_action, undefined) ->
    [];
unmarshal(remove_action, #ff_repairer_RemoveAction{}) ->
    [remove];

unmarshal(set_timer_action, {timeout, Timeout}) ->
    {timeout, unmarshal(integer, Timeout)};
unmarshal(set_timer_action, {deadline, Deadline}) ->
    {deadline, unmarshal(timestamp, Deadline)};

unmarshal(account_change, {created, Account}) ->
    {created, unmarshal(account, Account)};
unmarshal(account, #'account_Account'{
    id = ID,
    identity = IdentityID,
    currency = CurrencyRef,
    accounter_account_id = AAID
}) ->
    #{
        id => unmarshal(id, ID),
        identity => unmarshal(id, IdentityID),
        currency => unmarshal(currency_ref, CurrencyRef),
        accounter_account_id => unmarshal(accounter_account_id, AAID)
    };
unmarshal(accounter_account_id, V) ->
    unmarshal(integer, V);

unmarshal(resource, {bank_card, BankCard}) ->
    {bank_card, unmarshal(bank_card, BankCard)};
unmarshal(resource, {crypto_wallet, CryptoWallet}) ->
    {crypto_wallet, unmarshal(crypto_wallet, CryptoWallet)};

unmarshal(bank_card, #'BankCard'{
    token = Token,
    bin = Bin,
    masked_pan = MaskedPan,
    bank_name = BankName,
    payment_system = PaymentSystem,
    issuer_country = IsoCountryCode,
    card_type = CardType
}) ->
    genlib_map:compact(#{
        token => unmarshal(string, Token),
        payment_system => maybe_unmarshal(payment_system, PaymentSystem),
        bin => maybe_unmarshal(string, Bin),
        masked_pan => maybe_unmarshal(string, MaskedPan),
        bank_name => maybe_unmarshal(string, BankName),
        issuer_country => maybe_unmarshal(iso_country_code, IsoCountryCode),
        card_type => maybe_unmarshal(card_type, CardType)
    });

unmarshal(payment_system, V) when is_atom(V) ->
    V;

unmarshal(iso_country_code, V) when is_atom(V) ->
    V;

unmarshal(card_type, V) when is_atom(V) ->
    V;

unmarshal(crypto_wallet, #'CryptoWallet'{
    id = CryptoWalletID,
    currency = CryptoWalletCurrency,
    data = Data
}) ->
    genlib_map:compact(#{
        id => unmarshal(string, CryptoWalletID),
        currency => CryptoWalletCurrency,
        tag => maybe_unmarshal(crypto_data, Data)
    });

unmarshal(crypto_data, {ripple, #'CryptoDataRipple'{tag = Tag}}) ->
    maybe_unmarshal(string, Tag);
unmarshal(crypto_data, _) ->
    undefined;

unmarshal(cash, #'Cash'{
    amount   = Amount,
    currency = CurrencyRef
}) ->
    {unmarshal(amount, Amount), unmarshal(currency_ref, CurrencyRef)};

unmarshal(cash_range, #'CashRange'{
    lower = {BoundLower, CashLower},
    upper = {BoundUpper, CashUpper}
}) ->
    {
        {BoundLower, unmarshal(cash, CashLower)},
        {BoundUpper, unmarshal(cash, CashUpper)}
    };

unmarshal(currency_ref, #'CurrencyRef'{
    symbolic_code = SymbolicCode
}) ->
    unmarshal(string, SymbolicCode);
unmarshal(amount, V) ->
    unmarshal(integer, V);

unmarshal(failure, Failure) ->
    genlib_map:compact(#{
        code => unmarshal(string, Failure#'Failure'.code),
        reason => maybe_unmarshal(string, Failure#'Failure'.reason),
        sub => maybe_unmarshal(sub_failure, Failure#'Failure'.sub)
    });
unmarshal(sub_failure, Failure) ->
    genlib_map:compact(#{
        code => unmarshal(string, Failure#'SubFailure'.code),
        sub => maybe_unmarshal(sub_failure, Failure#'SubFailure'.sub)
    });

unmarshal(context, V) -> ff_entity_context_codec:unmarshal(V);

unmarshal(range, #evsink_EventRange{
    'after' = Cursor,
    limit   = Limit
}) ->
    {Cursor, Limit, forward};

unmarshal(timestamp, Timestamp) when is_binary(Timestamp) ->
    parse_timestamp(Timestamp);
unmarshal(string, V) when is_binary(V) ->
    V;
unmarshal(integer, V) when is_integer(V) ->
    V;

unmarshal(range, #'EventRange'{
    'after' = Cursor,
    limit   = Limit
}) ->
    {Cursor, Limit, forward};

unmarshal(bool, V) when is_boolean(V) ->
    V.

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).

%% Suppress dialyzer warning until rfc3339 spec will be fixed.
%% see https://github.com/talentdeficit/rfc3339/pull/5
-dialyzer([{nowarn_function, [parse_timestamp/1]}, no_match]).
-spec parse_timestamp(binary()) ->
    machinery:timestamp().
parse_timestamp(Bin) ->
    case rfc3339:parse(Bin) of
        {ok, {_Date, _Time, _Usec, TZ}} when TZ =/= 0 andalso TZ =/= undefined ->
            erlang:error({bad_deadline, not_utc}, [Bin]);
        {ok, {Date, Time, undefined, _TZ}} ->
            {to_calendar_datetime(Date, Time), 0};
        {ok, {Date, Time, Usec, _TZ}} ->
            {to_calendar_datetime(Date, Time), Usec div 1000};
        {error, Error} ->
            erlang:error({bad_timestamp, Error}, [Bin])
    end.

to_calendar_datetime(Date, Time = {H, _, S}) when H =:= 24 orelse S =:= 60 ->
    %% Type specifications for hours and seconds differ in calendar and rfc3339,
    %% so make a proper calendar:datetime() here.
    Sec = calendar:datetime_to_gregorian_seconds({Date, Time}),
    calendar:gregorian_seconds_to_datetime(Sec);
to_calendar_datetime(Date, Time) ->
    {Date, Time}.
