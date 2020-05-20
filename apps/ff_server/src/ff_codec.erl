-module(ff_codec).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_repairer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_account_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_msgpack_thrift.hrl").

-export([unmarshal/2]).
-export([unmarshal/3]).
-export([unmarshal_msgpack/1]).

-export([marshal/2]).
-export([marshal/3]).
-export([marshal_msgpack/1]).

%% Types

-type type_name() :: atom() | {list, atom()} | {set, atom()}.
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

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];
marshal({set, T}, V) ->
    ordsets:from_list([marshal(T, E) || E <- ordsets:to_list(V)]);

marshal(id, V) ->
    marshal(string, V);
marshal(event_id, V) ->
    marshal(integer, V);

marshal(blocking, blocked) ->
    blocked;
marshal(blocking, unblocked) ->
    unblocked;

marshal(transaction_info, TransactionInfo = #{
    id := TransactionID,
    extra := Extra
}) ->
    Timestamp = maps:get(timestamp, TransactionInfo, undefined),
    AddInfo = maps:get(additional_info, TransactionInfo, undefined),
    #'TransactionInfo'{
        id = marshal(id, TransactionID),
        timestamp = marshal(timestamp, Timestamp),
        extra = Extra,
        additional_info = marshal(additional_transaction_info, AddInfo)
    };

marshal(additional_transaction_info, AddInfo = #{}) ->
    #'AdditionalTransactionInfo'{
        rrn = marshal(string, maps:get(rrn, AddInfo, undefined)),
        approval_code = marshal(string, maps:get(approval_code, AddInfo, undefined)),
        acs_url = marshal(string, maps:get(acs_url, AddInfo, undefined)),
        pareq = marshal(string, maps:get(pareq, AddInfo, undefined)),
        md = marshal(string, maps:get(md, AddInfo, undefined)),
        term_url = marshal(string, maps:get(term_url, AddInfo, undefined)),
        pares = marshal(string, maps:get(pares, AddInfo, undefined)),
        eci = marshal(string, maps:get(eci, AddInfo, undefined)),
        cavv = marshal(string, maps:get(cavv, AddInfo, undefined)),
        xid = marshal(string, maps:get(xid, AddInfo, undefined)),
        cavv_algorithm = marshal(string, maps:get(cavv_algorithm, AddInfo, undefined)),
        three_ds_verification = marshal(
            three_ds_verification,
            maps:get(three_ds_verification, AddInfo, undefined)
        )
    };

marshal(three_ds_verification, Value) when
    Value =:= authentication_successful orelse
    Value =:= attempts_processing_performed orelse
    Value =:= authentication_failed orelse
    Value =:= authentication_could_not_be_performed
->
    Value;

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

marshal(resource, {bank_card, #{bank_card := BankCard} = ResourceBankCard}) ->
    {bank_card, #'ResourceBankCard'{
        bank_card = marshal(bank_card, BankCard),
        auth_data = maybe_marshal(bank_card_auth_data, maps:get(auth_data, ResourceBankCard, undefined))
    }};
marshal(resource, {crypto_wallet, #{crypto_wallet := CryptoWallet}}) ->
    {crypto_wallet, #'ResourceCryptoWallet'{
        crypto_wallet = marshal(crypto_wallet, CryptoWallet)
    }};

marshal(bank_card, BankCard = #{token := Token}) ->
    Bin = maps:get(bin, BankCard, undefined),
    PaymentSystem = maps:get(payment_system, BankCard, undefined),
    MaskedPan = maps:get(masked_pan, BankCard, undefined),
    BankName = maps:get(bank_name, BankCard, undefined),
    IsoCountryCode = maps:get(iso_country_code, BankCard, undefined),
    CardType = maps:get(card_type, BankCard, undefined),
    ExpDate = maps:get(exp_date, BankCard, undefined),
    CardholderName = maps:get(cardholder_name, BankCard, undefined),
    BinDataID = maps:get(bin_data_id, BankCard, undefined),
    #'BankCard'{
        token = marshal(string, Token),
        bin = marshal(string, Bin),
        masked_pan = marshal(string, MaskedPan),
        bank_name = marshal(string, BankName),
        payment_system = PaymentSystem,
        issuer_country = IsoCountryCode,
        card_type = CardType,
        exp_date = maybe_marshal(exp_date, ExpDate),
        cardholder_name = maybe_marshal(string, CardholderName),
        bin_data_id = marshal_msgpack(BinDataID)
    };

marshal(bank_card_auth_data, {session, #{session_id := ID}}) ->
    {session_data, #'SessionAuthData'{
        id = marshal(string, ID)
    }};

marshal(crypto_wallet, #{id := ID, currency := Currency}) ->
    #'CryptoWallet'{
        id       = marshal(string, ID),
        currency = marshal(crypto_currency, Currency),
        data     = marshal(crypto_data, Currency)
    };

marshal(exp_date, {Month, Year}) ->
    #'BankCardExpDate'{
        month = marshal(integer, Month),
        year = marshal(integer, Year)
    };

marshal(crypto_currency, {Currency, _}) ->
    Currency;

marshal(crypto_data, {bitcoin, #{}}) ->
    {bitcoin, #'CryptoDataBitcoin'{}};
marshal(crypto_data, {litecoin, #{}}) ->
    {litecoin, #'CryptoDataLitecoin'{}};
marshal(crypto_data, {bitcoin_cash, #{}}) ->
    {bitcoin_cash, #'CryptoDataBitcoinCash'{}};
marshal(crypto_data, {ethereum, #{}}) ->
    {ethereum, #'CryptoDataEthereum'{}};
marshal(crypto_data, {zcash, #{}}) ->
    {zcash, #'CryptoDataZcash'{}};
marshal(crypto_data, {usdt, #{}}) ->
    {usdt, #'CryptoDataUSDT'{}};
marshal(crypto_data, {ripple, Data}) ->
    {ripple, #'CryptoDataRipple'{
        tag = maybe_marshal(string, maps:get(tag, Data, undefined))
    }};

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

marshal(event_range, {After, Limit}) ->
    #'EventRange'{
        'after' = maybe_marshal(integer, After),
        limit   = maybe_marshal(integer, Limit)
    };

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
marshal(timestamp_ms, V) ->
    ff_time:to_rfc3339(V);
marshal(domain_revision, V) when is_integer(V) ->
    V;
marshal(party_revision, V) when is_integer(V) ->
    V;
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

unmarshal({list, T}, V) ->
    [marshal(T, E) || E <- V];
unmarshal({set, T}, V) ->
    ordsets:from_list([unmarshal(T, E) || E <- ordsets:to_list(V)]);

unmarshal(id, V) ->
    unmarshal(string, V);
unmarshal(event_id, V) ->
    unmarshal(integer, V);

unmarshal(blocking, blocked) ->
    blocked;
unmarshal(blocking, unblocked) ->
    unblocked;

unmarshal(transaction_info, #'TransactionInfo'{
    id = TransactionID,
    timestamp = Timestamp,
    extra = Extra,
    additional_info = AddInfo
}) ->
    genlib_map:compact(#{
        id => unmarshal(string, TransactionID),
        timestamp => maybe_unmarshal(string, Timestamp),
        extra => Extra,
        additional_info => maybe_unmarshal(additional_transaction_info, AddInfo)
    });

unmarshal(additional_transaction_info, #'AdditionalTransactionInfo'{
    rrn = RRN,
    approval_code = ApprovalCode,
    acs_url = AcsURL,
    pareq = Pareq,
    md = MD,
    term_url = TermURL,
    pares = Pares,
    eci = ECI,
    cavv = CAVV,
    xid = XID,
    cavv_algorithm = CAVVAlgorithm,
    three_ds_verification = ThreeDSVerification
}) ->
    genlib_map:compact(#{
        rrn => maybe_unmarshal(string, RRN),
        approval_code => maybe_unmarshal(string, ApprovalCode),
        acs_url => maybe_unmarshal(string, AcsURL),
        pareq => maybe_unmarshal(string, Pareq),
        md => maybe_unmarshal(string, MD),
        term_url => maybe_unmarshal(string, TermURL),
        pares => maybe_unmarshal(string, Pares),
        eci => maybe_unmarshal(string, ECI),
        cavv => maybe_unmarshal(string, CAVV),
        xid => maybe_unmarshal(string, XID),
        cavv_algorithm => maybe_unmarshal(string, CAVVAlgorithm),
        three_ds_verification => maybe_unmarshal(three_ds_verification, ThreeDSVerification)
    });

unmarshal(three_ds_verification, Value) when
    Value =:= authentication_successful orelse
    Value =:= attempts_processing_performed orelse
    Value =:= authentication_failed orelse
    Value =:= authentication_could_not_be_performed
->
    Value;

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

unmarshal(resource, {bank_card, #'ResourceBankCard'{
    bank_card = BankCard,
    auth_data = AuthData
}}) ->
    {bank_card, genlib_map:compact(#{
        bank_card => unmarshal(bank_card, BankCard),
        auth_data => maybe_unmarshal(bank_card_auth_data, AuthData)
    })};
unmarshal(resource, {crypto_wallet, #'ResourceCryptoWallet'{crypto_wallet = CryptoWallet}}) ->
    {crypto_wallet, #{
        crypto_wallet => unmarshal(crypto_wallet, CryptoWallet)
    }};

unmarshal(bank_card_auth_data, {session_data, #'SessionAuthData'{id = ID}}) ->
    #{
        session_id => unmarshal(string, ID)
    };

unmarshal(bank_card, #'BankCard'{
    token = Token,
    bin = Bin,
    masked_pan = MaskedPan,
    bank_name = BankName,
    payment_system = PaymentSystem,
    issuer_country = IsoCountryCode,
    card_type = CardType,
    bin_data_id = BinDataID,
    exp_date = ExpDate,
    cardholder_name = CardholderName
}) ->
    genlib_map:compact(#{
        token => unmarshal(string, Token),
        payment_system => maybe_unmarshal(payment_system, PaymentSystem),
        bin => maybe_unmarshal(string, Bin),
        masked_pan => maybe_unmarshal(string, MaskedPan),
        bank_name => maybe_unmarshal(string, BankName),
        iso_country_code => maybe_unmarshal(iso_country_code, IsoCountryCode),
        card_type => maybe_unmarshal(card_type, CardType),
        exp_date => maybe_unmarshal(exp_date, ExpDate),
        cardholder_name => maybe_unmarshal(string, CardholderName),
        bin_data_id => unmarshal_msgpack(BinDataID)
    });

unmarshal(exp_date, #'BankCardExpDate'{
    month = Month,
    year = Year
}) ->
    {unmarshal(integer, Month), unmarshal(integer, Year)};

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
        currency => {CryptoWalletCurrency, unmarshal(crypto_data, Data)}
    });

unmarshal(crypto_data, {ripple, #'CryptoDataRipple'{tag = Tag}}) ->
    genlib_map:compact(#{
        tag => maybe_unmarshal(string, Tag)
    });
unmarshal(crypto_data, _) ->
    #{};

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

unmarshal(event_range, #'EventRange'{'after' = After, limit = Limit}) ->
    {maybe_unmarshal(integer, After), maybe_unmarshal(integer, Limit)};

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
unmarshal(timestamp_ms, V) ->
    ff_time:from_rfc3339(V);
unmarshal(domain_revision, V) when is_integer(V) ->
    V;
unmarshal(party_revision, V) when is_integer(V) ->
    V;
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

% TODO: spec
-spec marshal_msgpack(_) -> _.

marshal_msgpack(nil)                  -> {nl, #msgp_Nil{}};
marshal_msgpack(V) when is_boolean(V) -> {b, V};
marshal_msgpack(V) when is_integer(V) -> {i, V};
marshal_msgpack(V) when is_float(V)   -> V;
marshal_msgpack(V) when is_binary(V)  -> {str, V}; % Assuming well-formed UTF-8 bytestring.
marshal_msgpack({binary, V}) when is_binary(V) ->
    {bin, V};
marshal_msgpack(V) when is_list(V) ->
    {arr, [marshal_msgpack(ListItem) || ListItem <- V]};
marshal_msgpack(V) when is_map(V) ->
    {obj, maps:fold(fun(Key, Value, Map) -> Map#{marshal_msgpack(Key) => marshal_msgpack(Value)} end, #{}, V)};
marshal_msgpack(undefined) ->
    undefined.

% TODO: spec
-spec unmarshal_msgpack(_) -> _.

unmarshal_msgpack({nl,  #msgp_Nil{}})        -> nil;
unmarshal_msgpack({b,   V}) when is_boolean(V) -> V;
unmarshal_msgpack({i,   V}) when is_integer(V) -> V;
unmarshal_msgpack({flt, V}) when is_float(V)   -> V;
unmarshal_msgpack({str, V}) when is_binary(V)  -> V; % Assuming well-formed UTF-8 bytestring.
unmarshal_msgpack({bin, V}) when is_binary(V)  -> {binary, V};
unmarshal_msgpack({arr, V}) when is_list(V)    -> [unmarshal_msgpack(ListItem) || ListItem <- V];
unmarshal_msgpack({obj, V}) when is_map(V)     ->
    maps:fold(fun(Key, Value, Map) -> Map#{unmarshal_msgpack(Key) => unmarshal_msgpack(Value)} end, #{}, V);
unmarshal_msgpack(undefined) ->
    undefined.
