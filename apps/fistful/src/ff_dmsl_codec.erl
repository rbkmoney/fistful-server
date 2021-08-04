-module(ff_dmsl_codec).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").
-include_lib("damsel/include/dmsl_proxy_inspector_p2p_thrift.hrl").

-export([unmarshal/2]).
-export([marshal/2]).

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

-spec unmarshal(ff_dmsl_codec:type_name(), ff_dmsl_codec:encoded_value()) -> ff_dmsl_codec:decoded_value().
unmarshal(transaction_info, #domain_TransactionInfo{
    id = ID,
    timestamp = Timestamp,
    extra = Extra,
    additional_info = AddInfo
}) ->
    genlib_map:compact(#{
        id => unmarshal(string, ID),
        timestamp => maybe_unmarshal(string, Timestamp),
        extra => Extra,
        additional_info => maybe_unmarshal(additional_transaction_info, AddInfo)
    });
unmarshal(additional_transaction_info, #domain_AdditionalTransactionInfo{
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
unmarshal(failure, #domain_Failure{
    code = Code,
    reason = Reason,
    sub = SubFailure
}) ->
    genlib_map:compact(#{
        code => unmarshal(string, Code),
        reason => maybe_unmarshal(string, Reason),
        sub => maybe_unmarshal(sub_failure, SubFailure)
    });
unmarshal(sub_failure, #domain_SubFailure{
    code = Code,
    sub = SubFailure
}) ->
    genlib_map:compact(#{
        code => unmarshal(string, Code),
        sub => maybe_unmarshal(sub_failure, SubFailure)
    });
unmarshal(cash, #domain_Cash{
    amount = Amount,
    currency = CurrencyRef
}) ->
    {unmarshal(amount, Amount), unmarshal(currency_ref, CurrencyRef)};
unmarshal(cash_range, #domain_CashRange{
    lower = {BoundLower, CashLower},
    upper = {BoundUpper, CashUpper}
}) ->
    {
        {BoundLower, unmarshal(cash, CashLower)},
        {BoundUpper, unmarshal(cash, CashUpper)}
    };
unmarshal(currency_ref, #domain_CurrencyRef{symbolic_code = SymbolicCode}) ->
    unmarshal(string, SymbolicCode);
unmarshal(risk_score, low) ->
    low;
unmarshal(risk_score, high) ->
    high;
unmarshal(risk_score, fatal) ->
    fatal;
unmarshal(currency, #domain_Currency{
    name = Name,
    symbolic_code = Symcode,
    numeric_code = Numcode,
    exponent = Exponent
}) ->
    #{
        name => Name,
        symcode => Symcode,
        numcode => Numcode,
        exponent => Exponent
    };
unmarshal(user_interaction, {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}}) ->
    {redirect, #{content => {get, URI}}};
unmarshal(user_interaction, {redirect, {post_request, #'BrowserPostRequest'{uri = URI, form = Form}}}) ->
    {redirect, #{content => {post, URI, Form}}};
unmarshal(
    resource,
    {disposable, #domain_DisposablePaymentResource{
        payment_tool =
            {bank_card, #domain_BankCard{
                token = Token,
                payment_system = PaymentSystem,
                payment_system_deprecated = PaymentSystemDeprecated,
                bin = Bin,
                last_digits = LastDigits,
                exp_date = ExpDate,
                cardholder_name = CardholderName
            }},
        payment_session_id = ID
    }}
) ->
    AuthData =
        case ID of
            undefined ->
                undefined;
            ID ->
                {session, #{session_id => unmarshal(string, ID)}}
        end,
    {bank_card,
        genlib_map:compact(#{
            bank_card => #{
                token => Token,
                payment_system => PaymentSystem,
                payment_system_deprecated => PaymentSystemDeprecated,
                bin => Bin,
                masked_pan => LastDigits,
                exp_date => maybe_unmarshal(exp_date, ExpDate),
                cardholder_name => maybe_unmarshal(string, CardholderName)
            },
            auth_data => AuthData
        })};
unmarshal(exp_date, #'domain_BankCardExpDate'{
    month = Month,
    year = Year
}) ->
    {unmarshal(integer, Month), unmarshal(integer, Year)};
unmarshal(attempt_limit, #domain_AttemptLimit{
    attempts = Attempts
}) ->
    unmarshal(integer, Attempts);
unmarshal(amount, V) ->
    unmarshal(integer, V);
unmarshal(string, V) when is_binary(V) ->
    V;
unmarshal(integer, V) when is_integer(V) ->
    V.

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, V) ->
    unmarshal(Type, V).

-spec marshal(ff_dmsl_codec:type_name(), ff_dmsl_codec:decoded_value()) -> ff_dmsl_codec:encoded_value().
marshal(cash, {Amount, CurrencyRef}) ->
    #domain_Cash{
        amount = marshal(amount, Amount),
        currency = marshal(currency_ref, CurrencyRef)
    };
marshal(cash_range, {{BoundLower, CashLower}, {BoundUpper, CashUpper}}) ->
    #domain_CashRange{
        lower = {BoundLower, marshal(cash, CashLower)},
        upper = {BoundUpper, marshal(cash, CashUpper)}
    };
marshal(currency_ref, CurrencyID) when is_binary(CurrencyID) ->
    #domain_CurrencyRef{
        symbolic_code = CurrencyID
    };
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
marshal(payment_resource_payer, Payer = #{resource := Resource}) ->
    ClientInfo = maps:get(client_info, Payer, undefined),
    ContactInfo = maps:get(contact_info, Payer, undefined),
    #domain_PaymentResourcePayer{
        resource = marshal(disposable_payment_resource, {Resource, ClientInfo}),
        contact_info = marshal(contact_info, ContactInfo)
    };
marshal(disposable_payment_resource, {Resource, ClientInfo}) ->
    #domain_DisposablePaymentResource{
        payment_tool = marshal(payment_tool, Resource),
        payment_session_id = try_get_session_auth_data(Resource),
        client_info = maybe_marshal(client_info, ClientInfo)
    };
marshal(payment_tool, {bank_card, #{bank_card := BankCard}}) ->
    {bank_card, marshal(bank_card, BankCard)};
marshal(bin_data, #{payment_system := PaymentSystem} = BinData) ->
    #domain_BinData{
        payment_system = PaymentSystem,
        bank_name = maps:get(bank_name, BinData, undefined)
    };
marshal(bank_card, BankCard) ->
    ExpDate = ff_resource:exp_date(BankCard),
    #domain_BankCard{
        token = ff_resource:token(BankCard),
        bin = ff_resource:bin(BankCard),
        last_digits = ff_resource:masked_pan(BankCard),
        payment_system = ff_resource:payment_system(BankCard),
        payment_system_deprecated = ff_resource:payment_system(BankCard),
        issuer_country = ff_resource:country_code(BankCard),
        bank_name = ff_resource:bank_name(BankCard),
        exp_date = maybe_marshal(exp_date, ExpDate),
        cardholder_name = ff_resource:cardholder_name(BankCard),
        category = ff_resource:category(BankCard)
    };
marshal(exp_date, {Month, Year}) ->
    #domain_BankCardExpDate{
        month = marshal(integer, Month),
        year = marshal(integer, Year)
    };
marshal(contact_info, undefined) ->
    #domain_ContactInfo{};
marshal(contact_info, ContactInfo) ->
    #domain_ContactInfo{
        phone_number = maps:get(phone_number, ContactInfo, undefined),
        email = maps:get(email, ContactInfo, undefined)
    };
marshal(client_info, ClientInfo) ->
    IPAddress = maps:get(ip_address, ClientInfo, undefined),
    Fingerprint = maps:get(fingerprint, ClientInfo, undefined),
    #domain_ClientInfo{
        ip_address = IPAddress,
        fingerprint = Fingerprint
    };
marshal(p2p_tool, {Sender, Receiver}) ->
    #domain_P2PTool{
        sender = marshal(payment_tool, Sender),
        receiver = marshal(payment_tool, Receiver)
    };
marshal(attempt_limit, Limit) ->
    #domain_AttemptLimit{
        attempts = Limit
    };
marshal(risk_score, low) ->
    low;
marshal(risk_score, high) ->
    high;
marshal(risk_score, fatal) ->
    fatal;
marshal(amount, V) ->
    marshal(integer, V);
marshal(string, V) when is_binary(V) ->
    V;
marshal(integer, V) when is_integer(V) ->
    V;
marshal(_, Other) ->
    Other.

maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).

try_get_session_auth_data({bank_card, #{auth_data := {session, #{session_id := ID}}}}) ->
    marshal(string, ID);
try_get_session_auth_data(_) ->
    undefined.
