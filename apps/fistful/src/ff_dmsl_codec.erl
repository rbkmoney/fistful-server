-module(ff_dmsl_codec).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
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


-spec unmarshal(ff_dmsl_codec:type_name(), ff_dmsl_codec:encoded_value()) ->
    ff_dmsl_codec:decoded_value().

unmarshal(transaction_info, #domain_TransactionInfo{
    id   = ID,
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
    amount   = Amount,
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

unmarshal(currency_ref, #domain_CurrencyRef{
    symbolic_code = SymbolicCode
}) ->
    unmarshal(string, SymbolicCode);

unmarshal(risk_score, low) ->
    low;
unmarshal(risk_score, high) ->
    high;
unmarshal(risk_score, fatal) ->
    fatal;

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

-spec marshal(ff_dmsl_codec:type_name(), ff_dmsl_codec:decoded_value()) ->
    ff_dmsl_codec:encoded_value().

marshal(cash, {Amount, CurrencyRef}) ->
    #domain_Cash{
        amount   = marshal(amount, Amount),
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

marshal(payment_resource_payer, {Resource, ContactInfo}) ->
    #domain_PaymentResourcePayer{
        resource = #domain_DisposablePaymentResource{
            payment_tool = marshal(resource, Resource)
        },
        contact_info = marshal(contact_info, ContactInfo)
    };
marshal(resource, {bank_card, BankCard}) ->
    {bank_card, #domain_BankCard{
        token           = ff_resource:token(BankCard),
        bin             = ff_resource:bin(BankCard),
        masked_pan      = ff_resource:masked_pan(BankCard),
        payment_system  = ff_resource:payment_system(BankCard),
        issuer_country  = ff_resource:country_code(BankCard),
        bank_name       = ff_resource:bank_name(BankCard)
    }};
marshal(contact_info, undefined) ->
    #domain_ContactInfo{};
marshal(contact_info, ContactInfo) ->
    #domain_ContactInfo{
        phone_number = maps:get(phone_number, ContactInfo, undefined),
        email = maps:get(email, ContactInfo, undefined)
    };

marshal(p2p_tool, {Sender, Receiver}) ->
    #domain_P2PTool{
        sender = marshal(resource, Sender),
        receiver = marshal(resource, Receiver)
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
