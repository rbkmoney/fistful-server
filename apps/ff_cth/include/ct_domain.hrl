-ifndef(__ct_domain_hrl__).
-define(__ct_domain_hrl__, 42).

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-define(ordset(Es), ordsets:from_list(Es)).

-define(glob(), #domain_GlobalsRef{}).
-define(cur(ID), #domain_CurrencyRef{symbolic_code = ID}).
-define(pmt(C, T), #domain_PaymentMethodRef{id = {C, T}}).
-define(pmtsys(ID), #domain_PaymentSystemRef{id = ID}).
-define(cat(ID), #domain_CategoryRef{id = ID}).
-define(prx(ID), #domain_ProxyRef{id = ID}).
-define(prv(ID), #domain_ProviderRef{id = ID}).
-define(trm(ID), #domain_TerminalRef{id = ID}).
-define(prv_trm(ID), #domain_ProviderTerminalRef{id = ID}).
-define(prv_trm(ID, P), #domain_ProviderTerminalRef{id = ID, priority = P}).
-define(tmpl(ID), #domain_ContractTemplateRef{id = ID}).
-define(trms(ID), #domain_TermSetHierarchyRef{id = ID}).
-define(sas(ID), #domain_SystemAccountSetRef{id = ID}).
-define(eas(ID), #domain_ExternalAccountSetRef{id = ID}).
-define(insp(ID), #domain_InspectorRef{id = ID}).
-define(payinst(ID), #domain_PaymentInstitutionRef{id = ID}).
-define(ruleset(ID), #domain_RoutingRulesetRef{id = ID}).

-define(cash(Amount, SymCode), #domain_Cash{amount = Amount, currency = ?cur(SymCode)}).

-define(cashrng(Lower, Upper), #domain_CashRange{lower = Lower, upper = Upper}).

-define(fixed(Amount, SymCode),
    {fixed, #domain_CashVolumeFixed{
        cash = #domain_Cash{
            amount = Amount,
            currency = ?cur(SymCode)
        }
    }}
).

-define(share(P, Q, C),
    {share, #domain_CashVolumeShare{
        parts = #'Rational'{p = P, q = Q},
        'of' = C
    }}
).

-define(share(P, Q, C, RM),
    {share, #domain_CashVolumeShare{
        parts = #'Rational'{p = P, q = Q},
        'of' = C,
        'rounding_method' = RM
    }}
).

-define(cfpost(A1, A2, V), #domain_CashFlowPosting{
    source = A1,
    destination = A2,
    volume = V
}).

-define(cfpost(A1, A2, V, D), #domain_CashFlowPosting{
    source = A1,
    destination = A2,
    volume = V,
    details = D
}).

-define(bank_card(BankName),
    {bank_card, #domain_BankCard{
        token = <<>>,
        bin = <<>>,
        last_digits = <<>>,
        bank_name = BankName,
        payment_system = #domain_PaymentSystemRef{id = <<"VISA">>},
        payment_system_deprecated = visa,
        issuer_country = rus
    }}
).

-endif.
