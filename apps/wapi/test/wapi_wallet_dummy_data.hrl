-define(STRING, <<"TEST">>).
-define(RUB, <<"RUB">>).
-define(USD, <<"USD">>).
-define(BANKID_RU, <<"PUTIN">>).
-define(BANKID_US, <<"TRAMP">>).
-define(WALLET_TOOL, <<"TOOL">>).
-define(JSON, <<"{}">>).
-define(INTEGER, 10000).
-define(INTEGER_BINARY, <<"10000">>).
-define(TIMESTAMP, <<"2016-03-22T06:12:27Z">>).
-define(MD5, <<"033BD94B1168D7E4F0D644C3C95E35BF">>).
-define(SHA256, <<"94EE059335E587E501CC4BF90613E0814F00A7B08BC7C648FD865A2AF6A22CC2">>).

-define(CASH, #'Cash'{
    amount = ?INTEGER,
    currency = #'CurrencyRef'{
        symbolic_code = ?RUB
    }
}).

-define(BLOCKING, unblocked).

-define(ACCOUNT, #account_Account{
    id = ?STRING,
    identity = ?STRING,
    currency = #'CurrencyRef'{
        symbolic_code = ?RUB
    },
    accounter_account_id = ?INTEGER
}).

-define(WALLET_STATE, #wlt_WalletState{
    id = ?STRING,
    name = ?STRING,
    blocking = ?BLOCKING,
    account = ?ACCOUNT
}).

-define(SNAPSHOT, #'Snapshot'{
    version = ?INTEGER,
    domain = #{
        {category, #domain_CategoryRef{id = ?INTEGER}} =>
        {category, #domain_CategoryObject{
            ref = #domain_CategoryRef{id = ?INTEGER},
            data = #domain_Category{
                name = ?STRING,
                description = ?STRING
            }
        }},
        {business_schedule, #domain_BusinessScheduleRef{id = ?INTEGER}} =>
        {business_schedule, #domain_BusinessScheduleObject{
            ref = #domain_BusinessScheduleRef{id = ?INTEGER},
            data = #domain_BusinessSchedule{
                name = ?STRING,
                description = ?STRING,
                schedule = #'Schedule'{
                    year = {every, #'ScheduleEvery'{}},
                    month = {every, #'ScheduleEvery'{}},
                    day_of_month = {every, #'ScheduleEvery'{}},
                    day_of_week = {every, #'ScheduleEvery'{}},
                    hour = {every, #'ScheduleEvery'{}},
                    minute = {every, #'ScheduleEvery'{}},
                    second = {every, #'ScheduleEvery'{}}
                },
                delay = #'TimeSpan'{},
                policy = #domain_PayoutCompilationPolicy{
                    assets_freeze_for = #'TimeSpan'{}
                }
            }
        }},
        {globals, #domain_GlobalsRef{}} =>
        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                external_account_set = {value, #domain_ExternalAccountSetRef{id = ?INTEGER}},
                payment_institutions = [#domain_PaymentInstitutionRef{id = ?INTEGER}]
            }
        }},
        {payment_institution, #domain_PaymentInstitutionRef{id = ?INTEGER}} =>
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = #domain_PaymentInstitutionRef{id = ?INTEGER},
            data = #domain_PaymentInstitution{
                name = ?STRING,
                description = ?STRING,
                system_account_set = {value, #domain_SystemAccountSetRef{id = ?INTEGER}},
                default_contract_template = {value, #domain_ContractTemplateRef{id = ?INTEGER}},
                providers = {value, []},
                inspector = {value, #domain_InspectorRef{id = ?INTEGER}},
                realm = test,
                residences = [rus]
            }
        }}
    }
}).

-define(TERM_SET, #domain_TermSet{
    payouts = ?PAYOUTS_SERVICE_TERMS,
    payments = ?PAYMENTS_SERVICE_TERMS
}).

-define(PAYOUTS_SERVICE_TERMS, #domain_PayoutsServiceTerms{}).

-define(PAYMENTS_SERVICE_TERMS, #domain_PaymentsServiceTerms{
    payment_methods = {value,
        [
            #domain_PaymentMethodRef{
                id = {bank_card, mastercard}
            },
            #domain_PaymentMethodRef{
                id = {bank_card, visa}
            },
            #domain_PaymentMethodRef{
                id = {tokenized_bank_card, #domain_TokenizedBankCard{
                    payment_system = mastercard,
                    token_provider = applepay
                }}
            },
            #domain_PaymentMethodRef{
                id = {tokenized_bank_card, #domain_TokenizedBankCard{
                    payment_system = visa,
                    token_provider = applepay
                }}
            }
        ]
    }
}).
