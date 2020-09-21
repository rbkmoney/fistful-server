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
-define(DEFAULT_CONTEXT(PartyID), #{
    <<"com.rbkmoney.wapi">> => {obj, #{
        {str, <<"owner">>} => {str, PartyID},
        {str, <<"name">>} => {str, ?STRING},
        {str, <<"metadata">>} => {obj, #{{str, <<"somedata">>} => {str, ?STRING}}}
    }}
}).
-define(BOOLEAN, true).

-define(DEFAULT_CONTEXT_NO_NAME(PartyID), #{
    <<"com.rbkmoney.wapi">> => {obj, #{
        {str, <<"owner">>} => {str, PartyID},
        {str, <<"metadata">>} => {obj, #{{str, <<"somedata">>} => {str, ?STRING}}}
    }}
}).

-define(DEFAULT_METADATA(), #{<<"somedata">> => {str, ?STRING}}).

-define(CASH, #'Cash'{
    amount = ?INTEGER,
    currency = #'CurrencyRef'{
        symbolic_code = ?RUB
    }
}).

-define(GET_INTERNAL_ID_RESULT, {
    'bender_GetInternalIDResult',
    ?STRING,
    {obj, #{{str, <<"context_data">>} => {str, ?STRING}}},
    undefined
}).

-define(GENERATE_ID_RESULT, {
    'bender_GenerationResult',
    ?STRING,
    undefined,
    undefined
}).

-define(WITHDRAWAL_STATUS, {pending, #wthd_status_Pending{}}).

-define(WITHDRAWAL(PartyID), #wthd_WithdrawalState{
    id = ?STRING,
    wallet_id = ?STRING,
    destination_id = ?STRING,
    body = ?CASH,
    external_id = ?STRING,
    status = ?WITHDRAWAL_STATUS,
    created_at = ?TIMESTAMP,
    effective_final_cash_flow = #cashflow_FinalCashFlow{postings = []},
    sessions = [],
    adjustments = [],
    metadata = ?DEFAULT_METADATA(),
    context = ?DEFAULT_CONTEXT(PartyID)
}).

-define(WITHDRAWAL_QUOTE, #wthd_Quote{
    cash_from = ?CASH,
    cash_to = ?CASH,
    created_at = ?TIMESTAMP,
    expires_on = ?TIMESTAMP,
    operation_timestamp = ?TIMESTAMP,
    domain_revision = 123,
    party_revision = 123,
    route = #wthd_Route{
        provider_id = 123,
        terminal_id = 123
    },
    quote_data = {str, ?STRING}
}).

-define(WITHDRAWAL_EVENT(Change), #wthd_Event{
    change = Change,
    occured_at = ?TIMESTAMP,
    event_id = ?INTEGER
}).

-define(WITHDRAWAL_STATUS_CHANGE, {status_changed, #wthd_StatusChange{status = {pending, #wthd_status_Pending{}}}}).

-define(BLOCKING, unblocked).

-define(ACCOUNT, #account_Account{
    id = ?STRING,
    identity = ?STRING,
    currency = #'CurrencyRef'{
        symbolic_code = ?RUB
    },
    accounter_account_id = ?INTEGER
}).

-define(ACCOUNT_BALANCE, #account_AccountBalance{
    id = ?STRING,
    currency = #'CurrencyRef'{
        symbolic_code = ?RUB
    },
    expected_min = ?INTEGER,
    current = ?INTEGER,
    expected_max = ?INTEGER
}).

-define(RESOURCE, {bank_card, #'BankCard'{
    token = ?STRING,
    bin = <<"424242">>,
    masked_pan = <<"4242">>,
    bank_name = ?STRING,
    payment_system = visa,
    issuer_country = rus,
    card_type = debit
}}).

-define(DESTINATION_STATUS, {authorized, #dst_Authorized{}}).

-define(DESTINATION(PartyID), #dst_DestinationState{
    id          = ?STRING,
    name        = ?STRING,
    status      = ?DESTINATION_STATUS,
    account     = ?ACCOUNT,
    resource    = ?RESOURCE,
    external_id = ?STRING,
    created_at  = ?TIMESTAMP,
    context     = ?DEFAULT_CONTEXT(PartyID)
}).

-define(WALLET(PartyID), #wlt_WalletState{
    id          = ?STRING,
    name        = ?STRING,
    blocking    = ?BLOCKING,
    account     = ?ACCOUNT,
    external_id = ?STRING,
    created_at  = ?TIMESTAMP,
    metadata    = ?DEFAULT_METADATA(),
    context     = ?DEFAULT_CONTEXT(PartyID)
}).

-define(P2PTEMPLATE(PartyID), #p2p_template_P2PTemplateState{
    id = ?STRING,
    identity_id = ?STRING,
    created_at = ?TIMESTAMP,
    domain_revision = 1,
    party_revision = 1,
    template_details = #p2p_template_P2PTemplateDetails{
        body = #p2p_template_P2PTemplateBody{
            value = #p2p_template_Cash{
                amount = ?INTEGER,
                currency = #'CurrencyRef'{
                    symbolic_code = ?RUB
                }
            }
        },
        metadata = #p2p_template_P2PTemplateMetadata{
            value = ?DEFAULT_METADATA()
        }
    },
    blocking = ?BLOCKING,
    external_id = ?STRING,
    context = ?DEFAULT_CONTEXT(PartyID)
}).

-define(IDENTITY(PartyID),
    ?IDENTITY(PartyID, ?DEFAULT_CONTEXT(PartyID))
).

-define(IDENTITY(PartyID, Context), #idnt_IdentityState{
    id = ?STRING,
    name = ?STRING,
    party_id = ?STRING,
    provider_id = ?STRING,
    contract_id = ?STRING,
    class_id = ?STRING,
    metadata = ?DEFAULT_METADATA(),
    context = Context
}).

-define(IDENTITY_CHALLENGE(Status), #idnt_ChallengeState{
    cls         = ?STRING,
    proofs      = [
        #idnt_ChallengeProof{
            type = rus_domestic_passport,
            token = ?STRING
        }
    ],
    id          = ?STRING,
    status      = Status
}).

-define(IDENTITY_CHALLENGE_STATUS_COMPLETED, {completed, #idnt_ChallengeCompleted{
    resolution = approved,
    valid_until = ?TIMESTAMP
}}).

-define(IDENTITY_CHALLENGE_EVENT(Change), #idnt_Event{
    change = Change,
    occured_at = ?TIMESTAMP,
    sequence = ?INTEGER
}).

-define(CHALLENGE_STATUS_CHANGE, {identity_challenge, #idnt_ChallengeChange{
    id = ?STRING,
    payload = {status_changed, ?IDENTITY_CHALLENGE_STATUS_COMPLETED}
}}).

-define(STAT_INVALID_EXCEPTION(Errors), #fistfulstat_InvalidRequest{errors = Errors}).
-define(STAT_BADTOKEN_EXCEPTION, #fistfulstat_BadToken{reason = ?STRING}).

-define(STAT_RESPONCE(Data), #fistfulstat_StatResponse{data = Data}).

-define(STAT_WALLETS, {wallets, [#fistfulstat_StatWallet{
    id = ?STRING,
    identity_id = ?STRING,
    name = ?STRING,
    created_at = ?TIMESTAMP,
    currency_symbolic_code = ?RUB
}]}).

-define(STAT_WITHDRAWALS, {withdrawals, [#fistfulstat_StatWithdrawal{
    id = ?STRING,
    created_at = ?TIMESTAMP,
    identity_id = ?STRING,
    source_id = ?STRING,
    destination_id = ?STRING,
    external_id = ?STRING,
    amount = ?INTEGER,
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB,
    status = {pending, #fistfulstat_WithdrawalPending{}}
}]}).

-define(STAT_DEPOSITS, {deposits, [#fistfulstat_StatDeposit{
    id = ?STRING,
    created_at = ?TIMESTAMP,
    identity_id = ?STRING,
    source_id = ?STRING,
    destination_id = ?STRING,
    amount = ?INTEGER,
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB,
    status = {pending, #fistfulstat_DepositPending{}}
}]}).

-define(STAT_DESTINATIONS, {destinations, [#fistfulstat_StatDestination{
    id = ?STRING,
    name = ?STRING,
    created_at = ?TIMESTAMP,
    is_blocked = ?BOOLEAN,
    identity = ?STRING,
    currency_symbolic_code = ?RUB,
    resource = ?RESOURCE,
    external_id = ?STRING,
    status = {unauthorized, #fistfulstat_Unauthorized{}}
}]}).

-define(STAT_IDENTITIES, {identities, [#fistfulstat_StatIdentity{
    id = ?STRING,
    name = ?STRING,
    created_at = ?TIMESTAMP,
    provider = ?STRING,
    identity_class = ?STRING,
    identity_level = ?STRING,
    effective_challenge = ?STRING,
    is_blocked = ?BOOLEAN,
    external_id = ?STRING
}]}).

-define(IDENT_DOC, {russian_domestic_passport, #'identdocstore_RussianDomesticPassport'{
    issuer = ?STRING,
    issuer_code = ?STRING,
    issued_at = ?TIMESTAMP,
    birth_date = ?TIMESTAMP,
    birth_place = ?STRING,
    series = ?STRING,
    number = ?STRING,
    first_name = ?STRING,
    family_name = ?STRING,
    patronymic = ?STRING
}}).

-define(REPORT_ID, ?INTEGER).

-define(REPORT_EXT(Status, FilesList), #ff_reports_Report{
    report_id = ?INTEGER,
    time_range = #ff_reports_ReportTimeRange{
        from_time = ?TIMESTAMP,
        to_time = ?TIMESTAMP
    },
    created_at = ?TIMESTAMP,
    report_type = <<"withdrawalRegistry">>,
    status = Status,
    file_data_ids = FilesList
}).

-define(REPORT_WITH_STATUS(Status), ?REPORT_EXT(Status, [?STRING, ?STRING,?STRING])).

-define(REPORT, ?REPORT_WITH_STATUS(created)).

-define(WITHDRAWAL_EVENT_FILTER,
    #webhooker_EventFilter{
        types = ordsets:from_list([
            {withdrawal, {started, #webhooker_WithdrawalStarted{}}},
            {withdrawal, {succeeded, #webhooker_WithdrawalSucceeded{}}},
            {withdrawal, {failed, #webhooker_WithdrawalFailed{}}}
        ])
}).

-define(DESTINATION_EVENT_FILTER, #webhooker_EventFilter{
    types = ordsets:from_list([
        {destination, {created, #webhooker_DestinationCreated{}}},
        {destination, {unauthorized, #webhooker_DestinationUnauthorized{}}},
        {destination, {authorized, #webhooker_DestinationAuthorized{}}}
    ])
}).

-define(WEBHOOK(EventFilter), #webhooker_Webhook{
    id = ?INTEGER,
    identity_id = ?STRING,
    wallet_id = ?STRING,
    event_filter = EventFilter,
    url = ?STRING,
    pub_key = ?STRING,
    enabled = false
}).

-define(W2W_TRANSFER(PartyID), #w2w_transfer_W2WTransferState{
    id = ?STRING,
    wallet_from_id = ?STRING,
    wallet_to_id = ?STRING,
    body = ?CASH,
    created_at = ?TIMESTAMP,
    domain_revision = ?INTEGER,
    party_revision = ?INTEGER,
    status = {pending, #w2w_status_Pending{}},
    external_id = ?STRING,
    metadata    = ?DEFAULT_METADATA(),
    context = ?DEFAULT_CONTEXT(PartyID),
    effective_final_cash_flow = #cashflow_FinalCashFlow{
        postings = []
    },
    adjustments = []
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
        ordsets:from_list([
            #domain_PaymentMethodRef{
                id = {bank_card_deprecated, mastercard}
            },
            #domain_PaymentMethodRef{
                id = {bank_card_deprecated, visa}
            },
            #domain_PaymentMethodRef{
                id = {tokenized_bank_card_deprecated, #domain_TokenizedBankCard{
                    payment_system = mastercard,
                    token_provider = applepay
                }}
            },
            #domain_PaymentMethodRef{
                id = {tokenized_bank_card_deprecated, #domain_TokenizedBankCard{
                    payment_system = visa,
                    token_provider = applepay
                }}
            }
        ])
    }
}).
