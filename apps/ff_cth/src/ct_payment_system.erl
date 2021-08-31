-module(ct_payment_system).

-export([setup/0]).
-export([setup/1]).
-export([shutdown/1]).

%% API types

-type options() :: #{
    identity_provider_config => map(),
    services => map(),
    domain_config => list(),
    default_termset => dmsl_domain_thrift:'TermSet'(),
    company_termset => dmsl_domain_thrift:'TermSet'(),
    payment_inst_identity_id => id(),
    dummy_payment_inst_identity_id => id(),
    provider_identity_id => id(),
    dummy_provider_identity_id => id(),
    optional_apps => list()
}.

-opaque system() :: #{
    started_apps := [atom()],
    suite_sup := pid()
}.

-export_type([options/0]).
-export_type([system/0]).

%% Internal types

-type id() :: binary().
-type config() :: ct_helper:config().

%% API

-spec setup() -> fun((config()) -> config()).
setup() ->
    setup(#{}).

-spec setup(options()) -> fun((config()) -> config()).
setup(Options) ->
    fun(C) -> do_setup(Options, C) end.

-spec shutdown(config()) -> ok.
shutdown(C) ->
    #{started_apps := Apps, suite_sup := Sup} = ct_helper:cfg(payment_system, C),
    ok = ct_sup:stop(Sup),
    ok = ct_helper:stop_apps(Apps).

%% Internals

-spec do_setup(options(), config()) -> config().
do_setup(Options0, C0) ->
    Options = Options0#{
        payment_inst_identity_id => genlib:unique(),
        dummy_payment_inst_identity_id => genlib:unique(),
        provider_identity_id => genlib:unique(),
        dummy_provider_identity_id => genlib:unique()
    },
    {ok, Processing0} = start_processing_apps(Options),
    C1 = ct_helper:makeup_cfg([ct_helper:woody_ctx()], [{services, services(Options)} | C0]),
    ok = ct_helper:set_context(C1),
    ok = setup_dominant(Options, C1),
    ok = ct_keyring:init(C1),
    %% TODO rewrite timer , check keyring status from cds health checker
    ok = timer:sleep(5000),
    ok = configure_processing_apps(Options),
    ok = ct_helper:unset_context(),
    [{payment_system, Processing0} | C1].

start_processing_apps(Options) ->
    P2PAdapterAdr = <<"/p2p_adapter">>,
    {StartedApps, _StartupCtx} = ct_helper:start_apps([
        scoper,
        woody,
        dmt_client,
        {fistful, [
            {services, services(Options)},
            {providers, identity_provider_config(Options)},
            {test, #{p2p_adapter_adr => P2PAdapterAdr}}
        ]},
        ff_server,
        p2p
    ]),
    SuiteSup = ct_sup:start(),
    {ok, _} = supervisor:start_child(
        SuiteSup,
        woody_server:child_spec(
            ?MODULE,
            #{
                ip => {127, 0, 0, 1},
                port => 8222,
                handlers => [
                    {
                        <<"/quotebank">>,
                        {{dmsl_withdrawals_provider_adapter_thrift, 'Adapter'}, {ff_ct_provider_handler, []}}
                    },
                    {
                        <<"/downbank">>,
                        {
                            {dmsl_withdrawals_provider_adapter_thrift, 'Adapter'},
                            {ff_ct_provider_handler, [{handler, ff_ct_fail_provider}]}
                        }
                    },
                    {
                        <<"/downbank2">>,
                        {
                            {dmsl_withdrawals_provider_adapter_thrift, 'Adapter'},
                            {ff_ct_provider_handler, [{handler, ff_ct_unknown_failure_provider}]}
                        }
                    },
                    {
                        <<"/sleepybank">>,
                        {
                            {dmsl_withdrawals_provider_adapter_thrift, 'Adapter'},
                            {ff_ct_provider_handler, [{handler, ff_ct_sleepy_provider}]}
                        }
                    },
                    {
                        P2PAdapterAdr,
                        {{dmsl_p2p_adapter_thrift, 'P2PAdapter'}, {p2p_ct_provider_handler, []}}
                    },
                    {
                        <<"/p2p_inspector">>,
                        {{dmsl_proxy_inspector_p2p_thrift, 'InspectorProxy'}, {p2p_ct_inspector_handler, []}}
                    },
                    {
                        <<"/binbase">>,
                        {{binbase_binbase_thrift, 'Binbase'}, {ff_ct_binbase_handler, []}}
                    }
                ],
                event_handler => scoper_woody_event_handler
            }
        )
    ),
    Processing = #{
        started_apps => StartedApps ++ start_optional_apps(Options),
        suite_sup => SuiteSup
    },
    {ok, Processing}.

start_optional_apps(#{optional_apps := Apps}) ->
    {StartedApps, _StartupCtx} = ct_helper:start_apps(Apps),
    StartedApps;
start_optional_apps(_) ->
    [].

setup_dominant(Options, C) ->
    ok = ct_domain_config:upsert(domain_config(Options, C)).

configure_processing_apps(Options) ->
    ok = set_app_env(
        [ff_transfer, withdrawal, system, accounts, settlement, <<"RUB">>],
        create_company_account()
    ),
    ok = set_app_env(
        [ff_transfer, withdrawal, system, accounts, subagent, <<"RUB">>],
        create_company_account()
    ),
    ok = set_app_env(
        [ff_transfer, withdrawal, provider, <<"mocketbank">>, accounts, <<"RUB">>],
        create_company_account()
    ),
    ok = create_crunch_identity(Options),
    PIIID = dummy_payment_inst_identity_id(Options),
    PRIID = dummy_provider_identity_id(Options),
    ok = create_crunch_identity(PIIID, PRIID, <<"quote-owner">>).

create_crunch_identity(Options) ->
    PaymentInstIdentityID = payment_inst_identity_id(Options),
    ProviderIdentityID = provider_identity_id(Options),
    create_crunch_identity(PaymentInstIdentityID, ProviderIdentityID, <<"good-one">>).

create_crunch_identity(PIIID, PRIID, ProviderID) ->
    PartyID = create_party(),
    PIIID = create_identity(PIIID, <<"ChurchPI">>, PartyID, ProviderID, <<"church">>),
    PRIID = create_identity(PRIID, <<"ChurchPR">>, PartyID, ProviderID, <<"church">>),
    ok.

create_company_account() ->
    PartyID = create_party(),
    IdentityID = create_company_identity(PartyID),
    {ok, Currency} = ff_currency:get(<<"RUB">>),
    {ok, IdentityMachine} = ff_identity_machine:get(IdentityID),
    Identity = ff_identity_machine:identity(IdentityMachine),
    {ok, [{created, Account}]} = ff_account:create(PartyID, Identity, Currency),
    Account.

create_company_identity(PartyID) ->
    create_company_identity(PartyID, <<"good-one">>).

create_company_identity(PartyID, ProviderID) ->
    create_identity(PartyID, ProviderID, <<"church">>).

create_party() ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_identity(PartyID, ProviderID, ClassID) ->
    ID = genlib:unique(),
    Name = <<"Test Identity">>,
    create_identity(ID, Name, PartyID, ProviderID, ClassID).

create_identity(ID, Name, PartyID, ProviderID, ClassID) ->
    ok = ff_identity_machine:create(
        #{id => ID, name => Name, party => PartyID, provider => ProviderID, class => ClassID},
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
    ),
    ID.

set_app_env([App, Key | Path], Value) ->
    Env = genlib_app:env(App, Key, #{}),
    NewEnv = do_set_env(Path, Value, Env),
    application:set_env(App, Key, NewEnv).

do_set_env([], Value, _Env) ->
    Value;
do_set_env([Key | Path], Value, Env) ->
    SubEnv = maps:get(Key, Env, #{}),
    Env#{Key => do_set_env(Path, Value, SubEnv)}.

%% Default options

identity_provider_config(Options) ->
    Default = #{
        <<"good-one">> => #{
            payment_institution_id => 1,
            routes => [<<"mocketbank">>],
            identity_classes => #{
                <<"person">> => #{
                    name => <<"Well, a person">>,
                    contract_template_id => 1,
                    initial_level => <<"peasant">>,
                    levels => #{
                        <<"peasant">> => #{
                            name => <<"Well, a peasant">>,
                            contractor_level => none
                        },
                        <<"nobleman">> => #{
                            name => <<"Well, a nobleman">>,
                            contractor_level => partial
                        }
                    },
                    challenges => #{
                        <<"sword-initiation">> => #{
                            name => <<"Initiation by sword">>,
                            base => <<"peasant">>,
                            target => <<"nobleman">>
                        }
                    }
                },
                <<"church">> => #{
                    name => <<"Well, a Сhurch">>,
                    contract_template_id => 2,
                    initial_level => <<"mainline">>,
                    levels => #{
                        <<"mainline">> => #{
                            name => <<"Well, a mainline Сhurch">>,
                            contractor_level => full
                        }
                    }
                }
            }
        },
        <<"good-two">> => #{
            payment_institution_id => 1,
            routes => [<<"mocketbank">>],
            identity_classes => #{
                <<"person">> => #{
                    name => <<"Well, a person">>,
                    contract_template_id => 1,
                    initial_level => <<"peasant">>,
                    levels => #{
                        <<"peasant">> => #{
                            name => <<"Well, a peasant">>,
                            contractor_level => none
                        },
                        <<"nobleman">> => #{
                            name => <<"Well, a nobleman">>,
                            contractor_level => partial
                        }
                    },
                    challenges => #{
                        <<"sword-initiation">> => #{
                            name => <<"Initiation by sword">>,
                            base => <<"peasant">>,
                            target => <<"nobleman">>
                        }
                    }
                },
                <<"church">> => #{
                    name => <<"Well, a Сhurch">>,
                    contract_template_id => 2,
                    initial_level => <<"mainline">>,
                    levels => #{
                        <<"mainline">> => #{
                            name => <<"Well, a mainline Сhurch">>,
                            contractor_level => full
                        }
                    }
                }
            }
        },
        <<"quote-owner">> => #{
            payment_institution_id => 2,
            routes => [<<"quotebank">>],
            identity_classes => #{
                <<"person">> => #{
                    name => <<"Well, a person">>,
                    contract_template_id => 1,
                    initial_level => <<"peasant">>,
                    levels => #{
                        <<"peasant">> => #{
                            name => <<"Well, a peasant">>,
                            contractor_level => none
                        },
                        <<"nobleman">> => #{
                            name => <<"Well, a nobleman">>,
                            contractor_level => partial
                        }
                    },
                    challenges => #{
                        <<"sword-initiation">> => #{
                            name => <<"Initiation by sword">>,
                            base => <<"peasant">>,
                            target => <<"nobleman">>
                        }
                    }
                },
                <<"church">> => #{
                    name => <<"Well, a Сhurch">>,
                    contract_template_id => 2,
                    initial_level => <<"mainline">>,
                    levels => #{
                        <<"mainline">> => #{
                            name => <<"Well, a mainline Сhurch">>,
                            contractor_level => full
                        }
                    }
                }
            }
        }
    },
    maps:get(identity_provider_config, Options, Default).

services(Options) ->
    Default = #{
        ff_p2p_adapter_host => "http://fistful-server:8022/v1/ff_p2p_adapter_host",
        ff_withdrawal_adapter_host => "http://fistful-server:8022/v1/ff_withdrawal_adapter_host",
        eventsink => "http://machinegun:8022/v1/event_sink",
        automaton => "http://machinegun:8022/v1/automaton",
        accounter => "http://shumway:8022/shumpune",
        kds => "http://kds:8022/v2/keyring",
        cds => "http://cds:8022/v2/storage",
        identdocstore => "http://cds:8022/v1/identity_document_storage",
        partymgmt => "http://party-management:8022/v1/processing/partymgmt",
        identification => "http://identification:8022/v1/identification",
        binbase => "http://localhost:8222/binbase"
    },
    maps:get(services, Options, Default).

%%

-include_lib("ff_cth/include/ct_domain.hrl").

payment_inst_identity_id(Options) ->
    maps:get(payment_inst_identity_id, Options).

provider_identity_id(Options) ->
    maps:get(provider_identity_id, Options).

dummy_payment_inst_identity_id(Options) ->
    maps:get(dummy_payment_inst_identity_id, Options).

dummy_provider_identity_id(Options) ->
    maps:get(dummy_provider_identity_id, Options).

domain_config(Options, C) ->
    P2PAdapterAdr = maps:get(p2p_adapter_adr, genlib_app:env(fistful, test, #{})),

    WithdrawalDecision1 =
        {delegates, [
            delegate(condition(party, <<"12345">>), ?ruleset(2)),
            delegate(condition(party, <<"67890">>), ?ruleset(4))
        ]},
    WithdrawalDecision2 =
        {delegates, [
            delegate(condition(cost_in, {0, 1000, <<"RUB">>}), ?ruleset(3))
        ]},
    WithdrawalDecision3 =
        {candidates, [
            candidate({constant, true}, ?trm(1)),
            candidate({constant, true}, ?trm(2))
        ]},
    WithdrawalDecision4 =
        {candidates, [
            candidate({constant, true}, ?trm(3)),
            candidate({constant, true}, ?trm(4)),
            candidate({constant, true}, ?trm(5))
        ]},
    WithdrawalDecision5 =
        {candidates, [
            candidate({constant, true}, ?trm(4))
        ]},

    P2PDecision1 =
        {delegates, [
            delegate(condition(party, <<"12345">>), ?ruleset(102)),
            delegate(condition(party, <<"67890">>), ?ruleset(104))
        ]},
    P2PDecision2 =
        {delegates, [
            delegate(condition(cost_in, {0, 1000, <<"RUB">>}), ?ruleset(103))
        ]},
    P2PDecision3 =
        {candidates, [
            candidate({constant, true}, ?trm(101)),
            candidate({constant, true}, ?trm(102))
        ]},
    P2PDecision4 =
        {candidates, [
            candidate({constant, true}, ?trm(103)),
            candidate({constant, true}, ?trm(104)),
            candidate({constant, true}, ?trm(105))
        ]},
    P2PDecision5 =
        {candidates, [
            candidate({constant, true}, ?trm(104))
        ]},

    Default = [
        ct_domain:globals(?eas(1), [?payinst(1)]),
        ct_domain:external_account_set(?eas(1), <<"Default">>, ?cur(<<"RUB">>), C),

        routing_ruleset(?ruleset(1), <<"WithdrawalRuleset#1">>, WithdrawalDecision1),
        routing_ruleset(?ruleset(2), <<"WithdrawalRuleset#2">>, WithdrawalDecision2),
        routing_ruleset(?ruleset(3), <<"WithdrawalRuleset#3">>, WithdrawalDecision3),
        routing_ruleset(?ruleset(4), <<"WithdrawalRuleset#4">>, WithdrawalDecision4),
        routing_ruleset(?ruleset(5), <<"WithdrawalRuleset#5">>, WithdrawalDecision5),

        routing_ruleset(?ruleset(101), <<"P2PRuleset#1">>, P2PDecision1),
        routing_ruleset(?ruleset(102), <<"P2PRuleset#2">>, P2PDecision2),
        routing_ruleset(?ruleset(103), <<"P2PRuleset#3">>, P2PDecision3),
        routing_ruleset(?ruleset(104), <<"P2PRuleset#4">>, P2PDecision4),
        routing_ruleset(?ruleset(105), <<"P2PRuleset#5">>, P2PDecision5),

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?payinst(1),
            data = #domain_PaymentInstitution{
                name = <<"Generic Payment Institution">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers = {value, ?ordset([])},
                withdrawal_routing_rules = #domain_RoutingRules{
                    policies = ?ruleset(1),
                    prohibitions = ?ruleset(5)
                },
                p2p_transfer_routing_rules = #domain_RoutingRules{
                    policies = ?ruleset(101),
                    prohibitions = ?ruleset(105)
                },
                inspector = {value, ?insp(1)},
                residences = ['rus'],
                realm = live,
                wallet_system_account_set = {value, ?sas(1)},
                identity = payment_inst_identity_id(Options),
                withdrawal_providers =
                    {decisions, [
                        #domain_ProviderDecision{
                            if_ =
                                {condition,
                                    {cost_in,
                                        ?cashrng(
                                            {inclusive, ?cash(300, <<"RUB">>)},
                                            {inclusive, ?cash(301, <<"RUB">>)}
                                        )}},
                            then_ = {value, [?prv(17)]}
                        },
                        #domain_ProviderDecision{
                            if_ =
                                {condition,
                                    {cost_in,
                                        ?cashrng(
                                            {inclusive, ?cash(123123, <<"RUB">>)},
                                            {inclusive, ?cash(123123, <<"RUB">>)}
                                        )}},
                            then_ = {value, [?prv(16)]}
                        },
                        #domain_ProviderDecision{
                            if_ =
                                {condition,
                                    {cost_in, #domain_CashRange{
                                        upper =
                                            {inclusive, #domain_Cash{
                                                amount = 100500,
                                                currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}
                                            }},
                                        lower =
                                            {inclusive, #domain_Cash{
                                                amount = 100500,
                                                currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}
                                            }}
                                    }}},
                            % provider 4 will be discarded by proxy 6
                            then_ = {value, [?prv(4), ?prv(5)]}
                        },
                        #domain_ProviderDecision{
                            if_ =
                                {condition,
                                    {cost_in, #domain_CashRange{
                                        upper =
                                            {inclusive, #domain_Cash{
                                                amount = 500100,
                                                currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}
                                            }},
                                        lower =
                                            {inclusive, #domain_Cash{
                                                amount = 500100,
                                                currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}
                                            }}
                                    }}},
                            then_ = {value, [?prv(4), ?prv(6), ?prv(7), ?prv(8)]}
                        },
                        #domain_ProviderDecision{
                            if_ =
                                {condition,
                                    {cost_in, #domain_CashRange{
                                        upper =
                                            {inclusive, #domain_Cash{
                                                amount = 500500,
                                                currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}
                                            }},
                                        lower =
                                            {inclusive, #domain_Cash{
                                                amount = 500500,
                                                currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}
                                            }}
                                    }}},
                            then_ = {value, [?prv(9), ?prv(10)]}
                        },
                        #domain_ProviderDecision{
                            if_ =
                                {condition,
                                    {cost_in, #domain_CashRange{
                                        upper =
                                            {inclusive, #domain_Cash{
                                                amount = 700700,
                                                currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}
                                            }},
                                        lower =
                                            {inclusive, #domain_Cash{
                                                amount = 700700,
                                                currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}
                                            }}
                                    }}},
                            then_ = {value, [?prv(11)]}
                        },
                        #domain_ProviderDecision{
                            if_ = {
                                condition,
                                {payment_tool,
                                    {bank_card, #domain_BankCardCondition{
                                        definition = {issuer_country_is, 'rus'}
                                    }}}
                            },
                            then_ = {value, [?prv(1)]}
                        },
                        #domain_ProviderDecision{
                            if_ = {condition, {payment_tool, {crypto_currency, #domain_CryptoCurrencyCondition{}}}},
                            then_ = {value, [?prv(2)]}
                        },
                        #domain_ProviderDecision{
                            if_ = {constant, true},
                            then_ = {value, []}
                        }
                    ]},
                payment_system =
                    {decisions, [
                        #domain_PaymentSystemDecision{
                            if_ =
                                {any_of,
                                    ordsets:from_list([
                                        {condition,
                                            {bin_data, #domain_BinDataCondition{
                                                payment_system = {equals, <<"VISA">>},
                                                bank_name = {equals, <<"uber">>}
                                            }}},
                                        {condition,
                                            {bin_data, #domain_BinDataCondition{
                                                payment_system = {equals, <<"VISA">>},
                                                bank_name = {equals, <<"sber">>}
                                            }}}
                                    ])},
                            then_ = {value, ?pmtsys(<<"VISA">>)}
                        },
                        #domain_PaymentSystemDecision{
                            if_ =
                                {any_of,
                                    ordsets:from_list([
                                        {condition,
                                            {bin_data, #domain_BinDataCondition{
                                                payment_system = {equals, <<"NSPK MIR">>},
                                                bank_name = {equals, <<"poopa">>}
                                            }}}
                                    ])},
                            then_ = {value, ?pmtsys(<<"NSPK MIR">>)}
                        }
                    ]}
            }
        }},

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?payinst(2),
            data = #domain_PaymentInstitution{
                name = <<"Generic Payment Institution">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = ['rus'],
                realm = live,
                wallet_system_account_set = {value, ?sas(1)},
                identity = dummy_payment_inst_identity_id(Options),
                withdrawal_providers =
                    {decisions, [
                        #domain_ProviderDecision{
                            if_ =
                                {condition,
                                    {cost_in, #domain_CashRange{
                                        upper =
                                            {inclusive, #domain_Cash{
                                                amount = 123,
                                                currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}
                                            }},
                                        lower =
                                            {inclusive, #domain_Cash{
                                                amount = 123,
                                                currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}
                                            }}
                                    }}},
                            then_ = {value, [?prv(3)]}
                        },
                        #domain_ProviderDecision{
                            if_ =
                                {condition,
                                    {payment_tool,
                                        {crypto_currency, #domain_CryptoCurrencyCondition{
                                            definition = {crypto_currency_is_deprecated, litecoin}
                                        }}}},
                            then_ = {value, [?prv(3)]}
                        },
                        #domain_ProviderDecision{
                            if_ =
                                {condition,
                                    {payment_tool,
                                        {digital_wallet, #domain_DigitalWalletCondition{
                                            definition = {provider_is_deprecated, webmoney}
                                        }}}},
                            then_ = {value, [?prv(3)]}
                        },
                        #domain_ProviderDecision{
                            if_ = {
                                condition,
                                {payment_tool,
                                    {bank_card, #domain_BankCardCondition{
                                        definition = {issuer_country_is, 'rus'}
                                    }}}
                            },
                            then_ = {value, [?prv(3)]}
                        }
                    ]},
                p2p_inspector = {value, ?p2p_insp(1)},
                p2p_providers =
                    {decisions, [
                        #domain_ProviderDecision{
                            if_ =
                                {condition,
                                    {p2p_tool, #domain_P2PToolCondition{
                                        sender_is =
                                            {bank_card, #domain_BankCardCondition{
                                                definition = {issuer_country_is, 'rus'}
                                            }},
                                        receiver_is =
                                            {bank_card, #domain_BankCardCondition{
                                                definition = {issuer_country_is, 'rus'}
                                            }}
                                    }}},
                            then_ = {value, [?prv(101)]}
                        },
                        #domain_ProviderDecision{
                            if_ = {constant, true},
                            then_ = {value, []}
                        }
                    ]},
                payment_system =
                    {decisions, [
                        #domain_PaymentSystemDecision{
                            if_ =
                                {any_of,
                                    ordsets:from_list([
                                        {condition,
                                            {bin_data, #domain_BinDataCondition{
                                                payment_system = {equals, <<"VISA">>},
                                                bank_name = {equals, <<"uber">>}
                                            }}},
                                        {condition,
                                            {bin_data, #domain_BinDataCondition{
                                                payment_system = {equals, <<"VISA">>},
                                                bank_name = {equals, <<"sber">>}
                                            }}}
                                    ])},
                            then_ = {value, ?pmtsys(<<"VISA">>)}
                        },
                        #domain_PaymentSystemDecision{
                            if_ =
                                {any_of,
                                    ordsets:from_list([
                                        {condition,
                                            {bin_data, #domain_BinDataCondition{
                                                payment_system = {equals, <<"NSPK MIR">>},
                                                bank_name = {equals, <<"poopa">>}
                                            }}}
                                    ])},
                            then_ = {value, ?pmtsys(<<"NSPK MIR">>)}
                        }
                    ]}
            }
        }},

        ct_domain:system_account_set(?sas(1), <<"System">>, ?cur(<<"RUB">>), C),

        ct_domain:inspector(?insp(1), <<"Low Life">>, ?prx(1), #{<<"risk_score">> => <<"low">>}),
        ct_domain:p2p_inspector(?p2p_insp(1), <<"Low Life">>, ?prx(4), #{<<"risk_score">> => <<"low">>}),
        ct_domain:proxy(?prx(1), <<"Inspector proxy">>),
        ct_domain:proxy(?prx(2), <<"Mocket proxy">>, <<"http://adapter-mocketbank:8022/proxy/mocketbank/p2p-credit">>),
        ct_domain:proxy(?prx(3), <<"Quote proxy">>, <<"http://localhost:8222/quotebank">>),
        ct_domain:proxy(?prx(4), <<"P2P inspector proxy">>, <<"http://localhost:8222/p2p_inspector">>),
        ct_domain:proxy(?prx(5), <<"P2P adapter">>, <<"http://localhost:8222", P2PAdapterAdr/binary>>),
        ct_domain:proxy(?prx(6), <<"Down proxy">>, <<"http://localhost:8222/downbank">>),
        ct_domain:proxy(?prx(7), <<"Another down proxy">>, <<"http://localhost:8222/downbank2">>),
        ct_domain:proxy(?prx(8), <<"Sleep proxy">>, <<"http://localhost:8222/sleepybank">>),

        ct_domain:withdrawal_provider(?prv(1), ?prx(2), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?prv(2), ?prx(2), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?prv(3), ?prx(3), dummy_provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?prv(4), ?prx(6), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?prv(5), ?prx(2), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?prv(6), ?prx(6), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?prv(7), ?prx(6), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?prv(8), ?prx(2), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?prv(9), ?prx(7), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?prv(10), ?prx(6), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?prv(11), ?prx(8), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?prv(16), ?prx(2), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?prv(17), ?prx(2), provider_identity_id(Options), C),

        ct_domain:p2p_provider(?prv(101), ?prx(5), dummy_provider_identity_id(Options), C),

        ct_domain:contract_template(?tmpl(1), ?trms(1)),
        ct_domain:term_set_hierarchy(?trms(1), [ct_domain:timed_term_set(default_termset(Options))]),
        ct_domain:contract_template(?tmpl(2), ?trms(2)),
        ct_domain:term_set_hierarchy(?trms(2), [ct_domain:timed_term_set(company_termset(Options))]),

        ct_domain:withdrawal_terminal(?trm(1)),
        ct_domain:withdrawal_terminal(?trm(2)),
        ct_domain:withdrawal_terminal(?trm(3)),
        ct_domain:withdrawal_terminal(?trm(4)),
        ct_domain:withdrawal_terminal(?trm(5)),
        ct_domain:withdrawal_terminal(?trm(6)),
        ct_domain:withdrawal_terminal(?trm(7)),
        % Provider 17 satellite
        ct_domain:withdrawal_terminal(?trm(8)),

        ct_domain:p2p_terminal(?trm(101)),
        ct_domain:p2p_terminal(?trm(102)),
        ct_domain:p2p_terminal(?trm(103)),
        ct_domain:p2p_terminal(?trm(104)),
        ct_domain:p2p_terminal(?trm(105)),

        ct_domain:currency(?cur(<<"RUB">>)),
        ct_domain:currency(?cur(<<"USD">>)),
        ct_domain:currency(?cur(<<"EUR">>)),
        ct_domain:currency(?cur(<<"BTC">>)),

        ct_domain:category(?cat(1), <<"Generic Store">>, live),

        ct_domain:payment_method(?pmt(bank_card_deprecated, visa)),
        ct_domain:payment_method(?pmt(bank_card_deprecated, mastercard)),

        ct_domain:payment_system(?pmtsys(<<"VISA">>), <<"VISA">>),
        ct_domain:payment_system(?pmtsys(<<"NSPK MIR">>), <<"NSPK MIR">>)
    ],
    maps:get(domain_config, Options, Default).

default_termset(Options) ->
    Default = #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            currencies = {value, ?ordset([?cur(<<"RUB">>), ?cur(<<"USD">>)])},
            wallet_limit =
                {decisions, [
                    #domain_CashLimitDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(0, <<"RUB">>)},
                                    {exclusive, ?cash(5000001, <<"RUB">>)}
                                )}
                    },
                    #domain_CashLimitDecision{
                        if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                        then_ =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(0, <<"USD">>)},
                                    {exclusive, ?cash(10000001, <<"USD">>)}
                                )}
                    }
                ]},
            withdrawals = #domain_WithdrawalServiceTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                attempt_limit = {value, #domain_AttemptLimit{attempts = 3}},
                cash_limit =
                    {decisions, [
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"RUB">>)},
                                        {exclusive, ?cash(10000001, <<"RUB">>)}
                                    )}
                        },
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"EUR">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"EUR">>)},
                                        {exclusive, ?cash(10000001, <<"EUR">>)}
                                    )}
                        },
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"USD">>)},
                                        {exclusive, ?cash(10000001, <<"USD">>)}
                                    )}
                        }
                    ]},
                cash_flow =
                    {decisions, [
                        % this is impossible cash flow decision to check
                        % if withdrawals cash flow calculates properly
                        #domain_CashFlowDecision{
                            if_ = {
                                condition,
                                {payment_tool, {payment_terminal, #domain_PaymentTerminalCondition{}}}
                            },
                            then_ = {value, []}
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {all_of,
                                    ?ordset([
                                        {condition, {currency_is, ?cur(<<"RUB">>)}},
                                        {condition,
                                            {payment_tool,
                                                {bank_card, #domain_BankCardCondition{
                                                    definition =
                                                        {payment_system, #domain_PaymentSystemCondition{
                                                            payment_system_is_deprecated = visa
                                                        }}
                                                }}}}
                                    ])},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_destination},
                                        ?share(1, 1, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, settlement},
                                        ?share(10, 100, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {all_of,
                                    ?ordset([
                                        {condition, {currency_is, ?cur(<<"EUR">>)}},
                                        {condition,
                                            {payment_tool,
                                                {bank_card, #domain_BankCardCondition{
                                                    definition =
                                                        {payment_system, #domain_PaymentSystemCondition{
                                                            payment_system_is_deprecated = visa
                                                        }}
                                                }}}}
                                    ])},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_destination},
                                        ?share(1, 1, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, settlement},
                                        ?share(10, 100, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {all_of,
                                    ?ordset([
                                        {condition, {currency_is, ?cur(<<"USD">>)}},
                                        {condition,
                                            {payment_tool,
                                                {bank_card, #domain_BankCardCondition{
                                                    definition =
                                                        {payment_system, #domain_PaymentSystemCondition{
                                                            payment_system_is_deprecated = visa
                                                        }}
                                                }}}}
                                    ])},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_destination},
                                        ?share(1, 1, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, settlement},
                                        ?share(10, 100, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {all_of,
                                    ?ordset([
                                        {condition, {currency_is, ?cur(<<"RUB">>)}},
                                        {condition,
                                            {payment_tool, {crypto_currency, #domain_CryptoCurrencyCondition{}}}}
                                    ])},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_destination},
                                        ?share(1, 1, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, settlement},
                                        ?share(10, 100, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {all_of,
                                    ?ordset([
                                        {condition, {currency_is, ?cur(<<"RUB">>)}},
                                        {condition, {payment_tool, {digital_wallet, #domain_DigitalWalletCondition{}}}}
                                    ])},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_destination},
                                        ?share(1, 1, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, settlement},
                                        ?share(10, 100, operation_amount)
                                    ),
                                    ?cfpost(
                                        {wallet, receiver_destination},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        }
                    ]}
            },
            p2p = #domain_P2PServiceTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>), ?cur(<<"USD">>)])},
                allow =
                    {any_of,
                        ordsets:from_list([
                            {condition,
                                {p2p_tool, #domain_P2PToolCondition{
                                    sender_is =
                                        {bank_card, #domain_BankCardCondition{
                                            definition =
                                                {payment_system, #domain_PaymentSystemCondition{
                                                    payment_system_is_deprecated = visa
                                                }}
                                        }},
                                    receiver_is =
                                        {bank_card, #domain_BankCardCondition{
                                            definition =
                                                {payment_system, #domain_PaymentSystemCondition{
                                                    payment_system_is_deprecated = visa
                                                }}
                                        }}
                                }}}
                        ])},
                cash_limit =
                    {decisions, [
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"RUB">>)},
                                        {exclusive, ?cash(10000001, <<"RUB">>)}
                                    )}
                        },
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"EUR">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"EUR">>)},
                                        {exclusive, ?cash(10000001, <<"EUR">>)}
                                    )}
                        },
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"USD">>)},
                                        {exclusive, ?cash(10000001, <<"USD">>)}
                                    )}
                        }
                    ]},
                cash_flow =
                    {decisions, [
                        #domain_CashFlowDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {system, settlement},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {system, settlement},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ = {condition, {currency_is, ?cur(<<"EUR">>)}},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {system, settlement},
                                        {system, subagent},
                                        ?share(10, 100, operation_amount)
                                    )
                                ]}
                        }
                    ]},
                fees =
                    {decisions, [
                        #domain_FeeDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {decisions, [
                                    #domain_FeeDecision{
                                        if_ =
                                            {condition,
                                                {p2p_tool, #domain_P2PToolCondition{
                                                    sender_is =
                                                        {bank_card, #domain_BankCardCondition{
                                                            definition =
                                                                {payment_system, #domain_PaymentSystemCondition{
                                                                    payment_system_is_deprecated = visa
                                                                }}
                                                        }},
                                                    receiver_is =
                                                        {bank_card, #domain_BankCardCondition{
                                                            definition =
                                                                {payment_system, #domain_PaymentSystemCondition{
                                                                    payment_system_is_deprecated = visa
                                                                }}
                                                        }}
                                                }}},
                                        then_ =
                                            {decisions, [
                                                #domain_FeeDecision{
                                                    if_ =
                                                        {condition,
                                                            {cost_in,
                                                                ?cashrng(
                                                                    {inclusive, ?cash(0, <<"RUB">>)},
                                                                    {exclusive, ?cash(7692, <<"RUB">>)}
                                                                )}},
                                                    then_ =
                                                        {value, #domain_Fees{
                                                            fees = #{surplus => ?fixed(50, <<"RUB">>)}
                                                        }}
                                                },
                                                #domain_FeeDecision{
                                                    if_ =
                                                        {condition,
                                                            {cost_in,
                                                                ?cashrng(
                                                                    {inclusive, ?cash(7692, <<"RUB">>)},
                                                                    {exclusive, ?cash(300000, <<"RUB">>)}
                                                                )}},
                                                    then_ =
                                                        {value, #domain_Fees{
                                                            fees = #{surplus => ?share(65, 10000, operation_amount)}
                                                        }}
                                                },
                                                #domain_FeeDecision{
                                                    if_ =
                                                        {condition,
                                                            {cost_in,
                                                                ?cashrng(
                                                                    {inclusive, ?cash(300000, <<"RUB">>)},
                                                                    {exclusive, ?cash(20000001, <<"RUB">>)}
                                                                )}},
                                                    then_ =
                                                        {value, #domain_Fees{
                                                            fees = #{surplus => ?fixed(50, <<"RUB">>)}
                                                        }}
                                                }
                                            ]}
                                    },
                                    #domain_FeeDecision{
                                        if_ =
                                            {condition,
                                                {p2p_tool, #domain_P2PToolCondition{
                                                    sender_is =
                                                        {bank_card, #domain_BankCardCondition{
                                                            definition =
                                                                {payment_system, #domain_PaymentSystemCondition{
                                                                    payment_system_is_deprecated = visa
                                                                }}
                                                        }},
                                                    receiver_is =
                                                        {bank_card, #domain_BankCardCondition{
                                                            definition =
                                                                {payment_system, #domain_PaymentSystemCondition{
                                                                    payment_system_is_deprecated = nspkmir
                                                                }}
                                                        }}
                                                }}},
                                        then_ =
                                            {decisions, [
                                                #domain_FeeDecision{
                                                    if_ =
                                                        {condition,
                                                            {cost_in,
                                                                ?cashrng(
                                                                    {inclusive, ?cash(0, <<"RUB">>)},
                                                                    {exclusive, ?cash(7692, <<"RUB">>)}
                                                                )}},
                                                    then_ =
                                                        {value, #domain_Fees{
                                                            fees = #{surplus => ?fixed(50, <<"RUB">>)}
                                                        }}
                                                },
                                                #domain_FeeDecision{
                                                    if_ =
                                                        {condition,
                                                            {cost_in,
                                                                ?cashrng(
                                                                    {inclusive, ?cash(7692, <<"RUB">>)},
                                                                    {exclusive, ?cash(300000, <<"RUB">>)}
                                                                )}},
                                                    then_ =
                                                        {value, #domain_Fees{
                                                            fees = #{surplus => ?share(65, 10000, operation_amount)}
                                                        }}
                                                }
                                            ]}
                                    }
                                ]}
                        },
                        #domain_FeeDecision{
                            if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                            then_ =
                                {value, #domain_Fees{
                                    fees = #{surplus => ?share(1, 1, operation_amount)}
                                }}
                        },
                        #domain_FeeDecision{
                            if_ = {condition, {currency_is, ?cur(<<"EUR">>)}},
                            then_ =
                                {value, #domain_Fees{
                                    fees = #{surplus => ?share(1, 1, operation_amount)}
                                }}
                        }
                    ]},
                quote_lifetime =
                    {value,
                        {interval, #domain_LifetimeInterval{
                            days = 1,
                            minutes = 1,
                            seconds = 1
                        }}},
                templates = #domain_P2PTemplateServiceTerms{
                    allow = {condition, {currency_is, ?cur(<<"RUB">>)}}
                }
            },
            w2w = #domain_W2WServiceTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>), ?cur(<<"USD">>)])},
                allow = {constant, true},
                cash_limit =
                    {decisions, [
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"RUB">>)},
                                        {exclusive, ?cash(10000001, <<"RUB">>)}
                                    )}
                        },
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"EUR">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"EUR">>)},
                                        {exclusive, ?cash(10000001, <<"EUR">>)}
                                    )}
                        },
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"USD">>)},
                                        {exclusive, ?cash(10000001, <<"USD">>)}
                                    )}
                        }
                    ]},
                cash_flow =
                    {decisions, [
                        #domain_CashFlowDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_settlement},
                                        ?share(1, 1, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_settlement},
                                        ?share(1, 1, operation_amount)
                                    )
                                ]}
                        },
                        #domain_CashFlowDecision{
                            if_ = {condition, {currency_is, ?cur(<<"EUR">>)}},
                            then_ =
                                {value, [
                                    ?cfpost(
                                        {wallet, sender_settlement},
                                        {wallet, receiver_settlement},
                                        ?share(1, 1, operation_amount)
                                    )
                                ]}
                        }
                    ]},
                fees =
                    {decisions, [
                        #domain_FeeDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {value, #domain_Fees{
                                    fees = #{surplus => ?share(1, 1, operation_amount)}
                                }}
                        },
                        #domain_FeeDecision{
                            if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                            then_ =
                                {value, #domain_Fees{
                                    fees = #{surplus => ?share(1, 1, operation_amount)}
                                }}
                        },
                        #domain_FeeDecision{
                            if_ = {condition, {currency_is, ?cur(<<"EUR">>)}},
                            then_ =
                                {value, #domain_Fees{
                                    fees = #{surplus => ?share(1, 1, operation_amount)}
                                }}
                        }
                    ]}
            }
        }
    },
    maps:get(default_termset, Options, Default).

company_termset(Options) ->
    Default = #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            currencies = {value, ?ordset([?cur(<<"RUB">>), ?cur(<<"USD">>)])},
            wallet_limit =
                {decisions, [
                    #domain_CashLimitDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(0, <<"RUB">>)},
                                    {exclusive, ?cash(5000000, <<"RUB">>)}
                                )}
                    },
                    #domain_CashLimitDecision{
                        if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                        then_ =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(0, <<"USD">>)},
                                    {exclusive, ?cash(5000000, <<"USD">>)}
                                )}
                    }
                ]}
        }
    },
    maps:get(company_termset, Options, Default).

routing_ruleset(Ref, Name, Decisions) ->
    {routing_rules, #domain_RoutingRulesObject{
        ref = Ref,
        data = #domain_RoutingRuleset{
            name = Name,
            decisions = Decisions
        }
    }}.

condition(cost_in, {Min, Max, Cur}) ->
    {condition,
        {cost_in,
            ?cashrng(
                {inclusive, ?cash(Min, Cur)},
                {exclusive, ?cash(Max, Cur)}
            )}};
condition(party, ID) ->
    {condition, {party, #domain_PartyCondition{id = ID}}}.

delegate(Allowed, RuleSetRef) ->
    #domain_RoutingDelegate{
        description = <<"Delagate description">>,
        allowed = Allowed,
        ruleset = RuleSetRef
    }.

candidate(Allowed, Terminal) ->
    #domain_RoutingCandidate{
        description = <<"Candidate description">>,
        allowed = Allowed,
        terminal = Terminal
    }.
