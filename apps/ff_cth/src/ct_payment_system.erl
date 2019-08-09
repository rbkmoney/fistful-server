-module(ct_payment_system).

-export([setup/0]).
-export([setup/1]).
-export([shutdown/1]).

%% API types

-type options() :: #{
    machinery_backend_config => map(),
    machinery_backend_options => map(),
    identity_provider_config => map(),
    withdrawal_provider_config => #{id() => ff_withdrawal_provider:provider()},
    services => map(),
    domain_config => list(),
    default_termset => dmsl_domain_thrift:'TermSet'(),
    company_termset => dmsl_domain_thrift:'TermSet'(),
    payment_inst_identity_id => id(),
    quote_payment_inst_identity_id => id(),
    provider_identity_id => id(),
    quote_provider_identity_id => id(),
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
        quote_payment_inst_identity_id => genlib:unique(),
        provider_identity_id => genlib:unique(),
        quote_provider_identity_id => genlib:unique()
    },
    {ok, Processing0} = start_processing_apps(Options),
    C1 = ct_helper:makeup_cfg([ct_helper:woody_ctx()], [{services, services(Options)} | C0]),
    ok = ff_woody_ctx:set(ct_helper:get_woody_ctx(C1)),
    ok = setup_dominant(Options, C1),
    ok = timer:sleep(3000),
    ok = configure_processing_apps(Options),
    ok = ff_woody_ctx:unset(),
    [{payment_system, Processing0} | C1].

start_processing_apps(Options) ->
    BeConf = machinery_backend_config(Options),
    Be = {machinery_mg_backend, BeConf#{
        client => ff_woody_client:new(<<"http://machinegun:8022/v1/automaton">>)
    }},
    {StartedApps, _StartupCtx} = ct_helper:start_apps([
        {sasl, [{sasl_error_logger, false}]},
        scoper,
        woody,
        dmt_client,
        {fistful, [
            {services, services(Options)},
            {backends, maps:from_list([{NS, Be} || NS <- [
                'ff/identity'              ,
                'ff/sequence'              ,
                'ff/external_id'           ,
                'ff/wallet_v2'             ,
                'ff/source_v1'             ,
                'ff/deposit_v1'            ,
                'ff/destination_v2'        ,
                'ff/withdrawal_v2'         ,
                'ff/withdrawal/session_v2'
            ]])},
            {providers, identity_provider_config(Options)}
        ]},
        {ff_transfer, [
            {withdrawal,
                #{provider => withdrawal_provider_config(Options)}
            }
        ]}
    ]),
    SuiteSup = ct_sup:start(),
    BeOpts = machinery_backend_options(Options),
    Routes = machinery_mg_backend:get_routes(
        [
            construct_handler(ff_identity_machine           , "identity"              , BeConf),
            construct_handler(ff_sequence                   , "sequence"              , BeConf),
            construct_handler(ff_external_id                , "external_id"           , BeConf),
            construct_handler(ff_wallet_machine             , "wallet_v2"             , BeConf),
            construct_handler(ff_instrument_machine         , "source_v1"             , BeConf),
            construct_handler(ff_transfer_machine           , "deposit_v1"            , BeConf),
            construct_handler(ff_instrument_machine         , "destination_v2"        , BeConf),
            construct_handler(ff_transfer_machine           , "withdrawal_v2"         , BeConf),
            construct_handler(ff_withdrawal_session_machine , "withdrawal/session_v2" , BeConf)
        ],
        BeOpts
    ),

    AdminRoutes      = get_admin_routes(),
    WalletRoutes     = ff_server:get_routes(
        {<<"/v1/wallet">>, {{ff_proto_wallet_thrift, 'Management'}, {ff_wallet_handler, []}}, #{}}),
    DestRoutes       = ff_server:get_routes(
        {<<"/v1/destination">>, {{ff_proto_destination_thrift, 'Management'}, {ff_destination_handler, []}}, #{}}),
    WithdrawalRoutes = ff_server:get_routes(
        {<<"/v1/withdrawal">>, {{ff_proto_withdrawal_thrift, 'Management'}, {ff_withdrawal_handler, []}}, #{}}),
    IdentityRoutes   = ff_server:get_routes(
        {<<"/v1/identity">>, {{ff_proto_identity_thrift, 'Management'}, {ff_identity_handler, []}}, #{}}),
    DummyProviderRoute = ff_server:get_routes(
        {<<"/quotebank">>, {{dmsl_withdrawals_provider_adapter_thrift, 'Adapter'}, {ff_ct_provider_handler, []}}, #{}}),
    DummyBinbaseRoute = ff_server:get_routes(
        {<<"/binbase">>, {{binbase_binbase_thrift, 'Binbase'}, {ff_ct_binbase_handler, []}}, #{}}),
    RepairRoutes     = get_repair_routes(),
    EventsinkRoutes  = get_eventsink_routes(BeConf),
    {ok, _} = supervisor:start_child(SuiteSup, woody_server:child_spec(
        ?MODULE,
        BeOpts#{
            ip                => {0, 0, 0, 0},
            port              => 8022,
            handlers          => [],

            additional_routes => lists:flatten([
                Routes,
                AdminRoutes,
                WalletRoutes,
                DestRoutes,
                WithdrawalRoutes,
                IdentityRoutes,
                EventsinkRoutes,
                RepairRoutes,
                DummyProviderRoute,
                DummyBinbaseRoute
            ])
        }
    )),
    Processing = #{
        started_apps => StartedApps ++ start_optional_apps(Options),
        suite_sup    => SuiteSup
    },
    {ok, Processing}.

start_optional_apps(#{optional_apps := Apps})->
    {StartedApps, _StartupCtx} = ct_helper:start_apps(Apps),
    StartedApps;
start_optional_apps(_)->
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
    PIIID = quote_payment_inst_identity_id(Options),
    PRIID = quote_provider_identity_id(Options),
    ok = create_crunch_identity(PIIID, PRIID, <<"quote-owner">>).

construct_handler(Module, Suffix, BeConf) ->
    {{fistful, Module},
        #{path => ff_string:join(["/v1/stateproc/ff/", Suffix]), backend_config => BeConf}}.

get_admin_routes() ->
    Path = <<"/v1/admin">>,
    woody_server_thrift_http_handler:get_routes(#{
        handlers => [{Path, {{ff_proto_fistful_thrift, 'FistfulAdmin'}, {ff_server_handler, []}}}],
        event_handler => scoper_woody_event_handler
    }).

get_eventsink_routes(BeConf) ->
    IdentityRoute = create_sink_route({<<"/v1/eventsink/identity">>,
        {{ff_proto_identity_thrift, 'EventSink'}, {ff_eventsink_handler,
        make_sink_handler_cfg(<<"ff/identity">>, ff_identity_eventsink_publisher, BeConf)}}}),
    WalletRoute = create_sink_route({<<"/v1/eventsink/wallet">>,
        {{ff_proto_wallet_thrift, 'EventSink'}, {ff_eventsink_handler,
        make_sink_handler_cfg(<<"ff/wallet_v2">>, ff_wallet_eventsink_publisher, BeConf)}}}),
    WithdrawalSessionRoute = create_sink_route({<<"/v1/eventsink/withdrawal/session">>,
        {{ff_proto_withdrawal_session_thrift, 'EventSink'}, {ff_eventsink_handler,
            make_sink_handler_cfg(
                <<"ff/withdrawal/session_v2">>,
                ff_withdrawal_session_eventsink_publisher,
                BeConf
            )
        }}}),
    WithdrawalRoute = create_sink_route({<<"/v1/eventsink/withdrawal">>,
        {{ff_proto_withdrawal_thrift, 'EventSink'}, {ff_eventsink_handler,
        make_sink_handler_cfg(<<"ff/withdrawal_v2">>, ff_withdrawal_eventsink_publisher, BeConf)}}}),
    DestinationRoute = create_sink_route({<<"/v1/eventsink/destination">>,
        {{ff_proto_destination_thrift, 'EventSink'}, {ff_eventsink_handler,
        make_sink_handler_cfg(<<"ff/destination_v2">>, ff_destination_eventsink_publisher, BeConf)}}}),
    SourceRoute = create_sink_route({<<"/v1/eventsink/source">>,
        {{ff_proto_source_thrift, 'EventSink'}, {ff_eventsink_handler,
        make_sink_handler_cfg(<<"ff/source_v1">>, ff_source_eventsink_publisher, BeConf)}}}),
    DepositRoute = create_sink_route({<<"/v1/eventsink/deposit">>,
        {{ff_proto_deposit_thrift, 'EventSink'}, {ff_eventsink_handler,
        make_sink_handler_cfg(<<"ff/deposit_v1">>, ff_deposit_eventsink_publisher, BeConf)}}}),
    lists:flatten([
        IdentityRoute,
        WalletRoute,
        WithdrawalRoute,
        WithdrawalSessionRoute,
        DestinationRoute,
        SourceRoute,
        DepositRoute
    ]).

get_repair_routes() ->
    Handlers = [
        {
            <<"withdrawal/session">>,
            {{ff_proto_withdrawal_session_thrift, 'Repairer'}, {ff_withdrawal_session_repair, #{}}}
        }
    ],
    woody_server_thrift_http_handler:get_routes(genlib_map:compact(#{
        handlers => [{<<"/v1/repair/", N/binary>>, H} || {N, H} <- Handlers],
        event_handler => scoper_woody_event_handler
    })).

create_crunch_identity(Options) ->
    PaymentInstIdentityID = payment_inst_identity_id(Options),
    ProviderIdentityID = provider_identity_id(Options),
    create_crunch_identity(PaymentInstIdentityID, ProviderIdentityID, <<"good-one">>).
create_crunch_identity(PIIID, PRIID, ProviderID) ->
    PartyID = create_party(),
    PIIID = create_identity(PIIID, PartyID, ProviderID, <<"church">>),
    PRIID = create_identity(PRIID, PartyID, ProviderID, <<"church">>),
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
    create_identity(ID, PartyID, ProviderID, ClassID).

create_identity(ID, PartyID, ProviderID, ClassID) ->
    ok = ff_identity_machine:create(
        ID,
        #{party => PartyID, provider => ProviderID, class => ClassID},
        ff_ctx:new()
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

create_sink_route({Path, {Module, {Handler, Cfg}}}) ->
    NewCfg = Cfg#{
        client => #{
            event_handler => scoper_woody_event_handler,
            url => "http://machinegun:8022/v1/event_sink"
        }},
    woody_server_thrift_http_handler:get_routes(genlib_map:compact(#{
        handlers => [{Path, {Module, {Handler, NewCfg}}}],
        event_handler => scoper_woody_event_handler
    })).

make_sink_handler_cfg(NS, Publisher, Cfg) ->
    Cfg#{
        ns => NS,
        publisher => Publisher,
        start_event => 0
    }.

%% Default options

machinery_backend_config(Options) ->
    maps:get(machinery_backend_config, Options, #{schema => machinery_mg_schema_generic}).

machinery_backend_options(Options) ->
    maps:get(machinery_backend_options, Options, #{event_handler => scoper_woody_event_handler}).

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
                            name   => <<"Initiation by sword">>,
                            base   => <<"peasant">>,
                            target => <<"nobleman">>
                        }
                    }
                },
                <<"church">>          => #{
                    name                 => <<"Well, a Сhurch">>,
                    contract_template_id => 2,
                    initial_level        => <<"mainline">>,
                    levels               => #{
                        <<"mainline">>    => #{
                            name               => <<"Well, a mainline Сhurch">>,
                            contractor_level   => full
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
                            name   => <<"Initiation by sword">>,
                            base   => <<"peasant">>,
                            target => <<"nobleman">>
                        }
                    }
                },
                <<"church">>          => #{
                    name                 => <<"Well, a Сhurch">>,
                    contract_template_id => 2,
                    initial_level        => <<"mainline">>,
                    levels               => #{
                        <<"mainline">>    => #{
                            name               => <<"Well, a mainline Сhurch">>,
                            contractor_level   => full
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
                            name   => <<"Initiation by sword">>,
                            base   => <<"peasant">>,
                            target => <<"nobleman">>
                        }
                    }
                },
                <<"church">>          => #{
                    name                 => <<"Well, a Сhurch">>,
                    contract_template_id => 2,
                    initial_level        => <<"mainline">>,
                    levels               => #{
                        <<"mainline">>    => #{
                            name               => <<"Well, a mainline Сhurch">>,
                            contractor_level   => full
                        }
                    }
                }
            }
        }
    },
    maps:get(identity_provider_config, Options, Default).

-spec withdrawal_provider_config(options()) ->
    #{id() => ff_withdrawal_provider:provider()}.
withdrawal_provider_config(Options) ->
    Default = #{
        <<"mocketbank">> => #{
            adapter => ff_woody_client:new(<<"http://adapter-mocketbank:8022/proxy/mocketbank/p2p-credit">>),
            accounts => #{},
            fee => #{
                <<"RUB">> => #{
                    postings => [
                        #{
                            sender => {system, settlement},
                            receiver => {provider, settlement},
                            volume => {product, {min_of, [
                                {fixed, {10, <<"RUB">>}},
                                {share, {genlib_rational:new(5, 100), operation_amount, round_half_towards_zero}}
                            ]}}
                        }
                    ]
                }
            }
        },
        <<"quotebank">> => #{
            adapter => ff_woody_client:new(<<"http://localhost:8022/quotebank">>),
            accounts => #{},
            fee => #{
                <<"RUB">> => #{
                    postings => [
                        #{
                            sender => {system, settlement},
                            receiver => {provider, settlement},
                            volume => {product, {min_of, [
                                {fixed, {10, <<"RUB">>}},
                                {share, {genlib_rational:new(5, 100), operation_amount, round_half_towards_zero}}
                            ]}}
                        }
                    ]
                }
            }
        }
    },
    maps:get(withdrawal_provider_config, Options, Default).

services(Options) ->
    Default = #{
        accounter      => "http://shumway:8022/accounter",
        cds            => "http://cds:8022/v1/storage",
        identdocstore  => "http://cds:8022/v1/identity_document_storage",
        partymgmt      => "http://hellgate:8022/v1/processing/partymgmt",
        identification => "http://identification:8022/v1/identification",
        binbase        => "http://localhost:8022/binbase"
    },
    maps:get(services, Options, Default).

%%

-include_lib("ff_cth/include/ct_domain.hrl").

payment_inst_identity_id(Options) ->
    maps:get(payment_inst_identity_id, Options).

provider_identity_id(Options) ->
    maps:get(provider_identity_id, Options).

quote_payment_inst_identity_id(Options) ->
    maps:get(quote_payment_inst_identity_id, Options).

quote_provider_identity_id(Options) ->
    maps:get(quote_provider_identity_id, Options).

domain_config(Options, C) ->
    Default = [

        ct_domain:globals(?eas(1), [?payinst(1)]),
        ct_domain:external_account_set(?eas(1), <<"Default">>, ?cur(<<"RUB">>), C),

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?payinst(1),
            data = #domain_PaymentInstitution{
                name                      = <<"Generic Payment Institution">>,
                system_account_set        = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers                 = {value, ?ordset([])},
                inspector                 = {value, ?insp(1)},
                residences                = ['rus'],
                realm                     = live,
                wallet_system_account_set = {value, ?sas(1)},
                identity                  = payment_inst_identity_id(Options),
                withdrawal_providers      = {decisions, [
                    #domain_WithdrawalProviderDecision{
                        if_ = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{}}}},
                        then_ = {value, [?wthdr_prv(1)]}
                    },
                    #domain_WithdrawalProviderDecision{
                        if_ = {condition, {payment_tool, {crypto_currency, #domain_CryptoCurrencyCondition{}}}},
                        then_ = {value, [?wthdr_prv(2)]}
                    }
                ]}
            }
        }},

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?payinst(2),
            data = #domain_PaymentInstitution{
                name                      = <<"Generic Payment Institution">>,
                system_account_set        = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers                 = {value, ?ordset([])},
                inspector                 = {value, ?insp(1)},
                residences                = ['rus'],
                realm                     = live,
                wallet_system_account_set = {value, ?sas(1)},
                identity                  = quote_payment_inst_identity_id(Options),
                withdrawal_providers      = {value, [?wthdr_prv(3)]}
            }
        }},

        ct_domain:system_account_set(?sas(1), <<"System">>, ?cur(<<"RUB">>), C),

        ct_domain:inspector(?insp(1), <<"Low Life">>, ?prx(1), #{<<"risk_score">> => <<"low">>}),
        ct_domain:proxy(?prx(1), <<"Inspector proxy">>),
        ct_domain:proxy(?prx(2), <<"Mocket proxy">>, <<"http://adapter-mocketbank:8022/proxy/mocketbank/p2p-credit">>),
        ct_domain:proxy(?prx(3), <<"Quote proxy">>, <<"http://localhost:8022/quotebank">>),

        ct_domain:withdrawal_provider(?wthdr_prv(1), ?prx(2), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?wthdr_prv(2), ?prx(2), provider_identity_id(Options), C),
        ct_domain:withdrawal_provider(?wthdr_prv(3), ?prx(3), quote_provider_identity_id(Options), C),

        ct_domain:contract_template(?tmpl(1), ?trms(1)),
        ct_domain:term_set_hierarchy(?trms(1), [ct_domain:timed_term_set(default_termset(Options))]),
        ct_domain:contract_template(?tmpl(2), ?trms(2)),
        ct_domain:term_set_hierarchy(?trms(2), [ct_domain:timed_term_set(company_termset(Options))]),

        ct_domain:currency(?cur(<<"RUB">>)),
        ct_domain:currency(?cur(<<"USD">>)),

        ct_domain:category(?cat(1), <<"Generic Store">>, live),

        ct_domain:payment_method(?pmt(bank_card, visa)),
        ct_domain:payment_method(?pmt(bank_card, mastercard))

    ],
    maps:get(domain_config, Options, Default).

default_termset(Options) ->
    Default = #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            currencies = {value, ?ordset([?cur(<<"RUB">>), ?cur(<<"USD">>)])},
            wallet_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(       0, <<"RUB">>)},
                        {exclusive, ?cash(10000001, <<"RUB">>)}
                    )}
                },
                #domain_CashLimitDecision{
                    if_   = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(       0, <<"USD">>)},
                        {exclusive, ?cash(10000000, <<"USD">>)}
                    )}
                }
            ]},
            withdrawals = #domain_WithdrawalServiceTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                cash_limit = {decisions, [
                    #domain_CashLimitDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, ?cashrng(
                            {inclusive, ?cash(       0, <<"RUB">>)},
                            {exclusive, ?cash(10000000, <<"RUB">>)}
                        )}
                    }
                ]},
                cash_flow = {decisions, [
                    % this is impossible cash flow decision to check
                    % if withdrawals cash flow calculates properly
                    #domain_CashFlowDecision{
                        if_   = {
                            condition,
                            {payment_tool, {payment_terminal, #domain_PaymentTerminalCondition{}}}
                        },
                        then_ = {value, []}
                    },
                    #domain_CashFlowDecision{
                        if_   = {all_of, ?ordset([
                            {condition, {currency_is, ?cur(<<"RUB">>)}},
                            {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system, #domain_PaymentSystemCondition{
                                    payment_system_is = visa
                                }}
                            }}}}
                        ])},
                        then_ = {value, [
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
                        if_   = {all_of, ?ordset([
                            {condition, {currency_is, ?cur(<<"RUB">>)}},
                            {condition, {payment_tool, {crypto_currency, #domain_CryptoCurrencyCondition{}}}}
                        ])},
                        then_ = {value, [
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
            }
        }
    },
    maps:get(default_termset, Options, Default).

company_termset(Options) ->
    Default = #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            currencies = {value, ?ordset([?cur(<<"RUB">>), ?cur(<<"USD">>)])},
            wallet_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(       0, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )}
                },
                #domain_CashLimitDecision{
                    if_   = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(       0, <<"USD">>)},
                        {exclusive, ?cash(10000000, <<"USD">>)}
                    )}
                }
            ]}
        }
    },
    maps:get(company_termset, Options, Default).
