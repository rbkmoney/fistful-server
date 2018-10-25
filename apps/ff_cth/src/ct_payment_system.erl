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
    company_termset => dmsl_domain_thrift:'TermSet'()
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
do_setup(Options, C0) ->
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
        lager,
        scoper,
        woody,
        dmt_client,
        {fistful, [
            {services, services(Options)},
            {backends, maps:from_list([{NS, Be} || NS <- [
                'ff/identity'              ,
                'ff/sequence'              ,
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
            construct_handler(ff_wallet_machine             , "wallet_v2"             , BeConf),
            construct_handler(ff_instrument_machine         , "source_v1"             , BeConf),
            construct_handler(ff_transfer_machine           , "deposit_v1"            , BeConf),
            construct_handler(ff_instrument_machine         , "destination_v2"        , BeConf),
            construct_handler(ff_transfer_machine           , "withdrawal_v2"         , BeConf),
            construct_handler(ff_withdrawal_session_machine , "withdrawal/session_v2" , BeConf)
        ],
        BeOpts
    ),
    AdminRoutes = get_admin_routes(),
    {ok, _} = supervisor:start_child(SuiteSup, woody_server:child_spec(
        ?MODULE,
        BeOpts#{
            ip                => {0, 0, 0, 0},
            port              => 8022,
            handlers          => [],
            additional_routes => AdminRoutes ++ Routes
        }
    )),
    Processing = #{
        started_apps => StartedApps,
        suite_sup    => SuiteSup
    },
    {ok, Processing}.

setup_dominant(Options, C) ->
    ok = ct_domain_config:upsert(domain_config(Options, C)).

configure_processing_apps(_Options) ->
    ok = set_app_env(
        [ff_transfer, withdrawal, system, accounts, <<"RUB">>],
        create_company_account()
    ),
    ok = set_app_env(
        [ff_transfer, withdrawal, provider, <<"mocketbank">>, accounts, <<"RUB">>],
        create_company_account()
    ).

construct_handler(Module, Suffix, BeConf) ->
    {{fistful, Module},
        #{path => ff_string:join(["/v1/stateproc/ff/", Suffix]), backend_config => BeConf}}.

get_admin_routes() ->
    Path = <<"/v1/admin">>,
    woody_server_thrift_http_handler:get_routes(#{
        handlers => [{Path, {{ff_proto_fistful_thrift, 'FistfulAdmin'}, {ff_server_handler, []}}}],
        event_handler => scoper_woody_event_handler
    }).

create_company_account() ->
    PartyID = create_party(),
    IdentityID = create_company_identity(PartyID),
    {ok, Currency} = ff_currency:get(<<"RUB">>),
    {ok, IdentityMachine} = ff_identity_machine:get(IdentityID),
    Identity = ff_identity_machine:identity(IdentityMachine),
    {ok, [{created, Account}]} = ff_account:create(PartyID, Identity, Currency),
    Account.

create_company_identity(Party) ->
    create_identity(Party, <<"good-one">>, <<"church">>).

create_party() ->
    ID = genlib:unique(),
    _ = ff_party:create(ID),
    ID.

create_identity(Party, ProviderID, ClassID) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        ID,
        #{party => Party, provider => ProviderID, class => ClassID},
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
    maps:get(withdrawal_provider_config, Options, Default).

services(Options) ->
    Default = #{
        accounter      => "http://shumway:8022/accounter",
        cds            => "http://cds:8022/v1/storage",
        identdocstore  => "http://cds:8022/v1/identity_document_storage",
        partymgmt      => "http://hellgate:8022/v1/processing/partymgmt",
        identification => "http://identification:8022/v1/identification"
    },
    maps:get(services, Options, Default).

%%

-include_lib("ff_cth/include/ct_domain.hrl").

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
                realm                     = live
            }
        }},

        ct_domain:system_account_set(?sas(1), <<"System">>, ?cur(<<"RUB">>), C),

        ct_domain:inspector(?insp(1), <<"Low Life">>, ?prx(1), #{<<"risk_score">> => <<"low">>}),
        ct_domain:proxy(?prx(1), <<"Inspector proxy">>),

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
            currencies = {value, ?ordset([?cur(<<"RUB">>)])},
            wallet_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(       0, <<"RUB">>)},
                        {exclusive, ?cash(10000001, <<"RUB">>)}
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
                    #domain_CashFlowDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
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
            currencies = {value, ?ordset([?cur(<<"RUB">>)])},
            wallet_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(       0, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )}
                }
            ]}
        }
    },
    maps:get(company_termset, Options, Default).
