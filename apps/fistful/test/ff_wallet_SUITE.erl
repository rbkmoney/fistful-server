-module(ff_wallet_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_missing_fails/1]).
-export([create_missing_identity_fails/1]).
-export([create_missing_currency_fails/1]).
-export([create_wallet_ok/1]).

-spec get_missing_fails(config()) -> test_return().
-spec create_missing_identity_fails(config()) -> test_return().
-spec create_missing_currency_fails(config()) -> test_return().
-spec create_wallet_ok(config()) -> test_return().

%%

-import(ct_helper, [cfg/2]).
-import(ff_pipeline, [unwrap/1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        get_missing_fails,
        create_missing_identity_fails,
        create_missing_currency_fails,
        create_wallet_ok
    ].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    BeConf = #{schema => machinery_mg_schema_generic},
    Be = {machinery_mg_backend, BeConf#{
        client => ff_woody_client:new("http://machinegun:8022/v1/automaton")
    }},
    {StartedApps, _StartupCtx} = ct_helper:start_apps([
        scoper,
        woody,
        dmt_client,
        {fistful, [
            {services, #{
                'partymgmt' => "http://hellgate:8022/v1/processing/partymgmt",
                'accounter' => "http://shumway:8022/accounter"
            }},
            {backends, #{
                'ff/identity'  => Be,
                'ff/wallet_v2' => Be
            }},
            {providers,
                get_provider_config()
            }
        ]}
    ]),
    SuiteSup = ct_sup:start(),
    BeOpts = #{event_handler => scoper_woody_event_handler},
    Routes = machinery_mg_backend:get_routes(
        [
            {{fistful, ff_identity_machine},
                #{path => <<"/v1/stateproc/ff/identity">>, backend_config => BeConf}},
            {{fistful, ff_wallet_machine},
                #{path => <<"/v1/stateproc/ff/wallet_v2">>, backend_config => BeConf}}
        ],
        BeOpts
    ),
    {ok, _} = supervisor:start_child(SuiteSup, woody_server:child_spec(
        ?MODULE,
        BeOpts#{
            ip                => {0, 0, 0, 0},
            port              => 8022,
            handlers          => [],
            additional_routes => Routes
        }
    )),
    C1 = ct_helper:makeup_cfg(
        [ct_helper:test_case_name(init), ct_helper:woody_ctx()],
        [
            {started_apps , StartedApps},
            {suite_sup    , SuiteSup},
            {services     , #{
                'accounter' => ff_woody_client:new("http://shumway:8022/accounter")
            }}
        | C]
    ),
    ok = ct_domain_config:upsert(get_domain_config(C1)),
    ok = timer:sleep(1000),
    C1.

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = ct_sup:stop(cfg(suite_sup, C)),
    ok = ct_helper:stop_apps(cfg(started_apps, C)),
    ok.

%%

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ff_woody_ctx:set(ct_helper:get_woody_ctx(C1)),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok = ff_woody_ctx:unset().

%%

get_missing_fails(_C) ->
    {error, notfound} = ff_wallet_machine:get(genlib:unique()).

create_missing_identity_fails(_C) ->
    ID = genlib:unique(),
    {error, {identity, notfound}} = ff_wallet_machine:create(
        ID,
        #{
            identity => genlib:unique(),
            name     => <<"HAHA NO">>,
            currency => <<"RUB">>
        },
        ff_ctx:new()
    ).

create_missing_currency_fails(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    IdentityID = create_identity(Party, C),
    {error, {currency, notfound}} = ff_wallet_machine:create(
        ID,
        #{
            identity => IdentityID,
            name     => <<"HAHA YES">>,
            currency => <<"EOS">>
        },
        ff_ctx:new()
    ).

create_wallet_ok(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    IdentityID = create_identity(Party, C),
    ok = ff_wallet_machine:create(
        ID,
        #{
            identity => IdentityID,
            name     => <<"HAHA YES">>,
            currency => <<"RUB">>
        },
        ff_ctx:new()
    ),
    Wallet = ff_wallet_machine:wallet(unwrap(ff_wallet_machine:get(ID))),
    {ok, accessible} = ff_wallet:is_accessible(Wallet),
    Account = ff_account:accounter_account_id(ff_wallet:account(Wallet)),
    {ok, {Amount, <<"RUB">>}} = ff_transaction:balance(Account),
    0 = ff_indef:current(Amount),
    ok.

%%

-include_lib("ff_cth/include/ct_domain.hrl").

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        ID,
        #{
            party    => Party,
            provider => ProviderID,
            class    => ClassID
        },
        ff_ctx:new()
    ),
    ID.

get_provider_config() ->
    #{
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
                        }
                    }
                }
            }
        }
    }.

get_domain_config(C) ->
    [

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
        ct_domain:term_set_hierarchy(?trms(1), [ct_domain:timed_term_set(get_default_termset())]),

        ct_domain:currency(?cur(<<"RUB">>)),
        ct_domain:currency(?cur(<<"USD">>)),

        ct_domain:category(?cat(1), <<"Generic Store">>, live),

        ct_domain:payment_method(?pmt(bank_card, visa)),
        ct_domain:payment_method(?pmt(bank_card, mastercard))

    ].

get_default_termset() ->
    #domain_TermSet{
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
    }.
