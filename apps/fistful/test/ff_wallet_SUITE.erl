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
    IBO = #{name => {?MODULE, identities}},
    WBO = #{name => {?MODULE, wallets}},
    {StartedApps, _StartupCtx} = ct_helper:start_apps([
        lager,
        scoper,
        {dmt_client, [
            {max_cache_size, #{
                elements => 1
            }},
            {service_urls, #{
                'Repository'       => <<"http://dominant:8022/v1/domain/repository">>,
                'RepositoryClient' => <<"http://dominant:8022/v1/domain/repository_client">>
            }}
        ]},
        {fistful, [
            {services, #{
                'partymgmt' => ff_woody_client:new("http://hellgate:8022/v1/processing/partymgmt"),
                'accounter' => ff_woody_client:new("http://shumway:8022/accounter")
            }},
            {backends, #{
                'identity'  => machinery_gensrv_backend:new(IBO),
                'wallet'    => machinery_gensrv_backend:new(WBO)
            }},
            {providers,
                get_provider_config()
            }
        ]}
    ]),
    SuiteSup = ct_sup:start(),
    {ok, _} = supervisor:start_child(SuiteSup, machinery_gensrv_backend:child_spec(ff_identity_machine, IBO)),
    {ok, _} = supervisor:start_child(SuiteSup, machinery_gensrv_backend:child_spec(ff_wallet_machine, WBO)),
    C1 = ct_helper:makeup_cfg(
        [ct_helper:test_case_name(init), ct_helper:woody_ctx()],
        [
            {started_apps , StartedApps},
            {suite_sup    , SuiteSup},
            {clients      , #{
                'accounter' => ff_woody_client:new("http://shumway:8022/accounter")
            }}
        | C]
    ),
    ok = ct_domain_config:upsert(get_domain_config(C1)),
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
    {ok, W} = ff_wallet_machine:get(ID),
    {ok, accessible} = ff_wallet:is_accessible(W),
    {ok, Account} = ff_wallet:account(W),
    {ok, {Amount, <<"RUB">>}} = ff_transaction:balance(Account),
    0 = ff_indef:current(Amount),
    ok.

%%

-include_lib("ff_cth/include/ct_domain.hrl").

create_party(_C) ->
    ID = genlib:unique(),
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

        {globals, #domain_GlobalsObject{
            ref = ?glob(),
            data = #domain_Globals{
                external_account_set = {value, ?eas(1)},
                payment_institutions = ?ordset([?payinst(1)])
            }
        }},

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

        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(1),
            data = #domain_TermSetHierarchy{
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = get_default_termset()
                }]
            }
        }},

        ct_domain:currency(?cur(<<"RUB">>)),
        ct_domain:currency(?cur(<<"USD">>)),

        ct_domain:category(?cat(1), <<"Generic Store">>, live),

        ct_domain:payment_method(?pmt(bank_card, visa)),
        ct_domain:payment_method(?pmt(bank_card, mastercard))

    ].

get_default_termset() ->
    #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ?ordset([?cur(<<"RUB">>)])},
            categories = {value, ?ordset([?cat(1)])},
            payment_methods = {value, ?ordset([
                ?pmt(bank_card, visa),
                ?pmt(bank_card, mastercard)
            ])},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(       0, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )}
                }
            ]},
            fees = {decisions, [
                #domain_CashFlowDecision{
                    if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(3, 100, operation_amount)
                        )
                    ]}
                }
            ]}
        }
    }.
