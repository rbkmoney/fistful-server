-module(ff_identity_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_missing_fails/1]).
-export([create_missing_fails/1]).
-export([create_ok/1]).
-export([identify_ok/1]).

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
        create_missing_fails,
        create_ok,
        identify_ok
    ].

-spec get_missing_fails(config()) -> test_return().
-spec create_missing_fails(config()) -> test_return().
-spec create_ok(config()) -> test_return().
-spec identify_ok(config()) -> test_return().

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    IBO = #{name => {?MODULE, identities}},
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
                'partymgmt'      => ff_woody_client:new("http://hellgate:8022/v1/processing/partymgmt"),
                'identification' => ff_woody_client:new("http://identification:8022/v1/identification")
            }},
            {backends, #{
                'ff/identity' => {fistful, machinery_gensrv_backend:new(IBO)}
            }},
            {providers,
                get_provider_config()
            }
        ]}
    ]),
    SuiteSup = ct_sup:start(),
    IBCS = machinery_gensrv_backend:child_spec({fistful, ff_identity_machine}, IBO),
    {ok, _} = supervisor:start_child(SuiteSup, IBCS),
    C1 = ct_helper:makeup_cfg(
        [ct_helper:test_case_name(init), ct_helper:woody_ctx()],
        [
            {started_apps , StartedApps},
            {suite_sup    , SuiteSup},
            {services     , #{
                'accounter'     => ff_woody_client:new("http://shumway:8022/accounter"),
                'identdocstore' => ff_woody_client:new("http://cds:8022/v1/identity_document_storage")
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
    ID = genlib:unique(),
    {error, notfound} = ff_identity_machine:get(ID).

create_missing_fails(_C) ->
    ID = genlib:unique(),
    {error, {provider, notfound}} = ff_identity_machine:create(
        ID,
        #{
            party    => <<"party">>,
            provider => <<"who">>,
            class    => <<"person">>
        },
        ff_ctx:new()
    ),
    {error, {identity_class, notfound}} = ff_identity_machine:create(
        ID,
        #{
            party    => <<"party">>,
            provider => <<"good-one">>,
            class    => <<"nosrep">>
        },
        ff_ctx:new()
    ).

create_ok(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    ok = ff_identity_machine:create(
        ID,
        #{
            party    => Party,
            provider => <<"good-one">>,
            class    => <<"person">>
        },
        ff_ctx:new()
    ),
    I1 = ff_identity_machine:identity(unwrap(ff_identity_machine:get(ID))),
    {ok, accessible} = ff_identity:is_accessible(I1),
    Party = ff_identity:party(I1),
    Party = ff_identity:party(I1).

identify_ok(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    ok = ff_identity_machine:create(
        ID,
        #{
            party    => Party,
            provider => <<"good-one">>,
            class    => <<"person">>
        },
        ff_ctx:new()
    ),
    ICID = genlib:unique(),
    {ok, S1} = ff_identity_machine:get(ID),
    I1 = ff_identity_machine:identity(S1),
    {error, notfound} = ff_identity:challenge(ICID, I1),
    D1 = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    D2 = ct_identdocstore:rus_domestic_passport(C),
    {error, {challenge, {proof, insufficient}}} = ff_identity_machine:start_challenge(
        ID, #{
            id     => ICID,
            class  => <<"sword-initiation">>,
            proofs => []
        }
    ),
    {error, {challenge, {proof, insufficient}}} = ff_identity_machine:start_challenge(
        ID, #{
            id     => ICID,
            class  => <<"sword-initiation">>,
            proofs => [D1]
        }
    ),
    ok = ff_identity_machine:start_challenge(
        ID, #{
            id     => ICID,
            class  => <<"sword-initiation">>,
            proofs => [D1, D2]
        }
    ),
    {ok, S2} = ff_identity_machine:get(ID),
    I2 = ff_identity_machine:identity(S2),
    {ok, IC} = ff_identity:challenge(ICID, I2).

create_party(_C) ->
    ID = genlib:unique(),
    _ = ff_party:create(ID),
    ID.

%%

-include_lib("ff_cth/include/ct_domain.hrl").

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
                term_sets = []
            }
        }},

        ct_domain:currency(?cur(<<"RUB">>)),
        ct_domain:currency(?cur(<<"USD">>)),

        ct_domain:category(?cat(1), <<"Generic Store">>, live),

        ct_domain:payment_method(?pmt(bank_card, visa)),
        ct_domain:payment_method(?pmt(bank_card, mastercard))

    ].
