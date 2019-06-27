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
                'partymgmt'      => "http://hellgate:8022/v1/processing/partymgmt",
                'identification' => "http://identification:8022/v1/identification"
            }},
            {backends, #{
                'ff/identity' => Be
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
                #{path => <<"/v1/stateproc/ff/identity">>, backend_config => BeConf}}
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
                'accounter'     => ff_woody_client:new("http://shumway:8022/accounter"),
                'identdocstore' => ff_woody_client:new("http://cds:8022/v1/identity_document_storage")
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
    ChallengeParams = #{
        id     => ICID,
        class  => <<"sword-initiation">>
    },
    {error, {challenge, {proof, insufficient}}} = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => []}
    ),
    {error, {challenge, {proof, insufficient}}} = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => [D1]}
    ),
    ok = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => [D1, D2]}
    ),
    {error, {challenge, {pending, ICID}}} = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => [D1, D2]}
    ),
    {completed, _} = ct_helper:await(
        {completed, #{resolution => approved}},
        fun () ->
            {ok, S}  = ff_identity_machine:get(ID),
            {ok, IC} = ff_identity:challenge(ICID, ff_identity_machine:identity(S)),
            ff_identity_challenge:status(IC)
        end
    ),
    {ok, S3}  = ff_identity_machine:get(ID),
    I3 = ff_identity_machine:identity(S3),
    {ok, ICID} = ff_identity:effective_challenge(I3).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

%%

-include_lib("ff_cth/include/ct_domain.hrl").

get_provider_config() ->
    #{
        <<"good-one">> => #{
            payment_institution_id => 1,
            routes => [<<"thebank">>],
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
        ct_domain:term_set_hierarchy(?trms(1)),

        ct_domain:currency(?cur(<<"RUB">>)),
        ct_domain:currency(?cur(<<"USD">>)),

        ct_domain:category(?cat(1), <<"Generic Store">>, live),

        ct_domain:payment_method(?pmt(bank_card, visa)),
        ct_domain:payment_method(?pmt(bank_card, mastercard))

    ].
