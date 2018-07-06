-module(ff_withdrawal_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_missing_fails/1]).
-export([withdrawal_ok/1]).

-import(ct_helper, [cfg/2]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        get_missing_fails,
        withdrawal_ok
    ].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    BeConf = #{schema => machinery_mg_schema_generic},
    Be = {machinery_mg_backend, BeConf#{
        client => ff_woody_client:new("http://machinegun:8022/v1/automaton")
    }},
    {StartedApps, _StartupCtx} = ct_helper:start_apps([
        lager,
        scoper,
        woody,
        dmt_client,
        {fistful, [
            {services, #{
                'partymgmt'      => "http://hellgate:8022/v1/processing/partymgmt",
                'accounter'      => "http://shumway:8022/accounter",
                'identification' => "http://identification:8022/v1/identification"
            }},
            {backends, maps:from_list([{NS, Be} || NS <- [
                'ff/identity'           ,
                'ff/wallet'             ,
                'ff/destination'        ,
                'ff/withdrawal'         ,
                'ff/withdrawal/session'
            ]])},
            {providers,
                get_provider_config()
            }
        ]},
        {ff_withdraw, [
            {provider, get_withdrawal_provider_config()}
        ]}
    ]),
    SuiteSup = ct_sup:start(),
    BeOpts = #{event_handler => scoper_woody_event_handler},
    Routes = machinery_mg_backend:get_routes(
        [
            construct_handler(ff_identity_machine           , "identity"           , BeConf),
            construct_handler(ff_wallet_machine             , "wallet"             , BeConf),
            construct_handler(ff_destination_machine        , "destination"        , BeConf),
            construct_handler(ff_withdrawal_machine         , "withdrawal"         , BeConf),
            construct_handler(ff_withdrawal_session_machine , "withdrawal/session" , BeConf)
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
                'accounter'     => "http://shumway:8022/accounter",
                'cds'           => "http://cds:8022/v1/storage",
                'identdocstore' => "http://cds:8022/v1/identity_document_storage"
            }}
        | C]
    ),
    ok = ct_domain_config:upsert(get_domain_config(C1)),
    ok = timer:sleep(1000),
    C1.

construct_handler(Module, Suffix, BeConf) ->
    {{fistful, Module},
        #{path => ff_string:join(["/v1/stateproc/", Suffix]), backend_config => BeConf}}.

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

-spec get_missing_fails(config()) -> test_return().
-spec withdrawal_ok(config()) -> test_return().

get_missing_fails(_C) ->
    ID = genlib:unique(),
    {error, notfound} = ff_withdrawal_machine:get(ID).

withdrawal_ok(C) ->
    Party = create_party(C),
    Resource = {bank_card, ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C)},
    IID = create_identity(Party, C),
    ICID = genlib:unique(),
    SID = create_wallet(IID, <<"HAHA NO">>, <<"RUB">>, C),
    % Create destination
    DID = create_destination(IID, <<"XDDD">>, <<"RUB">>, Resource, C),
    {ok, DS1} = ff_destination_machine:get(DID),
    D1 = ff_destination_machine:destination(DS1),
    unauthorized = ff_destination:status(D1),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, DS} = ff_destination_machine:get(DID),
            ff_destination:status(ff_destination_machine:destination(DS))
        end
    ),
    % Pass identification
    Doc1 = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    Doc2 = ct_identdocstore:rus_domestic_passport(C),
    ok = ff_identity_machine:start_challenge(
        IID, #{
            id     => ICID,
            class  => <<"sword-initiation">>,
            proofs => [Doc1, Doc2]
        }
    ),
    {completed, _} = ct_helper:await(
        {completed, #{resolution => approved}},
        fun () ->
            {ok, S}  = ff_identity_machine:get(IID),
            {ok, IC} = ff_identity:challenge(ICID, ff_identity_machine:identity(S)),
            ff_identity_challenge:status(IC)
        end
    ),
    % Process withdrawal
    WID = generate_id(),
    ok = ff_withdrawal_machine:create(
        WID,
        #{source => SID, destination => DID, body => {4242, <<"RUB">>}},
        ff_ctx:new()
    ),
    {ok, WS1} = ff_withdrawal_machine:get(WID),
    W1 = ff_withdrawal_machine:withdrawal(WS1),
    pending = ff_withdrawal:status(W1),
    succeeded = ct_helper:await(
        succeeded,
        fun () ->
            {ok, WS} = ff_withdrawal_machine:get(WID),
            ff_withdrawal:status(ff_withdrawal_machine:withdrawal(WS))
        end,
        genlib_retry:linear(3, 3000)
    ).

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
        #{party => Party, provider => ProviderID, class => ClassID},
        ff_ctx:new()
    ),
    ID.

create_wallet(IdentityID, Name, Currency, _C) ->
    ID = genlib:unique(),
    ok = ff_wallet_machine:create(
        ID,
        #{identity => IdentityID, name => Name, currency => Currency},
        ff_ctx:new()
    ),
    ID.

create_destination(IdentityID, Name, Currency, Resource, _C) ->
    ID = genlib:unique(),
    ok = ff_destination_machine:create(
        ID,
        #{identity => IdentityID, name => Name, currency => Currency, resource => Resource},
        ff_ctx:new()
    ),
    ID.

generate_id() ->
    genlib:to_binary(genlib_time:ticks() div 1000).

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
        % TODO
        %  - Strangely enough, hellgate checks wallet currency against _payments_
        %    terms.
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ?ordset([?cur(<<"RUB">>)])}
        }
    }.

get_withdrawal_provider_config() ->
    #{
        adapter      => ff_woody_client:new("http://adapter-vtb:8022/proxy/vtb-mpi-vtb/p2p-credit"),
        adapter_opts => #{
            <<"merchant_id">>   => <<"mcpitmpitest">>,
            <<"merchant_cn">>   => <<"rbkmoneyP2P9999">>,
            <<"merchant_name">> => <<"RBKMoney P2P">>,
            <<"version">>       => <<"109">>,
            <<"term_id">>       => <<"30001018">>,
            <<"FPTTI">>         => <<"PPP">>
        }
    }.
