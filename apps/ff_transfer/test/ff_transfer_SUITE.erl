-module(ff_transfer_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_missing_fails/1]).
-export([deposit_withdrawal_ok/1]).

-import(ct_helper, [cfg/2]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        get_missing_fails,
        deposit_withdrawal_ok
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
                'ff/identity'              ,
                'ff/wallet_v2'             ,
                'ff/source_v1'             ,
                'ff/deposit_v1'            ,
                'ff/deposit/session_v1'    ,
                'ff/destination_v2'        ,
                'ff/withdrawal_v2'         ,
                'ff/withdrawal/session_v2'
            ]])},
            {providers,
                get_provider_config()
            }
        ]},
        {ff_transfer, [
            {withdrawal,
                #{provider => get_withdrawal_provider_config()}
            }
        ]}
    ]),
    SuiteSup = ct_sup:start(),
    BeOpts = #{event_handler => scoper_woody_event_handler},
    Routes = machinery_mg_backend:get_routes(
        [
            construct_handler(ff_identity_machine           , "identity"           , BeConf),
            construct_handler(ff_wallet_machine             , "wallet"             , BeConf),
            construct_handler(ff_instrument_machine         , "source"             , BeConf),
            construct_handler(ff_transfer_machine           , "deposit"            , BeConf),
            construct_handler(ff_deposit_session_machine    , "deposit/session"    , BeConf),
            construct_handler(ff_instrument_machine         , "destination"        , BeConf),
            construct_handler(ff_transfer_machine           , "withdrawal"         , BeConf),
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
        #{path => ff_string:join(["/v1/stateproc/ff/", Suffix]), backend_config => BeConf}}.

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
-spec deposit_withdrawal_ok(config()) -> test_return().

get_missing_fails(_C) ->
    ID = genlib:unique(),
    {error, notfound} = ff_withdrawal:get_machine(ID).

deposit_withdrawal_ok(C) ->
    Party = create_party(C),
    IID = create_identity(Party, C),
    ICID = genlib:unique(),
    WalID = create_wallet(IID, <<"HAHA NO">>, <<"RUB">>, C),
    {0, <<"RUB">>} = get_wallet_balance(WalID),

    % Create source
    SrcResource = #{type => internal, details => "Infinite source of cash"},
    SrcID = create_instrument(source, IID, <<"XSource">>, <<"RUB">>, SrcResource, C),
    {ok, SrcM1} = ff_source:get_machine(SrcID),
    Src1 = ff_source:get(SrcM1),
    unauthorized = ff_source:status(Src1),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, SrcM} = ff_source:get_machine(SrcID),
            ff_source:status(ff_source:get(SrcM))
        end
    ),

    % Process deposit
    DepID = generate_id(),
    ok = ff_deposit:create(
        DepID,
        #{source => SrcID, destination => WalID, body => {10000, <<"RUB">>}},
        ff_ctx:new()
    ),
    {ok, DepM1} = ff_deposit:get_machine(DepID),
    pending = ff_deposit:status(ff_deposit:get(DepM1)),
    succeeded = ct_helper:await(
        succeeded,
        fun () ->
            {ok, DepM} = ff_deposit:get_machine(DepID),
            ff_deposit:status(ff_deposit:get(DepM))
        end,
        genlib_retry:linear(3, 5000)
    ),
    {10000, <<"RUB">>} = get_wallet_balance(WalID),

    % Create destination
    DestResource = {bank_card, ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C)},
    DestID = create_instrument(destination, IID, <<"XDesination">>, <<"RUB">>, DestResource, C),
    {ok, DestM1} = ff_destination:get_machine(DestID),
    Dest1 = ff_destination:get(DestM1),
    unauthorized = ff_destination:status(Dest1),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, DestM} = ff_destination:get_machine(DestID),
            ff_destination:status(ff_destination:get(DestM))
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
    WdrID = generate_id(),
    ok = ff_withdrawal:create(
        WdrID,
        #{source => WalID, destination => DestID, body => {4242, <<"RUB">>}},
        ff_ctx:new()
    ),
    {ok, WdrM1} = ff_withdrawal:get_machine(WdrID),
    pending = ff_withdrawal:status(ff_withdrawal:get(WdrM1)),
    succeeded = ct_helper:await(
        succeeded,
        fun () ->
            {ok, WdrM} = ff_withdrawal:get_machine(WdrID),
            ff_withdrawal:status(ff_withdrawal:get(WdrM))
        end,
        genlib_retry:linear(3, 5000)
    ),
    {10000 - 4242, <<"RUB">>} = get_wallet_balance(WalID).

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

get_wallet_balance(ID) ->
    {ok, Machine} = ff_wallet_machine:get(ID),
    Account = ff_wallet:account(ff_wallet_machine:wallet(Machine)),
    {ok, {Amounts, Currency}} = ff_transaction:balance(ff_account:accounter_account_id(Account)),
    {ff_indef:current(Amounts), Currency}.

create_instrument(Type, IdentityID, Name, Currency, Resource, C) ->
    ID = genlib:unique(),
    ok = create_instrument(
        Type,
        ID,
        #{identity => IdentityID, name => Name, currency => Currency, resource => Resource},
        ff_ctx:new(),
        C
    ),
    ID.

create_instrument(destination, ID, Params, Ctx, _C) ->
    ff_destination:create(ID, Params, Ctx);
create_instrument(source, ID, Params, Ctx, _C) ->
    ff_source:create(ID, Params, Ctx).

generate_id() ->
    genlib:to_binary(genlib_time:ticks() div 1000).

%%

-include_lib("ff_cth/include/ct_domain.hrl").

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
        wallets = #domain_WalletServiceTerms{
            currencies = {value, ?ordset([?cur(<<"RUB">>)])},
            cash_limit = {decisions, [
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

get_withdrawal_provider_config() ->
    #{
        <<"mocketbank">> => #{
            adapter => ff_woody_client:new("http://adapter-mocketbank:8022/proxy/mocketbank/p2p-credit")
        }
    }.
