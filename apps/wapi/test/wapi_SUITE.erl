-module(wapi_SUITE).

-include_lib("fistful_proto/include/ff_proto_fistful_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_identity/1]).
-export([get_identity/1]).
-export([create_wallet/1]).
-export([get_wallet/1]).
-export([store_bank_card/1]).
-export([get_bank_card/1]).
-export([create_desination/1]).
-export([get_destination/1]).

-import(ct_helper, [cfg/2]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {default, [sequence], [
            create_identity,
            get_identity,
            create_wallet,
            get_wallet,
            store_bank_card,
            get_bank_card,
            create_desination,
            get_destination
        ]}
    ].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
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
            {providers,
                get_provider_config()
            }
        ]},
        {ff_transfer, [
            {withdrawal,
                #{provider => get_withdrawal_provider_config()}
            }
        ]},
        wapi,
        ff_server
    ]),
    C1 = ct_helper:makeup_cfg(
        [ct_helper:test_case_name(init), ct_helper:woody_ctx()],
        [
            {started_apps , StartedApps},
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

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = ct_helper:stop_apps(cfg(started_apps, C)),
    ok.

%%

-spec init_per_group(group_name(), config()) -> config().

init_per_group(G, C) ->
    ok = ff_woody_ctx:set(woody_context:new(<<"init_per_group/", (atom_to_binary(G, utf8))/binary>>)),
    Party = create_party(C),
    Token = issue_token(Party, [{[party], write}], unlimited),
    Context = get_context("localhost:8080", Token),
    ContextPcidss = get_context("wapi-pcidss:8080", Token),
    [{context, Context}, {context_pcidss, ContextPcidss}, {party, Party} | C].

-spec end_per_group(group_name(), config()) -> _.

end_per_group(_, _) ->
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

-define(ID_PROVIDER, <<"good-one">>).
-define(ID_CLASS, <<"person">>).
-define(STRING, <<"TEST">>).

-spec create_identity(config()) -> test_return().
-spec get_identity(config()) -> test_return().
-spec create_wallet(config()) -> test_return().
-spec get_wallet(config()) -> test_return().
-spec store_bank_card(config()) -> test_return().
-spec get_bank_card(config()) -> test_return().
-spec create_desination(config()) -> test_return().
-spec get_destination(config()) -> test_return().

create_identity(C) ->
    {ok, Identity} = call_api(
        fun swag_client_wallet_identities_api:create_identity/3,
        #{body => #{
            <<"name">>     => <<"Keyn Fawkes">>,
            <<"provider">> => ?ID_PROVIDER,
            <<"class">>    => ?ID_CLASS,
            <<"metadata">> => #{
                ?STRING => ?STRING
            }
        }},
        ct_helper:cfg(context, C)
    ),
    IdentityID = maps:get(<<"id">>, Identity),
    {save_config, [{identity, IdentityID}]}.

get_identity(C) ->
    {create_identity, Cfg} = ct_helper:cfg(saved_config, C),
    {ok, Identity} = call_api(
        fun swag_client_wallet_identities_api:get_identity/3,
        #{binding => #{<<"identityID">> => ct_helper:cfg(identity, Cfg)}},
        ct_helper:cfg(context, C)
    ),
    #{
        <<"name">> := <<"Keyn Fawkes">>,
        <<"provider">> := ?ID_PROVIDER,
        <<"class">> := ?ID_CLASS,
        <<"metadata">> := #{
            ?STRING := ?STRING
        }
    } = maps:with([<<"name">>,
                   <<"provider">>,
                   <<"class">>,
                   <<"metadata">>], Identity),
    {save_config, Cfg}.

create_wallet(C) ->
    {get_identity, Cfg} = ct_helper:cfg(saved_config, C),
    {ok, Wallet} = call_api(
        fun swag_client_wallet_wallets_api:create_wallet/3,
        #{body => #{
            <<"name">>     => <<"Worldwide PHP Awareness Initiative">>,
            <<"identity">> => ct_helper:cfg(identity, Cfg),
            <<"currency">> => <<"RUB">>,
            <<"metadata">> => #{
                ?STRING => ?STRING
             }
        }},
        ct_helper:cfg(context, C)
    ),
    WalletID = maps:get(<<"id">>, Wallet),
    {save_config, [{wallet, WalletID} | Cfg]}.

get_wallet(C) ->
    {create_wallet, Cfg} = ct_helper:cfg(saved_config, C),
    {ok, Wallet} = call_api(
        fun swag_client_wallet_wallets_api:get_wallet/3,
        #{binding => #{<<"walletID">> => ct_helper:cfg(wallet, Cfg)}},
        ct_helper:cfg(context, C)
    ),
    #{
        <<"name">> := <<"Worldwide PHP Awareness Initiative">>,
        <<"currency">> := <<"RUB">>,
        <<"metadata">> := #{
            ?STRING := ?STRING
        }
    } = maps:with([<<"name">>, <<"currency">>, <<"metadata">>], Wallet),
    {save_config, Cfg}.

store_bank_card(C) ->
    {get_wallet, Cfg} = ct_helper:cfg(saved_config, C),
    {ok, Res} = call_api(
        fun swag_client_payres_payment_resources_api:store_bank_card/3,
        #{body => #{
            <<"type">>       => <<"BankCard">>,
            <<"cardNumber">> => <<"4150399999000900">>,
            <<"expDate">>    => <<"12/25">>,
            <<"cardHolder">> => <<"LEXA SVOTIN">>
        }},
        ct_helper:cfg(context_pcidss, C)
    ),
    CardToken = maps:get(<<"token">>, Res),
    {save_config, [{card_token, CardToken} | Cfg]}.

get_bank_card(C) ->
    {store_bank_card, Cfg} = ct_helper:cfg(saved_config, C),
    {ok, _Card} = call_api(
        fun swag_client_payres_payment_resources_api:get_bank_card/3,
        #{binding => #{<<"token">> => ct_helper:cfg(card_token, Cfg)}},
        ct_helper:cfg(context_pcidss, C)
    ),
    {save_config, Cfg}.

create_desination(C) ->
    {get_bank_card, Cfg} = ct_helper:cfg(saved_config, C),
    {ok, Dest} = call_api(
        fun swag_client_wallet_withdrawals_api:create_destination/3,
        #{body => #{
            <<"name">>     => <<"Worldwide PHP Awareness Initiative">>,
            <<"identity">> => ct_helper:cfg(identity, Cfg),
            <<"currency">> => <<"RUB">>,
            <<"resource">> => #{
                <<"type">>  => <<"BankCardDestinationResource">>,
                <<"token">> => ct_helper:cfg(card_token, Cfg)
            },
            <<"metadata">> => #{
                ?STRING => ?STRING
             }
        }},
        ct_helper:cfg(context, C)
    ),
    DestID = maps:get(<<"id">>, Dest),
    {save_config, [{dest, DestID} | Cfg]}.

get_destination(C) ->
    {create_desination, Cfg} = ct_helper:cfg(saved_config, C),
    {ok, Wallet} = call_api(
        fun swag_client_wallet_withdrawals_api:get_destination/3,
        #{binding => #{<<"destinationID">> => ct_helper:cfg(dest, Cfg)}},
        ct_helper:cfg(context, C)
    ),
    #{<<"resource">> := Res} = W1 = maps:with([<<"name">>,
                                               <<"identity">>,
                                               <<"currency">>,
                                               <<"resource">>,
                                               <<"metadata">>], Wallet),
    IdentityID = ct_helper:cfg(identity, Cfg),
    #{
        <<"name">> := <<"Worldwide PHP Awareness Initiative">>,
        <<"identity">> := IdentityID,
        <<"currency">> := <<"RUB">>,
        <<"resource">> := #{
            <<"type">>  := <<"BankCardDestinationResource">>
        },
        <<"metadata">> := #{
            ?STRING := ?STRING
        }
    } = W1#{<<"resource">> => maps:with([<<"type">>], Res)},
    {save_config, Cfg}.

%%

-spec call_api(function(), map(), wapi_client_lib:context()) ->
    {ok, term()} | {error, term()}.
call_api(F, Params, Context) ->
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = F(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).

%%

create_party(_C) ->
    ID = genlib:unique(),
    _ = ff_party:create(ID),
    ID.

issue_token(PartyID, ACL, LifeTime) ->
    Claims = #{?STRING => ?STRING},
    {ok, Token} = wapi_authorizer_jwt:issue({{PartyID, wapi_acl:from_list(ACL)}, Claims}, LifeTime),
    Token.

get_context(Endpoint, Token) ->
    wapi_client_lib:get_context(Endpoint, Token, 10000, ipv4).

%%

-include_lib("ff_cth/include/ct_domain.hrl").

get_provider_config() ->
    #{
        ?ID_PROVIDER => #{
            payment_institution_id => 1,
            routes => [<<"mocketbank">>],
            identity_classes => #{
                ?ID_CLASS => #{
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

get_withdrawal_provider_config() ->
    #{
        <<"mocketbank">> => #{
            adapter => ff_woody_client:new("http://adapter-mocketbank:8022/proxy/mocketbank/p2p-credit")
        }
    }.
