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
-export([issue_destination_grants/1]).
-export([create_withdrawal/1]).
-export([get_withdrawal/1]).
-export([woody_retry_test/1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [ {group, default}
    , {group, woody}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {default, [sequence, {repeat, 1}], [
            create_identity,
            get_identity,
            create_wallet
            % get_wallet,
            % store_bank_card,
            % get_bank_card,
            % create_desination,
            % get_destination,
            % issue_destination_grants,
            % create_withdrawal,
            % get_withdrawal
        ]},
        {woody, [], [
            woody_retry_test
        ]}
    ].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
     ct_helper:makeup_cfg([
        ct_helper:test_case_name(init),
        ct_payment_system:setup(#{
            default_termset => get_default_termset(),
            optional_apps => [
                wapi,
                wapi_woody_client
            ]
        })
    ], C).

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C).

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
-spec woody_retry_test(config()) -> test_return().
-spec issue_destination_grants(config()) -> test_return().
-spec create_withdrawal(config()) -> test_return().
-spec get_withdrawal(config()) -> test_return().

create_identity(C) ->
    lager:error(">>>>>>>>>>>>>>>>~n", []),
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
    lager:error("~ncreate wallet >>>~n", []),
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

issue_destination_grants(C) ->
    {get_destination, Cfg} = ct_helper:cfg(saved_config, C),
    DestinationID = ct_helper:cfg(dest, Cfg),
    {ok, _Grants} = call_api(
        fun swag_client_wallet_withdrawals_api:issue_destination_grant/3,
        #{
            binding => #{
                <<"destinationID">> => DestinationID
            },
            body => #{
                <<"validUntil">> => <<"2800-12-12T00:00:00.0Z">>
            }
        },
        ct_helper:cfg(context, C)
    ),
    {save_config, Cfg}.

create_withdrawal(C) ->
    % ожидаем выполнения асинхронного вызова выдачи прав на вывод
    timer:sleep(1000),
    {issue_destination_grants, Cfg} = ct_helper:cfg(saved_config, C),
    WalletID = ct_helper:cfg(wallet, Cfg),
    DestinationID = ct_helper:cfg(dest, Cfg),
    {ok, Withdrawal} = call_api(
        fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
        #{body => #{
            <<"wallet">> => WalletID,
            <<"destination">> => DestinationID,
            <<"body">> => #{
                <<"amount">> => 100,
                <<"currency">> => <<"RUB">>
            }
        }},
        ct_helper:cfg(context, C)
    ),
    WithdrawalID = maps:get(<<"id">>, Withdrawal),
    {save_config, [{withdrawal, WithdrawalID} | Cfg]}.

get_withdrawal(C) ->
    {create_withdrawal, Cfg} = ct_helper:cfg(saved_config, C),
    WalletID = ct_helper:cfg(wallet, Cfg),
    DestinationID = ct_helper:cfg(dest, Cfg),
    WithdrawalID = ct_helper:cfg(withdrawal, Cfg),
    ct_helper:await(
        ok,
        fun () ->
            R = call_api(fun swag_client_wallet_withdrawals_api:list_withdrawals/3,
                         #{qs_val => #{
                             <<"withdrawalID">> => WithdrawalID,
                             <<"limit">> => 100
                            }},
                         ct_helper:cfg(context, C)),
            case R of
                {ok, #{<<"result">> := []}} ->
                    R;
                {ok, Withdrawal} ->
                    #{<<"result">> := [
                        #{<<"wallet">> := WalletID,
                          <<"destination">> := DestinationID,
                          <<"body">> := #{
                              <<"amount">> := 100,
                              <<"currency">> := <<"RUB">>
                          }
                    }]} = Withdrawal,
                    ok;
                _ ->
                    R
            end
        end,
        {linear, 20, 1000}
    ),
    {save_config, Cfg}.

woody_retry_test(C) ->
    Urls = application:get_env(wapi_woody_client, service_urls, #{}),
    ok = application:set_env(
        wapi_woody_client,
        service_urls,
        Urls#{fistful_stat => "http://spanish.inquision/fistful_stat"}
    ),
    Params = #{
        identityID => <<"12332">>,
        currencyID => <<"RUB">>,
        limit      => <<"123">>
    },
    Ctx = create_auth_ctx(<<"12332">>),
    T1 = erlang:monotonic_time(),
    try
        wapi_wallet_ff_backend:list_wallets(Params, Ctx#{woody_context => ct_helper:get_woody_ctx(C)})
    catch
        error:{woody_error, {_Source, Class, _Details}} = _Error
        when Class =:= resource_unavailable orelse Class =:= result_unknown
        ->
            ok
    end,
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T1, native, micro_seconds),
    true = (Time > 3000000) and (Time < 6000000),
    ok = application:set_env(wapi_woody_client, service_urls, Urls).

%%

-spec call_api(function(), map(), wapi_client_lib:context()) ->
    {ok, term()} | {error, term()}.
call_api(F, Params, Context) ->
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = F(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).

%%

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

issue_token(PartyID, ACL, LifeTime) ->
    Claims = #{?STRING => ?STRING},
    {ok, Token} = wapi_authorizer_jwt:issue({{PartyID, wapi_acl:from_list(ACL)}, Claims}, LifeTime),
    Token.

get_context(Endpoint, Token) ->
    wapi_client_lib:get_context(Endpoint, Token, 10000, ipv4).

create_auth_ctx(PartyID) ->
    #{
        swagger_context => #{auth_context => {{PartyID, empty}, empty}}
    }.

%%

-include_lib("ff_cth/include/ct_domain.hrl").

get_default_termset() ->
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            currencies = {value, ?ordset([?cur(<<"RUB">>)])},
            wallet_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(-10000000, <<"RUB">>)},
                        {exclusive, ?cash( 10000001, <<"RUB">>)}
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
    }.
