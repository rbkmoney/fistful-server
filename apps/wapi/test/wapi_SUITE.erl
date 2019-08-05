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

-export([withdrawal_to_bank_card/1]).
-export([withdrawal_to_crypto_wallet/1]).
-export([woody_retry_test/1]).
-export([get_quote_test/1]).
-export([get_quote_without_destination_test/1]).
-export([get_quote_without_destination_fail_test/1]).
-export([get_quote_fail_test/1]).
-export([quote_withdrawal/1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [ {group, default}
    , {group, quote}
    , {group, woody}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {default, [sequence, {repeat, 2}], [
            withdrawal_to_bank_card,
            withdrawal_to_crypto_wallet
        ]},
        {quote, [], [
            get_quote_test,
            get_quote_without_destination_test,
            get_quote_without_destination_fail_test,
            get_quote_fail_test,
            quote_withdrawal
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
-define(ID_PROVIDER2, <<"good-two">>).
-define(ID_CLASS, <<"person">>).
-define(STRING, <<"TEST">>).

-spec woody_retry_test(config()) -> test_return().

-spec withdrawal_to_bank_card(config()) -> test_return().

withdrawal_to_bank_card(C) ->
    Name          = <<"Keyn Fawkes">>,
    Provider      = ?ID_PROVIDER,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    ok            = check_identity(Name, IdentityID, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),
    ok            = check_wallet(WalletID, C),
    CardToken     = store_bank_card(C),
    {ok, _Card}   = get_bank_card(CardToken, C),
    Resource      = make_bank_card_resource(CardToken),
    DestID        = create_desination(IdentityID, Resource, C),
    ok            = check_destination(IdentityID, DestID, Resource, C),
    {ok, _Grants} = issue_destination_grants(DestID, C),
    % ожидаем выполнения асинхронного вызова выдачи прав на вывод
    await_destination(DestID),

    WithdrawalID  = create_withdrawal(WalletID, DestID, C),
    ok            = check_withdrawal(WalletID, DestID, WithdrawalID, C).

-spec withdrawal_to_crypto_wallet(config()) -> test_return().

withdrawal_to_crypto_wallet(C) ->
    Name          = <<"Tyler Durden">>,
    Provider      = ?ID_PROVIDER2,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    ok            = check_identity(Name, IdentityID, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),
    ok            = check_wallet(WalletID, C),
    Resource      = make_crypto_wallet_resource(),
    DestID        = create_desination(IdentityID, Resource, C),
    ok            = check_destination(IdentityID, DestID, Resource, C),
    {ok, _Grants} = issue_destination_grants(DestID, C),
    % ожидаем выполнения асинхронного вызова выдачи прав на вывод
    await_destination(DestID),

    WithdrawalID  = create_withdrawal(WalletID, DestID, C),
    ok            = check_withdrawal(WalletID, DestID, WithdrawalID, C).

-spec get_quote_test(config()) -> test_return().

get_quote_test(C) ->
    Name          = <<"Keyn Fawkes">>,
    Provider      = <<"quote-owner">>,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),
    CardToken     = store_bank_card(C),
    Resource      = make_bank_card_resource(CardToken),
    DestID        = create_desination(IdentityID, Resource, C),
    % ожидаем авторизации назначения вывода
    await_destination(DestID),

    CashFrom = #{
        <<"amount">> => 100,
        <<"currency">> => <<"RUB">>
    },

    {ok, Quote} = call_api(
        fun swag_client_wallet_withdrawals_api:create_quote/3,
        #{
            body => #{
                <<"walletID">> => WalletID,
                <<"destinationID">> => DestID,
                <<"currencyFrom">> => <<"RUB">>,
                <<"currencyTo">> => <<"USD">>,
                <<"cash">> => CashFrom
        }},
        ct_helper:cfg(context, C)
    ),
    CashFrom = maps:get(<<"cashFrom">>, Quote),
    {ok, JSONData} = wapi_signer:verify(maps:get(<<"quoteToken">>, Quote)),
    #{
        <<"version">>       := 1,
        <<"cashFrom">>     := CashFrom
    } = jsx:decode(JSONData, [return_maps]).

-spec get_quote_without_destination_test(config()) -> test_return().

get_quote_without_destination_test(C) ->
    Name          = <<"Keyn Fawkes">>,
    Provider      = <<"quote-owner">>,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),

    CashFrom = #{
        <<"amount">> => 100,
        <<"currency">> => <<"RUB">>
    },

    {ok, Quote} = call_api(
        fun swag_client_wallet_withdrawals_api:create_quote/3,
        #{
            body => #{
                <<"walletID">> => WalletID,
                <<"currencyFrom">> => <<"RUB">>,
                <<"currencyTo">> => <<"USD">>,
                <<"cash">> => CashFrom
        }},
        ct_helper:cfg(context, C)
    ),
    CashFrom = maps:get(<<"cashFrom">>, Quote),
    {ok, JSONData} = wapi_signer:verify(maps:get(<<"quoteToken">>, Quote)),
    #{
        <<"version">>       := 1,
        <<"cashFrom">>     := CashFrom
    } = jsx:decode(JSONData, [return_maps]).

-spec get_quote_without_destination_fail_test(config()) -> test_return().

get_quote_without_destination_fail_test(C) ->
    Name          = <<"Keyn Fawkes">>,
    Provider      = ?ID_PROVIDER,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),

    CashFrom = #{
        <<"amount">> => 100,
        <<"currency">> => <<"RUB">>
    },

    {error, {422, #{<<"message">> := <<"Provider not found">>}}} = call_api(
        fun swag_client_wallet_withdrawals_api:create_quote/3,
        #{
            body => #{
                <<"walletID">> => WalletID,
                <<"currencyFrom">> => <<"RUB">>,
                <<"currencyTo">> => <<"USD">>,
                <<"cash">> => CashFrom
        }},
        ct_helper:cfg(context, C)
    ).

-spec get_quote_fail_test(config()) -> test_return().

get_quote_fail_test(C) ->
    Name          = <<"Keyn Fawkes">>,
    Provider      = ?ID_PROVIDER,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),

    CashFrom = #{
        <<"amount">> => 999,
        <<"currency">> => <<"RUB">>
    },

    {error, {422, _}} = call_api(
        fun swag_client_wallet_withdrawals_api:create_quote/3,
        #{
            body => #{
                <<"walletID">> => WalletID,
                <<"currencyFrom">> => <<"RUB">>,
                <<"currencyTo">> => <<"USD">>,
                <<"cash">> => CashFrom
        }},
        ct_helper:cfg(context, C)
    ).

-spec quote_withdrawal(config()) -> test_return().

quote_withdrawal(C) ->
    Name          = <<"Keyn Fawkes">>,
    Provider      = <<"quote-owner">>,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),
    CardToken     = store_bank_card(C),
    Resource      = make_bank_card_resource(CardToken),
    DestID        = create_desination(IdentityID, Resource, C),
    % ожидаем авторизации назначения вывода
    await_destination(DestID),

    CashFrom = #{
        <<"amount">> => 100,
        <<"currency">> => <<"RUB">>
    },

    {ok, Quote} = call_api(
        fun swag_client_wallet_withdrawals_api:create_quote/3,
        #{
            body => #{
                <<"walletID">> => WalletID,
                <<"destinationID">> => DestID,
                <<"currencyFrom">> => <<"RUB">>,
                <<"currencyTo">> => <<"USD">>,
                <<"cash">> => CashFrom
        }},
        ct_helper:cfg(context, C)
    ),
    WithdrawalID = create_withdrawal(
        WalletID,
        DestID,
        C,
        maps:get(<<"quoteToken">>, Quote)
    ),
    ok = check_withdrawal(WalletID, DestID, WithdrawalID, C).

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

create_identity(Name, Provider, Class, C) ->
    {ok, Identity} = call_api(
        fun swag_client_wallet_identities_api:create_identity/3,
        #{body => #{
            <<"name">>     => Name,
            <<"provider">> => Provider,
            <<"class">>    => Class,
            <<"metadata">> => #{
                ?STRING => ?STRING
            }
        }},
        ct_helper:cfg(context, C)
    ),
    maps:get(<<"id">>, Identity).

check_identity(Name, IdentityID, Provider, Class, C) ->
    {ok, Identity} = call_api(
        fun swag_client_wallet_identities_api:get_identity/3,
        #{binding => #{<<"identityID">> => IdentityID}},
        ct_helper:cfg(context, C)
    ),
    #{
        <<"name">>     := Name,
        <<"provider">> := Provider,
        <<"class">>    := Class,
        <<"metadata">> := #{
            ?STRING := ?STRING
        }
    } = maps:with([<<"name">>,
                   <<"provider">>,
                   <<"class">>,
                   <<"metadata">>], Identity),
    ok.

create_wallet(IdentityID, C) ->
    {ok, Wallet} = call_api(
        fun swag_client_wallet_wallets_api:create_wallet/3,
        #{body => #{
            <<"name">>     => <<"Worldwide PHP Awareness Initiative">>,
            <<"identity">> => IdentityID,
            <<"currency">> => <<"RUB">>,
            <<"metadata">> => #{
                ?STRING => ?STRING
             }
        }},
        ct_helper:cfg(context, C)
    ),
    maps:get(<<"id">>, Wallet).

check_wallet(WalletID, C) ->
    {ok, Wallet} = get_wallet(WalletID, C),
    #{
        <<"name">> := <<"Worldwide PHP Awareness Initiative">>,
        <<"currency">> := <<"RUB">>,
        <<"metadata">> := #{
            ?STRING := ?STRING
        }
    } = maps:with([<<"name">>, <<"currency">>, <<"metadata">>], Wallet),
    ok.

get_wallet(WalletID, C) ->
    call_api(
        fun swag_client_wallet_wallets_api:get_wallet/3,
        #{binding => #{<<"walletID">> => WalletID}},
        ct_helper:cfg(context, C)
    ).

store_bank_card(C) ->
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
    maps:get(<<"token">>, Res).

get_bank_card(CardToken, C) ->
    call_api(
        fun swag_client_payres_payment_resources_api:get_bank_card/3,
        #{binding => #{<<"token">> => CardToken}},
        ct_helper:cfg(context_pcidss, C)
    ).

make_bank_card_resource(CardToken) ->
    #{
        <<"type">>  => <<"BankCardDestinationResource">>,
        <<"token">> => CardToken
    }.

make_crypto_wallet_resource() ->
    #{
        <<"type">>     => <<"CryptoWalletDestinationResource">>,
        <<"id">>       => <<"0610899fa9a3a4300e375ce582762273">>,
        <<"currency">> => <<"Ethereum">>,
        <<"tag">>      => <<"test_tag">>
    }.

create_desination(IdentityID, Resource, C) ->
    {ok, Dest} = call_api(
        fun swag_client_wallet_withdrawals_api:create_destination/3,
        #{body => #{
            <<"name">>     => <<"Worldwide PHP Awareness Initiative">>,
            <<"identity">> => IdentityID,
            <<"currency">> => <<"RUB">>,
            <<"resource">> => Resource,
            <<"metadata">> => #{
                ?STRING => ?STRING
             }
        }},
        ct_helper:cfg(context, C)
    ),
    maps:get(<<"id">>, Dest).

check_destination(IdentityID, DestID, Resource0, C) ->
    {ok, Dest} = get_destination(DestID, C),
    ResourceFields = [<<"type">>, <<"token">>, <<"id">>, <<"currency">>],
    Resource = convert_token(maps:with(ResourceFields, Resource0)),
    #{<<"resource">> := Res} = D1 = maps:with([<<"name">>,
                                               <<"identity">>,
                                               <<"currency">>,
                                               <<"resource">>,
                                               <<"metadata">>], Dest),
    #{
        <<"name">> := <<"Worldwide PHP Awareness Initiative">>,
        <<"identity">> := IdentityID,
        <<"currency">> := <<"RUB">>,
        <<"resource">> := Resource,
        <<"metadata">> := #{
            ?STRING := ?STRING
        }
    } = D1#{<<"resource">> => maps:with(ResourceFields, Res)},
    ok.

await_destination(DestID) ->
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, DestM} = ff_destination:get_machine(DestID),
            ff_destination:status(ff_destination:get(DestM))
        end
    ).

convert_token(#{<<"token">> := Base64} = Resource) ->
    BankCard = wapi_utils:base64url_to_map(Base64),
    Resource#{<<"token">> => maps:get(<<"token">>, BankCard)};
convert_token(Resource) ->
    Resource.

get_destination(DestID, C) ->
    call_api(
        fun swag_client_wallet_withdrawals_api:get_destination/3,
        #{binding => #{<<"destinationID">> => DestID}},
        ct_helper:cfg(context, C)
    ).

issue_destination_grants(DestID, C) ->
    call_api(
        fun swag_client_wallet_withdrawals_api:issue_destination_grant/3,
        #{
            binding => #{
                <<"destinationID">> => DestID
            },
            body => #{
                <<"validUntil">> => <<"2800-12-12T00:00:00.0Z">>
            }
        },
        ct_helper:cfg(context, C)
    ).

create_withdrawal(WalletID, DestID, C) ->
    create_withdrawal(WalletID, DestID, C, undefined).

create_withdrawal(WalletID, DestID, C, QuoteToken) ->
    {ok, Withdrawal} = call_api(
        fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
        #{body => genlib_map:compact(#{
            <<"wallet">> => WalletID,
            <<"destination">> => DestID,
            <<"body">> => #{
                <<"amount">> => 100,
                <<"currency">> => <<"RUB">>
            },
            <<"quoteToken">> => QuoteToken
        })},
        ct_helper:cfg(context, C)
    ),
    maps:get(<<"id">>, Withdrawal).

check_withdrawal(WalletID, DestID, WithdrawalID, C) ->
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
                          <<"destination">> := DestID,
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
    ).

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
