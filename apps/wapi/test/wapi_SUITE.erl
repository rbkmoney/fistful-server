-module(wapi_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/ff_proto_fistful_thrift.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_destination_failed_test/1]).
-export([withdrawal_to_bank_card_test/1]).
-export([withdrawal_to_crypto_wallet_test/1]).
-export([withdrawal_to_ripple_wallet_test/1]).
-export([withdrawal_to_ripple_wallet_with_tag_test/1]).
-export([woody_retry_test/1]).
-export([quote_encode_decode_test/1]).
-export([get_quote_test/1]).
-export([get_quote_without_destination_test/1]).
-export([get_quote_without_destination_fail_test/1]).
-export([unknown_withdrawal_test/1]).
-export([quote_withdrawal_test/1]).
-export([not_allowed_currency_test/1]).
-export([get_wallet_by_external_id/1]).
-export([check_withdrawal_limit_test/1]).

-export([consume_eventsinks/1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-import(ct_helper, [cfg/2]).

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [ {group, default}
    , {group, quote}
    , {group, woody}
    , {group, errors}
    , {group, eventsink}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {default, [sequence, {repeat, 2}], [
            create_destination_failed_test,
            withdrawal_to_bank_card_test,
            withdrawal_to_crypto_wallet_test,
            withdrawal_to_ripple_wallet_test,
            withdrawal_to_ripple_wallet_with_tag_test,
            unknown_withdrawal_test,
            get_wallet_by_external_id
        ]},
        {quote, [], [
            quote_encode_decode_test,
            get_quote_test,
            get_quote_without_destination_test,
            get_quote_without_destination_fail_test,
            quote_withdrawal_test
        ]},
        {woody, [], [
            woody_retry_test
        ]},
        {errors, [], [
            not_allowed_currency_test,
            check_withdrawal_limit_test
        ]},
        {eventsink, [], [
            consume_eventsinks
        ]}
    ].

-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    ct_helper:makeup_cfg([
        ct_helper:test_case_name(init),
        ct_payment_system:setup(#{
            default_termset => get_default_termset(),
            optional_apps => [
                bender_client,
                wapi_woody_client,
                wapi
            ]
        })
    ], Config).

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C).

%%

-spec init_per_group(group_name(), config()) -> config().

init_per_group(G, C) ->
    ok = ff_context:save(ff_context:create(#{
        party_client => party_client:create_client(),
        woody_context => woody_context:new(<<"init_per_group/", (atom_to_binary(G, utf8))/binary>>)
    })),
    Party = create_party(C),
    Token = issue_token(Party, [{[party], write}, {[party], read}], {deadline, 10}),
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
    ok = ct_helper:set_context(C1),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

-define(ID_PROVIDER, <<"good-one">>).
-define(ID_PROVIDER2, <<"good-two">>).
-define(ID_CLASS, <<"person">>).

-spec woody_retry_test(config()) -> test_return().

-spec create_destination_failed_test(config()) -> test_return().

create_destination_failed_test(C) ->
    Name          = <<"Keyn Fawkes">>,
    Provider      = ?ID_PROVIDER,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    Resource      = #{
        <<"type">>  => <<"BankCardDestinationResource">>,
        <<"token">> => <<"v1.megatoken">>
    },
    {error, {400, #{<<"errorType">> := <<"InvalidResourceToken">>}}}
        = create_destination(IdentityID, Resource, C).

-spec withdrawal_to_bank_card_test(config()) -> test_return().

withdrawal_to_bank_card_test(C) ->
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
    {ok, Dest}    = create_destination(IdentityID, Resource, C),
    DestID        = destination_id(Dest),
    ok            = check_destination(IdentityID, DestID, Resource, C),
    {ok, _Grants} = issue_destination_grants(DestID, C),
    % ожидаем выполнения асинхронного вызова выдачи прав на вывод
    await_destination(DestID),

    WithdrawalID  = create_withdrawal(WalletID, DestID, C),
    ok            = check_withdrawal(WalletID, DestID, WithdrawalID, C).

-spec withdrawal_to_crypto_wallet_test(config()) -> test_return().

withdrawal_to_crypto_wallet_test(C) ->
    Name          = <<"Tyler Durden">>,
    Provider      = ?ID_PROVIDER2,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    ok            = check_identity(Name, IdentityID, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),
    ok            = check_wallet(WalletID, C),
    Resource      = make_crypto_wallet_resource('Ethereum'),
    {ok, Dest}    = create_destination(IdentityID, Resource, C),
    DestID        = destination_id(Dest),
    ok            = check_destination(IdentityID, DestID, Resource, C),
    {ok, _Grants} = issue_destination_grants(DestID, C),
    % ожидаем выполнения асинхронного вызова выдачи прав на вывод
    await_destination(DestID),

    WithdrawalID  = create_withdrawal(WalletID, DestID, C),
    ok            = check_withdrawal(WalletID, DestID, WithdrawalID, C).

-spec withdrawal_to_ripple_wallet_test(config()) -> test_return().

withdrawal_to_ripple_wallet_test(C) ->
    Name          = <<"Tyler The Creator">>,
    Provider      = ?ID_PROVIDER2,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    ok            = check_identity(Name, IdentityID, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),
    ok            = check_wallet(WalletID, C),
    Resource      = make_crypto_wallet_resource('Ripple'), % tagless to test thrift compat
    {ok, Dest}    = create_destination(IdentityID, Resource, C),
    DestID        = destination_id(Dest),
    ok            = check_destination(IdentityID, DestID, Resource, C),
    {ok, _Grants} = issue_destination_grants(DestID, C),
    % ожидаем выполнения асинхронного вызова выдачи прав на вывод
    await_destination(DestID),

    WithdrawalID  = create_withdrawal(WalletID, DestID, C),
    ok            = check_withdrawal(WalletID, DestID, WithdrawalID, C).

-spec withdrawal_to_ripple_wallet_with_tag_test(config()) -> test_return().

withdrawal_to_ripple_wallet_with_tag_test(C) ->
    Name          = <<"Tyler The Creator">>,
    Provider      = ?ID_PROVIDER2,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    ok            = check_identity(Name, IdentityID, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),
    ok            = check_wallet(WalletID, C),
    Resource      = make_crypto_wallet_resource('Ripple', <<"191191191">>),
    {ok, Dest}    = create_destination(IdentityID, Resource, C),
    DestID        = destination_id(Dest),
    ok            = check_destination(IdentityID, DestID, Resource, C),
    {ok, _Grants} = issue_destination_grants(DestID, C),
    % ожидаем выполнения асинхронного вызова выдачи прав на вывод
    await_destination(DestID),

    WithdrawalID  = create_withdrawal(WalletID, DestID, C),
    ok            = check_withdrawal(WalletID, DestID, WithdrawalID, C).

-spec check_withdrawal_limit_test(config()) -> test_return().

check_withdrawal_limit_test(C) ->
    Name          = <<"Tony Dacota">>,
    Provider      = ?ID_PROVIDER,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    ok            = check_identity(Name, IdentityID, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),
    ok            = check_wallet(WalletID, C),
    CardToken     = store_bank_card(C),
    {ok, _Card}   = get_bank_card(CardToken, C),
    Resource      = make_bank_card_resource(CardToken),
    {ok, Dest}    = create_destination(IdentityID, Resource, C),
    DestID        = destination_id(Dest),
    ok            = check_destination(IdentityID, DestID, Resource, C),
    {ok, _Grants} = issue_destination_grants(DestID, C),
    % ожидаем выполнения асинхронного вызова выдачи прав на вывод
    await_destination(DestID),

    {error, {422, #{<<"message">> := <<"Invalid cash amount">>}}} = call_api(
        fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
        #{body => genlib_map:compact(#{
            <<"wallet">> => WalletID,
            <<"destination">> => DestID,
            <<"body">> => #{
                <<"amount">> => 1000000000,
                <<"currency">> => <<"RUB">>
            },
            <<"quoteToken">> => undefined
        })},
        ct_helper:cfg(context, C)
    ).

-spec unknown_withdrawal_test(config()) -> test_return().

unknown_withdrawal_test(C) ->
    ?assertEqual({error, {404, #{}}}, get_withdrawal(<<"unexist withdrawal">>, C)).

-spec quote_encode_decode_test(config()) -> test_return().

quote_encode_decode_test(C) ->
    PartyID       = cfg(party, C),
    Name          = <<"Keyn Fawkes">>,
    Provider      = <<"quote-owner">>,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),
    CardToken     = store_bank_card(C),
    Resource      = make_bank_card_resource(CardToken),
    {ok, Dest}    = create_destination(IdentityID, Resource, C),
    DestID        = destination_id(Dest),
    % ожидаем авторизации назначения вывода
    await_destination(DestID),


    Data = #{
        <<"version">>       => 1,
        <<"walletID">>      => WalletID,
        <<"destinationID">> => DestID,
        <<"partyID">>       => PartyID,
        <<"cashFrom">>      => #{
            <<"amount">>   => 100,
            <<"currency">> => <<"RUB">>
        },
        <<"cashTo">>        => #{
            <<"amount">>   => 100,
            <<"currency">> => <<"USD">>
        },
        <<"createdAt">>     => ?TIMESTAMP,
        <<"expiresOn">>     => ?TIMESTAMP,
        <<"quoteData">>     => #{
            <<"version">> => ?INTEGER,
            <<"quote_data">> => #{<<"test">> => <<"test">>},
            <<"provider_id">> => ?INTEGER,
            <<"resource_id">> => #{<<"bank_card">> => <<"test">>}
        }
    },
    JSONData = jsx:encode(Data),
    {ok, Token} = wapi_signer:sign(JSONData),

    _WithdrawalID = create_withdrawal(
        WalletID,
        DestID,
        C,
        Token
    ).

-spec get_quote_test(config()) -> test_return().

get_quote_test(C) ->
    Name          = <<"Keyn Fawkes">>,
    Provider      = <<"quote-owner">>,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),
    CardToken     = store_bank_card(C),
    Resource      = make_bank_card_resource(CardToken),
    {ok, Dest}    = create_destination(IdentityID, Resource, C),
    DestID        = destination_id(Dest),
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
        <<"amount">> => 123,
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

-spec quote_withdrawal_test(config()) -> test_return().

quote_withdrawal_test(C) ->
    Name          = <<"Keyn Fawkes">>,
    Provider      = <<"quote-owner">>,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    WalletID      = create_wallet(IdentityID, C),
    CardToken     = store_bank_card(C),
    Resource      = make_bank_card_resource(CardToken),
    {ok, Dest}    = create_destination(IdentityID, Resource, C),
    DestID        = destination_id(Dest),
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

-spec get_wallet_by_external_id(config()) ->
    test_return().

get_wallet_by_external_id(C) ->
    Name          = <<"Keyn Fawkes">>,
    Provider      = <<"quote-owner">>,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    ExternalID    = ?STRING,
    WalletID      = create_wallet(IdentityID, #{<<"externalID">> => ExternalID}, C),
    {ok, Wallet} = call_api(
        fun swag_client_wallet_wallets_api:get_wallet_by_external_id/3,
        #{qs_val => #{<<"externalID">> => ExternalID}},
        ct_helper:cfg(context, C)
    ),
    WalletID = maps:get(<<"id">>, Wallet).

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
    create_wallet(IdentityID, #{}, C).

create_wallet(IdentityID, Params, C) ->
    DefaultParams = #{
            <<"name">>     => <<"Worldwide PHP Awareness Initiative">>,
            <<"identity">> => IdentityID,
            <<"currency">> => <<"RUB">>,
            <<"metadata">> => #{
                ?STRING => ?STRING
            }
    },
    {ok, Wallet} = call_api(
        fun swag_client_wallet_wallets_api:create_wallet/3,
        #{body => maps:merge(DefaultParams, Params)},
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

make_crypto_wallet_resource(Currency) ->
    make_crypto_wallet_resource(Currency, undefined).

make_crypto_wallet_resource(Currency, MaybeTag) ->
    genlib_map:compact(#{
        <<"type">>     => <<"CryptoWalletDestinationResource">>,
        <<"id">>       => <<"0610899fa9a3a4300e375ce582762273">>,
        <<"currency">> => genlib:to_binary(Currency),
        <<"tag">>      => MaybeTag
    }).

destination_id(Dest) ->
    maps:get(<<"id">>, Dest).

create_destination(IdentityID, Resource, C) ->
    call_api(
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
    ).

check_destination(IdentityID, DestID, Resource0, C) ->
    {ok, Dest} = get_destination(DestID, C),
    ResourceFields = [<<"type">>, <<"id">>, <<"currency">>],
    Resource = maps:with(ResourceFields, Resource0),
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

% TODO: Use this function variant after fistful-magista protocol update
% check_withdrawal(WalletID, DestID, WithdrawalID, C) ->
%     ct_helper:await(
%         ok,
%         fun () ->
%             R = call_api(fun swag_client_wallet_withdrawals_api:list_withdrawals/3,
%                          #{qs_val => #{
%                              <<"withdrawalID">> => WithdrawalID,
%                              <<"limit">> => 100
%                             }},
%                          ct_helper:cfg(context, C)),
%             case R of
%                 {ok, #{<<"result">> := []}} ->
%                     R;
%                 {ok, Withdrawal} ->
%                     #{<<"result">> := [
%                         #{<<"wallet">> := WalletID,
%                           <<"destination">> := DestID,
%                           <<"body">> := #{
%                               <<"amount">> := 100,
%                               <<"currency">> := <<"RUB">>
%                           }
%                     }]} = Withdrawal,
%                     ok;
%                 _ ->
%                     R
%             end
%         end,
%         {linear, 20, 1000}
%     ).

check_withdrawal(WalletID, DestID, WithdrawalID, C) ->
    ct_helper:await(
        ok,
        fun () ->
            case get_withdrawal(WithdrawalID, C) of
                {ok, Withdrawal} ->
                    #{
                        <<"wallet">> := WalletID,
                        <<"destination">> := DestID,
                        <<"body">> := #{
                            <<"amount">> := 100,
                            <<"currency">> := <<"RUB">>
                        }
                    } = Withdrawal,
                    ok;
                Other ->
                    Other
            end
        end,
        {linear, 20, 1000}
    ).

get_withdrawal(WithdrawalID, C) ->
    call_api(
        fun swag_client_wallet_withdrawals_api:get_withdrawal/3,
        #{binding => #{<<"withdrawalID">> => WithdrawalID}},
        ct_helper:cfg(context, C)
    ).

%%

-include_lib("ff_cth/include/ct_domain.hrl").

-spec get_default_termset() ->
    dmsl_domain_thrift:'TermSet'().

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

-spec not_allowed_currency_test(config()) -> test_return().

not_allowed_currency_test(C) ->
    Name          = <<"Keyn Fawkes">>,
    Provider      = ?ID_PROVIDER,
    Class         = ?ID_CLASS,
    IdentityID    = create_identity(Name, Provider, Class, C),
    ok            = check_identity(Name, IdentityID, Provider, Class, C),
    {error, {422, #{<<"message">> := <<"Currency not allowed">>}}} = call_api(
        fun swag_client_wallet_wallets_api:create_wallet/3,
        #{body => #{
            <<"name">>     => <<"Worldwide PHP Awareness Initiative">>,
            <<"identity">> => IdentityID,
            <<"currency">> => <<"USD">>,
            <<"metadata">> => #{
                ?STRING => ?STRING
            }
        }},
        ct_helper:cfg(context, C)
    ).

-spec consume_eventsinks(config()) -> test_return().

consume_eventsinks(_) ->
    EventSinks = [
          deposit_event_sink
        , source_event_sink
        , destination_event_sink
        , identity_event_sink
        , wallet_event_sink
        , withdrawal_event_sink
        , withdrawal_session_event_sink
    ],
    [_Events = ct_eventsink:consume(1000, Sink) || Sink <- EventSinks].
