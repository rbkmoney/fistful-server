-module(wapi_destination_tests_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([resource_get_destination_notfound_test/1]).
-export([bank_card_resource_test/1]).
-export([bank_card_create_identity_notfound_test/1]).
-export([bank_card_create_currency_notfound_test/1]).
-export([bank_card_create_party_inaccessible_test/1]).
-export([bitcoin_resource_test/1]).
-export([bitcoin_create_identity_notfound_test/1]).
-export([bitcoin_create_currency_notfound_test/1]).
-export([bitcoin_create_party_inaccessible_test/1]).
-export([litecoin_resource_test/1]).
-export([litecoin_create_identity_notfound_test/1]).
-export([litecoin_create_currency_notfound_test/1]).
-export([litecoin_create_party_inaccessible_test/1]).
-export([bitcoin_cash_resource_test/1]).
-export([bitcoin_cash_create_identity_notfound_test/1]).
-export([bitcoin_cash_create_currency_notfound_test/1]).
-export([bitcoin_cash_create_party_inaccessible_test/1]).
-export([ripple_resource_test/1]).
-export([ripple_create_identity_notfound_test/1]).
-export([ripple_create_currency_notfound_test/1]).
-export([ripple_create_party_inaccessible_test/1]).
-export([ethereum_resource_test/1]).
-export([ethereum_create_identity_notfound_test/1]).
-export([ethereum_create_currency_notfound_test/1]).
-export([ethereum_create_party_inaccessible_test/1]).
-export([usdt_resource_test/1]).
-export([usdt_create_identity_notfound_test/1]).
-export([usdt_create_currency_notfound_test/1]).
-export([usdt_create_party_inaccessible_test/1]).
-export([zcash_resource_test/1]).
-export([zcash_create_identity_notfound_test/1]).
-export([zcash_create_currency_notfound_test/1]).
-export([zcash_create_party_inaccessible_test/1]).

% common-api is used since it is the domain used in production RN
% TODO: change to wallet-api (or just omit since it is the default one) when new tokens will be a thing
-define(DOMAIN, <<"common-api">>).
-define(badresp(Code), {error, {invalid_response_code, Code}}).
-define(emptyresp(Code), {error, {Code, #{}}}).

-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].
-type group_name()      :: atom().

-behaviour(supervisor).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() ->
    [test_case_name()].
all() ->
    [
        {group, default}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [], [
            resource_get_destination_notfound_test,
            bank_card_resource_test,
            bank_card_create_identity_notfound_test,
            bank_card_create_currency_notfound_test,
            bank_card_create_party_inaccessible_test,
            bitcoin_resource_test,
            bitcoin_create_identity_notfound_test,
            bitcoin_create_currency_notfound_test,
            bitcoin_create_party_inaccessible_test,
            litecoin_resource_test,
            litecoin_create_identity_notfound_test,
            litecoin_create_currency_notfound_test,
            litecoin_create_party_inaccessible_test,
            bitcoin_cash_resource_test,
            bitcoin_cash_create_identity_notfound_test,
            bitcoin_cash_create_currency_notfound_test,
            bitcoin_cash_create_party_inaccessible_test,
            ripple_resource_test,
            ripple_create_identity_notfound_test,
            ripple_create_currency_notfound_test,
            ripple_create_party_inaccessible_test,
            ethereum_resource_test,
            ethereum_create_identity_notfound_test,
            ethereum_create_currency_notfound_test,
            ethereum_create_party_inaccessible_test,
            usdt_resource_test,
            usdt_create_identity_notfound_test,
            usdt_create_currency_notfound_test,
            usdt_create_party_inaccessible_test,
            zcash_resource_test,
            zcash_create_identity_notfound_test,
            zcash_create_currency_notfound_test,
            zcash_create_party_inaccessible_test
        ]}
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config0) ->
    %% TODO remove this after cut off wapi
    ok = application:set_env(wapi, transport, thrift),
    ct_helper:makeup_cfg([
        ct_helper:test_case_name(init),
        ct_payment_system:setup(#{
            optional_apps => [
                bender_client,
                wapi_woody_client,
                wapi
            ]
        })
    ], Config0).

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    %% TODO remove this after cut off wapi
    ok = application:unset_env(wapi, transport),
    ok = ct_payment_system:shutdown(C).

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(default = Group, Config) ->
    ok = ff_context:save(ff_context:create(#{
        party_client => party_client:create_client(),
        woody_context => woody_context:new(<<"init_per_group/", (atom_to_binary(Group, utf8))/binary>>)
    })),
    Party = create_party(Config),
    {ok, Token} = wapi_ct_helper:issue_token(Party, [{[party], write}], {deadline, 10}, ?DOMAIN),
    Config1 = [{party, Party} | Config],
    [{context, wapi_ct_helper:get_context(Token)} | Config1];
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) ->
    _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    [{test_sup, wapi_ct_helper:start_mocked_service_sup(?MODULE)} | C1].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    ok = ct_helper:unset_context(),
    wapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec resource_get_destination_notfound_test(config()) -> _.
resource_get_destination_notfound_test(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('Get', _) -> throw(#fistful_DestinationNotFound{}) end}
    ], C),
    ?assertEqual(
        {error, {404, #{}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:get_destination/3,
            #{
                binding => #{
                    <<"destinationID">> => ?STRING
                }
            },
            ct_helper:cfg(context, C)
        )
    ).

-spec bank_card_resource_test(config()) -> _.
bank_card_resource_test(C) ->
    {ok, Resource, SwagResource} = do_destination_lifecycle(bank_card, C),
    {bank_card, #'ResourceBankCard'{bank_card = R}} = Resource,
    ?assertEqual(<<"BankCardDestinationResource">>, maps:get(<<"type">>, SwagResource)),
    ?assertEqual(R#'BankCard'.token, maps:get(<<"token">>, SwagResource)),
    ?assertEqual(R#'BankCard'.bin, maps:get(<<"bin">>, SwagResource)),
    ?assertEqual(R#'BankCard'.masked_pan, maps:get(<<"lastDigits">>, SwagResource)).

-spec bank_card_create_identity_notfound_test(config()) -> _.
bank_card_create_identity_notfound_test(C) ->
    Destination = make_destination(C, bank_card),
    create_destination_start_mocks(C, fun() -> throw(#fistful_IdentityNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec bank_card_create_currency_notfound_test(config()) -> _.
bank_card_create_currency_notfound_test(C) ->
    Destination = make_destination(C, bank_card),
    create_destination_start_mocks(C, fun() -> throw(#fistful_CurrencyNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Currency not supported">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec bank_card_create_party_inaccessible_test(config()) -> _.
bank_card_create_party_inaccessible_test(C) ->
    Destination = make_destination(C, bank_card),
    create_destination_start_mocks(C, fun() -> throw(#fistful_PartyInaccessible{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Identity inaccessible">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec bitcoin_resource_test(config()) -> _.
bitcoin_resource_test(C) ->
    {ok, Resource, SwagResource} = do_destination_lifecycle(bitcoin, C),
    ?assertEqual(<<"CryptoWalletDestinationResource">>, maps:get(<<"type">>, SwagResource)),
    ?assertEqual(<<"Bitcoin">>, maps:get(<<"currency">>, SwagResource)),
    {crypto_wallet, #'ResourceCryptoWallet'{crypto_wallet = #'CryptoWallet'{id = ID}}} = Resource,
    ?assertEqual(ID, maps:get(<<"id">>, SwagResource)).

-spec bitcoin_create_identity_notfound_test(config()) -> _.
bitcoin_create_identity_notfound_test(C) ->
    Destination = make_destination(C, bitcoin),
    create_destination_start_mocks(C, fun() -> throw(#fistful_IdentityNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec bitcoin_create_currency_notfound_test(config()) -> _.
bitcoin_create_currency_notfound_test(C) ->
    Destination = make_destination(C, bitcoin),
    create_destination_start_mocks(C, fun() -> throw(#fistful_CurrencyNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Currency not supported">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec bitcoin_create_party_inaccessible_test(config()) -> _.
bitcoin_create_party_inaccessible_test(C) ->
    Destination = make_destination(C, bitcoin),
    create_destination_start_mocks(C, fun() -> throw(#fistful_PartyInaccessible{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Identity inaccessible">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec litecoin_resource_test(config()) -> _.
litecoin_resource_test(C) ->
    {ok, Resource, SwagResource} = do_destination_lifecycle(litecoin, C),
    ?assertEqual(<<"CryptoWalletDestinationResource">>, maps:get(<<"type">>, SwagResource)),
    ?assertEqual(<<"Litecoin">>, maps:get(<<"currency">>, SwagResource)),
    {crypto_wallet, #'ResourceCryptoWallet'{crypto_wallet = #'CryptoWallet'{id = ID}}} = Resource,
    ?assertEqual(ID, maps:get(<<"id">>, SwagResource)).

-spec litecoin_create_identity_notfound_test(config()) -> _.
litecoin_create_identity_notfound_test(C) ->
    Destination = make_destination(C, litecoin),
    create_destination_start_mocks(C, fun() -> throw(#fistful_IdentityNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec litecoin_create_currency_notfound_test(config()) -> _.
litecoin_create_currency_notfound_test(C) ->
    Destination = make_destination(C, litecoin),
    create_destination_start_mocks(C, fun() -> throw(#fistful_CurrencyNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Currency not supported">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec litecoin_create_party_inaccessible_test(config()) -> _.
litecoin_create_party_inaccessible_test(C) ->
    Destination = make_destination(C, litecoin),
    create_destination_start_mocks(C, fun() -> throw(#fistful_PartyInaccessible{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Identity inaccessible">>}}},
        create_destination_call_api(C, Destination)
    ).


-spec bitcoin_cash_resource_test(config()) -> _.
bitcoin_cash_resource_test(C) ->
    {ok, Resource, SwagResource} = do_destination_lifecycle(bitcoin_cash, C),
    ?assertEqual(<<"CryptoWalletDestinationResource">>, maps:get(<<"type">>, SwagResource)),
    ?assertEqual(<<"BitcoinCash">>, maps:get(<<"currency">>, SwagResource)),
    {crypto_wallet, #'ResourceCryptoWallet'{crypto_wallet = #'CryptoWallet'{id = ID}}} = Resource,
    ?assertEqual(ID, maps:get(<<"id">>, SwagResource)).

-spec bitcoin_cash_create_identity_notfound_test(config()) -> _.
bitcoin_cash_create_identity_notfound_test(C) ->
    Destination = make_destination(C, bitcoin_cash),
    create_destination_start_mocks(C, fun() -> throw(#fistful_IdentityNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec bitcoin_cash_create_currency_notfound_test(config()) -> _.
bitcoin_cash_create_currency_notfound_test(C) ->
    Destination = make_destination(C, bitcoin_cash),
    create_destination_start_mocks(C, fun() -> throw(#fistful_CurrencyNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Currency not supported">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec bitcoin_cash_create_party_inaccessible_test(config()) -> _.
bitcoin_cash_create_party_inaccessible_test(C) ->
    Destination = make_destination(C, bitcoin_cash),
    create_destination_start_mocks(C, fun() -> throw(#fistful_PartyInaccessible{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Identity inaccessible">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec ripple_resource_test(config()) -> _.
ripple_resource_test(C) ->
    {ok, Resource, SwagResource} = do_destination_lifecycle(ripple, C),
    ?assertEqual(<<"CryptoWalletDestinationResource">>, maps:get(<<"type">>, SwagResource)),
    ?assertEqual(<<"Ripple">>, maps:get(<<"currency">>, SwagResource)),
    {crypto_wallet, #'ResourceCryptoWallet'{crypto_wallet = #'CryptoWallet'{
        id = ID,
        data = {ripple, #'CryptoDataRipple'{
            tag = Tag
        }}
    }}} = Resource,
    ?assertEqual(ID, maps:get(<<"id">>, SwagResource)),
    ?assertEqual(Tag, maps:get(<<"tag">>, SwagResource)).

-spec ripple_create_identity_notfound_test(config()) -> _.
ripple_create_identity_notfound_test(C) ->
    Destination = make_destination(C, ripple),
    create_destination_start_mocks(C, fun() -> throw(#fistful_IdentityNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec ripple_create_currency_notfound_test(config()) -> _.
ripple_create_currency_notfound_test(C) ->
    Destination = make_destination(C, ripple),
    create_destination_start_mocks(C, fun() -> throw(#fistful_CurrencyNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Currency not supported">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec ripple_create_party_inaccessible_test(config()) -> _.
ripple_create_party_inaccessible_test(C) ->
    Destination = make_destination(C, ripple),
    create_destination_start_mocks(C, fun() -> throw(#fistful_PartyInaccessible{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Identity inaccessible">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec ethereum_resource_test(config()) -> _.
ethereum_resource_test(C) ->
    {ok, Resource, SwagResource} = do_destination_lifecycle(ethereum, C),
    ?assertEqual(<<"CryptoWalletDestinationResource">>, maps:get(<<"type">>, SwagResource)),
    ?assertEqual(<<"Ethereum">>, maps:get(<<"currency">>, SwagResource)),
    {crypto_wallet, #'ResourceCryptoWallet'{crypto_wallet = #'CryptoWallet'{id = ID}}} = Resource,
    ?assertEqual(ID, maps:get(<<"id">>, SwagResource)).

-spec ethereum_create_identity_notfound_test(config()) -> _.
ethereum_create_identity_notfound_test(C) ->
    Destination = make_destination(C, ethereum),
    create_destination_start_mocks(C, fun() -> throw(#fistful_IdentityNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec ethereum_create_currency_notfound_test(config()) -> _.
ethereum_create_currency_notfound_test(C) ->
    Destination = make_destination(C, ethereum),
    create_destination_start_mocks(C, fun() -> throw(#fistful_CurrencyNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Currency not supported">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec ethereum_create_party_inaccessible_test(config()) -> _.
ethereum_create_party_inaccessible_test(C) ->
    Destination = make_destination(C, ethereum),
    create_destination_start_mocks(C, fun() -> throw(#fistful_PartyInaccessible{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Identity inaccessible">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec usdt_resource_test(config()) -> _.
usdt_resource_test(C) ->
    {ok, Resource, SwagResource} = do_destination_lifecycle(usdt, C),
    ?assertEqual(<<"CryptoWalletDestinationResource">>, maps:get(<<"type">>, SwagResource)),
    ?assertEqual(<<"USDT">>, maps:get(<<"currency">>, SwagResource)),
    {crypto_wallet, #'ResourceCryptoWallet'{crypto_wallet = #'CryptoWallet'{id = ID}}} = Resource,
    ?assertEqual(ID, maps:get(<<"id">>, SwagResource)).

-spec usdt_create_identity_notfound_test(config()) -> _.
usdt_create_identity_notfound_test(C) ->
    Destination = make_destination(C, usdt),
    create_destination_start_mocks(C, fun() -> throw(#fistful_IdentityNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec usdt_create_currency_notfound_test(config()) -> _.
usdt_create_currency_notfound_test(C) ->
    Destination = make_destination(C, usdt),
    create_destination_start_mocks(C, fun() -> throw(#fistful_CurrencyNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Currency not supported">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec usdt_create_party_inaccessible_test(config()) -> _.
usdt_create_party_inaccessible_test(C) ->
    Destination = make_destination(C, usdt),
    create_destination_start_mocks(C, fun() -> throw(#fistful_PartyInaccessible{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Identity inaccessible">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec zcash_resource_test(config()) -> _.
zcash_resource_test(C) ->
    {ok, Resource, SwagResource} = do_destination_lifecycle(zcash, C),
    ?assertEqual(<<"CryptoWalletDestinationResource">>, maps:get(<<"type">>, SwagResource)),
    ?assertEqual(<<"Zcash">>, maps:get(<<"currency">>, SwagResource)),
    {crypto_wallet, #'ResourceCryptoWallet'{crypto_wallet = #'CryptoWallet'{id = ID}}} = Resource,
    ?assertEqual(ID, maps:get(<<"id">>, SwagResource)).

-spec zcash_create_identity_notfound_test(config()) -> _.
zcash_create_identity_notfound_test(C) ->
    Destination = make_destination(C, zcash),
    create_destination_start_mocks(C, fun() -> throw(#fistful_IdentityNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec zcash_create_currency_notfound_test(config()) -> _.
zcash_create_currency_notfound_test(C) ->
    Destination = make_destination(C, zcash),
    create_destination_start_mocks(C, fun() -> throw(#fistful_CurrencyNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Currency not supported">>}}},
        create_destination_call_api(C, Destination)
    ).

-spec zcash_create_party_inaccessible_test(config()) -> _.
zcash_create_party_inaccessible_test(C) ->
    Destination = make_destination(C, zcash),
    create_destination_start_mocks(C, fun() -> throw(#fistful_PartyInaccessible{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Identity inaccessible">>}}},
        create_destination_call_api(C, Destination)
    ).

%%

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

-spec call_api(function(), map(), wapi_client_lib:context()) ->
    {ok, term()} | {error, term()}.
call_api(F, Params, Context) ->
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = F(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).

create_destination_call_api(C, Destination) ->
    call_api(
        fun swag_client_wallet_withdrawals_api:create_destination/3,
        #{
            body => build_destination_spec(Destination)
        },
        ct_helper:cfg(context, C)
    ).

create_destination_start_mocks(C, CreateDestinationResultFun) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('Create', _) -> CreateDestinationResultFun() end}
    ], C).

make_destination(C, ResourceType) ->
    PartyID = ?config(party, C),
    Identity = generate_identity(PartyID),
    Resource = generate_resource(ResourceType),
    Context = generate_context(PartyID),
    generate_destination(Identity#idnt_IdentityState.id, Resource, Context).

do_destination_lifecycle(ResourceType, C) ->
    PartyID = ?config(party, C),
    Identity = generate_identity(PartyID),
    Resource = generate_resource(ResourceType),
    Context = generate_context(PartyID),
    Destination = generate_destination(Identity#idnt_IdentityState.id, Resource, Context),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination,
            fun
                ('Create', _) -> {ok, Destination};
                ('Get', _) -> {ok, Destination}
            end
        }
    ], C),
    {ok, CreateResult} = call_api(
        fun swag_client_wallet_withdrawals_api:create_destination/3,
        #{
            body => build_destination_spec(Destination)
        },
        ct_helper:cfg(context, C)
    ),
    {ok, GetResult} = call_api(
        fun swag_client_wallet_withdrawals_api:get_destination/3,
        #{
            binding => #{
                <<"destinationID">> => ?STRING
            }
        },
        ct_helper:cfg(context, C)
    ),
    ?assertEqual(CreateResult, GetResult),
    {ok, GetByIDResult} = call_api(
        fun swag_client_wallet_withdrawals_api:get_destination_by_external_id/3,
        #{
            binding => #{
                <<"externalID">> => Destination#dst_DestinationState.external_id
            }
        },
        ct_helper:cfg(context, C)
    ),
    ?assertEqual(GetResult, GetByIDResult),
    ?assertEqual(Destination#dst_DestinationState.id, maps:get(<<"id">>, CreateResult)),
    ?assertEqual(Destination#dst_DestinationState.external_id, maps:get(<<"externalID">>, CreateResult)),
    ?assertEqual(Identity#idnt_IdentityState.id, maps:get(<<"identity">>, CreateResult)),
    ?assertEqual(
        ((Destination#dst_DestinationState.account)#account_Account.currency)#'CurrencyRef'.symbolic_code,
        maps:get(<<"currency">>, CreateResult)
    ),
    ?assertEqual(<<"Authorized">>, maps:get(<<"status">>, CreateResult)),
    ?assertEqual(false, maps:get(<<"isBlocked">>, CreateResult)),
    ?assertEqual(Destination#dst_DestinationState.created_at, maps:get(<<"createdAt">>, CreateResult)),
    ?assertEqual(#{<<"key">> => <<"val">>}, maps:get(<<"metadata">>, CreateResult)),
    {ok, Resource, maps:get(<<"resource">>, CreateResult)}.

build_destination_spec(D) ->
    #{
        <<"name">> => D#dst_DestinationState.name,
        <<"identity">> => (D#dst_DestinationState.account)#account_Account.identity,
        <<"currency">> => ((D#dst_DestinationState.account)#account_Account.currency)#'CurrencyRef'.symbolic_code,
        <<"externalID">> => D#dst_DestinationState.external_id,
        <<"resource">> => build_resource_spec(D#dst_DestinationState.resource)
    }.

build_resource_spec({bank_card, R}) ->
    #{
        <<"type">> => <<"BankCardDestinationResource">>,
        <<"token">> => wapi_crypto:encrypt_bankcard_token(R#'ResourceBankCard'.bank_card)
    };
build_resource_spec({crypto_wallet, R}) ->
    Spec = build_crypto_cyrrency_spec((R#'ResourceCryptoWallet'.crypto_wallet)#'CryptoWallet'.data),
    Spec#{
        <<"type">> => <<"CryptoWalletDestinationResource">>,
        <<"id">> => (R#'ResourceCryptoWallet'.crypto_wallet)#'CryptoWallet'.id
    }.

build_crypto_cyrrency_spec({bitcoin, #'CryptoDataBitcoin'{}}) ->
    #{<<"currency">> => <<"Bitcoin">>};
build_crypto_cyrrency_spec({litecoin, #'CryptoDataLitecoin'{}}) ->
    #{<<"currency">> => <<"Litecoin">>};
build_crypto_cyrrency_spec({bitcoin_cash, #'CryptoDataBitcoinCash'{}}) ->
    #{<<"currency">> => <<"BitcoinCash">>};
build_crypto_cyrrency_spec({ripple, #'CryptoDataRipple'{tag = Tag}}) ->
    #{
        <<"currency">> => <<"Ripple">>,
        <<"tag">> => Tag
    };
build_crypto_cyrrency_spec({ethereum, #'CryptoDataEthereum'{}}) ->
    #{<<"currency">> => <<"Ethereum">>};
build_crypto_cyrrency_spec({usdt, #'CryptoDataUSDT'{}}) ->
    #{<<"currency">> => <<"USDT">>};
build_crypto_cyrrency_spec({zcash, #'CryptoDataZcash'{}}) ->
    #{<<"currency">> => <<"Zcash">>}.

uniq() ->
    genlib:bsuuid().

generate_identity(PartyID) ->
    #idnt_IdentityState{
        id = uniq(),
        name = uniq(),
        party_id = PartyID,
        provider_id = uniq(),
        class_id = uniq(),
        context = generate_context(PartyID)
    }.

generate_context(PartyID) ->
    #{
        <<"com.rbkmoney.wapi">> => {obj, #{
            {str, <<"owner">>} => {str, PartyID},
            {str, <<"name">>} => {str, uniq()},
            {str, <<"metadata">>} => {obj, #{{str, <<"key">>} => {str, <<"val">>}}}
        }}
    }.

generate_destination(IdentityID, Resource, Context) ->
    ID = uniq(),
    #dst_DestinationState{
        id          = ID,
        name        = uniq(),
        status      = {authorized, #dst_Authorized{}},
        account     = #account_Account{
            id = ID,
            identity = IdentityID,
            currency = #'CurrencyRef'{
                symbolic_code = <<"RUB">>
            },
            accounter_account_id = 123
        },
        resource    = Resource,
        external_id = uniq(),
        created_at  = <<"2016-03-22T06:12:27Z">>,
        blocking    = unblocked,
        metadata    = #{<<"key">> => {str, <<"val">>}},
        context     = Context
    }.

generate_resource(bank_card) ->
    {bank_card, #'ResourceBankCard'{bank_card = #'BankCard'{
        token = uniq(),
        bin = <<"424242">>,
        masked_pan = <<"4242">>,
        bank_name = uniq(),
        payment_system = visa,
        issuer_country = rus,
        card_type = debit,
        exp_date = #'BankCardExpDate'{
            month = 12,
            year = 2200
        }
    }}};
generate_resource(ResourceType) ->
    {Currency, Params} = generate_wallet_data(ResourceType),
    {crypto_wallet, #'ResourceCryptoWallet'{crypto_wallet = #'CryptoWallet'{
        id = uniq(),
        data = {Currency, Params},
        currency = Currency
    }}}.

generate_wallet_data(bitcoin) ->
    {bitcoin, #'CryptoDataBitcoin'{}};
generate_wallet_data(litecoin) ->
    {litecoin, #'CryptoDataLitecoin'{}};
generate_wallet_data(bitcoin_cash) ->
    {bitcoin_cash, #'CryptoDataBitcoinCash'{}};
generate_wallet_data(ripple) ->
    {ripple, #'CryptoDataRipple'{
        tag = <<"191919192">>
    }};
generate_wallet_data(ethereum) ->
    {ethereum, #'CryptoDataEthereum'{}};
generate_wallet_data(usdt) ->
    {usdt, #'CryptoDataUSDT'{}};
generate_wallet_data(zcash) ->
    {zcash, #'CryptoDataZcash'{}}.
