-module(wapi_wallet_tests_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([
    create_ok/1,
    create_fail_identity_notfound/1,
    create_fail_currency_notfound/1,
    create_fail_party_inaccessible/1,
    get_ok/1,
    get_fail_wallet_notfound/1,
    get_by_external_id_ok/1,
    get_account_ok/1,
    get_account_fail_get_context_wallet_notfound/1,
    get_account_fail_get_accountbalance_wallet_notfound/1
]).

% common-api is used since it is the domain used in production RN
% TODO: change to wallet-api (or just omit since it is the default one) when new tokens will be a thing
-define(DOMAIN, <<"common-api">>).
-define(badresp(Code), {error, {invalid_response_code, Code}}).
-define(emptyresp(Code), {error, {Code, #{}}}).

-type test_case_name() :: atom().
-type config() :: [{atom(), any()}].
-type group_name() :: atom().

-behaviour(supervisor).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() -> [test_case_name()].
all() ->
    [
        {group, base}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {base, [], [
            create_ok,
            create_fail_identity_notfound,
            create_fail_currency_notfound,
            create_fail_party_inaccessible,
            get_ok,
            get_fail_wallet_notfound,
            get_by_external_id_ok,
            get_account_ok,
            get_account_fail_get_context_wallet_notfound,
            get_account_fail_get_accountbalance_wallet_notfound
        ]}
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    %% TODO remove this after cut off wapi
    ok = application:set_env(wapi, transport, thrift),
    ct_helper:makeup_cfg(
        [
            ct_helper:test_case_name(init),
            ct_payment_system:setup(#{
                optional_apps => [
                    bender_client,
                    wapi_woody_client,
                    wapi
                ]
            })
        ],
        Config
    ).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    %% TODO remove this after cut off wapi
    ok = application:unset_env(wapi, transport),
    ok = ct_payment_system:shutdown(C).

-spec init_per_group(group_name(), config()) -> config().
init_per_group(Group, Config) when Group =:= base ->
    ok = ff_context:save(
        ff_context:create(#{
            party_client => party_client:create_client(),
            woody_context => woody_context:new(<<"init_per_group/", (atom_to_binary(Group, utf8))/binary>>)
        })
    ),
    Party = create_party(Config),
    BasePermissions = [
        {[party], read},
        {[party], write}
    ],
    {ok, Token} = wapi_ct_helper:issue_token(Party, BasePermissions, {deadline, 10}, ?DOMAIN),
    Config1 = [{party, Party} | Config],
    [{context, wapi_ct_helper:get_context(Token)} | Config1];
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    [{test_sup, wapi_ct_helper:start_mocked_service_sup(?MODULE)} | C1].

-spec end_per_testcase(test_case_name(), config()) -> config().
end_per_testcase(_Name, C) ->
    ok = ct_helper:unset_context(),
    wapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec create_ok(config()) -> _.
create_ok(C) ->
    PartyID = ?config(party, C),
    create_wallet_start_mocks(C, fun() -> {ok, ?WALLET(PartyID)} end),
    {ok, _} = create_wallet_call_api(C).

-spec create_fail_identity_notfound(config()) -> _.
create_fail_identity_notfound(C) ->
    create_wallet_start_mocks(C, fun() -> throw(#fistful_IdentityNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity">>}}},
        create_wallet_call_api(C)
    ).

-spec create_fail_currency_notfound(config()) -> _.
create_fail_currency_notfound(C) ->
    create_wallet_start_mocks(C, fun() -> throw(#fistful_CurrencyNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Currency not supported">>}}},
        create_wallet_call_api(C)
    ).

-spec create_fail_party_inaccessible(config()) -> _.
create_fail_party_inaccessible(C) ->
    create_wallet_start_mocks(C, fun() -> throw(#fistful_PartyInaccessible{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Identity inaccessible">>}}},
        create_wallet_call_api(C)
    ).

-spec get_ok(config()) -> _.
get_ok(C) ->
    PartyID = ?config(party, C),
    get_wallet_start_mocks(C, fun() -> {ok, ?WALLET(PartyID)} end),
    {ok, _} = get_wallet_call_api(C).

-spec get_fail_wallet_notfound(config()) -> _.
get_fail_wallet_notfound(C) ->
    get_wallet_start_mocks(C, fun() -> throw(#fistful_WalletNotFound{}) end),
    ?assertEqual(
        {error, {404, #{}}},
        get_wallet_call_api(C)
    ).

-spec get_by_external_id_ok(config()) -> _.
get_by_external_id_ok(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services(
        [
            {bender_thrift, fun('GetInternalID', _) -> {ok, ?GET_INTERNAL_ID_RESULT} end},
            {fistful_wallet, fun('Get', _) -> {ok, ?WALLET(PartyID)} end}
        ],
        C
    ),
    {ok, _} = call_api(
        fun swag_client_wallet_wallets_api:get_wallet_by_external_id/3,
        #{
            qs_val => #{
                <<"externalID">> => ?STRING
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec get_account_ok(config()) -> _.
get_account_ok(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services(
        [
            {fistful_wallet, fun
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
                ('GetAccountBalance', _) -> {ok, ?ACCOUNT_BALANCE}
            end}
        ],
        C
    ),
    {ok, _} = get_account_call_api(C).

-spec get_account_fail_get_context_wallet_notfound(config()) -> _.
get_account_fail_get_context_wallet_notfound(C) ->
    wapi_ct_helper:mock_services(
        [
            {fistful_wallet, fun
                ('GetContext', _) -> throw(#fistful_WalletNotFound{});
                ('GetAccountBalance', _) -> {ok, ?ACCOUNT_BALANCE}
            end}
        ],
        C
    ),
    ?assertEqual(
        {error, {404, #{}}},
        get_account_call_api(C)
    ).

-spec get_account_fail_get_accountbalance_wallet_notfound(config()) -> _.
get_account_fail_get_accountbalance_wallet_notfound(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services(
        [
            {fistful_wallet, fun
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
                ('GetAccountBalance', _) -> throw(#fistful_WalletNotFound{})
            end}
        ],
        C
    ),
    ?assertEqual(
        {error, {404, #{}}},
        get_account_call_api(C)
    ).

%%

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

-spec call_api(function(), map(), wapi_client_lib:context()) -> {ok, term()} | {error, term()}.
call_api(F, Params, Context) ->
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = F(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).

create_wallet_call_api(C) ->
    call_api(
        fun swag_client_wallet_wallets_api:create_wallet/3,
        #{
            body => #{
                <<"name">> => ?STRING,
                <<"identity">> => ?STRING,
                <<"currency">> => ?RUB,
                <<"metadata">> => #{
                    <<"somedata">> => ?STRING
                }
            }
        },
        ct_helper:cfg(context, C)
    ).

get_wallet_call_api(C) ->
    call_api(
        fun swag_client_wallet_wallets_api:get_wallet/3,
        #{
            binding => #{
                <<"walletID">> => ?STRING
            }
        },
        ct_helper:cfg(context, C)
    ).

get_account_call_api(C) ->
    call_api(
        fun swag_client_wallet_wallets_api:get_wallet_account/3,
        #{
            binding => #{
                <<"walletID">> => ?STRING
            }
        },
        ct_helper:cfg(context, C)
    ).

create_wallet_start_mocks(C, CreateResultFun) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services(
        [
            {fistful_identity, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
            {fistful_wallet, fun('Create', _) -> CreateResultFun() end}
        ],
        C
    ).

get_wallet_start_mocks(C, GetResultFun) ->
    wapi_ct_helper:mock_services(
        [
            {fistful_wallet, fun('Get', _) -> GetResultFun() end}
        ],
        C
    ).
