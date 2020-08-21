-module(wapi_stat_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("wapi_wallet_dummy_data.hrl").

-include_lib("fistful_proto/include/ff_proto_fistful_stat_thrift.hrl").

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
    list_wallets/1,
    list_wallets_invalid_error/1,
    list_wallets_bad_token_error/1,
    list_withdrawals/1,
    list_withdrawals_invalid_error/1,
    list_withdrawals_bad_token_error/1,
    list_deposits/1,
    list_deposits_invalid_error/1,
    list_deposits_bad_token_error/1
]).

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
        {group, base}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {base, [],
            [
                list_wallets,
                list_wallets_invalid_error,
                list_wallets_bad_token_error,
                list_withdrawals,
                list_withdrawals_invalid_error,
                list_withdrawals_bad_token_error,
                list_deposits,
                list_deposits_invalid_error,
                list_deposits_bad_token_error
            ]
        }
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config) ->
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
    ], Config).

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    %% TODO remove this after cut off wapi
    ok = application:unset_env(wapi, transport),
    ok = ct_payment_system:shutdown(C).

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(Group, Config) when Group =:= base ->
    ok = ff_context:save(ff_context:create(#{
        party_client => party_client:create_client(),
        woody_context => woody_context:new(<<"init_per_group/", (atom_to_binary(Group, utf8))/binary>>)
    })),
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

-spec list_wallets(config()) ->
    _.
list_wallets(C) ->
    wapi_ct_helper:mock_services([
        {fistful_stat, fun('GetWallets', _) -> {ok, ?STAT_RESPONCE(?STAT_WALLETS)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_wallets_api:list_wallets/3,
        #{
            qs_val => #{
                <<"limit">> => <<"123">>
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec list_wallets_invalid_error(config()) ->
    _.
list_wallets_invalid_error(C) ->
    MockFunc = fun('GetWallets', _) ->
            woody_error:raise(business, ?STAT_INVALID_EXCEPTION([<<"Error 1">>, <<"Error 2">>])) end,
    SwagFunc = fun swag_client_wallet_wallets_api:list_wallets/3,
    check_invalid_error(MockFunc, SwagFunc, C).

-spec list_wallets_bad_token_error(config()) ->
    _.
list_wallets_bad_token_error(C) ->
    MockFunc = fun('GetWallets', _) ->
            woody_error:raise(business, ?STAT_BADTOKEN_EXCEPTION) end,
    SwagFunc = fun swag_client_wallet_wallets_api:list_wallets/3,
    check_bad_token_error(MockFunc, SwagFunc, C).

-spec list_withdrawals(config()) ->
    _.
list_withdrawals(C) ->
    wapi_ct_helper:mock_services([
        {fistful_stat, fun('GetWithdrawals', _) -> {ok, ?STAT_RESPONCE(?STAT_WITHDRAWALS)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:list_withdrawals/3,
        #{
            qs_val => #{
                <<"limit">> => <<"123">>
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec list_withdrawals_invalid_error(config()) ->
    _.
list_withdrawals_invalid_error(C) ->
    MockFunc = fun('GetWithdrawals', _) ->
            woody_error:raise(business, ?STAT_INVALID_EXCEPTION([<<"Error 1">>, <<"Error 2">>])) end,
    SwagFunc = fun swag_client_wallet_withdrawals_api:list_withdrawals/3,
    check_invalid_error(MockFunc, SwagFunc, C).

-spec list_withdrawals_bad_token_error(config()) ->
    _.
list_withdrawals_bad_token_error(C) ->
    MockFunc = fun('GetWithdrawals', _) ->
            woody_error:raise(business, ?STAT_BADTOKEN_EXCEPTION) end,
    SwagFunc = fun swag_client_wallet_withdrawals_api:list_withdrawals/3,
    check_bad_token_error(MockFunc, SwagFunc, C).

-spec list_deposits(config()) ->
    _.
list_deposits(C) ->
    wapi_ct_helper:mock_services([
        {fistful_stat, fun('GetDeposits', _) -> {ok, ?STAT_RESPONCE(?STAT_DEPOSITS)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_deposits_api:list_deposits/3,
        #{
            qs_val => #{
                <<"limit">> => <<"123">>
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec list_deposits_invalid_error(config()) ->
    _.
list_deposits_invalid_error(C) ->
    MockFunc = fun('GetDeposits', _) ->
            woody_error:raise(business, ?STAT_INVALID_EXCEPTION([<<"Error 1">>, <<"Error 2">>])) end,
    SwagFunc = fun swag_client_wallet_deposits_api:list_deposits/3,
    check_invalid_error(MockFunc, SwagFunc, C).

-spec list_deposits_bad_token_error(config()) ->
    _.
list_deposits_bad_token_error(C) ->
    MockFunc = fun('GetDeposits', _) ->
            woody_error:raise(business, ?STAT_BADTOKEN_EXCEPTION) end,
    SwagFunc = fun swag_client_wallet_deposits_api:list_deposits/3,
    check_bad_token_error(MockFunc, SwagFunc, C).

%%

check_invalid_error(MockFunc, SwagFunc, C) ->
    check_error(<<"NoMatch">>, MockFunc, SwagFunc, C).

check_bad_token_error(MockFunc, SwagFunc, C) ->
    check_error(<<"InvalidToken">>, MockFunc, SwagFunc, C).

check_error(Error, MockFunc, SwagFunc, C) ->
    wapi_ct_helper:mock_services([
        {fistful_stat, MockFunc}
    ], C),
    {error, {400, #{<<"errorType">> := Error}}} = call_api(
        SwagFunc,
        #{
            qs_val => #{
                <<"limit">> => <<"123">>
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec call_api(function(), map(), wapi_client_lib:context()) ->
    {ok, term()} | {error, term()}.
call_api(F, Params, Context) ->
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = F(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.
