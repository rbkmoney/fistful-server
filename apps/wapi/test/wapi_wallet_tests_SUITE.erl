-module(wapi_wallet_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("jose/include/jose_jwk.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

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
    create_wallet_ok_test/1
]).

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
                create_wallet_ok_test
            ]
        }
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config) ->
    wapi_ct_helper:init_suite(?MODULE, Config).

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    _ = wapi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(base, Config) ->
    BasePermissions = [
        {[party], write},
        {[party], read}
    ],
    {ok, Token} = wapi_ct_helper:issue_token(BasePermissions, unlimited),
    [{context, wapi_ct_helper:get_context(Token)} | Config];
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) ->
    _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(_Name, C) ->
    [{test_sup, wapi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    wapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec create_wallet_ok_test(config()) ->
    _.
create_wallet_ok_test(Config) ->
    wapi_ct_helper:mock_services([{fistful_wallet, fun('Create', _) -> {ok, ?WALLET_STATE} end}], Config),
    Req = #{
        <<"identity">> => ?STRING,
        <<"currency">> => ?RUB,
        <<"name">> => ?STRING
    },
    {ok, _} = wapi_client_wallet_wallets_api:create_wallet(?config(context, Config), Req).

