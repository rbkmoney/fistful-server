-module(ct_helper).

-export([cfg/2]).

-export([start_apps/1]).
-export([start_app/1]).
-export([stop_apps/1]).
-export([stop_app/1]).

-export([makeup_cfg/2]).

-export([set_context/1]).
-export([unset_context/0]).

-export([woody_ctx/0]).
-export([get_woody_ctx/1]).

-export([test_case_name/1]).
-export([get_test_case_name/1]).

-export([await/2]).
-export([await/3]).

-type test_case_name() :: atom().
-type group_name() :: atom().
-type config() :: [{atom(), term()}].

-export_type([test_case_name/0]).
-export_type([group_name/0]).
-export_type([config/0]).

%%

-spec cfg(atom(), config()) -> term().

cfg(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        {Key, V} -> V;
        _        -> error({'ct config entry missing', Key})
    end.

-spec cfg(atom(), _, config()) -> config().

cfg(Key, Value, Config) ->
    lists:keystore(Key, 1, Config, {Key, Value}).

%%

-type app_name()     :: atom().
-type app_env()      :: [{atom(), term()}].
-type app_with_env() :: {app_name(), app_env()}.
-type startup_ctx()  :: #{atom() => _}.

-spec start_apps([app_name() | app_with_env()]) -> {[Started :: app_name()], startup_ctx()}.

start_apps(AppNames) ->
    lists:foldl(
        fun (AppName, {SAcc, CtxAcc}) ->
            {Started, Ctx} = start_app(AppName),
            {SAcc ++ Started, maps:merge(CtxAcc, Ctx)}
        end,
        {[], #{}},
        AppNames
    ).

-spec start_app(app_name() | app_with_env()) -> {[Started :: app_name()], startup_ctx()}.

start_app(scoper = AppName) ->
    {start_app_with(AppName, [
        {storage, scoper_storage_logger}
    ]), #{}};

start_app(woody = AppName) ->
    {start_app_with(AppName, [
        {acceptors_pool_size, 4}
    ]), #{}};

start_app(dmt_client = AppName) ->
    {start_app_with(AppName, [
        {cache_update_interval, 500},
        {max_cache_size, #{
            elements => 1
        }},
        {service_urls, #{
            'Repository'       => <<"http://dominant:8022/v1/domain/repository">>,
            'RepositoryClient' => <<"http://dominant:8022/v1/domain/repository_client">>
        }}
    ]), #{}};

start_app(wapi = AppName) ->
    {start_app_with(AppName, [
        {ip, "::"},
        {port, 8080},
        {realm, <<"external">>},
        {public_endpoint, <<"localhost:8080">>},
        {access_conf, #{
            jwt => #{
                keyset => #{
                    wapi     => {pem_file, "/opt/wapi/config/private.pem"}
                }
            }
        }}
    ]), #{}};

start_app(wapi_woody_client = AppName) ->
    {start_app_with(AppName, [
        {service_urls, #{
            cds_storage         => "http://cds:8022/v1/storage",
            identdoc_storage    => "http://cds:8022/v1/identity_document_storage",
            fistful_stat        => "http://fistful-magista:8022/stat"
        }},
        {service_retries, #{
            fistful_stat    => #{
                'GetWallets'   => {linear, 3, 1000},
                '_'            => finish
            }
        }},
        {api_deadlines, #{
            fistful_stat => 5000
        }}
    ]), #{}};

start_app(ff_server = AppName) ->
    {start_app_with(AppName, [
        {ip, "::"},
        {port, 8022},
        {admin, #{
            path => <<"/v1/admin">>
        }},
        {eventsink, #{
            identity => #{
                namespace => <<"ff/identity">>
            },
            wallet => #{
                namespace => <<"ff/wallet_v2">>
            },
            withdrawal => #{
                namespace => <<"ff/withdrawal_v2">>
            },
            deposit => #{
                namespace => <<"ff/deposit_v1">>
            },
            destination => #{
                namespace => <<"ff/destination_v2">>
            },
            source => #{
                namespace => <<"ff/source_v1">>
            },
            withdrawal_session => #{
                namespace => <<"ff/withdrawal/session_v2">>
            }
        }}
    ]), #{}};

start_app(bender_client = AppName) ->
    {start_app_with(AppName, [
        {service_url, <<"http://bender:8022/v1/bender">>},
        {deadline, 60000}
    ]), #{}};

start_app({AppName, AppEnv}) ->
    {start_app_with(AppName, AppEnv), #{}};

start_app(AppName) ->
    {start_app_with(AppName, []), #{}}.

-spec start_app_with(app_name(), app_env()) -> [app_name()].

start_app_with(AppName, Env) ->
    _ = application:load(AppName),
    _ = set_app_env(AppName, Env),
    case application:ensure_all_started(AppName) of
        {ok, Apps} ->
            Apps;
        {error, Reason} ->
            exit({start_app_failed, AppName, Reason})
    end.

set_app_env(AppName, Env) ->
    lists:foreach(
        fun ({K, V}) ->
            ok = application:set_env(AppName, K, V)
        end,
        Env
    ).

-spec stop_apps([app_name()]) -> ok.

stop_apps(AppNames) ->
    lists:foreach(fun stop_app/1, lists:reverse(AppNames)).

-spec stop_app(app_name()) -> ok.

stop_app(AppName) ->
    case application:stop(AppName) of
        ok ->
            case application:unload(AppName) of
                ok ->
                    ok;
                {error, Reason} ->
                    exit({unload_app_failed, AppName, Reason})
            end;
        {error, Reason} ->
            exit({unload_app_failed, AppName, Reason})
    end.

-spec set_context(config()) -> ok.

set_context(C) ->
    ok = ff_context:save(ff_context:create(#{
        party_client => party_client:create_client(),
        woody_context => cfg('$woody_ctx', C)
    })).

-spec unset_context() -> ok.

unset_context() ->
    ok = ff_context:cleanup().

%%

-type config_mut_fun() :: fun((config()) -> config()).

-spec makeup_cfg([config_mut_fun()], config()) -> config().

makeup_cfg(CMFs, C0) ->
    lists:foldl(fun (CMF, C) -> CMF(C) end, C0, CMFs).

-spec woody_ctx() -> config_mut_fun().

woody_ctx() ->
    fun (C) -> cfg('$woody_ctx', construct_woody_ctx(C), C) end.

construct_woody_ctx(C) ->
    woody_context:new(construct_rpc_id(get_test_case_name(C))).

construct_rpc_id(TestCaseName) ->
    woody_context:new_rpc_id(
        <<"undefined">>,
        list_to_binary(lists:sublist(atom_to_list(TestCaseName), 32)),
        woody_context:new_req_id()
    ).

-spec get_woody_ctx(config()) -> woody_context:ctx().

get_woody_ctx(C) ->
    cfg('$woody_ctx', C).

%%

-spec test_case_name(test_case_name()) -> config_mut_fun().

test_case_name(TestCaseName) ->
    fun (C) -> cfg('$test_case_name', TestCaseName, C) end.

-spec get_test_case_name(config()) -> test_case_name().

get_test_case_name(C) ->
    cfg('$test_case_name', C).

%%

-spec await(Expect, fun(() -> Expect | _)) ->
    Expect.

await(Expect, Compute) ->
    await(Expect, Compute, genlib_retry:linear(3, 1000)).

-spec await(Expect, fun(() -> Expect | _), genlib_retry:strategy()) ->
    Expect.

await(Expect, Compute, Retry0) ->
    case Compute() of
        Expect ->
            Expect;
        NotYet ->
            case genlib_retry:next_step(Retry0) of
                {wait, To, Retry1} ->
                    ok = timer:sleep(To),
                    await(Expect, Compute, Retry1);
                finish ->
                    error({'await failed', NotYet})
            end
    end.
