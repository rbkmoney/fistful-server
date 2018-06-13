-module(ct_helper).

-export([cfg/2]).

-export([start_apps/1]).
-export([start_app/1]).
-export([stop_apps/1]).
-export([stop_app/1]).

-export([makeup_cfg/2]).

-export([woody_ctx/0]).
-export([get_woody_ctx/1]).

-export([test_case_name/1]).
-export([get_test_case_name/1]).

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
        _        -> error({undefined, Key, Config})
    end.

%%

-type app_name()    :: atom().
-type app_env()     :: [{atom(), term()}].
-type startup_ctx() :: #{atom() => _}.

-spec start_apps([app_name()]) -> {[Started :: app_name()], startup_ctx()}.

start_apps(AppNames) ->
    lists:foldl(
        fun (AppName, {SAcc, CtxAcc}) ->
            {Started, Ctx} = start_app(AppName),
            {SAcc ++ Started, maps:merge(CtxAcc, Ctx)}
        end,
        {[], #{}},
        AppNames
    ).

-spec start_app(app_name()) -> {[Started :: app_name()], startup_ctx()}.

start_app(lager = AppName) ->
    {start_app_with(AppName, [
        {async_threshold, 1},
        {async_threshold_window, 0},
        {error_logger_hwm, 600},
        {suppress_application_start_stop, true},
        {suppress_supervisor_start_stop, true},
        {handlers, [
            {lager_common_test_backend, debug}
        ]}
    ]), #{}};

start_app(scoper = AppName) ->
    {start_app_with(AppName, [
        {storage, scoper_storage_lager}
    ]), #{}};

start_app(woody = AppName) ->
    {start_app_with(AppName, [
        {acceptors_pool_size, 4}
    ]), #{}};

start_app(AppName) ->
    {start_app_with(AppName, []), #{}}.

-spec start_app_with(app_name(), app_env()) -> {[app_name()], #{atom() => _}}.

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

%%

-type config_mut_fun() :: fun((config()) -> config()).

-spec makeup_cfg([config_mut_fun()], config()) -> config().

makeup_cfg(CMFs, C0) ->
    lists:foldl(fun (CMF, C) -> CMF(C) end, C0, CMFs).

-spec woody_ctx() -> config_mut_fun().

woody_ctx() ->
    fun (C) -> [{'$woody_ctx', construct_woody_ctx(C)} | C] end.

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
    fun (C) -> [{'$test_case_name', TestCaseName} | C] end.

-spec get_test_case_name(config()) -> test_case_name().

get_test_case_name(C) ->
    cfg('$test_case_name', C).
