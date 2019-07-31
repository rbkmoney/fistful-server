-module(ff_sequence_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([get_next_success/1]).
-export([consistency_holds/1]).

-spec get_next_success(config()) -> test_return().
-spec consistency_holds(config()) -> test_return().

%%

-import(ct_helper, [cfg/2]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        get_next_success,
        consistency_holds
    ].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    {StartedApps, _StartupCtx} = ct_helper:start_apps([fistful]),
    SuiteSup         = ct_sup:start(),
    BackendOpts      = #{name => ?MODULE},
    BackendChildSpec = machinery_gensrv_backend:child_spec(ff_sequence, BackendOpts),
    {ok, _}          = supervisor:start_child(SuiteSup, BackendChildSpec),
    [
        {started_apps , StartedApps},
        {suite_sup    , SuiteSup},
        {backend      , machinery_gensrv_backend:new(BackendOpts)}
    | C].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = ct_sup:stop(cfg(suite_sup, C)),
    ok = ct_helper:stop_apps(cfg(started_apps, C)),
    ok.

%%

-define(NS, ?MODULE).

get_next_success(C) ->
    Be = cfg(backend, C),
    ID = <<"hoola-boola">>,
    0 = ff_sequence:get(?NS, ID, Be),
    1 = ff_sequence:next(?NS, ID, Be),
    1 = ff_sequence:get(?NS, ID, Be),
    2 = ff_sequence:next(?NS, ID, Be).

consistency_holds(C) ->
    Be = cfg(backend, C),
    ID = genlib:unique(),
    Trials = lists:seq(1, 100),
    Results = genlib_pmap:map(
        fun (_) -> ff_sequence:next(?NS, ID, Be) end,
        Trials
    ),
    Trials = lists:sort(Results).
