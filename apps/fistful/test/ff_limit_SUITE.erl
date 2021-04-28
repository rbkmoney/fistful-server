-module(ff_limit_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([get_missing_fails/1]).
-export([accounting_works/1]).
-export([spanning_works/1]).

-spec get_missing_fails(config()) -> test_return().
-spec accounting_works(config()) -> test_return().
-spec spanning_works(config()) -> test_return().

%%

-import(ct_helper, [cfg/2]).

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        get_missing_fails,
        accounting_works,
        spanning_works
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    {StartedApps, _StartupCtx} = ct_helper:start_apps([fistful]),
    SuiteSup = ct_sup:start(),
    BackendOpts = #{name => ?MODULE},
    BackendChildSpec = machinery_gensrv_backend:child_spec(ff_limit, BackendOpts),
    {ok, _} = supervisor:start_child(SuiteSup, BackendChildSpec),
    [
        {started_apps, StartedApps},
        {suite_sup, SuiteSup},
        {backend, machinery_gensrv_backend:new(BackendOpts)}
        | C
    ].

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    ok = ct_sup:stop(cfg(suite_sup, C)),
    ok = ct_helper:stop_apps(cfg(started_apps, C)),
    ok.

%%

-define(NS, ?MODULE).

get_missing_fails(C) ->
    Be = cfg(backend, C),
    Limit = {<<"hurgy-gurdy">>, {infinity, infinity}, day},
    {error, notfound} = ff_limit:get(?NS, Limit, {calendar:universal_time(), 0}, Be).

accounting_works(C) ->
    Be = cfg(backend, C),
    Range = {{inclusive, 0}, {exclusive, 42}},
    Limit = {genlib:unique(), Range, day},
    Date1 = ff_random:date(),
    Date2 = ff_random:date(),
    true = Date1 /= Date2,
    ID1 = <<"H">>,
    {ok, #{expected_max := 10}} = ff_limit:account(?NS, Limit, {ID1, Ts1 = rand_ts(Date1), 10}, Be),
    ID2 = <<"E">>,
    {ok, #{expected_max := 18}} = ff_limit:account(?NS, Limit, {ID2, Ts2 = rand_ts(Date1), 8}, Be),
    {ok, #{expected_max := 18}} = ff_limit:account(?NS, Limit, {ID1, Ts1, 10}, Be),
    {error, {conflict, {_, _, 8}}} = ff_limit:account(?NS, Limit, {ID2, Ts2, 10}, Be),
    {error, {conflict, {_, Ts2, _}}} = ff_limit:account(?NS, Limit, {ID2, rand_ts(Date1), 8}, Be),
    ID3 = <<"L">>,
    {ok, #{expected_max := 32}} = ff_limit:account(?NS, Limit, {ID3, Ts3 = rand_ts(Date1), 14}, Be),
    ID4 = <<"P">>,
    {error, {exceeded, #{expected_max := 44}}} = ff_limit:account(?NS, Limit, {ID4, Ts4 = rand_ts(Date1), 12}, Be),
    {ok, #{expected_max := 12}} = ff_limit:account(?NS, Limit, {ID4, rand_ts(Date2), 12}, Be),
    {error, {exceeded, #{expected_max := 42}}} = ff_limit:account(?NS, Limit, {ID4, Ts4, 10}, Be),
    {ok, #{expected_max := 41}} = ff_limit:account(?NS, Limit, {ID4, Ts4, 9}, Be),
    ID5 = <<"!">>,
    {error, {exceeded, #{expected_max := 50}}} = ff_limit:account(?NS, Limit, {ID5, Ts5 = rand_ts(Date1), 9}, Be),
    {ok, #{expected_max := 41, current := 10}} = ff_limit:confirm(?NS, Limit, {ID1, Ts1, 10}, Be),
    {ok, #{expected_max := 27, current := 10}} = ff_limit:reject(?NS, Limit, {ID3, Ts3, 14}, Be),
    {ok, #{expected_max := 36}} = ff_limit:account(?NS, Limit, {ID5, Ts5, 9}, Be).

spanning_works(C) ->
    Be = cfg(backend, C),
    LimID = genlib:unique(),
    Range = {{inclusive, 0}, infinity},
    Lim1 = {LimID, Range, day},
    Lim2 = {LimID, Range, week},
    Lim3 = {LimID, Range, month},
    Time = ff_random:time(),
    USec = rand_usec(),
    Trx1 = {genlib:unique(), Ts1 = {{{2018, 06, 30}, Time}, USec}, Dv1 = rand:uniform(100)},
    % same week
    Trx2 = {genlib:unique(), Ts2 = {{{2018, 07, 01}, Time}, USec}, Dv2 = rand:uniform(100)},
    % next week
    Trx3 = {genlib:unique(), Ts3 = {{{2018, 07, 02}, Time}, USec}, Dv3 = rand:uniform(100)},
    _ = [
        {ok, _} = ff_limit:account(?NS, Lim, Trx, Be)
     || Lim <- [Lim1, Lim2, Lim3],
        Trx <- [Trx1, Trx2, Trx3]
    ],
    Dv12 = Dv1 + Dv2,
    Dv23 = Dv2 + Dv3,
    {ok, #{expected_max := Dv1}} = ff_limit:get(?NS, Lim1, Ts1, Be),
    {ok, #{expected_max := Dv2}} = ff_limit:get(?NS, Lim1, Ts2, Be),
    {ok, #{expected_max := Dv3}} = ff_limit:get(?NS, Lim1, Ts3, Be),
    {ok, #{expected_max := Dv12}} = ff_limit:get(?NS, Lim2, Ts1, Be),
    {ok, #{expected_max := Dv12}} = ff_limit:get(?NS, Lim2, Ts2, Be),
    {ok, #{expected_max := Dv3}} = ff_limit:get(?NS, Lim2, Ts3, Be),
    {ok, #{expected_max := Dv1}} = ff_limit:get(?NS, Lim3, Ts1, Be),
    {ok, #{expected_max := Dv23}} = ff_limit:get(?NS, Lim3, Ts2, Be),
    {ok, #{expected_max := Dv23}} = ff_limit:get(?NS, Lim3, Ts3, Be).

rand_ts(Date) ->
    {{Date, ff_random:time()}, rand_usec()}.

rand_usec() ->
    ff_random:from_range(0, 999999).
