-module(wapi_drainer).
-behaviour(gen_server).

-type options() :: #{
    shutdown  := timeout(),
    ranch_ref := ranch:ref()
}.

-export([child_spec/1]).
-export([start_link/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([terminate/2]).

%% API

-spec child_spec(options()) ->
    supervisor:child_spec().

child_spec(Opts) ->
    RanchRef = maps:get(ranch_ref, Opts),
    Shutdown = get_shutdown_param(Opts),
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, [RanchRef]},
        shutdown => Shutdown
    }.

-spec start_link(ranch:ref()) ->
    genlib_gen:start_ret().

start_link(RanchRef) ->
    gen_server:start_link(?MODULE, RanchRef, []).

%% gen_server callbacks

-spec init(ranch:ref()) ->
    {ok, ranch:ref()}.

init(RanchRef) ->
    process_flag(trap_exit, true),
    {ok, RanchRef}.

-spec handle_call(_, _, ranch:ref()) ->
    {noreply, ranch:ref()}.

handle_call(_Call, _From, St) ->
    {noreply, St}.

-spec handle_cast(_, ranch:ref()) ->
    {noreply, ranch:ref()}.

handle_cast(_Call, St) ->
    {noreply, St}.

-spec terminate(_, ranch:ref()) ->
    ok.

terminate(shutdown, Ref) ->
    ok = ranch:suspend_listener(Ref),
    ok = ranch:wait_for_connections(Ref, '==', 0);
terminate(_, _) ->
    ok.

%% Internal function

get_shutdown_param(#{shutdown := 0}) ->
    brutal_kill;
get_shutdown_param(#{shutdown := Timeout}) ->
    Timeout.
