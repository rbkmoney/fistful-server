%%%
%%% Fistful
%%%

-module(fistful).

-behaviour(machinery).
-behaviour(machinery_backend).

-type namespace() :: machinery:namespace().
-type backend()   :: machinery:backend(machinery:backend(_)).

-export([backend/1]).

-export([get/4]).
-export([start/4]).
-export([call/5]).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%%

-spec backend(namespace()) ->
    backend().

backend(NS) ->
    {?MODULE, maps:get(NS, genlib_app:env(?MODULE, backends, #{}))}.

%%

-type id()          :: machinery:id().
-type args(T)       :: machinery:args(T).
-type range()       :: machinery:range().
-type machine(E, A) :: machinery:machine(E, A).
-type result(E, A)  :: machinery:result(E, A).
-type response(T)   :: machinery:response(T).

-spec get(namespace(), id(), range(), machinery:backend(_)) ->
    {ok, machine(_, _)} | {error, notfound}.

get(NS, ID, Range, Backend) ->
    {Mod, Opts} = machinery_utils:get_backend(Backend),
    machinery:get(NS, ID, Range, {Mod, Opts#{woody_ctx => ff_woody_ctx:get()}}).

-spec start(namespace(), id(), args(_), machinery:backend(_)) ->
    ok | {error, exists}.

start(NS, ID, Args, Backend) ->
    {Mod, Opts} = machinery_utils:get_backend(Backend),
    machinery:start(NS, ID, Args, {Mod, Opts#{woody_ctx => ff_woody_ctx:get()}}).

-spec call(namespace(), id(), range(), args(_), machinery:backend(_)) ->
    {ok, response(_)} | {error, notfound}.

call(NS, ID, Range, Args, Backend) ->
    {Mod, Opts} = machinery_utils:get_backend(Backend),
    machinery:call(NS, ID, Range, Args, {Mod, Opts#{woody_ctx => ff_woody_ctx:get()}}).

%%

-type handler_opts() :: _.

-spec init(args(_), machine(E, A), machinery:modopts(_), handler_opts()) ->
    result(E, A).

init(Args, Machine, Handler, #{woody_ctx := WoodyCtx}) ->
    ok = ff_woody_ctx:set(WoodyCtx),
    try machinery:dispatch_signal({init, Args}, Machine, machinery_utils:get_handler(Handler), #{}) after
        ff_woody_ctx:unset()
    end.

-spec process_timeout(machine(E, A), module(), handler_opts()) ->
    result(E, A).

process_timeout(Machine, Handler, #{woody_ctx := WoodyCtx}) ->
    ok = ff_woody_ctx:set(WoodyCtx),
    try machinery:dispatch_signal(timeout, Machine, machinery_utils:get_handler(Handler), #{}) after
        ff_woody_ctx:unset()
    end.

-spec process_call(args(_), machine(E, A), machinery:modopts(_), handler_opts()) ->
    {response(_), result(E, A)}.

process_call(Args, Machine, Handler, #{woody_ctx := WoodyCtx}) ->
    ok = ff_woody_ctx:set(WoodyCtx),
    try machinery:dispatch_call(Args, Machine, machinery_utils:get_handler(Handler), #{}) after
        ff_woody_ctx:unset()
    end.
