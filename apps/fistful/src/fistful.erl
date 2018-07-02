%%%
%%% Fistful
%%%

-module(fistful).

-behaviour(machinery).
-behaviour(machinery_backend).

-type namespace() :: machinery:namespace().
-type backend()   :: machinery:backend(_).

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
    maps:get(NS, genlib_app:env(?MODULE, backends, #{})).

%%

-type id()          :: machinery:id().
-type args(T)       :: machinery:args(T).
-type range()       :: machinery:range().
-type machine(E, A) :: machinery:machine(E, A).
-type result(E, A)  :: machinery:result(E, A).
-type response(T)   :: machinery:response(T).

-spec get(namespace(), id(), range(), backend()) ->
    {ok, machine(_, _)} | {error, notfound}.

get(NS, ID, Range, Backend) ->
    machinery:get(NS, ID, Range, Backend).

-spec start(namespace(), id(), args(_), backend()) ->
    ok | {error, exists}.

start(NS, ID, Args, Backend) ->
    WoodyCtx = woody_context:new_child(ff_woody_ctx:get()),
    machinery:start(NS, ID, {Args, WoodyCtx}, Backend).

-spec call(namespace(), id(), range(), args(_), backend()) ->
    {ok, response(_)} | {error, notfound}.

call(NS, ID, Range, Args, Backend) ->
    WoodyCtx = woody_context:new_child(ff_woody_ctx:get()),
    machinery:call(NS, ID, Range, {Args, WoodyCtx}, Backend).

%%

-type wctx()         :: woody_context:ctx().
-type handler_opts() :: _.

-spec init({args(_), wctx()}, machine(E, A), machinery:modopts(_), handler_opts()) ->
    result(E, A).

init({Args, WoodyCtx}, Machine, Handler, Opts) ->
    ok = ff_woody_ctx:set(WoodyCtx),
    {Module, HandlerArgs} = machinery_utils:get_handler(Handler),
    try Module:init(Args, Machine, HandlerArgs, Opts) after
        ff_woody_ctx:unset()
    end.

-spec process_timeout(machine(E, A), module(), handler_opts()) ->
    result(E, A).

process_timeout(Machine, Handler, Opts) ->
    ok = ff_woody_ctx:set(woody_context:new()),
    {Module, HandlerArgs} = machinery_utils:get_handler(Handler),
    try Module:process_timeout(Machine, HandlerArgs, Opts) after
        ff_woody_ctx:unset()
    end.

-spec process_call({args(_), wctx()}, machine(E, A), machinery:modopts(_), handler_opts()) ->
    {response(_), result(E, A)}.

process_call({Args, WoodyCtx}, Machine, Handler, Opts) ->
    ok = ff_woody_ctx:set(WoodyCtx),
    {Module, HandlerArgs} = machinery_utils:get_handler(Handler),
    try Module:process_call(Args, Machine, HandlerArgs, Opts) after
        ff_woody_ctx:unset()
    end.
