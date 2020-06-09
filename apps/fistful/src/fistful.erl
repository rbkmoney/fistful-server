%%%
%%% Fistful
%%%

-module(fistful).

-behaviour(machinery).
-behaviour(machinery_backend).

-type namespace() :: machinery:namespace().
-type backend()   :: machinery:backend(machinery:backend(_)).

-type options() :: #{
    handler := machinery:modopts(_),
    party_client := party_client:client()
}.

-export([backend/1]).

-export([get/4]).
-export([start/4]).
-export([call/5]).
-export([repair/5]).

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
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
    machinery:get(NS, ID, Range, set_backend_context(Backend)).

-spec start(namespace(), id(), args(_), machinery:backend(_)) ->
    ok | {error, exists}.

start(NS, ID, Args, Backend) ->
    machinery:start(NS, ID, Args, set_backend_context(Backend)).

-spec call(namespace(), id(), range(), args(_), machinery:backend(_)) ->
    {ok, response(_)} | {error, notfound}.

call(NS, ID, Range, Args, Backend) ->
    machinery:call(NS, ID, Range, Args, set_backend_context(Backend)).

-spec repair(namespace(), id(), range(), args(_), machinery:backend(_)) ->
    {ok, response(_)} | {error, notfound | working | {failed, machinery:error(_)}}.

repair(NS, ID, Range, Args, Backend) ->
    machinery:repair(NS, ID, Range, Args, set_backend_context(Backend)).

%%

-type handler_opts() :: _.

-spec init(args(_), machine(E, A), options(), handler_opts()) ->
    result(E, A).

init(Args, Machine, Options, MachineryOptions) ->
    #{handler := Handler} = Options,
    ok = ff_context:save(create_context(Options, MachineryOptions)),
    try
        machinery:dispatch_signal({init, Args}, Machine, machinery_utils:get_handler(Handler), #{})
    after
        ff_context:cleanup()
    end.

-spec process_timeout(machine(E, A), options(), handler_opts()) ->
    result(E, A).

process_timeout(Machine, Options, MachineryOptions) ->
    #{handler := Handler} = Options,
    ok = ff_context:save(create_context(Options, MachineryOptions)),
    try
        machinery:dispatch_signal(timeout, Machine, machinery_utils:get_handler(Handler), #{})
    after
        ff_context:cleanup()
    end.

-spec process_call(args(_), machine(E, A), options(), handler_opts()) ->
    {response(_), result(E, A)}.

process_call(Args, Machine, Options, MachineryOptions) ->
    #{handler := Handler} = Options,
    ok = ff_context:save(create_context(Options, MachineryOptions)),
    try
        machinery:dispatch_call(Args, Machine, machinery_utils:get_handler(Handler), #{})
    after
        ff_context:cleanup()
    end.

-spec process_repair(args(_), machine(E, A), options(), handler_opts()) ->
    {ok, {response(_), result(E, A)}} | {error, machinery:error(_)}.

process_repair(Args, Machine, Options, MachineryOptions) ->
    #{handler := Handler} = Options,
    ok = ff_context:save(create_context(Options, MachineryOptions)),
    try
        machinery:dispatch_repair(Args, Machine, machinery_utils:get_handler(Handler), #{})
    after
        ff_context:cleanup()
    end.

%% Internals

-spec create_context(options(), handler_opts()) ->
    ff_context:context().

create_context(Options, MachineryOptions) ->
    #{party_client := PartyClient} = Options,
    #{woody_ctx := WoodyCtx} = MachineryOptions,
    ContextOptions = #{
        woody_context => WoodyCtx,
        party_client => PartyClient
    },
    ff_context:create(ContextOptions).

-spec set_backend_context(machinery:backend(_)) ->
    machinery:backend(_).

set_backend_context(Backend) ->
    {Mod, Opts} = machinery_utils:get_backend(Backend),
    {Mod, Opts#{
        woody_ctx => ff_context:get_woody_context(ff_context:load())
    }}.
