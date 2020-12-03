-module(ff_woody_wrapper).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-export_type([options/0]).
-export_type([handler/0]).
-export_type([client_opts/0]).

-type handler() :: module() | {module(), handler_options()}.
-type handler_options() :: any().
-type options() :: #{
    handler := handler(),
    party_client := party_client:client(),
    default_handling_timeout => timeout()
}.

-type client_opts() :: #{
    url := woody:url(),
    transport_opts => [{_, _}]
}.

% 30 seconds
-define(DEFAULT_HANDLING_TIMEOUT, 30000).

%% Callbacks

-callback handle_function(woody:func(), woody:args(), handler_options()) -> {ok, woody:result()} | no_return().

%% API

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), options()) -> {ok, woody:result()} | no_return().
handle_function(Func, Args, WoodyContext0, #{handler := Handler} = Opts) ->
    WoodyContext = ensure_woody_deadline_set(WoodyContext0, Opts),
    {HandlerMod, HandlerOptions} = get_handler_opts(Handler),
    ok = ff_context:save(create_context(WoodyContext, Opts)),
    try
        HandlerMod:handle_function(
            Func,
            Args,
            HandlerOptions
        )
    after
        ff_context:cleanup()
    end.

%% Internal functions

create_context(WoodyContext, Opts) ->
    ContextOptions = #{
        woody_context => WoodyContext,
        party_client => maps:get(party_client, Opts)
    },
    ff_context:create(ContextOptions).

-spec ensure_woody_deadline_set(woody_context:ctx(), options()) -> woody_context:ctx().
ensure_woody_deadline_set(WoodyContext, Opts) ->
    case woody_context:get_deadline(WoodyContext) of
        undefined ->
            DefaultTimeout = maps:get(default_handling_timeout, Opts, ?DEFAULT_HANDLING_TIMEOUT),
            Deadline = woody_deadline:from_timeout(DefaultTimeout),
            woody_context:set_deadline(Deadline, WoodyContext);
        _Other ->
            WoodyContext
    end.

-spec get_handler_opts(handler()) -> {module(), handler_options()}.
get_handler_opts(Handler) when is_atom(Handler) ->
    {Handler, undefined};
get_handler_opts({Handler, Options}) when is_atom(Handler) ->
    {Handler, Options}.
