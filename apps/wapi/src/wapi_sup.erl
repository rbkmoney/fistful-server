%% @doc Top level supervisor.
%% @end

-module(wapi_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    LechiffreOpts = genlib_app:env(wapi, lechiffre_opts),
    LechiffreSpec = lechiffre:child_spec(lechiffre, LechiffreOpts),
    AuthorizerSpecs = get_authorizer_child_specs(),
    {LogicHandlers, LogicHandlerSpecs} = get_logic_handler_info(),
    HealthCheck = enable_health_logging(genlib_app:env(wapi, health_check, #{})),
    HealthRoutes = [{'_', [erl_health_handle:get_route(HealthCheck)]}],
    SwaggerSpec = wapi_swagger_server:child_spec(HealthRoutes, LogicHandlers),
    {ok, {
        {one_for_all, 0, 1},
            [LechiffreSpec] ++
            AuthorizerSpecs ++ LogicHandlerSpecs ++ [SwaggerSpec]
    }}.

-spec get_authorizer_child_specs() -> [supervisor:child_spec()].

get_authorizer_child_specs() ->
    Authorizers = genlib_app:env(wapi, authorizers, #{}),
    [
        get_authorizer_child_spec(jwt, maps:get(jwt, Authorizers))
    ].

-spec get_authorizer_child_spec(Name :: atom(), Options :: #{}) -> supervisor:child_spec().

get_authorizer_child_spec(jwt, Options) ->
    wapi_authorizer_jwt:get_child_spec(Options).

-spec get_logic_handler_info() ->
    {wapi_swagger_server:logic_handlers(), [supervisor:child_spec()]}.

get_logic_handler_info() ->
    HandlerOptions = #{
        %% TODO: Remove after fistful and wapi split
        party_client => party_client:create_client()
    },
    {#{
        wallet  => {wapi_wallet_handler, HandlerOptions}
    }, []}.

-spec enable_health_logging(erl_health:check()) ->
    erl_health:check().

enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun (_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Check).
