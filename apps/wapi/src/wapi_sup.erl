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
    {LogicHandlers, LogicHandlerSpecs} = get_logic_handler_info(),
    HealthCheck = enable_health_logging(genlib_app:env(wapi, health_check, #{})),
    HealthRoutes = [{'_', [erl_health_handle:get_route(HealthCheck)]}],
    SwaggerHandlerOpts = genlib_app:env(wapi, swagger_handler_opts, #{}),
    SwaggerSpec = wapi_swagger_server:child_spec(HealthRoutes, LogicHandlers, SwaggerHandlerOpts),
    UacConf = get_uac_config(),
    ok = uac:configure(UacConf),
    {ok, {
        {one_for_all, 0, 1},
            [LechiffreSpec] ++
            LogicHandlerSpecs ++ [SwaggerSpec]
    }}.

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

get_uac_config() ->
    maps:merge(
        genlib_app:env(wapi, access_conf),
        #{access => wapi_auth:get_access_config()}
    ).
