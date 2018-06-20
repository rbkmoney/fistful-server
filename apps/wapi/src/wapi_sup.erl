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
    AuthorizerSpecs = get_authorizer_child_specs(),
    {LogicHandlers, LogicHandlerSpecs} = get_logic_handler_info(),
    HealthRoutes = [{'_', [erl_health_handle:get_route(genlib_app:env(wapi, health_checkers, []))]}],
    SwaggerSpec = wapi_swagger_server:child_spec({HealthRoutes, LogicHandlers}),
    {ok, {
        {one_for_all, 0, 1},
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

-spec get_logic_handler_info() -> {Handlers :: #{atom() => module()}, [Spec :: supervisor:child_spec()] | []} .

get_logic_handler_info() ->
    {#{
        wallet  => wapi_wallet_handler,
        payres  => wapi_payres_handler,
        privdoc => wapi_privdoc_handler
    }, []}.
