%%%
%%% Server startup
%%%
%%% TODOs
%%%
%%%  - We should probably most of what is hardcoded here to the application
%%%    environment.
%%%  - Provide healthcheck.
%%%

-module(ff_server).

-export([start/0]).

%% Application

-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% Supervisor

-behaviour(supervisor).

-export([init/1]).

% 30 seconds
-define(DEFAULT_HANDLING_TIMEOUT, 30000).

%%

-spec start() -> {ok, _}.
start() ->
    application:ensure_all_started(?MODULE).

%% Application

-spec start(normal, any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%% Supervisor

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    IpEnv = genlib_app:env(?MODULE, ip, "::1"),
    Port = genlib_app:env(?MODULE, port, 8022),
    HealthCheck = genlib_app:env(?MODULE, health_check, #{}),
    WoodyOptsEnv = genlib_app:env(?MODULE, woody_opts, #{}),
    RouteOptsEnv = genlib_app:env(?MODULE, route_opts, #{}),

    PartyClient = party_client:create_client(),
    DefaultTimeout = genlib_app:env(?MODULE, default_woody_handling_timeout, ?DEFAULT_HANDLING_TIMEOUT),
    WrapperOpts = #{
        party_client => PartyClient,
        default_handling_timeout => DefaultTimeout
    },

    {ok, Ip} = inet:parse_address(IpEnv),
    WoodyOpts = maps:with([net_opts, handler_limits], WoodyOptsEnv),
    EventHandlerOpts = genlib_app:env(?MODULE, scoper_event_handler_options, #{}),
    RouteOpts = RouteOptsEnv#{event_handler => {scoper_woody_event_handler, EventHandlerOpts}},

    % TODO
    %  - Make it palatable
    {Backends, MachineHandlers, ModernizerHandlers} = lists:unzip3([
        contruct_backend_childspec('ff/identity', ff_identity_machine, PartyClient),
        contruct_backend_childspec('ff/wallet_v2', ff_wallet_machine, PartyClient),
        contruct_backend_childspec('ff/source_v1', ff_source_machine, PartyClient),
        contruct_backend_childspec('ff/destination_v2', ff_destination_machine, PartyClient),
        contruct_backend_childspec('ff/deposit_v1', ff_deposit_machine, PartyClient),
        contruct_backend_childspec('ff/withdrawal_v2', ff_withdrawal_machine, PartyClient),
        contruct_backend_childspec('ff/withdrawal/session_v2', ff_withdrawal_session_machine, PartyClient),
        contruct_backend_childspec('ff/w2w_transfer_v1', w2w_transfer_machine, PartyClient)
    ]),
    ok = application:set_env(fistful, backends, maps:from_list(Backends)),

    Services =
        [
            {fistful_admin, ff_server_admin_handler},
            {fistful_provider, ff_provider_handler},
            {ff_withdrawal_adapter_host, ff_withdrawal_adapter_host},
            {wallet_management, ff_wallet_handler},
            {identity_management, ff_identity_handler},
            {destination_management, ff_destination_handler},
            {source_management, ff_source_handler},
            {withdrawal_management, ff_withdrawal_handler},
            {withdrawal_session_management, ff_withdrawal_session_handler},
            {deposit_management, ff_deposit_handler},
            {withdrawal_session_repairer, ff_withdrawal_session_repair},
            {withdrawal_repairer, ff_withdrawal_repair},
            {deposit_repairer, ff_deposit_repair},
            {w2w_transfer_management, ff_w2w_transfer_handler},
            {w2w_transfer_repairer, ff_w2w_transfer_repair}
        ] ++ get_eventsink_handlers(),
    WoodyHandlers = [get_handler(Service, Handler, WrapperOpts) || {Service, Handler} <- Services],

    ServicesChildSpec = woody_server:child_spec(
        ?MODULE,
        maps:merge(
            WoodyOpts,
            #{
                ip => Ip,
                port => Port,
                handlers => WoodyHandlers,
                event_handler => scoper_woody_event_handler,
                additional_routes =>
                    get_prometheus_routes() ++
                    machinery_mg_backend:get_routes(MachineHandlers, RouteOpts) ++
                    machinery_modernizer_mg_backend:get_routes(ModernizerHandlers, RouteOpts) ++
                    [erl_health_handle:get_route(enable_health_logging(HealthCheck))]
            }
        )
    ),
    PartyClientSpec = party_client:child_spec(party_client, PartyClient),
    % TODO
    %  - Zero thoughts given while defining this strategy.
    {ok, {#{strategy => one_for_one}, [PartyClientSpec, ServicesChildSpec]}}.

-spec enable_health_logging(erl_health:check()) -> erl_health:check().
enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun(_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Check).

-spec get_prometheus_routes() -> [{iodata(), module(), _Opts :: any()}].
get_prometheus_routes() ->
    [{"/metrics/[:registry]", prometheus_cowboy2_handler, []}].

-spec get_handler(ff_services:service_name(), woody:handler(_), map()) -> woody:http_handler(woody:th_handler()).
get_handler(Service, Handler, WrapperOpts) ->
    {Path, ServiceSpec} = ff_services:get_service_spec(Service),
    {Path, {ServiceSpec, wrap_handler(Handler, WrapperOpts)}}.

contruct_backend_childspec(NS, Handler, PartyClient) ->
    Schema = get_namespace_schema(NS),
    {
        construct_machinery_backend_spec(NS, Schema),
        construct_machinery_handler_spec(NS, Handler, Schema, PartyClient),
        construct_machinery_modernizer_spec(NS, Schema)
    }.

construct_machinery_backend_spec(NS, Schema) ->
    {NS,
        {machinery_mg_backend, #{
            schema => Schema,
            client => get_service_client(automaton)
        }}}.

construct_machinery_handler_spec(NS, Handler, Schema, PartyClient) ->
    {{fistful, #{handler => Handler, party_client => PartyClient}}, #{
        path => ff_string:join(["/v1/stateproc/", NS]),
        backend_config => #{schema => Schema}
    }}.

construct_machinery_modernizer_spec(NS, Schema) ->
    #{
        path => ff_string:join(["/v1/modernizer/", NS]),
        backend_config => #{schema => Schema}
    }.

get_service_client(ServiceID) ->
    case genlib_app:env(fistful, services, #{}) of
        #{ServiceID := V} ->
            ff_woody_client:new(V);
        #{} ->
            error({unknown_service, ServiceID})
    end.

get_eventsink_handlers() ->
    Client = get_service_client(eventsink),
    Cfg = #{
        client => Client
    },
    Publishers = [
        {deposit, deposit_event_sink, ff_deposit_eventsink_publisher},
        {source, source_event_sink, ff_source_eventsink_publisher},
        {destination, destination_event_sink, ff_destination_eventsink_publisher},
        {identity, identity_event_sink, ff_identity_eventsink_publisher},
        {wallet, wallet_event_sink, ff_wallet_eventsink_publisher},
        {withdrawal, withdrawal_event_sink, ff_withdrawal_eventsink_publisher},
        {withdrawal_session, withdrawal_session_event_sink, ff_withdrawal_session_eventsink_publisher},
        {w2w_transfer, w2w_transfer_event_sink, ff_w2w_transfer_eventsink_publisher}
    ],
    [get_eventsink_handler(Name, Service, Publisher, Cfg) || {Name, Service, Publisher} <- Publishers].

get_eventsink_handler(Name, Service, Publisher, Config) ->
    Sinks = genlib_app:env(?MODULE, eventsink, #{}),
    case maps:find(Name, Sinks) of
        {ok, Opts} ->
            NS = maps:get(namespace, Opts),
            StartEvent = maps:get(start_event, Opts, 0),
            FullConfig = Config#{
                ns => erlang:atom_to_binary(NS, utf8),
                publisher => Publisher,
                start_event => StartEvent,
                schema => get_namespace_schema(NS)
            },
            {Service, {ff_eventsink_handler, FullConfig}};
        error ->
            erlang:error({unknown_eventsink, Name, Sinks})
    end.

get_namespace_schema('ff/identity') ->
    ff_identity_machinery_schema;
get_namespace_schema('ff/wallet_v2') ->
    ff_wallet_machinery_schema;
get_namespace_schema('ff/source_v1') ->
    ff_source_machinery_schema;
get_namespace_schema('ff/destination_v2') ->
    ff_destination_machinery_schema;
get_namespace_schema('ff/deposit_v1') ->
    ff_deposit_machinery_schema;
get_namespace_schema('ff/withdrawal_v2') ->
    ff_withdrawal_machinery_schema;
get_namespace_schema('ff/withdrawal/session_v2') ->
    ff_withdrawal_session_machinery_schema;
get_namespace_schema('ff/w2w_transfer_v1') ->
    ff_w2w_transfer_machinery_schema.

wrap_handler(Handler, WrapperOpts) ->
    FullOpts = maps:merge(#{handler => Handler}, WrapperOpts),
    {ff_woody_wrapper, FullOpts}.
