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

-export([get_routes/1]).

%% Supervisor

-behaviour(supervisor).

-export([init/1]).
%%

-spec start() ->
    {ok, _}.

start() ->
    application:ensure_all_started(?MODULE).

%% Application

-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) ->
    ok.

stop(_State) ->
    ok.

%% Supervisor

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    % TODO
    %  - Make it palatable
    {Backends, Handlers} = lists:unzip([
        contruct_backend_childspec('ff/external_id'           , ff_external_id),
        contruct_backend_childspec('ff/sequence'              , ff_sequence),
        contruct_backend_childspec('ff/identity'              , ff_identity_machine),
        contruct_backend_childspec('ff/wallet_v2'             , ff_wallet_machine),
        contruct_backend_childspec('ff/source_v1'             , ff_instrument_machine),
        contruct_backend_childspec('ff/destination_v2'        , ff_instrument_machine),
        contruct_backend_childspec('ff/deposit_v1'            , ff_deposit_machine),
        contruct_backend_childspec('ff/withdrawal_v2'         , ff_withdrawal_machine),
        contruct_backend_childspec('ff/withdrawal/session_v2' , ff_withdrawal_session_machine)
    ]),
    ok = application:set_env(fistful, backends, maps:from_list(Backends)),

    IpEnv          = genlib_app:env(?MODULE, ip, "::0"),
    Port           = genlib_app:env(?MODULE, port, 8022),
    HealthCheck    = genlib_app:env(?MODULE, health_check, #{}),
    WoodyOptsEnv   = genlib_app:env(?MODULE, woody_opts, #{}),
    RouteOptsEnv   = genlib_app:env(?MODULE, route_opts, #{}),

    {ok, Ip}       = inet:parse_address(IpEnv),
    WoodyOpts      = maps:with([net_opts, handler_limits], WoodyOptsEnv),
    RouteOpts      = RouteOptsEnv#{event_handler => scoper_woody_event_handler},

    Routes = lists:merge(lists:map(fun get_routes/1, [
        {<<"/v1/wallet">>,      {{ff_proto_wallet_thrift, 'Management'}, {ff_wallet_handler, []}}, WoodyOpts},
        {<<"/v1/identity">>,    {{ff_proto_identity_thrift, 'Management'}, {ff_identity_handler, []}}, WoodyOpts},
        {<<"/v1/destination">>, {{ff_proto_destination_thrift, 'Management'}, {ff_destination_handler, []}}, WoodyOpts},
        {<<"/v1/withdrawal">>,  {{ff_proto_withdrawal_thrift, 'Management'}, {ff_withdrawal_handler, []}}, WoodyOpts}
    ])),

    ChildSpec = woody_server:child_spec(
        ?MODULE,
        maps:merge(
            WoodyOpts,
            #{
                ip                => Ip,
                port              => Port,
                handlers          => [],
                event_handler     => scoper_woody_event_handler,
                additional_routes =>
                    machinery_mg_backend:get_routes(Handlers, RouteOpts) ++
                    get_admin_routes() ++
                    Routes ++
                    get_eventsink_routes() ++
                    get_repair_routes(WoodyOpts) ++
                    [erl_health_handle:get_route(enable_health_logging(HealthCheck))]
            }
        )
    ),
    % TODO
    %  - Zero thoughts given while defining this strategy.
    {ok, {#{strategy => one_for_one}, [ChildSpec]}}.

-spec enable_health_logging(erl_health:check()) ->
    erl_health:check().

enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun (_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Check).

-spec get_routes({binary(), woody:th_handler(), map()}) ->
    [woody_server_thrift_http_handler:route(_)].

get_routes({Path, Handler, Opts}) ->
    Limits = genlib_map:get(handler_limits, Opts),
    woody_server_thrift_http_handler:get_routes(genlib_map:compact(#{
        handlers => [{Path, Handler}],
        event_handler => scoper_woody_event_handler,
        handler_limits => Limits
    })).

contruct_backend_childspec(NS, Handler) ->
    Be = {machinery_mg_backend, #{
        schema => machinery_mg_schema_generic,
        client => get_service_client('automaton')
    }},
    {
        {NS, Be},
        {{fistful, Handler},
            #{
                path           => ff_string:join(["/v1/stateproc/", NS]),
                backend_config => #{schema => machinery_mg_schema_generic}
            }
        }
    }.

get_service_client(ServiceID) ->
    case genlib_app:env(?MODULE, services, #{}) of
        #{ServiceID := V} ->
            ff_woody_client:new(V);
        #{} ->
            error({'woody service undefined', ServiceID})
    end.

get_admin_routes() ->
    Opts = genlib_app:env(?MODULE, admin, #{}),
    Path = maps:get(path, Opts, <<"/v1/admin">>),
    Limits = genlib_map:get(handler_limits, Opts),
    woody_server_thrift_http_handler:get_routes(genlib_map:compact(#{
        handlers => [{Path, {{ff_proto_fistful_admin_thrift, 'FistfulAdmin'}, {ff_server_admin_handler, []}}}],
        event_handler => scoper_woody_event_handler,
        handler_limits => Limits
    })).

get_eventsink_routes() ->
    Url = maps:get(eventsink, genlib_app:env(?MODULE, services, #{}),
        "http://machinegun:8022/v1/event_sink"),
    Cfg = #{
        schema => machinery_mg_schema_generic,
        client => #{
            event_handler => scoper_woody_event_handler,
            url => Url
        }
    },
    get_eventsink_route(withdrawal_session, {<<"/v1/eventsink/withdrawal/session">>,
        {
            {ff_proto_withdrawal_session_thrift, 'EventSink'},
            {ff_withdrawal_session_eventsink_publisher, Cfg}
        }}) ++
    get_eventsink_route(deposit,        {<<"/v1/eventsink/deposit">>,
        {{ff_proto_deposit_thrift,      'EventSink'}, {ff_deposit_eventsink_publisher, Cfg}}}) ++
    get_eventsink_route(source,         {<<"/v1/eventsink/source">>,
        {{ff_proto_source_thrift,       'EventSink'}, {ff_source_eventsink_publisher, Cfg}}}) ++
    get_eventsink_route(destination,    {<<"/v1/eventsink/destination">>,
        {{ff_proto_destination_thrift,  'EventSink'}, {ff_destination_eventsink_publisher, Cfg}}}) ++
    get_eventsink_route(identity,       {<<"/v1/eventsink/identity">>,
        {{ff_proto_identity_thrift,     'EventSink'}, {ff_identity_eventsink_publisher, Cfg}}}) ++
    get_eventsink_route(wallet,         {<<"/v1/eventsink/wallet">>,
        {{ff_proto_wallet_thrift,       'EventSink'}, {ff_wallet_eventsink_publisher, Cfg}}}) ++
    get_eventsink_route(withdrawal,     {<<"/v1/eventsink/withdrawal">>,
        {{ff_proto_withdrawal_thrift,   'EventSink'}, {ff_withdrawal_eventsink_publisher, Cfg}}}).

get_eventsink_route(RouteType, {DefPath, {Module, {Publisher, Cfg}}}) ->
    RouteMap = genlib_app:env(?MODULE, eventsink, #{}),
    case maps:get(RouteType, RouteMap, undefined) of
        undefined ->
            erlang:error({eventsink_undefined, RouteType});
        Opts ->
            Path = maps:get(path, Opts, DefPath),
            NS = maps:get(namespace, Opts),
            StartEvent = maps:get(start_event, Opts, 0),
            Limits = genlib_map:get(handler_limits, Opts),
            woody_server_thrift_http_handler:get_routes(genlib_map:compact(#{
                handlers => [
                    {Path, {Module, {
                        ff_eventsink_handler,
                        Cfg#{ns => NS, publisher => Publisher, start_event => StartEvent}
                }}}],
                event_handler => scoper_woody_event_handler,
                handler_limits => Limits
            }))
    end.

get_repair_routes(WoodyOpts) ->
    Limits = genlib_map:get(handler_limits, WoodyOpts),
    Handlers = [
        {
            <<"withdrawal/session">>,
            {{ff_proto_withdrawal_session_thrift, 'Repairer'}, {ff_withdrawal_session_repair, #{}}}
        }
    ],
    woody_server_thrift_http_handler:get_routes(genlib_map:compact(#{
        handlers => [{<<"/v1/repair/", N/binary>>, H} || {N, H} <- Handlers],
        event_handler => scoper_woody_event_handler,
        handler_limits => Limits
    })).
