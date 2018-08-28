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
        contruct_backend_childspec('ff/sequence'      , 'ff/sequence'   , ff_sequence),
        contruct_backend_childspec('ff/identity'      , 'ff/identity'   , ff_identity_machine),
        contruct_backend_childspec('ff/wallet_v2'     , 'ff/wallet'     , ff_wallet_machine),
        contruct_backend_childspec('ff/destination_v2', 'ff/destination', ff_destination_machine),
        contruct_backend_childspec('ff/withdrawal_v2' , 'ff/withdrawal' , ff_withdrawal_machine),
        contruct_backend_childspec('ff/withdrawal/session_v2' , 'ff/withdrawal/session', ff_withdrawal_session_machine)
    ]),
    ok = application:set_env(fistful, backends, maps:from_list(Backends)),
    {ok, IP} = inet:parse_address(genlib_app:env(?MODULE, ip, "::0")),
    HealthCheckers = genlib_app:env(?MODULE, health_checkers, []),
    {ok, {
        % TODO
        %  - Zero thoughts given while defining this strategy.
        #{strategy => one_for_one},
        [
            woody_server:child_spec(
                ?MODULE,
                maps:merge(
                    maps:with([net_opts, handler_limits], genlib_app:env(?MODULE, woody_opts, #{})),
                    #{
                        ip                => IP,
                        port              => genlib_app:env(?MODULE, port, 8022),
                        handlers          => [],
                        event_handler     => scoper_woody_event_handler,
                        additional_routes => machinery_mg_backend:get_routes(
                            Handlers,
                            maps:merge(
                                genlib_app:env(?MODULE, route_opts, #{}),
                                #{
                                    event_handler => scoper_woody_event_handler
                                }
                            )
                        ) ++ [erl_health_handle:get_route(HealthCheckers)]
                    }
                )
            )
        ]
    }}.

contruct_backend_childspec(NS, UrlPart, Handler) ->
    Be = {machinery_mg_backend, #{
        schema => machinery_mg_schema_generic,
        client => get_service_client('automaton')
    }},
    {
        {NS, Be},
        {{fistful, Handler},
            #{
                path           => ff_string:join(["/v1/stateproc/", UrlPart]),
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
