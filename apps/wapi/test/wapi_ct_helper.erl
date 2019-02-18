-module(wapi_ct_helper).

-include_lib("common_test/include/ct.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").
-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").

-export([init_suite/2]).
-export([start_app/1]).
-export([start_app/2]).
-export([start_wapi/1]).
-export([issue_token/2]).
-export([issue_token/3]).
-export([get_context/1]).
-export([get_keysource/2]).
-export([start_mocked_service_sup/1]).
-export([stop_mocked_service_sup/1]).
-export([mock_services/2]).
-export([mock_services_/2]).
-export([get_lifetime/0]).

-define(WAPI_IP                     , "::").
-define(WAPI_PORT                   , 8080).
-define(WAPI_HOST_NAME              , "localhost").
-define(WAPI_URL                    , ?WAPI_HOST_NAME ++ ":" ++ integer_to_list(?WAPI_PORT)).

%%
-type config()          :: [{atom(), any()}].
-type app_name() :: atom().

-spec init_suite(module(), config()) ->
    config().
init_suite(Module, Config) ->
    SupPid = start_mocked_service_sup(Module),
    Apps1 =
        start_app(lager) ++
        start_app(scoper) ++
        start_app(woody),
    ServiceURLs = mock_services_([
        {
            'Repository',
            {dmsl_domain_config_thrift, 'Repository'},
            fun('Checkout', _) -> {ok, ?SNAPSHOT} end
        }
    ], SupPid),
    Apps2 =
        start_app(dmt_client, [{max_cache_size, #{}}, {service_urls, ServiceURLs}, {cache_update_interval, 50000}]) ++
        start_wapi(Config),
    [{apps, lists:reverse(Apps2 ++ Apps1)}, {suite_test_sup, SupPid} | Config].

-spec start_app(app_name()) ->
    [app_name()].

start_app(lager = AppName) ->
    start_app(AppName, [
        {async_threshold, 1},
        {async_threshold_window, 0},
        {error_logger_hwm, 600},
        {suppress_application_start_stop, false},
        {suppress_supervisor_start_stop, false},
        {handlers, [
            {lager_common_test_backend, [debug, {lager_logstash_formatter, []}]}
        ]}
    ]);

start_app(scoper = AppName) ->
    start_app(AppName, [
        {storage, scoper_storage_lager}
    ]);

start_app(woody = AppName) ->
    start_app(AppName, [
        {acceptors_pool_size, 4}
    ]);

start_app(AppName) ->
    genlib_app:start_application(AppName).

-spec start_app(app_name(), list()) ->
    [app_name()].

start_app(AppName, Env) ->
    genlib_app:start_application_with(AppName, Env).

-spec start_wapi(config()) ->
    [app_name()].
start_wapi(Config) ->
    start_app(wapi, [
        {ip, ?WAPI_IP},
        {port, ?WAPI_PORT},
        {realm, <<"external">>},
        {public_endpoint, <<"localhost:8080">>},
        {authorizers, #{
            jwt => #{
                signee => wapi,
                keyset => #{
                    wapi => {pem_file, get_keysource("keys/local/private.pem", Config)}
                }
            }
        }},
        {service_retries, #{
            fistful_stat    => #{
                'GetWallets'   => {linear, 3, 1000},
                '_'            => finish
            }
        }},
        {api_deadlines, #{
            fistful_stat => 5000
        }}
    ]).

-spec get_keysource(_, config()) ->
    _.

get_keysource(Key, Config) ->
    filename:join(?config(data_dir, Config), Key).

-spec issue_token(_, _) ->
    {ok, binary()} |
    {error,
        nonexistent_signee
    }.

issue_token(ACL, LifeTime) ->
    issue_token(?STRING, ACL, LifeTime).

-spec issue_token(_, _, _) ->
    {ok, binary()} |
    {error,
        nonexistent_signee
    }.

issue_token(PartyID, ACL, LifeTime) ->
    Claims = #{?STRING => ?STRING},
    wapi_authorizer_jwt:issue({{PartyID, wapi_acl:from_list(ACL)}, Claims}, LifeTime).

-spec get_context(binary()) ->
    wapi_client_lib:context().

get_context(Token) ->
    wapi_client_lib:get_context(?WAPI_URL, Token, 10000, ipv4).

% TODO move it to `wapi_dummy_service`, looks more appropriate

-spec start_mocked_service_sup(module()) ->
    pid().

start_mocked_service_sup(Module) ->
    {ok, SupPid} = supervisor:start_link(Module, []),
    _ = unlink(SupPid),
    SupPid.

-spec stop_mocked_service_sup(pid()) ->
    _.

stop_mocked_service_sup(SupPid) ->
    exit(SupPid, shutdown).

-spec mock_services(_, _) ->
    _.

mock_services(Services, SupOrConfig) ->
    start_woody_client(mock_services_(Services, SupOrConfig)).

start_woody_client(ServiceURLs) ->
    ok = application:set_env(
        wapi,
        service_urls,
        ServiceURLs
    ),
    start_app(wapi_woody_client, []).

-spec mock_services_(_, _) ->
    _.

% TODO need a better name
mock_services_(Services, Config) when is_list(Config) ->
    mock_services_(Services, ?config(test_sup, Config));

mock_services_(Services, SupPid) when is_pid(SupPid) ->
    Name = lists:map(fun get_service_name/1, Services),
    Port = get_random_port(),
    {ok, IP} = inet:parse_address(?WAPI_IP),
    ChildSpec = woody_server:child_spec(
        {dummy, Name},
        #{
            ip => IP,
            port => Port,
            event_handler => wapi_woody_event_handler,
            handlers => lists:map(fun mock_service_handler/1, Services)
        }
    ),
    {ok, _} = supervisor:start_child(SupPid, ChildSpec),
    lists:foldl(
        fun (Service, Acc) ->
            ServiceName = get_service_name(Service),
            Acc#{ServiceName => make_url(ServiceName, Port)}
        end,
        #{},
        Services
    ).

get_service_name({ServiceName, _Fun}) ->
    ServiceName;
get_service_name({ServiceName, _WoodyService, _Fun}) ->
    ServiceName.

mock_service_handler({ServiceName, Fun}) ->
    mock_service_handler(ServiceName, wapi_woody_client:get_service_modname(ServiceName), Fun);
mock_service_handler({ServiceName, WoodyService, Fun}) ->
    mock_service_handler(ServiceName, WoodyService, Fun).

mock_service_handler(ServiceName, WoodyService, Fun) ->
    {make_path(ServiceName), {WoodyService, {wapi_dummy_service, #{function => Fun}}}}.

% TODO not so failproof, ideally we need to bind socket first and then give to a ranch listener
get_random_port() ->
    rand:uniform(32768) + 32767.

make_url(ServiceName, Port) ->
    iolist_to_binary(["http://", ?WAPI_HOST_NAME, ":", integer_to_list(Port), make_path(ServiceName)]).

make_path(ServiceName) ->
    "/" ++ atom_to_list(ServiceName).

-spec get_lifetime() ->
    map().

get_lifetime() ->
    get_lifetime(0, 0, 7).

get_lifetime(YY, MM, DD) ->
    #{
       <<"years">>  => YY,
       <<"months">> => MM,
       <<"days">>   => DD
    }.
