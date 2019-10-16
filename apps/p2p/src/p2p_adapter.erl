%% P2P adapter client

-module(p2p_adapter).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").

%% Exports

-export([process/2]).
-export([handle_callback/3]).

%% Types

-type route()           :: dmsl_domain_thrift:'PaymentRoute'().

-type context()         :: p2p_adapter_thrift:'Context'().
-type callback()        :: p2p_adapter_thrift:'Callback'().

-type process_result()  :: p2p_adapter_thrift:'ProcessResult'().
-type callback_result() :: p2p_adapter_thrift:'CallbackResult'().

%% API

-spec process(context(), route()) ->
    {ok, process_result()} | no_return().
process(Context, Route) ->
    issue_call('Process', [Context], Route).

-spec handle_callback(callback(), context(), route()) ->
    {ok, callback_result()} | no_return().
handle_callback(Callback, Context, Route) ->
    issue_call('HandleCallback', [Callback, Context], Route).

%% Implementation

issue_call(Function, Args, Route) ->
    Opts    = get_call_options(Route),
    Client  = ff_woody_client:new(Opts),
    Request = {{p2p_adapter_thrift, 'Adapter'}, Function, Args},
    ff_woody_client:call(Client, Request).

get_route_provider_ref(#domain_PaymentRoute{provider = ProviderRef}) ->
    ProviderRef.

% TODO: mostly copypasta from hg_proxy, might require more ff/p2p specific tweaks
get_call_options(Route) ->
    ProviderRef    = get_route_provider_ref(Route),
    Revision       = ff_domain_config:head(),
    {ok, Provider} = ff_domain_config:get(Revision, ProviderRef),
    get_call_options(Provider#domain_Provider.proxy, Revision).

get_call_options(#domain_Proxy{ref = ProxyRef}, Revision) ->
    {ok, ProxyDef} = ff_domain_config:get(Revision, {proxy, ProxyRef}),
    construct_call_options(ProxyDef).

construct_call_options(#domain_ProxyDefinition{url = Url}) ->
    construct_transport_options(#{url => Url}).

construct_transport_options(Opts) ->
    construct_transport_options(Opts, genlib_app:env(fistful, proxy_opts, #{})).

construct_transport_options(Opts, #{transport_opts := TransportOpts = #{}}) ->
    Fields = [connect_timeout, recv_timeout, pool, max_connections],
    Opts#{transport_opts => maps:with(Fields, TransportOpts)};
construct_transport_options(Opts, #{}) ->
    Opts.
