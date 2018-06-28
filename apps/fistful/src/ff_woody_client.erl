%%%
%%% Keep woody well typed

-module(ff_woody_client).

%%

-type url()            :: woody:url().
-type event_handler()  :: woody:ev_handler().
-type transport_opts() :: woody_client_thrift_http_transport:options().
-type context()        :: woody_context:ctx().

-type service_id()     :: atom().

-type client() :: #{
    url            := url(),
    event_handler  := event_handler(),
    transport_opts => transport_opts()
}.

-type caller() :: #{
    client         := client(),
    context        => context()
}.

-export_type([client/0]).
-export_type([caller/0]).

-export([new/1]).
-export([call/2]).

%%

-type opts() :: #{
    url            := url(),
    event_handler  => event_handler(),
    transport_opts => transport_opts()
}.

-spec new(woody:url() | opts()) ->
    client().

new(Opts = #{url := _}) ->
    maps:merge(
        #{
            event_handler => scoper_woody_event_handler
        },
        maps:with([url, event_handler, transport_opts], Opts)
    );
new(Url) when is_binary(Url); is_list(Url) ->
    new(#{
        url => genlib:to_binary(Url)
    }).

-spec call(service_id() | client(), woody:request()) ->
    {ok, woody:result()}                      |
    {exception, woody_error:business_error()}.

call(ServiceID, Request) when is_atom(ServiceID) ->
    Client = get_service_client(ServiceID),
    woody_client:call(Request, Client, ff_woody_ctx:get());
call(Client, Request) when is_map(Client) ->
    woody_client:call(Request, Client, ff_woody_ctx:get()).

%%

get_service_client(ServiceID) ->
    case maps:find(ServiceID, genlib_app:env(fistful, services, #{})) of
        {ok, Client} ->
            Client;
        error ->
            error({'woody client undefined', ServiceID})
    end.
