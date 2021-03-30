%% @doc Public API and application startup.
%% @end

-module(wapi).

-behaviour(application).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

%%

-spec start(normal, any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    case application:get_all_env(wapi) of
        [] ->
            % TODO #ED-96 на период разделения отсутствие настройки wapi - признак его отключения
            _ = logger:warning("wapi is not configured - will not be start"),
            {error, normal};
        _ ->
            wapi_sup:start_link()
    end.

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
