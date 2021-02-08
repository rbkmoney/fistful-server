-module(wapi_dummy_service).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(FunName, Args, _, #{function := Fun}) ->
    case Fun(FunName, Args) of
        {throwing, Exception} ->
            erlang:throw(Exception);
        Result ->
            Result
    end.
