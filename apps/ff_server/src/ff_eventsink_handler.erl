-module(ff_eventsink_handler).

-export([handle_function/5]).
-export([marshal/2]).

-include_lib("fistful_proto/include/ff_proto_eventsink_thrift.hrl").

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options(), module()) ->
    {ok, woody:result()} | no_return().

handle_function(
    'GetEvents', [#'evsink_EventRange'{'after' = After, limit = Limit}],
    Context, #{schema := Schema, client := Client, ns := NS}, Handler
) ->
    {ok, Events} = machinery_mg_eventsink:get_events(NS, After, Limit,
        #{client => {Client, Context}, schema => Schema}),
    {ok, Handler:publish_events(Events)};
handle_function(
    'GetLastEventID', _Params, Context,
    #{schema := Schema, client := Client, ns := NS}, _Handler
) ->
    case machinery_mg_eventsink:get_last_event_id(NS,
        #{client => {Client, Context}, schema => Schema}) of
        {ok, _} = Result ->
            Result;
        {error, no_last_event} ->
            woody_error:raise(business, #'evsink_NoLastEvent'{})
    end.

-spec marshal(atom() | tuple(), term()) ->
    any().

marshal(id, V) ->
    marshal(string, V);
marshal(event_id, V) ->
    marshal(integer, V);

marshal(timestamp, {{Date, Time}, USec} = V) ->
    case rfc3339:format({Date, Time, USec, 0}) of
        {ok, R} when is_binary(R) ->
            R;
        Error ->
            error({bad_timestamp, Error}, [timestamp, V])
    end;
marshal(string, V) when is_binary(V) ->
    V;
marshal(integer, V) when is_integer(V) ->
    V;
% Catch this up in thrift validation
marshal(_, Other) ->
    Other.

