%%%
%%% Well, a map
%%%

-module(ff_map).

-export([find/2]).

%%

-spec find(Key, #{Key => Value}) ->
    {ok, Value} |
    {error, notfound}.

find(Key, Map) ->
    case Map of
        #{Key := Value} ->
            {ok, Value};
        #{} ->
            {error, notfound}
    end.
