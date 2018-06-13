%%%
%%% Well, a map
%%%

-module(ff_map).

-export([find/2]).

%%

-spec find(_Key, #{}) ->
    {ok, _Value} |
    {error, notfound}.

find(Key, Map) ->
    case Map of
        #{Key := Value} ->
            {ok, Value};
        #{} ->
            {error, notfound}
    end.
