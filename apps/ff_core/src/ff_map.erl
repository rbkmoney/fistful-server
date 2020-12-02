%%%
%%% Well, a map
%%%

-module(ff_map).

-type result(T) ::
    {ok, T}
    | {error, notfound}.

-export_type([result/1]).

-export([find/2]).

%%

-spec find(Key, #{Key => Value}) -> result(Value).
find(Key, Map) ->
    case Map of
        #{Key := Value} ->
            {ok, Value};
        #{} ->
            {error, notfound}
    end.
