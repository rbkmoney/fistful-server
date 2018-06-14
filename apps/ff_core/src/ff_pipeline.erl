%%%
%%% Pipeline
%%%

-module(ff_pipeline).

-export([do/1]).
-export([unwrap/1]).
-export([unwrap/2]).
-export([valid/2]).

%%

-type thrown(_E) :: no_return().

-spec do(fun(() -> T | thrown(E))) ->
    {ok, T} | {error, E}.

do(Fun) ->
    try {ok, Fun()} catch
        Thrown -> {error, Thrown}
    end.

-spec unwrap
    (ok)         -> ok;
    ({ok, V})    -> V;
    ({error, E}) -> thrown(E).

unwrap(ok) ->
    ok;
unwrap({ok, V}) ->
    V;
unwrap({error, E}) ->
    throw(E).

-spec unwrap
    (_Tag, ok)         -> ok;
    (_Tag, {ok, V})    -> V;
    ( Tag, {error, E}) -> thrown({Tag, E}).

unwrap(_, ok) ->
    ok;
unwrap(_, {ok, V}) ->
    V;
unwrap(Tag, {error, E}) ->
    throw({Tag, E}).

-spec valid(T, T) ->
    {ok, T} | {error, T}.

valid(V, V) ->
    {ok, V};
valid(_, V) ->
    {error, V}.
