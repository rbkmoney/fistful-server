%%%
%%% Call me maybe
%%%

-module(ff_maybe).

-type maybe(T) ::
    undefined | T.

-export_type([maybe/1]).

-export([from_result/1]).
-export([to_list/1]).
-export([apply/2]).
-export([apply/3]).
-export([get_defined/1]).
-export([get_defined/2]).

%%

-spec from_result({ok, T} | {error, _}) -> maybe(T).
from_result({ok, T}) ->
    T;
from_result({error, _}) ->
    undefined.

-spec to_list(maybe(T)) -> [T].
to_list(undefined) ->
    [];
to_list(T) ->
    [T].

-spec apply(fun(), Arg :: undefined | term()) -> term().
apply(Fun, Arg) ->
    ff_maybe:apply(Fun, Arg, undefined).

-spec apply(fun(), Arg :: undefined | term(), Default :: term()) -> term().
apply(Fun, Arg, _Default) when Arg =/= undefined ->
    Fun(Arg);
apply(_Fun, undefined, Default) ->
    Default.

-spec get_defined([maybe(T)]) -> T.
get_defined([]) ->
    erlang:error(badarg);
get_defined([Value | _Tail]) when Value =/= undefined ->
    Value;
get_defined([undefined | Tail]) ->
    get_defined(Tail).

-spec get_defined(maybe(T), maybe(T)) -> T.
get_defined(V1, V2) ->
    get_defined([V1, V2]).
