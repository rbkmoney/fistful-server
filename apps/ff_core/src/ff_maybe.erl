%%%
%%% Call me maybe
%%%

-module(ff_maybe).

-type maybe(T) ::
    undefined | T.

-export_type([maybe/1]).

-export([from_result/1]).

%%

-spec from_result({ok, T} | {error, _}) ->
    maybe(T).

from_result({ok, T}) ->
    T;
from_result({error, _}) ->
    undefined.
