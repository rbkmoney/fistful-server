%%%
%%% String manipultion facilities.

-module(ff_string).

-export([join/1]).
-export([join/2]).

%%

-type fragment() ::
    iodata()
    | char()
    | atom()
    | number().

-spec join([fragment()]) -> binary().
join(Fragments) ->
    join(<<>>, Fragments).

-spec join(Delim, [fragment()]) -> binary() when
    Delim ::
        char()
        | iodata().
join(Delim, Fragments) ->
    genlib_string:join(Delim, lists:map(fun genlib:to_binary/1, Fragments)).
