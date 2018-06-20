%%%
%%% String manipultion facilities.

-module(ff_string).

-export([join/2]).

%%

-spec join(Delim, [Fragment]) -> binary() when
    Delim ::
        char()   |
        iodata() ,
    Fragment ::
        iodata() |
        char()   |
        atom()   |
        number() .

join(Delim, Fragments) ->
    genlib_string:join(Delim, lists:map(fun genlib:to_binary/1, Fragments)).
