%%%
%%% Chaos Industries.

-module(ff_random).

-export([date/0]).
-export([time/0]).

-export([from_choices/1]).
-export([from_list/1]).
-export([from_range/2]).

%%

-spec date() -> calendar:date().

-spec time() -> calendar:time().

date() ->
    Y = from_range(1970, 9999),
    M = from_range(1, 12),
    D = from_range(1, calendar:last_day_of_the_month(Y, M)),
    {Y, M, D}.

time() ->
    H = from_range(0, 23),
    M = from_range(0, 59),
    S = from_range(0, 59),
    {H, M, S}.

%%

-type choice(T) :: {probability(), T}.
% >= 0
-type probability() :: number().

-spec from_choices([choice(T)]) -> T.
from_choices(Choices) ->
    Psum = lists:sum([assert_probability(P) || {P, _} <- Choices]),
    Roll = rand:uniform() * Psum,
    {_, C} = lists:foldl(
        fun
            ({P, _}, R) when is_number(R), P < R ->
                R - P;
            ({_, _} = C, R) when is_number(R) ->
                C;
            (_, {_, _} = C) ->
                C
        end,
        Roll,
        Choices
    ),
    C.

assert_probability(P) when is_number(P), P >= 0 ->
    P;
assert_probability(_) ->
    error(badarg).

-spec from_list([T, ...]) -> T.
from_list(List) ->
    from_choices([{1, E} || E <- List]).

-spec from_range(M :: integer(), N :: integer()) ->
    % from [M; N]
    integer().
from_range(M, N) when M < N ->
    rand:uniform(N - M + 1) - 1 + M.
