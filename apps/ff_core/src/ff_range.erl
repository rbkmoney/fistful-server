%%%
%%% Ranges w/ optional bounds on ordered types.

-module(ff_range).

-type range(T)    :: {maybe(bound(T)), maybe(bound(T))}.
-type bound(T)    :: {exclusive | inclusive, ord(T)}.

-type maybe(T)    :: infinity | T.
-type ord(T)      :: T. % totally ordered

-export_type([range/1]).
-export_type([bound/1]).

-export([intersect/2]).
-export([contains/2]).

%%

-spec intersect(range(T), range(T)) ->
    range(T) | undefined.

intersect(R1, R2) ->
    B1 = max_bound(lower(R1), lower(R2)),
    B2 = min_bound(upper(R1), upper(R2)),
    case compare_bounds(B1, B2) of
        gt ->
            undefined;
        _ ->
            from_bounds(B1, B2)
    end.

-spec contains(range(T), range(T)) ->
    boolean().

contains(R1, R2) ->
    intersect(R1, R2) =:= R2.

%%

compare_bounds(B1, B1) ->
    eq;
compare_bounds(_B1, neginf) ->
    gt;
compare_bounds(_B1, posinf) ->
    lt;
compare_bounds({_, V1}, {_, V2}) when V1 > V2 ->
    gt;
compare_bounds({from, V1}, {_, V1}) ->
    gt;
compare_bounds({upto, V1}, {_, V1}) ->
    lt;
compare_bounds(B1, B2) ->
    case compare_bounds(B2, B1) of
        gt -> lt;
        lt -> gt
    end.

max_bound(B1, B2) ->
    case compare_bounds(B1, B2) of
        gt -> B1;
        _  -> B2
    end.

min_bound(B1, B2) ->
    case compare_bounds(B1, B2) of
        lt -> B1;
        _  -> B2
    end.

%%

lower({infinity, _}) ->
    neginf;
lower({{exclusive, V}, _}) ->
    {from, V};
lower({{inclusive, V}, _}) ->
    {point, V}.

upper({_, infinity}) ->
    posinf;
upper({_, {exclusive, V}}) ->
    {upto, V};
upper({_, {inclusive, V}}) ->
    {point, V}.

from_bounds(B1, B2) ->
    {from_lower(B1), from_upper(B2)}.

from_lower(neginf) ->
    infinity;
from_lower({from, V}) ->
    {exclusive, V};
from_lower({point, V}) ->
    {inclusive, V}.

from_upper(posinf) ->
    infinity;
from_upper({upto, V}) ->
    {exclusive, V};
from_upper({point, V}) ->
    {inclusive, V}.

%% Tests

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-type testcase() :: {_, fun()}.

-spec intersect_test_() -> [testcase()].
intersect_test_() ->
    [
        ?_assertEqual(
            {infinity, infinity},
            intersect(
                {infinity, infinity},
                {infinity, infinity}
            )
        ),
        ?_assertEqual(
            undefined,
            intersect(
                {infinity, {exclusive, 0}},
                {{exclusive, 0}, infinity}
            )
        ),
        ?_assertEqual(
            undefined,
            intersect(
                {{exclusive, 0}, infinity},
                {infinity, {exclusive, 0}}
            )
        ),
        ?_assertEqual(
            {{inclusive, 0}, {inclusive, 0}},
            intersect(
                {infinity, {inclusive, 0}},
                {{inclusive, 0}, infinity}
            )
        ),
        ?_assertEqual(
            {{inclusive, 0}, {inclusive, 0}},
            intersect(
                {{inclusive, 0}, infinity},
                {infinity, {inclusive, 0}}
            )
        ),
        ?_assertEqual(
            {{inclusive, 1}, {exclusive, 42}},
            intersect(
                {infinity, {exclusive, 42}},
                {{inclusive, 1}, infinity}
            )
        ),
        ?_assertEqual(
            {{exclusive, 42}, infinity},
            intersect(
                {{exclusive, 42}, infinity},
                {{exclusive, 42}, infinity}
            )
        ),
        ?_assertEqual(
            {{exclusive, 42}, {exclusive, 43}},
            intersect(
                {{inclusive, 42}, {exclusive, 43}},
                {{exclusive, 42}, {inclusive, 43}}
            )
        ),
        ?_assertEqual(
            {{inclusive, 42}, {inclusive, 42}},
            intersect(
                {{inclusive, 41}, {inclusive, 42}},
                {{inclusive, 42}, {inclusive, 43}}
            )
        ),
        ?_assertEqual(
            {{inclusive, 42}, {exclusive, 43}},
            intersect(
                {{exclusive, 41}, {inclusive, 44}},
                {{inclusive, 42}, {exclusive, 43}}
            )
        )
    ].

-spec contains_test_() -> [testcase()].
contains_test_() ->
    [
        ?_assertEqual(
            true,
            contains(
                {infinity, infinity},
                {infinity, infinity}
            )
        ),
        ?_assertEqual(
            true,
            contains(
                {infinity, infinity},
                {{exclusive, 0}, {inclusive, 1000}}
            )
        ),
        ?_assertEqual(
            false,
            contains(
                {{exclusive, 0}, {inclusive, 1000}},
                {infinity, infinity}
            )
        ),
        ?_assertEqual(
            true,
            contains(
                {{exclusive, 41}, {inclusive, 43}},
                {{inclusive, 42}, {exclusive, 43}}
            )
        ),
        ?_assertEqual(
            false,
            contains(
                {{exclusive, 41}, {exclusive, 43}},
                {{inclusive, 42}, {inclusive, 43}}
            )
        )
    ].

-endif.
