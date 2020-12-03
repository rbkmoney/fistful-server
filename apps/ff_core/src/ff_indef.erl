%%%
%%% Indefinite value.

-module(ff_indef).

-export([new/1]).
-export([new/3]).
-export([current/1]).
-export([expmin/1]).
-export([expmax/1]).
-export([account/2]).
-export([confirm/2]).
-export([reject/2]).

-export([to_range/1]).

% totally ordered
-type ord(T) :: T.

-type indef(T) :: #{
    expected_min := ord(T),
    current := ord(T),
    expected_max := ord(T)
}.

-export_type([indef/1]).

%%

-spec new(T) -> indef(T).

-spec new(T, T, T) -> indef(T).

-spec current(indef(T)) -> T.

-spec expmin(indef(T)) -> T.

-spec expmax(indef(T)) -> T.
-spec account(T, indef(T)) -> indef(T).

-spec confirm(T, indef(T)) -> indef(T).

-spec reject(T, indef(T)) -> indef(T).

new(Seed) ->
    #{
        expected_min => Seed,
        current => Seed,
        expected_max => Seed
    }.

new(ExpMin, Current, ExpMax) ->
    #{
        expected_min => ExpMin,
        current => Current,
        expected_max => ExpMax
    }.

current(#{current := V}) ->
    V.

expmin(#{expected_min := V}) ->
    V.

expmax(#{expected_max := V}) ->
    V.

account(Delta, Indef = #{expected_min := ExpMin, expected_max := ExpMax}) ->
    Indef#{
        expected_min := erlang:min(ExpMin + Delta, ExpMin),
        expected_max := erlang:max(ExpMax + Delta, ExpMax)
    }.

confirm(Delta, Indef = #{current := Current, expected_min := ExpMin, expected_max := ExpMax}) ->
    Indef#{
        current := Current + Delta,
        expected_min := erlang:max(ExpMin + Delta, ExpMin),
        expected_max := erlang:min(ExpMax + Delta, ExpMax)
    }.

reject(Delta, Indef = #{expected_min := ExpMin, expected_max := ExpMax}) ->
    Indef#{
        expected_min := erlang:max(ExpMin - Delta, ExpMin),
        expected_max := erlang:min(ExpMax - Delta, ExpMax)
    }.

-spec to_range(indef(T)) -> ff_range:range(T).
to_range(#{expected_min := ExpMin, expected_max := ExpMax}) ->
    {{inclusive, ExpMin}, {inclusive, ExpMax}}.

%% Tests

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec convergency_test() -> _.

convergency_test() ->
    Opset = gen_opset(3 / 5, 2 / 3, 20),
    #{
        current := C,
        expected_min := ExpMin,
        expected_max := ExpMax
    } = lists:foldl(
        fun({Op, Delta}, Indef) -> Op(Delta, Indef) end,
        new(0),
        Opset
    ),
    ?assertEqual(C, ExpMin),
    ?assertEqual(C, ExpMax).

gen_opset(Pe, Pc, N) ->
    lists:reverse(gen_opset({Pe, Pc, N, []}, [])).

gen_opset(St, Acc) ->
    case gen_op(St) of
        {Op, St1} ->
            gen_opset(St1, [Op | Acc]);
        done ->
            Acc
    end.

gen_op({_, _, 0, []}) ->
    done;
gen_op({Pe, Pc, N, Ds}) ->
    case
        ff_random:from_choices([
            {Pe * sign(N), account},
            {(1 - Pe) * sign(length(Ds)), commit}
        ])
    of
        account ->
            Delta = ff_random:from_range(-1000, 1000),
            {{fun account/2, Delta}, {Pe, Pc, N - 1, [Delta | Ds]}};
        commit ->
            Delta = ff_random:from_list(Ds),
            Op = ff_random:from_choices([{Pc, fun confirm/2}, {1 - Pc, fun reject/2}]),
            {{Op, Delta}, {Pe, Pc, N, Ds -- [Delta]}}
    end.

sign(I) when I > 0 ->
    1;
sign(0) ->
    0;
sign(I) when I < 0 ->
    -1.

-endif.
