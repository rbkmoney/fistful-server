%%%
%%% Pipeline
%%%
%%% TODO
%%%  - A simple `ok` as a possible result only make everything more complex
%%%

-module(ff_pipeline).

-export([do/1]).
-export([do/2]).
-export([unwrap/1]).
-export([unwrap/2]).
-export([expect/2]).
-export([flip/1]).
-export([valid/2]).

-export([with/3]).

%%

-type thrown(_E) ::
    no_return().

-type result(T, E) ::
    {ok, T} | {error, E}.

-spec do(fun(() -> ok | T | thrown(E))) -> ok | result(T, E).
do(Fun) ->
    try Fun() of
        ok ->
            ok;
        R ->
            {ok, R}
    catch
        Thrown -> {error, Thrown}
    end.

-spec do(Tag, fun(() -> ok | T | thrown(E))) -> ok | result(T, {Tag, E}).
do(Tag, Fun) ->
    do(fun() -> unwrap(Tag, do(Fun)) end).

-spec unwrap
    (ok) -> ok;
    ({ok, V}) -> V;
    ({error, E}) -> thrown(E).
unwrap(ok) ->
    ok;
unwrap({ok, V}) ->
    V;
unwrap({error, E}) ->
    throw(E).

-spec expect
    (_E, ok) -> ok;
    (_E, {ok, V}) -> V;
    (E, {error, _}) -> thrown(E).
expect(_, ok) ->
    ok;
expect(_, {ok, V}) ->
    V;
expect(E, {error, _}) ->
    throw(E).

-spec flip(result(T, E)) -> result(E, T).
flip({ok, T}) ->
    {error, T};
flip({error, E}) ->
    {ok, E}.

-spec unwrap
    (_Tag, ok) -> ok;
    (_Tag, {ok, V}) -> V;
    (Tag, {error, E}) -> thrown({Tag, E}).
unwrap(_, ok) ->
    ok;
unwrap(_, {ok, V}) ->
    V;
unwrap(Tag, {error, E}) ->
    throw({Tag, E}).

-spec valid(T, T) -> ok | {error, T}.
valid(V, V) ->
    ok;
valid(_, V) ->
    {error, V}.

%% TODO
%%  - Too complex
%%  - Not the right place

-type outcome(E, R) ::
    {ok, [E]} | {error, R}.

-spec with(Sub, St, fun((SubSt | undefined) -> outcome(SubEv, Reason))) -> outcome({Sub, SubEv}, {Sub, Reason}) when
    Sub :: atom(),
    St :: #{Sub => SubSt},
    SubSt :: _.
with(Model, St, F) ->
    case F(maps:get(Model, St, undefined)) of
        {ok, Events0} when is_list(Events0) ->
            Events1 = [{Model, Ev} || Ev <- Events0],
            {ok, Events1};
        {error, Reason} ->
            {error, {Model, Reason}}
    end.
