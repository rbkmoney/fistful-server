%%%
%%% Pipeline
%%%
%%% TODO
%%%  - A simple `ok` as a possible result only make everything more complex
%%%

-module(ff_pipeline).

-include_lib("syntax_tools/include/merl.hrl").

-export([do/1]).
-export([do/2]).
-export([unwrap/1]).
-export([unwrap/2]).
-export([wrap/1]).
-export([expect/2]).
-export([flip/1]).
-export([valid/2]).

-export([with/3]).

-export([parse_transform/2]).

%%

-type thrown(_E) ::
    no_return().

-type result(T, E) ::
    {ok, T} | {error, E}.

-spec do(fun(() -> ok | T | thrown(E))) ->
    ok | result(T, E).

do(Fun) ->
    try Fun() of
        ok ->
            ok;
        R ->
            {ok, R}
    catch
        Thrown -> {error, Thrown}
    end.

-spec do(Tag, fun(() -> ok | T | thrown(E))) ->
    ok | result(T, {Tag, E}).

do(Tag, Fun) ->
    ff_pipeline:do(fun () -> unwrap(Tag, do(Fun)) end).

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

-spec wrap
    (ok)         -> ok;
    (V)          -> {ok, V}.

wrap(ok) ->
    ok;
wrap(V) ->
    {ok, V}.

-spec expect
    (_E, ok)         -> ok;
    (_E, {ok, V})    -> V;
    ( E, {error, _}) -> thrown(E).

expect(_, ok) ->
    ok;
expect(_, {ok, V}) ->
    V;
expect(E, {error, _}) ->
    throw(E).

-spec flip(result(T, E)) ->
    result(E, T).

flip({ok, T}) ->
    {error, T};
flip({error, E}) ->
    {ok, E}.

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
    ok | {error, T}.

valid(V, V) ->
    ok;
valid(_, V) ->
    {error, V}.

%% TODO
%%  - Too complex
%%  - Not the right place

-type outcome(E, R) ::
    {ok, [E]} | {error, R}.

-spec with(Sub, St, fun((SubSt | undefined) -> outcome(SubEv, Reason))) ->
    outcome({Sub, SubEv}, {Sub, Reason}) when
        Sub   :: atom(),
        St    :: #{Sub => SubSt},
        SubSt :: _.

with(Model, St, F) ->
    case F(maps:get(Model, St, undefined)) of
        {ok, Events0} when is_list(Events0) ->
            Events1 = [{Model, Ev} || Ev <- Events0],
            {ok, Events1};
        {error, Reason} ->
            {error, {Model, Reason}}
    end.

%% Parse transform
% f() ->
%     do(fun() ->
%         R0 = 1,
%         R1 = unwrap(do_smth(R0)),
%         R2 = unwrap(do_smth2(R1)),
%         R2 + 1
%     end).
% To
% f() ->
%     try
%         ff_pipeline:wrap(begin
%             R0 = 1,
%             R1 = ff_pipeline:unwrap(do_smth(R0)),
%             R2 = ff_pipeline:unwrap(do_smth2(R1)),
%             R2 + 1
%         end)
%     catch
%         Thrown -> {error, Thrown}
%     end.

-spec parse_transform(Forms, [compile:option()]) -> Forms when
    Forms :: [erl_parse:abstract_form() | erl_parse:form_info()].
parse_transform(Forms, _Options) ->
    [erl_syntax:revert(erl_syntax_lib:map(fun transform_do/1, Form)) || Form <- Forms].

transform_do(Tree) ->
    TreePos = erl_syntax:get_pos(Tree),
    case Tree of
        ?Q("do(_@Tag, fun() -> _@@Body end)") ->
            build_do_statement(TreePos, Tag, Body);
        ?Q("ff_pipeline:do(_@Tag, fun() -> _@@Body end)") ->
            build_do_statement(TreePos, Tag, Body);
        ?Q("do(fun() -> _@@Body end)") ->
            build_do_statement(TreePos, Body);
        ?Q("ff_pipeline:do(fun() -> _@@Body end)") ->
            build_do_statement(TreePos, Body);
        _ ->
            Tree
    end.

build_do_statement(Pos, Body) ->
    ThrownVar = build_var(<<"Thrown">>, Pos),
    Error = ?Q("_@ThrownVar"),
    build_do_statement(Pos, ThrownVar, Error, Body).

build_do_statement(Pos, Tag, Body) ->
    ThrownVar = build_var(<<"Thrown">>, Pos),
    Error = ?Q("{_@Tag, _@ThrownVar}"),
    build_do_statement(Pos, ThrownVar, Error, Body).

build_do_statement(_Pos, ThrownVar, Error, Body) ->
    ?Q([
        "try ff_pipeline:wrap(begin _@@Body end)",
        "catch _@ThrownVar -> {error, _@Error} end"
    ]).

build_var(Name, Line) ->
    LineStr = erlang:integer_to_binary(Line),
    VarName = erlang:binary_to_atom(<<"$Pipeline", Name/binary, "-", LineStr/binary>>, latin1),
    merl:var(VarName).
