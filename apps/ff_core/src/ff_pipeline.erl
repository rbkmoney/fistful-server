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
    apply_transform(fun transform_do/1, Forms).

transform_do({call, Line, {atom, Line, do}, Args}) ->
    build_do_statement(Line, Args);
transform_do({call, Line, {remote, Line, {atom, Line, ff_pipeline}, {atom, Line, do}}, Args}) ->
    build_do_statement(Line, Args);
transform_do(Node) ->
    Node.

build_do_statement(Line, [Tag, Fun]) ->
    ThrownVar = build_var_name(<<"Thrown">>, Line),
    Error = {tuple, Line, [{atom, Line, error}, {tuple, Line, [Tag, {var, Line, ThrownVar}]}]},
    build_do_statement(Line, ThrownVar, Error, Fun);
build_do_statement(Line, [Fun]) ->
    ThrownVar = build_var_name(<<"Thrown">>, Line),
    Error = {tuple, Line, [{atom, Line, error}, {var, Line, ThrownVar}]},
    build_do_statement(Line, ThrownVar, Error, Fun).

build_do_statement(Line, ThrownVar, Error, Fun) ->
    {'fun', _, {clauses, [{clause, _, [], [], Content}]}} = Fun,
    NewContent = [
        {call, Line, {remote, Line, {atom, Line, ff_pipeline}, {atom, Line, wrap}}, [
            {block, Line, Content}
        ]}
    ],
    {'try', Line,
        NewContent,
        [],
        [{clause, Line,
            [{tuple, Line, [{atom, Line, throw}, {var, Line, ThrownVar}, {var, Line, '_'}]}],
            [],
            [Error]}],
        []}.

build_var_name(Name, Line) ->
    LineStr = erlang:integer_to_binary(Line),
    erlang:binary_to_atom(<<"$Pipeline", Name/binary, "-", LineStr/binary>>, latin1).

%% AST traversal
apply_transform(F, Node) ->
    NewNode = F(Node),
    do_apply_transform(F, NewNode).

do_apply_transform(F, Nodes) when is_list(Nodes) ->
    [apply_transform(F, N) || N <- Nodes];
do_apply_transform(F, {function, Line, Name, Arity, Clauses}) ->
    {function, Line, Name, Arity, apply_transform(F, Clauses)};
do_apply_transform(F, {clause, Line, Head, Guard, Body}) ->
    {clause, Line, Head, Guard, apply_transform(F, Body)};
do_apply_transform(F, {block, Line, Es}) ->
    {block, Line, apply_transform(F, Es)};
do_apply_transform(F, {'if', Line, Cs}) ->
    {'if', Line, apply_transform(F, Cs)};
do_apply_transform(F, {'case', Line, E, Cs}) ->
    {'case', Line, apply_transform(F, E), apply_transform(F, Cs)};
do_apply_transform(F, {'receive', Line, Cs}) ->
    {'receive', Line, apply_transform(F, Cs)};
do_apply_transform(F, {'receive', Line, Cs, To, ToEs}) ->
    {'receive', Line, apply_transform(F, Cs), apply_transform(F, To), apply_transform(F, ToEs)};
do_apply_transform(F, {'try', Line, Es, Scs, Ccs, As}) ->
    {'try', Line, apply_transform(F, Es), apply_transform(F, Scs), apply_transform(F, Ccs), apply_transform(F, As)};
do_apply_transform(F, {'fun', Line, Body}) ->
    {'fun', Line, apply_transform(F, Body)};
do_apply_transform(F, {'catch', Line, E}) ->
    {'catch', Line, apply_transform(F, E)};
do_apply_transform(F, {call, Line, FName, As}) ->
    {call, Line, apply_transform(F, FName), apply_transform(F, As)};
do_apply_transform(F, {match, Line, L, R}) ->
    {match, Line, apply_transform(F, L), apply_transform(F, R)};
do_apply_transform(F, {cons, Line, H, T}) ->
    {cons, Line, apply_transform(F, H), apply_transform(F, T)};
do_apply_transform(F, {tuple, Line, Ps}) ->
    {tuple, Line, apply_transform(F, Ps)};
do_apply_transform(F, {map, Line, Fields}) ->
    {map, Line, apply_transform(F, Fields)};
do_apply_transform(F, {map, Line, Expr, Fields}) ->
    {map, Line, apply_transform(F, Expr), apply_transform(F, Fields)};
do_apply_transform(F, {record, Line, Name, Pfs}) ->
    {record, Line, Name, apply_transform(F, Pfs)};
do_apply_transform(F, {op, Line, Op, A}) ->
    {op, Line, Op, apply_transform(F, A)};
do_apply_transform(F, {op, Line, Op, L, R}) ->
    {op, Line, Op, apply_transform(F, L), apply_transform(F, R)};
do_apply_transform(F, {record_field, Line, Name, P}) ->
    {record_field, Line, Name, apply_transform(F, P)};
do_apply_transform(_F, Node) ->
    Node.
