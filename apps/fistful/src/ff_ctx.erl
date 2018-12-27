%%%
%%% Internal client context
%%%

-module(ff_ctx).

-include_lib("fistful_proto/include/ff_proto_msgpack_thrift.hrl").

-type ctx() :: #{namespace() => md()}.

-type namespace() :: binary().
-type md()        :: %% as stolen from `machinery_msgpack`
    nil                |
    boolean()          |
    integer()          |
    float()            |
    binary()           | %% string
    {binary, binary()} | %% binary
    [md()]             |
    #{md() => md()}    .

-export_type([ctx/0]).
-export_type([md/0]).

-export([new/0]).
-export([get/2]).
-export([wrap/1]).
-export([unwrap/1]).
%%

-spec new() ->
    ctx().
new() ->
    #{}.

-spec get(namespace(), ctx()) ->
    {ok, md()}       |
    {error, notfound}.
get(Ns, Ctx) ->
    ff_map:find(Ns, Ctx).

%% snatch from https://github.com/rbkmoney/erlang_capi/blob/v2/apps/capi/src/capi_msgpack.erl
-spec unwrap(map()) ->
    ctx().
unwrap(Ctx) when is_map(Ctx) ->
    maps:map(fun(_NS, V) -> unwrap_(V) end, Ctx).

unwrap_({nl, #msgp_Nil{}})           -> nil;
unwrap_({b,   V}) when is_boolean(V) -> V;
unwrap_({i,   V}) when is_integer(V) -> V;
unwrap_({flt, V}) when is_float(V)   -> V;
unwrap_({str, V}) when is_binary(V)  -> V; % Assuming well-formed UTF-8 bytestring.
unwrap_({bin, V}) when is_binary(V)  -> {binary, V};
unwrap_({arr, V}) when is_list(V)    -> [unwrap_(ListItem) || ListItem <- V];
unwrap_({obj, V}) when is_map(V)     ->
    maps:fold(fun(Key, Value, Map) -> Map#{unwrap_(Key) => unwrap_(Value)} end, #{}, V).

-spec wrap(map()) -> ctx().
wrap(Value) when is_map(Value) ->
    maps:map(fun(_K, V) -> wrap_(V) end, Value).

wrap_(nil)                  -> {nl, #msgp_Nil{}};
wrap_(V) when is_boolean(V) -> {b, V};
wrap_(V) when is_integer(V) -> {i, V};
wrap_(V) when is_float(V)   -> V;
wrap_(V) when is_binary(V)  -> {str, V}; % Assuming well-formed UTF-8 bytestring.
wrap_({binary, V}) when is_binary(V) ->
    {bin, V};
wrap_(V) when is_list(V) ->
    {arr, [wrap_(ListItem) || ListItem <- V]};
wrap_(V) when is_map(V) ->
    {obj, maps:fold(fun(Key, Value, Map) -> Map#{wrap_(Key) => wrap_(Value)} end, #{}, V)}.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec unwrap_test() -> _.
unwrap_test() ->
    K = {str, <<"Key">>},
    K2 = {i, 1}, V = {str, <<"Value">>},
    Obj = {obj, #{K => V}},
    Obj2 = {obj, #{K2 => Obj}},
    MsgPack = {arr, [Obj2]},
    Ctx = #{<<"namespace">> => MsgPack},

    UnwrapMsgPack = unwrap(Ctx),
    ?assertEqual(#{<<"namespace">> => [#{1 => #{<<"Key">> => <<"Value">>}}]}, UnwrapMsgPack).

-spec wrap_test() -> _.
wrap_test() ->
    Str = <<"Привет, Dude!">>,
    Obj = #{123 => Str},
    Arr = [Obj],
    MsgPack = wrap(#{<<"NS">> => Arr}),
    ?assertEqual(#{<<"NS">> => {arr, [{obj, #{ {i, 123} => {str, Str} }}]}}, MsgPack).

-endif.