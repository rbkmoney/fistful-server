-module(ff_msgpack_codec).

-include_lib("fistful_proto/include/ff_proto_msgpack_thrift.hrl").

-export([unmarshal/2]).
-export([marshal/2]).

%% Types

-type type_name() :: msgpack.
-type decoded_value() ::
    #{decoded_value() => decoded_value()} |
    [decoded_value()] |
    integer() |
    float() |
    binary() |
    {binary, binary()} |
    nil.
-type encoded_value() :: ff_proto_msgpack_thrift:'Value'().

-export_type([type_name/0]).
-export_type([encoded_value/0]).
-export_type([decoded_value/0]).

-spec marshal(type_name(), decoded_value()) ->
    encoded_value().
marshal(msgpack, nil)                  -> {nl, #msgp_Nil{}};
marshal(msgpack, V) when is_boolean(V) -> {b, V};
marshal(msgpack, V) when is_integer(V) -> {i, V};
marshal(msgpack, V) when is_float(V)   -> V;
marshal(msgpack, V) when is_binary(V)  -> {str, V}; % Assuming well-formed UTF-8 bytestring.
marshal(msgpack, {binary, V}) when is_binary(V) ->
    {bin, V};
marshal(msgpack, V) when is_list(V) ->
    {arr, [marshal(msgpack, ListItem) || ListItem <- V]};
marshal(msgpack, V) when is_map(V) ->
    {obj, maps:fold(fun(Key, Value, Map) -> Map#{marshal(msgpack, Key) => marshal(msgpack, Value)} end, #{}, V)}.

-spec unmarshal(type_name(), encoded_value()) ->
    decoded_value().
unmarshal(msgpack, {nl,  #msgp_Nil{}})          -> nil;
unmarshal(msgpack, {b,   V}) when is_boolean(V) -> V;
unmarshal(msgpack, {i,   V}) when is_integer(V) -> V;
unmarshal(msgpack, {flt, V}) when is_float(V)   -> V;
unmarshal(msgpack, {str, V}) when is_binary(V)  -> V; % Assuming well-formed UTF-8 bytestring.
unmarshal(msgpack, {bin, V}) when is_binary(V)  -> {binary, V};
unmarshal(msgpack, {arr, V}) when is_list(V)    -> [unmarshal(msgpack, ListItem) || ListItem <- V];
unmarshal(msgpack, {obj, V}) when is_map(V)     ->
    maps:fold(fun(Key, Value, Map) -> Map#{unmarshal(msgpack, Key) => unmarshal(msgpack, Value)} end, #{}, V).
