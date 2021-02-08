-module(ff_proto_utils).

-export([serialize/2]).
-export([deserialize/2]).

-spec serialize(thrift_type(), term()) -> binary().

-type thrift_type() ::
    thrift_base_type()
    | thrift_collection_type()
    | thrift_enum_type()
    | thrift_struct_type().

-type thrift_base_type() ::
    bool
    | double
    | i8
    | i16
    | i32
    | i64
    | string.

-type thrift_collection_type() ::
    {list, thrift_type()}
    | {set, thrift_type()}
    | {map, thrift_type(), thrift_type()}.

-type thrift_enum_type() ::
    {enum, thrift_type_ref()}.

-type thrift_struct_type() ::
    {struct, thrift_struct_flavor(), thrift_type_ref() | thrift_struct_def()}.

-type thrift_struct_flavor() :: struct | union | exception.

-type thrift_type_ref() :: {module(), Name :: atom()}.

-type thrift_struct_def() ::
    list({
        Tag :: pos_integer(),
        Requireness :: required | optional | undefined,
        Type :: thrift_struct_type(),
        Name :: atom(),
        Default :: any()
    }).

serialize(Type, Data) ->
    {ok, Trans} = thrift_membuffer_transport:new(),
    {ok, Proto} = new_protocol(Trans),
    case thrift_protocol:write(Proto, {Type, Data}) of
        {NewProto, ok} ->
            {_, {ok, Result}} = thrift_protocol:close_transport(NewProto),
            Result;
        {_NewProto, {error, Reason}} ->
            erlang:error({thrift, {protocol, Reason}})
    end.

-spec deserialize(thrift_type(), binary()) -> term().
deserialize(Type, Data) ->
    {ok, Trans} = thrift_membuffer_transport:new(Data),
    {ok, Proto} = new_protocol(Trans),
    case thrift_protocol:read(Proto, Type) of
        {_NewProto, {ok, Result}} ->
            Result;
        {_NewProto, {error, Reason}} ->
            erlang:error({thrift, {protocol, Reason}})
    end.

new_protocol(Trans) ->
    thrift_binary_protocol:new(Trans, [{strict_read, true}, {strict_write, true}]).
