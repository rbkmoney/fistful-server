-module(ff_withdrawal_machinery_mg_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).
-export([get_version/1]).

-spec get_version(_) -> 2.
get_version(_) ->
    2.

-spec marshal(_, _) -> _.
marshal(T, V) ->
    ct:log("Marshal schema: ~p, Value: ~p", [T,V]),
    do_marshal(T, V).

do_marshal({args, init}, {Changes, Ctx}) ->
    Type = {struct, union, {ff_proto_withdrawal_thrift, 'InitArgs'}},
    ThriftChanges = lists:map(fun(Change) -> ff_withdrawal_codec:marshal(change, Change) end, Changes),
    Data = #wthd_InitArgs{changes = ThriftChanges, context = ff_codec:marshal(context, Ctx)},
    Bin = ff_proto_utils:serialize(Type, Data),
    % Data = #mg_stateproc_Content{
    %     format_version = 2,
    %     data = {bin, Bin}
    % },
    % Bin1 = ff_proto_utils:serialize({struct, struct, {mg_proto_state_processing_thrift, 'Content'}}, Data),
    {bin, Bin};
do_marshal(aux_state, #{ctx := AuxState}) ->
    Type = {map, string, {struct, union, {ff_proto_msgpack_thrift, 'Value'}}},
    Bin = ff_proto_utils:serialize(Type, ff_codec:marshal(context, AuxState)),
    {bin, Bin};
do_marshal(event, {ev, Ts, Change}) ->
    ThriftChange = ff_withdrawal_codec:marshal(change, Change),
    TimeStamp = ff_codec:marshal(timestamp, Ts),
    Data = #wthd_NoIdEvent{change = ThriftChange, occured_at = TimeStamp},
    Type = {struct, union, {ff_proto_withdrawal_thrift, 'NoIdEvent'}},
    Bin = ff_proto_utils:serialize(Type, Data),
    {bin, Bin}.

-spec unmarshal(_, _) -> _.
unmarshal(T, V) ->
    ct:log("Unmarshal schema: ~p, Value: ~p", [T, V]),
    do_unmarshal(T, V).

do_unmarshal({args, init}, V) ->
    Type = {struct, struct}
    % Type = {struct, union, {ff_proto_withdrawal_thrift, 'InitArgs'}},
    % {ThriftChanges, ThriftContext} = ff_proto_utils:deserialize(Type, V),
    Changes = lists:map(fun(Change) -> ff_withdrawal_codec:unmarshal(change, Change) end, ThriftChanges),
    {Changes, ff_codec:unmarshal(context, ThriftContext)};
do_unmarshal(aux_state, {bin, <<>>}) ->
    #{ctx => #{}};
do_unmarshal(aux_state, V) ->
    Type = {map, string, {struct, union, {ff_proto_msgpack_thrift, 'Value'}}},
    ThriftAux = ff_proto_utils:deserialize(Type, V),
    #{ctx => ff_codec:unmarshal(context, ThriftAux)};
do_unmarshal(event, V) ->
    Type = {struct, union, {ff_proto_withdrawal_thrift, 'NoIdEvent'}},
    {TimeStamp, ThriftChange} = ff_proto_utils:deserialize(Type, V),
    {ev, ff_codec:unmarshal(timestamp, TimeStamp), ff_withdrawal_codec:unmarshal(change, ThriftChange)}.
