-module(ff_withdrawal_machinery_mg_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).
-export([get_version/1]).

-define(CONTENT_TYPE, {struct, struct, {mg_proto_state_processing_thrift, 'Content'}}).

-spec get_version(_) -> 2.
get_version(_) ->
    2.

-spec marshal(_, _) -> _.
marshal(T, V) ->
    do_marshal(T, V).

do_marshal({args, init}, {Changes, Ctx}) ->
    Type = {struct, union, {ff_proto_withdrawal_thrift, 'InitArgs'}},
    ThriftChanges = lists:map(fun(Change) -> ff_withdrawal_codec:marshal(change, Change) end, Changes),
    Data = #wthd_InitArgs{changes = ThriftChanges, context = ff_codec:marshal(context, Ctx)},
    Version = get_withdrawal_version(Changes),
    Bin = wrap(Type, Data, Version),
    {bin, Bin};
do_marshal(aux_state, #{ctx := AuxState}) ->
    Type = {map, string, {struct, union, {ff_proto_msgpack_thrift, 'Value'}}},
    Bin = wrap(Type, ff_codec:marshal(context, AuxState), undefined),
    {bin, Bin};
do_marshal(event, {ev, Ts, Change}) ->
    ThriftChange = ff_withdrawal_codec:marshal(change, Change),
    TimeStamp = ff_codec:marshal(timestamp, Ts),
    Data = #wthd_NoIdEvent{change = ThriftChange, occured_at = TimeStamp},
    Type = {struct, union, {ff_proto_withdrawal_thrift, 'NoIdEvent'}},
    Version = get_withdrawal_version(Change),
    Bin = wrap(Type, Data, Version),
    {bin, Bin}.

-spec unmarshal(_, _) -> _.
unmarshal(T, V) ->
    do_unmarshal(T, V).

do_unmarshal({args, init}, V) ->
    Type = {struct, struct, {ff_proto_withdrawal_thrift, 'InitArgs'}},
    #{
        data := #wthd_InitArgs{
            changes = ThriftChanges,
            context = ThriftContext
        },
        version := Version
    } =  unwrap(Type, V),
    Changes0 = lists:map(fun(Change) -> ff_withdrawal_codec:unmarshal(change, Change) end, ThriftChanges),
    Changes = add_version(Changes0, Version),
    {Changes, ff_codec:unmarshal(context, ThriftContext)};
do_unmarshal(aux_state, {bin, <<>>}) -> % TODO: discover where it comes from
    #{ctx => #{}};
do_unmarshal(aux_state, V) ->
    Type = {map, string, {struct, union, {ff_proto_msgpack_thrift, 'Value'}}},
    #{data := Aux} = unwrap(Type, V),
    #{ctx => ff_codec:unmarshal(context, Aux)};
do_unmarshal(event, V) ->
    Type = {struct, union, {ff_proto_withdrawal_thrift, 'NoIdEvent'}},
    #{
        data := {TimeStamp, Change},
        version := Version
    } = unwrap(Type, V),
    {ev, ff_codec:unmarshal(timestamp, TimeStamp), add_version(ff_withdrawal_codec:unmarshal(change, Change), Version)}.


add_version(Changes, Version) when is_list(Changes) ->
    F = fun(Change) when is_tuple(Change) -> add_version(Change, Version) end,
    lists:map(F, Changes);

add_version({created, Withdrawal}, Version) ->
    {created, Withdrawal#{version => Version}};
add_version(Change, _) ->
    Change.

get_withdrawal_version([{created, _} = Change | _]) ->
    get_withdrawal_version(Change);
get_withdrawal_version({created, Withdrawal}) ->
    maps:get(version, Withdrawal, undefined); % 1?
get_withdrawal_version(_) ->
    undefined. % 1?


wrap(Type, Data, Version) ->
    Content = #mg_stateproc_Content{
        data = {bin, ff_proto_utils:serialize(Type, Data)},
        format_version = Version
    },
    ff_proto_utils:serialize(?CONTENT_TYPE, Content).

unwrap(Type, Bin) ->
    #mg_stateproc_Content{
        format_version = Version,
        data = Data
    } = ff_proto_utils:deserialize(?CONTENT_TYPE, Bin),
    #{data => ff_proto_utils:deserialize(Type, Data), version => Version}.
