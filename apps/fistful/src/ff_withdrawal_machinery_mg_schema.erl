-module(ff_withdrawal_machinery_mg_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

-define(CONTENT_TYPE, {struct, struct, {mg_proto_state_processing_thrift, 'Content'}}).


-type t()  :: machinery_mg_schema:t().
-type v(T) :: machinery_mg_schema:v(T).

-type changes() :: [ff_withdrawal:event()].
-type context() :: ff_entity_context:context().
-type event()   :: ff_machine:timestamped_event(ff_withdrawal:event()).
-type aux_state() :: ff_machine:auxst().

-type withdrawal_data() :: {changes(), context()} | aux_state() | event().

-spec marshal(t(), v(withdrawal_data())) ->
    machinery_msgpack:t().
marshal(event, {ev, Ts, Change}) ->
    ThriftChange = ff_withdrawal_codec:marshal(change, Change),
    TimeStamp = ff_codec:marshal(timestamp, Ts),
    Data = #wthd_NoIdEvent{change = ThriftChange, occured_at = TimeStamp},
    Type = {struct, union, {ff_proto_withdrawal_thrift, 'NoIdEvent'}},
    Version = get_withdrawal_version(Change),
    Bin = wrap(Type, Data, Version),
    {bin, Bin};
marshal(T, V) ->
    machinery_mg_schema_generic:marshal(T, V).

-spec unmarshal(t(), machinery_msgpack:t()) ->
    withdrawal_data().
unmarshal(event, V) ->
    Type = {struct, union, {ff_proto_withdrawal_thrift, 'NoIdEvent'}},
    #{
        data := {TimeStamp, Change},
        version := Version
    } = unwrap(Type, V),
    {ev, ff_codec:unmarshal(timestamp, TimeStamp), add_version(ff_withdrawal_codec:unmarshal(change, Change), Version)};

unmarshal(T, V) ->
    machinery_mg_schema_generic:unmarshal(T, V).

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
