-module(ff_withdrawal_machinery_mg_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

-define(CONTENT_TYPE, {struct, struct, {mg_proto_state_processing_thrift, 'Content'}}).
-define(VERSION, 1).

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
    Bin = wrap(Type, Data),
    {bin, Bin};
marshal(T, V) ->
    machinery_mg_schema_generic:marshal(T, V).

-spec unmarshal(t(), machinery_msgpack:t()) ->
    withdrawal_data().
unmarshal(event, V) ->
    Type = {struct, union, {ff_proto_withdrawal_thrift, 'NoIdEvent'}},
    {TimeStamp, Change} = unwrap(Type, V),
    {ev, ff_codec:unmarshal(timestamp, TimeStamp), add_version(ff_withdrawal_codec:unmarshal(change, Change), 2)};

unmarshal(T, V) ->
    machinery_mg_schema_generic:unmarshal(T, V).

% TODO: migrations
add_version({created, Withdrawal}, Version) ->
    {created, Withdrawal#{version => Version}};
add_version(Change, _) ->
    Change.

wrap(Type, Data) ->
    Content = #mg_stateproc_Content{
        data = {bin, ff_proto_utils:serialize(Type, Data)},
        format_version = ?VERSION
    },
    ff_proto_utils:serialize(?CONTENT_TYPE, Content).

unwrap(Type, Bin) ->
    #mg_stateproc_Content{
        data = Data,
        format_version = _Version
    } = ff_proto_utils:deserialize(?CONTENT_TYPE, Bin),
    ff_proto_utils:deserialize(Type, Data).
