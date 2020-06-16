-module(ff_wallet_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([get_version/1]).
-export([marshal/3]).
-export([unmarshal/3]).

%% Constants
-define(CURRENT_EVENT_FORMAT_VERSION, 1).

%% Internal types

-type type() :: machinery_mg_schema:t().
-type value(T) :: machinery_mg_schema:v(T).
-type value_type() :: machinery_mg_schema:vt().
-type context() :: machinery_mg_schema:context().

-type event()   :: ff_machine:timestamped_event(p2p_transfer:event()).
-type aux_state() :: ff_machine:auxst().
-type call_args() :: term().
-type call_response() :: term().

-type data() ::
    aux_state() |
    event() |
    call_args() |
    call_response().

%% machinery_mg_schema callbacks

-spec get_version(value_type()) ->
    machinery_mg_schema:version().
get_version(event) ->
    ?CURRENT_EVENT_FORMAT_VERSION;
get_version(aux_state) ->
    undefined.

-spec marshal(type(), value(data()), context()) ->
    {machinery_msgpack:t(), context()}.
marshal({event, Format}, TimestampedChange, Context) ->
    marshal_event(Format, TimestampedChange, Context);
marshal(T, V, C) when
    T =:= {args, init} orelse
    T =:= {args, call} orelse
    T =:= {args, repair} orelse
    T =:= {aux_state, undefined} orelse
    T =:= {response, call} orelse
    T =:= {response, {repair, success}} orelse
    T =:= {response, {repair, failure}}
->
    machinery_mg_schema_generic:marshal(T, V, C).

-spec unmarshal(type(), machinery_msgpack:t(), context()) ->
    {data(), context()}.
unmarshal({event, FormatVersion}, EncodedChange, Context) ->
    unmarshal_event(FormatVersion, EncodedChange, Context);
unmarshal(T, V, C) when
    T =:= {args, init} orelse
    T =:= {args, call} orelse
    T =:= {args, repair} orelse
    T =:= {aux_state, undefined} orelse
    T =:= {response, call} orelse
    T =:= {response, {repair, success}} orelse
    T =:= {response, {repair, failure}}
->
    machinery_mg_schema_generic:unmarshal(T, V, C).

%% Internals

-spec marshal_event(machinery_mg_schema:version(), event(), context()) ->
    {machinery_msgpack:t(), context()}.
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = ff_wallet_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_wallet_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) ->
    {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_wallet_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_wallet_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context0) ->
    {Event, Context1} = machinery_mg_schema_generic:unmarshal({event, Version}, EncodedChange, Context0),
    {ev, Timestamp, Change} = Event,
    {{ev, Timestamp, maybe_migrate(Change, Context1)}, Context1}.

-spec maybe_migrate(any(), context()) ->
    ff_wallet:event().
maybe_migrate(Event = {created, #{version := 2}}, _MigrateContext) ->
    Event;
maybe_migrate({created, Wallet = #{version := 1}}, MigrateContext) ->
    Context = case maps:get(machine_ref, MigrateContext, undefined) of
        undefined ->
            undefined;
        ID ->
            {ok, State} = ff_machine:get(ff_wallet, 'ff/wallet_v2', ID, {undefined, 0, forward}),
            maps:get(ctx, State, undefined)
    end,
    maybe_migrate({created, genlib_map:compact(Wallet#{
        version => 2,
        metadata => ff_entity_context:try_get_legacy_metadata(Context)
    })}, MigrateContext);
maybe_migrate({created, Wallet}, MigrateContext) ->
    Timestamp = maps:get(timestamp, MigrateContext),
    CreatedAt = ff_codec:unmarshal(timestamp_ms, ff_codec:marshal(timestamp, Timestamp)),
    maybe_migrate({created, Wallet#{
        version => 1,
        created_at => CreatedAt
    }}, MigrateContext);
maybe_migrate(Ev, _MigrateContext) ->
    Ev.
