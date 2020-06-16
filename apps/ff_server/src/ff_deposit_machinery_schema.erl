-module(ff_deposit_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_deposit_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([get_version/1]).
-export([marshal/2]).
-export([unmarshal/2]).

%% Constants

-define(CURRENT_EVENT_FORMAT_VERSION, undefined).

%% Internal types

-type type() :: machinery_mg_schema:t().
-type value(T) :: machinery_mg_schema:v(T).
-type value_type() :: machinery_mg_schema:vt().

-type event()   :: ff_machine:timestamped_event(ff_deposit:event()).
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

-spec marshal(type(), value(data())) ->
    machinery_msgpack:t().
marshal({event, Format}, TimestampedChange) ->
    marshal_event(Format, TimestampedChange);
marshal(T, V) when
    T =:= {args, init} orelse
        T =:= {args, call} orelse
        T =:= {args, repair} orelse
        T =:= {aux_state, undefined} orelse
        T =:= {response, call} orelse
        T =:= {response, {repair, success}} orelse
        T =:= {response, {repair, failure}}
    ->
    machinery_mg_schema_generic:marshal(T, V).

-spec unmarshal(type(), machinery_msgpack:t()) ->
    data().
unmarshal({event, FormatVersion}, EncodedChange) ->
    unmarshal_event(FormatVersion, EncodedChange);
unmarshal(T, V) when
    T =:= {args, init} orelse
        T =:= {args, call} orelse
        T =:= {args, repair} orelse
        T =:= {aux_state, undefined} orelse
        T =:= {response, call} orelse
        T =:= {response, {repair, success}} orelse
        T =:= {response, {repair, failure}}
    ->
    machinery_mg_schema_generic:unmarshal(T, V).

%% Internals

-spec marshal_event(machinery_mg_schema:version(), event()) ->
    machinery_msgpack:t().
marshal_event(undefined = Version, TimestampedChange) ->
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange);
marshal_event(1, TimestampedChange) ->
    ThriftChange = ff_deposit_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_deposit_thrift, 'TimestampedChange'}},
    {bin, ff_proto_utils:serialize(Type, ThriftChange)}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t()) ->
    event().
unmarshal_event(1, EncodedChange) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_deposit_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    ff_deposit_codec:unmarshal(timestamped_change, ThriftChange);
unmarshal_event(undefined = Version, EncodedChange) ->
    {ev, Timestamp, Change} = machinery_mg_schema_generic:unmarshal({event, Version}, EncodedChange),
    {ev, Timestamp, maybe_migrate(Change)}.

-spec maybe_migrate(any()) ->
    ff_deposit:event().
maybe_migrate({created, #{version := 3}} = Event) ->
    Event;
% Other events
maybe_migrate(Ev) ->
    Ev.
