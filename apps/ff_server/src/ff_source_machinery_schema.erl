-module(ff_source_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_source_thrift.hrl").
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

-type event() :: ff_machine:timestamped_event(ff_source:event()).
-type aux_state() :: ff_machine:auxst().
-type call_args() :: term().
-type call_response() :: term().

-type data() ::
    aux_state()
    | event()
    | call_args()
    | call_response().

%% machinery_mg_schema callbacks

-spec get_version(value_type()) -> machinery_mg_schema:version().
get_version(event) ->
    ?CURRENT_EVENT_FORMAT_VERSION;
get_version(aux_state) ->
    undefined.

-spec marshal(type(), value(data()), context()) -> {machinery_msgpack:t(), context()}.
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

-spec unmarshal(type(), machinery_msgpack:t(), context()) -> {data(), context()}.
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

-spec marshal_event(machinery_mg_schema:version(), event(), context()) -> {machinery_msgpack:t(), context()}.
marshal_event(undefined = Version, TimestampedChange, Context) ->
    % @TODO: Remove after migration
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange, Context);
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = ff_source_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_source_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_source_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_source_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context0) ->
    {Event, Context1} = machinery_mg_schema_generic:unmarshal({event, Version}, EncodedChange, Context0),
    {maybe_migrate(Event), Context1}.

-spec maybe_migrate(any()) -> event().
maybe_migrate({ev, Timestamp, Change0}) ->
    Change = ff_source:maybe_migrate(Change0, #{timestamp => Timestamp}),
    {ev, Timestamp, Change}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec created_3_undef_3_1_decoding_test() -> _.

created_3_undef_3_1_decoding_test() ->
    Resource = #{
        type => internal,
        details => <<"details">>
    },
    Source = #{
        version => 3,
        resource => Resource,
        name => <<"name">>,
        created_at => 1590434350293,
        external_id => <<"external_id">>,
        metadata => #{}
    },
    Change = {created, Source},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    ResourceMsgpack =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"type">>} => {str, <<"internal">>},
                {str, <<"details">>} => {bin, <<"details">>}
            }}
        ]},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"version">>} => {i, 3},
                    {str, <<"resource">>} => ResourceMsgpack,
                    {str, <<"name">>} => {bin, <<"name">>},
                    {str, <<"created_at">>} => {i, 1590434350293},
                    {str, <<"external_id">>} => {bin, <<"external_id">>},
                    {str, <<"metadata">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{}}
                        ]}
                }}
            ]}
        ]},
    LegacyEvent =
        {arr, [
            {str, <<"tup">>},
            {str, <<"ev">>},
            {arr, [
                {str, <<"tup">>},
                {arr, [
                    {str, <<"tup">>},
                    {arr, [{str, <<"tup">>}, {i, 2020}, {i, 5}, {i, 25}]},
                    {arr, [{str, <<"tup">>}, {i, 19}, {i, 19}, {i, 10}]}
                ]},
                {i, 293305}
            ]},
            LegacyChange
        ]},

    DecodedLegacy = unmarshal({event, undefined}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_1_undef_3_1_decoding_test() -> _.
created_1_undef_3_1_decoding_test() ->
    Resource = #{
        type => internal,
        details => <<"details">>
    },
    Source = #{
        version => 3,
        resource => Resource,
        name => <<"name">>,
        created_at => 1590434350293,
        external_id => <<"external_id">>
    },
    Change = {created, Source},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    ResourceMsgpack =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"type">>} => {str, <<"internal">>},
                {str, <<"details">>} => {bin, <<"details">>}
            }}
        ]},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"version">>} => {i, 1},
                    {str, <<"resource">>} => ResourceMsgpack,
                    {str, <<"name">>} => {bin, <<"name">>},
                    {str, <<"external_id">>} => {bin, <<"external_id">>}
                }}
            ]}
        ]},
    LegacyEvent =
        {arr, [
            {str, <<"tup">>},
            {str, <<"ev">>},
            {arr, [
                {str, <<"tup">>},
                {arr, [
                    {str, <<"tup">>},
                    {arr, [{str, <<"tup">>}, {i, 2020}, {i, 5}, {i, 25}]},
                    {arr, [{str, <<"tup">>}, {i, 19}, {i, 19}, {i, 10}]}
                ]},
                {i, 293305}
            ]},
            LegacyChange
        ]},

    DecodedLegacy = unmarshal({event, undefined}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec account_undef_1_decoding_test() -> _.
account_undef_1_decoding_test() ->
    Change =
        {account,
            {created, #{
                id => <<"id">>,
                identity => <<"identity">>,
                currency => <<"USD">>,
                accounter_account_id => 1
            }}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"account">>},
            {arr, [
                {str, <<"tup">>},
                {str, <<"created">>},
                {arr, [
                    {str, <<"map">>},
                    {obj, #{
                        {str, <<"id">>} => {bin, <<"id">>},
                        {str, <<"identity">>} => {bin, <<"identity">>},
                        {str, <<"currency">>} => {bin, <<"USD">>},
                        {str, <<"accounter_account_id">>} => {i, 1}
                    }}
                ]}
            ]}
        ]},
    LegacyEvent =
        {arr, [
            {str, <<"tup">>},
            {str, <<"ev">>},
            {arr, [
                {str, <<"tup">>},
                {arr, [
                    {str, <<"tup">>},
                    {arr, [{str, <<"tup">>}, {i, 2020}, {i, 5}, {i, 25}]},
                    {arr, [{str, <<"tup">>}, {i, 19}, {i, 19}, {i, 10}]}
                ]},
                {i, 293305}
            ]},
            LegacyChange
        ]},

    DecodedLegacy = unmarshal({event, undefined}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec status_undef_1_decoding_test() -> _.
status_undef_1_decoding_test() ->
    Change = {status_changed, unauthorized},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"status_changed">>},
            {str, <<"unauthorized">>}
        ]},
    LegacyEvent =
        {arr, [
            {str, <<"tup">>},
            {str, <<"ev">>},
            {arr, [
                {str, <<"tup">>},
                {arr, [
                    {str, <<"tup">>},
                    {arr, [{str, <<"tup">>}, {i, 2020}, {i, 5}, {i, 25}]},
                    {arr, [{str, <<"tup">>}, {i, 19}, {i, 19}, {i, 10}]}
                ]},
                {i, 293305}
            ]},
            LegacyChange
        ]},

    DecodedLegacy = unmarshal({event, undefined}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_1_decoding_test() -> _.
created_1_decoding_test() ->
    Resource = #{
        type => internal,
        details => <<"details">>
    },
    Source = #{
        version => 3,
        resource => Resource,
        name => <<"name">>,
        created_at => 1590434350293,
        external_id => <<"external_id">>
    },
    Change = {created, Source},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAQsAAQAAAARuYW1lDA"
                "ACDAABCwABAAAAB2RldGFpbHMAAAsAAwAAAAtleHRlcm5hbF9pZAsABgAAABgyMDIwLTA1"
                "LTI1VDE5OjE5OjEwLjI5M1oAAAA="
            >>)},

    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec account_1_decoding_test() -> _.
account_1_decoding_test() ->
    Change =
        {account,
            {created, #{
                id => <<"id">>,
                identity => <<"identity">>,
                currency => <<"USD">>,
                accounter_account_id => 1
            }}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAgwAAQsAAwAAAAJpZA"
                "sAAQAAAAhpZGVudGl0eQwAAgsAAQAAAANVU0QACgAEAAAAAAAAAAEAAAAA"
            >>)},

    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec status_1_decoding_test() -> _.
status_1_decoding_test() ->
    Change = {status_changed, unauthorized},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwwAAQwAAgAAAAAA"
            >>)},

    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec make_legacy_context(map()) -> context().
make_legacy_context(Map) ->
    % drop mandatory attributes for backward compatible
    maps:without([machine_ref, machine_ns], Map).

-spec marshal(type(), value(data())) -> machinery_msgpack:t().
marshal(Type, Value) ->
    element(1, marshal(Type, Value, make_legacy_context(#{}))).

-spec unmarshal(type(), machinery_msgpack:t()) -> value(data()).
unmarshal(Type, Value) ->
    element(1, unmarshal(Type, Value, make_legacy_context(#{}))).

-endif.
