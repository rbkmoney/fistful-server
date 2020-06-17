-module(ff_destination_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

%% Constants

-define(CURRENT_EVENT_FORMAT_VERSION, 1).

-export([get_version/1]).
-export([marshal/3]).
-export([unmarshal/3]).

%% Internal types

-type type() :: machinery_mg_schema:t().
-type value(T) :: machinery_mg_schema:v(T).
-type value_type() :: machinery_mg_schema:vt().
-type context() :: machinery_mg_schema:context().

-type event()   :: ff_machine:timestamped_event(ff_destination:event()).
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
    ThriftChange = ff_destination_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_destination_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) ->
    {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_destination_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_destination_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context0) ->
    {Event, Context1} = machinery_mg_schema_generic:unmarshal({event, Version}, EncodedChange, Context0),
    {maybe_migrate(Event), Context1}.

-spec maybe_migrate(any()) ->
    event().
maybe_migrate({ev, Timestamp, Change}) ->
    {ev, Timestamp, ff_instrument:maybe_migrate(Change, #{timestamp => Timestamp})}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec marshal(type(), value(data())) ->
    machinery_msgpack:t().
marshal(Type, Value) ->
    {Result, _Context} = marshal(Type, Value, #{}),
    Result.

-spec unmarshal(type(), machinery_msgpack:t()) ->
    data().
unmarshal(Type, Value) ->
    {Result, _Context} = unmarshal(Type, Value, #{}),
    Result.

-spec created_v0_0_decoding_test() -> _.
created_v0_0_decoding_test() ->
    Resource = {crypto_wallet, #{crypto_wallet => #{
        id => <<"kek">>,
        currency => {bitcoin, #{}}
    }}},
    Destination = #{
        version     => 3,
        resource    => Resource,
        name        => <<"name">>,
        created_at  => 1590434350293,
        external_id => <<"external_id">>
    },
    Change = {created, Destination},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyResource = {arr, [
        {str, <<"tup">>},
        {str, <<"crypto_wallet">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"currency">>} => {arr, [
                    {str, <<"tup">>},
                    {str, <<"bitcoin">>},
                    {arr, [{str, <<"map">>}, {obj, #{}}]}
                ]},
                {str, <<"id">>} => {bin, <<"kek">>}
            }}
        ]}
    ]},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"created">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"resource">>} => LegacyResource,
                {str, <<"name">>} => {bin, <<"name">>},
                {str, <<"external_id">>} => {bin, <<"external_id">>}
            }}
        ]}
    ]},
    LegacyEvent = {arr, [
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

-spec created_v0_1_decoding_test() -> _.
created_v0_1_decoding_test() ->
    Resource = {bank_card, #{bank_card => #{
        token => <<"token">>,
        bin_data_id => {binary, <<"ebin">>}
    }}},
    Destination = #{
        version     => 3,
        resource    => Resource,
        name        => <<"name">>,
        created_at  => 1590434350293,
        external_id => <<"external_id">>
    },
    Change = {created, Destination},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyResource = {arr, [
        {str, <<"tup">>},
        {str, <<"bank_card">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"bank_card">>} =>
                {arr, [
                    {str, <<"map">>},
                    {obj, #{
                        {str, <<"bin_data_id">>} => {arr, [
                            {str, <<"tup">>},
                            {str, <<"binary">>},
                            {bin, <<"ebin">>}
                        ]},
                        {str, <<"token">>} => {bin, <<"token">>}
                    }}
                ]}
            }}
        ]}
    ]},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"created">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"version">>} => {i, 1},
                {str, <<"resource">>} => LegacyResource,
                {str, <<"name">>} => {bin, <<"name">>},
                {str, <<"created_at">>} => {i, 1590434350293},
                {str, <<"external_id">>} => {bin, <<"external_id">>}
            }}
        ]}
    ]},
    LegacyEvent = {arr, [
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

-spec account_v0_decoding_test() -> _.
account_v0_decoding_test() ->
    Change = {account, {created, #{
        id => <<"1">>,
        identity => <<"Solo">>,
        currency => <<"USD">>,
        accounter_account_id => 322
    }}},

    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"account">>},
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"id">>} => {bin, <<"1">>},
                    {str, <<"identity">>} => {bin, <<"Solo">>},
                    {str, <<"currency">>} => {bin, <<"USD">>},
                    {str, <<"accounter_account_id">>} => {i, 322}
                }}
            ]}
        ]}
    ]},

    LegacyEvent = {arr, [
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

-spec status_v0_decoding_test() -> _.
status_v0_decoding_test() ->
    Event = {
        ev,
        {{{2020, 5, 25}, {19, 19, 10}}, 293305},
        {status_changed, unauthorized}
    },

    LegacyEvent = {arr, [
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
        {arr, [
            {str, <<"tup">>},
            {str, <<"status_changed">>},
            {str, <<"unauthorized">>}
        ]}
    ]},

    DecodedLegacy = unmarshal({event, undefined}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_v1_3_decoding_test() -> _.
created_v1_3_decoding_test() ->
    Resource = {crypto_wallet, #{crypto_wallet => #{
        id => <<"kek">>,
        currency => {bitcoin, #{}}
    }}},
    Destination = #{
        version     => 3,
        resource    => Resource,
        name        => <<"name">>,
        created_at  => 1590434350293,
        external_id => <<"external_id">>
    },
    Change = {created, Destination},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyEvent = {bin, base64:decode(<<
        "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAQsAAQAAAARuYW1lDAA"
        "CDAACDAABCwABAAAAA2tlawwAAwwAAQAACAACAAAAAAAAAAsAAwAAAAtleHRlcm5hbF9pZA"
        "sABwAAABgyMDIwLTA1LTI1VDE5OjE5OjEwLjI5M1oAAAA="
    >>)},

    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec account_v1_decoding_test() -> _.
account_v1_decoding_test() ->
    Change = {account, {created, #{
        id => <<"1">>,
        identity => <<"Solo">>,
        currency => <<"USD">>,
        accounter_account_id => 322
    }}},

    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyEvent = {bin, base64:decode(<<
        "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAgwAAQsAAwAAAAExCw"
        "ABAAAABFNvbG8MAAILAAEAAAADVVNEAAoABAAAAAAAAAFCAAAAAA=="
    >>)},

    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec status_v1_decoding_test() -> _.
status_v1_decoding_test() ->
    Event = {
        ev,
        {{{2020, 5, 25}, {19, 19, 10}}, 293305},
        {status_changed, unauthorized}
    },

    LegacyEvent = {bin, base64:decode(<<
        "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwwAAQwAAgAAAAAA"
    >>)},

    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-endif.
