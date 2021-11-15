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

-type event() :: ff_machine:timestamped_event(ff_wallet:event()).
-type aux_state() :: ff_machine:auxst().
-type call_args() :: term().
-type call_response() :: term().

-type data() ::
    aux_state()
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
    % TODO: Удалить после выкатки
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange, Context);
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = ff_wallet_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_wallet_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_wallet_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_wallet_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context0) ->
    {Event, Context1} = machinery_mg_schema_generic:unmarshal({event, Version}, EncodedChange, Context0),
    {ev, Timestamp, Change} = Event,
    {{ev, Timestamp, maybe_migrate(Change, Context1)}, Context1}.

-spec maybe_migrate(any(), context()) -> ff_wallet:event().
maybe_migrate(Event = {created, #{version := 2}}, _MigrateContext) ->
    Event;
maybe_migrate({created, Wallet = #{version := 1}}, MigrateContext) ->
    Context =
        case maps:get(machine_ref, MigrateContext, undefined) of
            undefined ->
                undefined;
            ID ->
                {ok, State} = ff_machine:get(ff_wallet, 'ff/wallet_v2', ID, {undefined, 0, forward}),
                maps:get(ctx, State, undefined)
        end,
    maybe_migrate(
        {created,
            genlib_map:compact(Wallet#{
                version => 2,
                metadata => ff_entity_context:try_get_legacy_metadata(Context)
            })},
        MigrateContext
    );
maybe_migrate({created, Wallet}, MigrateContext) ->
    Timestamp = maps:get(created_at, MigrateContext),
    CreatedAt = ff_codec:unmarshal(timestamp_ms, ff_codec:marshal(timestamp, Timestamp)),
    maybe_migrate(
        {created, Wallet#{
            version => 1,
            created_at => CreatedAt
        }},
        MigrateContext
    );
maybe_migrate(Ev, _MigrateContext) ->
    Ev.

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

% tests helpers

-spec make_legacy_context(map()) -> context().
make_legacy_context(Map) ->
    % drop mandatory attributes for backward compatible
    maps:without([machine_ref, machine_ns], Map).

-spec marshal(type(), value(data())) -> machinery_msgpack:t().
marshal(Type, Value) ->
    {Result, _Context} = marshal(Type, Value, make_legacy_context(#{})),
    Result.

-spec unmarshal(type(), machinery_msgpack:t()) -> value(data()).
unmarshal(Type, Value) ->
    {Result, _Context} = unmarshal(Type, Value, make_legacy_context(#{})),
    Result.

-spec created_v0_2_decoding_test() -> _.
created_v0_2_decoding_test() ->
    Change =
        {created, #{
            version => 2,
            name => <<"name">>,
            blocking => unblocked,
            created_at => 123
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"version">>} => {i, 1},
                    {str, <<"name">>} => {bin, <<"name">>},
                    {str, <<"blocking">>} => {str, <<"unblocked">>},
                    {str, <<"created_at">>} => {i, 123}
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

-spec created_account_v0_2_decoding_test() -> _.
created_account_v0_2_decoding_test() ->
    Change =
        {account,
            {created, #{
                id => <<"id">>,
                identity => <<"identity_id">>,
                currency => <<"RUB">>,
                accounter_account_id => 123
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
                        {str, <<"identity">>} => {bin, <<"identity_id">>},
                        {str, <<"currency">>} => {bin, <<"RUB">>},
                        {str, <<"accounter_account_id">>} => {i, 123}
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

-spec created_v2_decoding_test() -> _.
created_v2_decoding_test() ->
    Change =
        {created, #{
            version => 2,
            name => <<"name">>,
            blocking => unblocked,
            created_at => 123
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAQsAAQAAAARuYW1lC"
                "AAEAAAAAAsABgAAABgxOTcwLTAxLTAxVDAwOjAwOjAwLjEyM1oAAAA="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_account_v2_decoding_test() -> _.
created_account_v2_decoding_test() ->
    Change =
        {account,
            {created, #{
                id => <<"id">>,
                identity => <<"identity_id">>,
                currency => <<"RUB">>,
                accounter_account_id => 123
            }}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAgwAAQsAAwAAAAJpZAs"
                "AAQAAAAtpZGVudGl0eV9pZAwAAgsAAQAAAANSVUIACgAEAAAAAAAAAHsAAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-endif.
