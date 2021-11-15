-module(ff_w2w_transfer_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_w2w_transfer_thrift.hrl").
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

-type event() :: ff_machine:timestamped_event(w2w_transfer:event()).
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
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = ff_w2w_transfer_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_w2w_transfer_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_w2w_transfer_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_w2w_transfer_codec:unmarshal(timestamped_change, ThriftChange), Context}.

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

-spec created_v1_decoding_test() -> _.
created_v1_decoding_test() ->
    W2WTransfer = #{
        version => 1,
        id => <<"transfer">>,
        body => {123, <<"RUB">>},
        wallet_from_id => <<"WalletFromID">>,
        wallet_to_id => <<"WalletToID">>,
        created_at => 1590426777985,
        domain_revision => 123,
        party_revision => 321,
        external_id => <<"external_id">>
    },
    Change = {created, W2WTransfer},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAQwAAQsAAQ"
                "AAAAh0cmFuc2ZlcgsAAgAAAAxXYWxsZXRGcm9tSUQLAAMAAAAKV2FsbGV0VG9J"
                "RAwABAoAAQAAAAAAAAB7DAACCwABAAAAA1JVQgAACwAFAAAAGDIwMjAtMDUtMj"
                "VUMTc6MTI6NTcuOTg1WgoABgAAAAAAAAB7CgAHAAAAAAAAAUELAAkAAAALZXh0"
                "ZXJuYWxfaWQAAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec status_changes_v1_decoding_test() -> _.
status_changes_v1_decoding_test() ->
    Change = {status_changed, succeeded},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAgwAAQwAAgAAAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec limit_check_v1_decoding_test() -> _.
limit_check_v1_decoding_test() ->
    Change = {limit_check, {wallet_sender, ok}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABQwAAQwAAQwAAQAAAAAAAA=="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec p_transfer_created_v1_decoding_test() -> _.
p_transfer_created_v1_decoding_test() ->
    Change =
        {p_transfer,
            {created, #{
                id => <<"id">>,
                final_cash_flow => #{postings => []}
            }}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwwAAQwAAQwAAQ"
                "sAAgAAAAJpZAwAAQ8AAQwAAAAAAAAAAAAAAA=="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec p_transfer_clock_updated_v1_decoding_test() -> _.
p_transfer_clock_updated_v1_decoding_test() ->
    Change =
        {p_transfer,
            {clock_updated, #{
                version => 1,
                type => latest
            }}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwwAAQwAAwwAAQwAAQAAAAAAAAA="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec p_transfer_status_changed_v1_decoding_test() -> _.
p_transfer_status_changed_v1_decoding_test() ->
    Change = {p_transfer, {status_changed, committed}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwwAAQwAAgwAAQwAAwAAAAAAAAA="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec adjustment_created_v1_decoding_test() -> _.
adjustment_created_v1_decoding_test() ->
    Change =
        {adjustment, #{
            id => <<"id">>,
            payload =>
                {created, #{
                    version => 1,
                    id => <<"id">>,
                    status => succeeded,
                    created_at => 1590426777985,
                    changes_plan => #{},
                    domain_revision => 123,
                    party_revision => 321,
                    operation_timestamp => 1590426777985,
                    external_id => <<"external_id">>
                }}
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABAsAAQAAAAJpZAwAAgwAAQwAAQs"
                "AAQAAAAJpZAwAAgwAAgAADAADAAsABAAAABgyMDIwLTA1LTI1VDE3OjEyOjU3Ljk4NVoKAAUAAAAAAA"
                "AAewoABgAAAAAAAAFBCwAHAAAAC2V4dGVybmFsX2lkCwAIAAAAGDIwMjAtMDUtMjVUMTc6MTI6NTcuO"
                "Tg1WgAAAAAAAA=="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-endif.
