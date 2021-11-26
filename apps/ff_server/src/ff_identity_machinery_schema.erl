-module(ff_identity_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([get_version/1]).
-export([marshal/3]).
-export([unmarshal/3]).

%% Constants

-define(CURRENT_EVENT_FORMAT_VERSION, 2).

%% Internal types

-type type() :: machinery_mg_schema:t().
-type value(T) :: machinery_mg_schema:v(T).
-type value_type() :: machinery_mg_schema:vt().

-type event() :: ff_machine:timestamped_event(ff_identity:event()).
-type aux_state() :: ff_machine:auxst().
-type call_args() :: term().
-type call_response() :: term().
-type context() :: machinery_mg_schema:context().

-type legacy_change() :: any().

-type timestamped_change() :: ff_proto_identity_thrift:'TimestampedChange'().
-type thrift_change() :: ff_proto_identity_thrift:'Change'().

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
unmarshal({aux_state, undefined} = T, V, C0) ->
    {AuxState, C1} = machinery_mg_schema_generic:unmarshal(T, V, C0),
    {AuxState, C1#{ctx => get_aux_state_ctx(AuxState)}};
unmarshal(T, V, C) when
    T =:= {args, init} orelse
        T =:= {args, call} orelse
        T =:= {args, repair} orelse
        T =:= {response, call} orelse
        T =:= {response, {repair, success}} orelse
        T =:= {response, {repair, failure}}
->
    machinery_mg_schema_generic:unmarshal(T, V, C).

%% Internals

-spec marshal_event(machinery_mg_schema:version(), event(), context()) -> {machinery_msgpack:t(), context()}.
marshal_event(undefined = Version, TimestampedChange, Context) ->
    % TODO: Remove this clause after MSPF-561 finish
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange, Context);
marshal_event(Version, TimestampedChange, Context) when Version =:= 1; Version =:= 2 ->
    ThriftChange = ff_identity_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_identity_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {event(), context()}.
unmarshal_event(2, EncodedChange, Context) ->
    ThriftChange = unmashal_thrift_change(EncodedChange),
    {ff_identity_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(1, EncodedChange, Context) ->
    ThriftChange = unmashal_thrift_change(EncodedChange),
    MigratedChange = ThriftChange#idnt_TimestampedChange{
        change = maybe_migrate_thrift_change(ThriftChange#idnt_TimestampedChange.change, Context)
    },
    {ff_identity_codec:unmarshal(timestamped_change, MigratedChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context) ->
    {{ev, Timestamp, Change}, Context1} = machinery_mg_schema_generic:unmarshal(
        {event, Version},
        EncodedChange,
        Context
    ),
    {{ev, Timestamp, maybe_migrate_change(Change, Context1)}, Context1}.

-spec unmashal_thrift_change(machinery_msgpack:t()) -> timestamped_change().
unmashal_thrift_change(EncodedChange) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_identity_thrift, 'TimestampedChange'}},
    ff_proto_utils:deserialize(Type, EncodedThriftChange).

-spec maybe_migrate_thrift_change(thrift_change(), context()) -> thrift_change().
maybe_migrate_thrift_change({created, #idnt_Identity{name = undefined} = Identity}, MigrateContext) ->
    Context = fetch_entity_context(Identity#idnt_Identity.id, MigrateContext),
    {created, Identity#idnt_Identity{name = get_legacy_name(Context)}};
maybe_migrate_thrift_change(Change, _MigrateContext) ->
    Change.

-spec maybe_migrate_change(legacy_change(), context()) -> ff_identity:event().
maybe_migrate_change(Event = {created, #{version := 2, name := _}}, _MigrateContext) ->
    Event;
maybe_migrate_change({created, Identity = #{version := 2, id := ID}}, MigrateContext) ->
    Context = fetch_entity_context(ID, MigrateContext),
    maybe_migrate_change(
        {created,
            genlib_map:compact(Identity#{
                name => get_legacy_name(Context)
            })},
        MigrateContext
    );
maybe_migrate_change({created, Identity = #{version := 1, id := ID}}, MigrateContext) ->
    Context = fetch_entity_context(ID, MigrateContext),
    maybe_migrate_change(
        {created,
            genlib_map:compact(Identity#{
                version => 2,
                metadata => ff_entity_context:try_get_legacy_metadata(Context)
            })},
        MigrateContext
    );
maybe_migrate_change({created, Identity = #{created_at := _CreatedAt}}, MigrateContext) ->
    maybe_migrate_change(
        {created, Identity#{
            version => 1
        }},
        MigrateContext
    );
maybe_migrate_change({created, Identity}, MigrateContext) ->
    Timestamp = maps:get(created_at, MigrateContext),
    CreatedAt = ff_codec:unmarshal(timestamp_ms, ff_codec:marshal(timestamp, Timestamp)),
    maybe_migrate_change(
        {created, Identity#{
            created_at => CreatedAt
        }},
        MigrateContext
    );
maybe_migrate_change(Ev, _MigrateContext) ->
    Ev.

get_aux_state_ctx(AuxState) when is_map(AuxState) ->
    maps:get(ctx, AuxState, undefined);
get_aux_state_ctx(_) ->
    undefined.

fetch_entity_context(MachineID, MigrateContext) ->
    case maps:get(ctx, MigrateContext, undefined) of
        undefined ->
            {ok, State} = ff_machine:get(ff_identity, 'ff/identity', MachineID, {undefined, 0, forward}),
            maps:get(ctx, State, undefined);
        Data ->
            Data
    end.

get_legacy_name(#{<<"com.rbkmoney.wapi">> := #{<<"name">> := Name}}) ->
    Name.

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

-spec created_v0_decoding_test() -> _.
created_v0_decoding_test() ->
    Identity = #{
        contract => <<"ContractID">>,
        created_at => 1592576943762,
        id => <<"ID">>,
        name => <<"Name">>,
        party => <<"PartyID">>,
        provider => <<"good-one">>,
        metadata => #{<<"some key">> => <<"some val">>},
        version => 2
    },
    Change = {created, Identity},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"class">>} => {bin, <<"class">>},
                    {str, <<"contract">>} => {bin, <<"ContractID">>},
                    {str, <<"created_at">>} => {i, 1592576943762},
                    {str, <<"id">>} => {bin, <<"ID">>},
                    {str, <<"party">>} => {bin, <<"PartyID">>},
                    {str, <<"provider">>} => {bin, <<"good-one">>},
                    {str, <<"version">>} => {i, 2},
                    {str, <<"metadata">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {bin, <<"some key">>} => {bin, <<"some val">>}
                            }}
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
    C = make_legacy_context(#{ctx => #{<<"com.rbkmoney.wapi">> => #{<<"name">> => <<"Name">>}}}),
    {DecodedLegacy, _} = unmarshal({event, undefined}, LegacyEvent, C),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    {Decoded, _} = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary, C),
    ?assertEqual(Event, Decoded).

-spec level_changed_v0_decoding_test() -> _.
level_changed_v0_decoding_test() ->
    Change = {level_changed, <<"level_changed">>},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"level_changed">>},
            {bin, <<"level_changed">>}
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

-spec effective_challenge_changed_v0_decoding_test() -> _.
effective_challenge_changed_v0_decoding_test() ->
    Change = {effective_challenge_changed, <<"effective_challenge_changed">>},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"effective_challenge_changed">>},
            {bin, <<"effective_challenge_changed">>}
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

-spec challenge_created_v0_decoding_test() -> _.
challenge_created_v0_decoding_test() ->
    Change = {
        {challenge, <<"challengeID">>},
        {created, #{
            id => <<"id">>,
            claimant => <<"claimant">>,
            provider => <<"provider">>,
            identity_class => <<"identity_class">>,
            challenge_class => <<"challenge_class">>,
            proofs => [{rus_domestic_passport, <<"identdoc_token">>}],
            master_id => <<"master_id">>,
            claim_id => <<"claim_id">>
        }}
    },
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {arr, [
                {str, <<"tup">>},
                {str, <<"challenge">>},
                {bin, <<"challengeID">>}
            ]},
            {arr, [
                {str, <<"tup">>},
                {str, <<"created">>},
                {arr, [
                    {str, <<"map">>},
                    {obj, #{
                        {str, <<"id">>} => {bin, <<"id">>},
                        {str, <<"claimant">>} => {bin, <<"claimant">>},
                        {str, <<"provider">>} => {bin, <<"provider">>},
                        {str, <<"identity_class">>} => {bin, <<"identity_class">>},
                        {str, <<"challenge_class">>} => {bin, <<"challenge_class">>},
                        {str, <<"master_id">>} => {bin, <<"master_id">>},
                        {str, <<"claim_id">>} => {bin, <<"claim_id">>},
                        {str, <<"proofs">>} =>
                            {arr, [
                                {str, <<"lst">>},
                                {arr, [
                                    {str, <<"tup">>},
                                    {str, <<"rus_domestic_passport">>},
                                    {bin, <<"identdoc_token">>}
                                ]}
                            ]}
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
    ?assertEqual(Event, DecodedLegacy).

-spec challenge_status_changed_v0_decoding_test() -> _.
challenge_status_changed_v0_decoding_test() ->
    Change = {{challenge, <<"challengeID">>}, {status_changed, pending}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {arr, [
                {str, <<"tup">>},
                {str, <<"challenge">>},
                {bin, <<"challengeID">>}
            ]},
            {arr, [
                {str, <<"tup">>},
                {str, <<"status_changed">>},
                {str, <<"pending">>}
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
    ?assertEqual(Event, DecodedLegacy).

-spec created_v1_decoding_test() -> _.
created_v1_decoding_test() ->
    Identity = #{
        contract => <<"ContractID">>,
        created_at => 1592576943762,
        id => <<"ID">>,
        name => <<"Name">>,
        party => <<"PartyID">>,
        provider => <<"good-one">>,
        metadata => #{<<"some key">> => <<"some val">>},
        version => 2
    },
    Change = {created, Identity},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAQsABgAAAAJJRAsAAQAAAAd"
                "QYXJ0eUlECwACAAAACGdvb2Qtb25lCwADAAAABWNsYXNzCwAEAAAACkNvbnRyYWN0SUQLAAoAAA"
                "AYMjAyMC0wNi0xOVQxNDoyOTowMy43NjJaDQALCwwAAAABAAAACHNvbWUga2V5CwAFAAAACHNvbWUgdmFsAAAAAA=="
            >>)},
    C = make_legacy_context(#{ctx => #{<<"com.rbkmoney.wapi">> => #{<<"name">> => <<"Name">>}}}),
    {DecodedLegacy, _} = unmarshal({event, 1}, LegacyEvent, C),
    ?assertEqual(Event, DecodedLegacy).

-spec level_changed_v1_decoding_test() -> _.
level_changed_v1_decoding_test() ->
    Change = {level_changed, <<"level_changed">>},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgsAAgAAAA1sZXZlbF9jaGFuZ2VkAAA="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ?assertEqual(Event, DecodedLegacy).

-spec effective_challenge_changed_v1_decoding_test() -> _.
effective_challenge_changed_v1_decoding_test() ->
    Change = {effective_challenge_changed, <<"effective_challenge_changed">>},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgsABAAAABtlZmZlY3RpdmVfY2hhbGxlbmdlX2NoYW5nZWQAAA=="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec challenge_created_v1_decoding_test() -> _.
challenge_created_v1_decoding_test() ->
    Change = {
        {challenge, <<"challengeID">>},
        {created, #{
            id => <<"id">>,
            claimant => <<"claimant">>,
            provider => <<"provider">>,
            identity_class => <<"identity_class">>,
            challenge_class => <<"challenge_class">>,
            proofs => [{rus_domestic_passport, <<"identdoc_token">>}],
            master_id => <<"master_id">>,
            claim_id => <<"claim_id">>
        }}
    },
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwsAAQAAAAtjaGFsbGVuZ2VJRA"
                "wAAgwAAQsAAwAAAAJpZAsAAQAAAA9jaGFsbGVuZ2VfY2xhc3MPAAIMAAAAAQgAAQAAAAALAAIAAAAO"
                "aWRlbnRkb2NfdG9rZW4ACwAFAAAACHByb3ZpZGVyCwAGAAAADmlkZW50aXR5X2NsYXNzCwAHAAAACG"
                "NsYWltX2lkCwAIAAAACW1hc3Rlcl9pZAsACQAAAAhjbGFpbWFudAAAAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ?assertEqual(Event, DecodedLegacy).

-spec challenge_status_changed_v1_decoding_test() -> _.
challenge_status_changed_v1_decoding_test() ->
    Change = {{challenge, <<"challengeID">>}, {status_changed, pending}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwsAAQAAAAtjaGFsbGVuZ2VJRAwAAgwAAgwAAQAAAAAAAA=="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ?assertEqual(Event, DecodedLegacy).

-spec created_v2_decoding_test() -> _.
created_v2_decoding_test() ->
    Identity = #{
        contract => <<"ContractID">>,
        created_at => 1592576943762,
        id => <<"ID">>,
        name => <<"Name">>,
        party => <<"PartyID">>,
        provider => <<"good-one">>,
        metadata => #{<<"some key">> => <<"some val">>},
        version => 2
    },
    Change = {created, Identity},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAQsABgAAAAJJRAsADAAAAAROYW1lC"
                "wABAAAAB1BhcnR5SUQLAAIAAAAIZ29vZC1vbmULAAMAAAAFY2xhc3MLAAQAAAAKQ29udHJhY3RJRAsACg"
                "AAABgyMDIwLTA2LTE5VDE0OjI5OjAzLjc2MloNAAsLDAAAAAEAAAAIc29tZSBrZXkLAAUAAAAIc29tZSB2"
                "YWwAAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 2}, LegacyEvent),
    ?assertEqual(Event, DecodedLegacy).

-spec level_changed_v2_decoding_test() -> _.
level_changed_v2_decoding_test() ->
    Change = {level_changed, <<"level_changed">>},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgsAAgAAAA1sZXZlbF9jaGFuZ2VkAAA="
            >>)},
    DecodedLegacy = unmarshal({event, 2}, LegacyEvent),
    ?assertEqual(Event, DecodedLegacy).

-spec effective_challenge_changed_v2_decoding_test() -> _.
effective_challenge_changed_v2_decoding_test() ->
    Change = {effective_challenge_changed, <<"effective_challenge_changed">>},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgsABAAAABtlZmZlY3RpdmVfY2hhbGxlbmdlX2NoYW5nZWQAAA=="
            >>)},
    DecodedLegacy = unmarshal({event, 2}, LegacyEvent),
    ?assertEqual(Event, DecodedLegacy).

-spec challenge_created_v2_decoding_test() -> _.
challenge_created_v2_decoding_test() ->
    Change = {
        {challenge, <<"challengeID">>},
        {created, #{
            id => <<"id">>,
            claimant => <<"claimant">>,
            provider => <<"provider">>,
            identity_class => <<"identity_class">>,
            challenge_class => <<"challenge_class">>,
            proofs => [{rus_domestic_passport, <<"identdoc_token">>}],
            master_id => <<"master_id">>,
            claim_id => <<"claim_id">>
        }}
    },
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwsAAQAAAAtjaGFsbGVuZ2VJRA"
                "wAAgwAAQsAAwAAAAJpZAsAAQAAAA9jaGFsbGVuZ2VfY2xhc3MPAAIMAAAAAQgAAQAAAAALAAIAAAAO"
                "aWRlbnRkb2NfdG9rZW4ACwAFAAAACHByb3ZpZGVyCwAGAAAADmlkZW50aXR5X2NsYXNzCwAHAAAACG"
                "NsYWltX2lkCwAIAAAACW1hc3Rlcl9pZAsACQAAAAhjbGFpbWFudAAAAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 2}, LegacyEvent),
    ?assertEqual(Event, DecodedLegacy).

-spec challenge_status_changed_v2_decoding_test() -> _.
challenge_status_changed_v2_decoding_test() ->
    Change = {{challenge, <<"challengeID">>}, {status_changed, pending}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwsAAQAAAAtjaGFsbGVuZ2VJRAwAAgwAAgwAAQAAAAAAAA=="
            >>)},
    DecodedLegacy = unmarshal({event, 2}, LegacyEvent),
    ?assertEqual(Event, DecodedLegacy).

-endif.
