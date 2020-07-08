-module(ff_identity_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").
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

-type event()   :: ff_machine:timestamped_event(p2p_transfer:event()).
-type aux_state() :: ff_machine:auxst().
-type call_args() :: term().
-type call_response() :: term().
-type context() :: machinery_mg_schema:context().

-type legacy_event() :: any().

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
marshal_event(undefined = Version, TimestampedChange, Context) ->
    % TODO: Remove this clause after MSPF-561 finish
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange, Context);
marshal_event(?CURRENT_EVENT_FORMAT_VERSION, TimestampedChange, Context) ->
    ThriftChange = ff_identity_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_identity_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) ->
    {event(), context()}.
unmarshal_event(?CURRENT_EVENT_FORMAT_VERSION, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_identity_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_identity_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context) ->
    {{ev, Timestamp, Change}, Context1} = machinery_mg_schema_generic:unmarshal(
        {event, Version},
        EncodedChange,
        Context
    ),
    {{ev, Timestamp, maybe_migrate(Change, Context1)}, Context1}.


-spec maybe_migrate(event() | legacy_event(), context()) ->
    event().

maybe_migrate(Event = {created, #{version := 2}}, _MigrateContext) ->
    Event;
maybe_migrate({created, Identity = #{version := 1}}, MigrateContext) ->
    Context = case maps:get(machine_ref, MigrateContext, undefined) of
        undefined ->
            undefined;
        ID ->
            {ok, State} = ff_machine:get(ff_identity, 'ff/identity', ID, {undefined, 0, forward}),
            maps:get(ctx, State, undefined)
    end,
    maybe_migrate({created, genlib_map:compact(Identity#{
        version => 2,
        metadata => ff_entity_context:try_get_legacy_metadata(Context)
    })}, MigrateContext);
maybe_migrate({created, Identity = #{created_at := _CreatedAt}}, MigrateContext) ->
    maybe_migrate({created, Identity#{
        version => 1
    }}, MigrateContext);
maybe_migrate({created, Identity}, MigrateContext) ->
    Timestamp = maps:get(created_at, MigrateContext),
    CreatedAt = ff_codec:unmarshal(timestamp_ms, ff_codec:marshal(timestamp, Timestamp)),
    maybe_migrate({created, Identity#{
        created_at => CreatedAt
    }}, MigrateContext);
maybe_migrate(Ev, _MigrateContext) ->
    Ev.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

% tests helpers

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

% -spec v2_created_migration_test() -> _.

% v2_created_migration_test() ->
%     ID = genlib:unique(),
%     LegacyEvent = {created, #{
%         version       => 2,
%         id            => ID,
%         created_at    => ff_time:now()
%     }},
%     {created, Identity} = maybe_migrate(LegacyEvent, #{}),
%     ?assertEqual(ID, ff_identity:id(Identity)),
%     ?assertEqual(#{<<"some key">> => <<"some val">>}, ff_identity:metadata(Identity)).

-spec created_v0_2_decoding_test() -> _.
created_v0_2_decoding_test() ->
    Identity = #{
        class => <<"class">>,
        contract => <<"ContractID">>,
        created_at => 1592576943762,
        id => <<"ID">>,
        party => <<"PartyID">>,
        provider => <<"good-one">>,
        version => 2
    },
    Change = {created, Identity},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange = {arr, [
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
                {str, <<"provider">>} => {bin, <<"good-one">>}
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

-spec created_v1_2_decoding_test() -> _.
created_v1_2_decoding_test() ->
    Identity = #{
        class => <<"class">>,
        contract => <<"ContractID">>,
        created_at => 1592576943762,
        id => <<"ID">>,
        party => <<"PartyID">>,
        provider => <<"good-one">>,
        version => 2
    },
    Change = {created, Identity},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange = {arr, [
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
                {str, <<"version">>} => {i, 1}
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

    {DecodedLegacy, _} = unmarshal({event, undefined}, LegacyEvent, #{
        ctx => #{
            <<"com.rbkmoney.wapi">> => #{
                <<"metadata">> => #{
                    <<"some key">> => <<"some val">>
                }
            }
        }
    }),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-endif.
