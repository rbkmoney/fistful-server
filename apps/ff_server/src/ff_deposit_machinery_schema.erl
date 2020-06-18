-module(ff_deposit_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_deposit_thrift.hrl").
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
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange, Context);
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = ff_deposit_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_deposit_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) ->
    {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_deposit_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_deposit_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context0) ->
    {Event, Context1} = machinery_mg_schema_generic:unmarshal({event, Version}, EncodedChange, Context0),
    {ev, Timestamp, Change} = Event,
    {{ev, Timestamp, maybe_migrate(Change)}, Context1}.

-spec maybe_migrate(any()) ->
    ff_deposit:event().
maybe_migrate({created, #{version := 3}} = Event) ->
    Event;
maybe_migrate({created, Deposit}) ->
    {created, Deposit#{version => 3}};
% Other events
maybe_migrate(Ev) ->
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

-spec created_v0_3_decoding_test() -> _.
created_v0_3_decoding_test() ->
    Deposit = #{
        version => 3,
        id => <<"deposit">>,
        status => pending,
        body => {123, <<"RUB">>},
        created_at => 1590426777985,
        domain_revision => 123,
        party_revision => 321,
        external_id => <<"external_id">>,
        params => #{
            wallet_id => <<"wallet_id">>,
            source_id => <<"source_id">>
        }
    },
    Change = {created, Deposit},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"created">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"id">>} => {bin, <<"deposit">>},
                {str, <<"status">>} => {str, <<"pending">>},
                {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                {str, <<"created_at">>} => {i, 1590426777985},
                {str, <<"domain_revision">>} => {i, 123},
                {str, <<"party_revision">>} => {i, 321},
                {str, <<"external_id">>} => {bin, <<"external_id">>},
                {str, <<"params">>} => {arr, [{str, <<"map">>}, {obj, #{
                    {str, <<"wallet_id">>} => {bin, <<"wallet_id">>},
                    {str, <<"source_id">>} => {bin, <<"source_id">>}
                }}]}
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

-spec p_transfer_v0_3_decoding_test() -> _.
p_transfer_v0_3_decoding_test() ->
    PTransfer = #{
        id => <<"external_id">>,
        final_cash_flow => #{
            postings => []
        }
    },
    Change = {p_transfer, {created, PTransfer}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"p_transfer">>},
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"final_cash_flow">>} => {arr, [
                        {str, <<"map">>},
                        {obj, #{{str, <<"postings">>} => {arr, []}}}
                    ]},
                    {str, <<"id">>} => {bin, <<"external_id">>}
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

-spec limit_check_v0_3_decoding_test() -> _.
limit_check_v0_3_decoding_test() ->
    Change = {limit_check, {wallet_sender, ok}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"limit_check">>},
        {arr, [
            {str, <<"tup">>},
            {str, <<"wallet_sender">>},
            {str, <<"ok">>}
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

-spec revert_v0_3_decoding_test() -> _.
revert_v0_3_decoding_test() ->
    Revert = #{
        id => <<"id">>,
        payload => {created, #{
            id => <<"deposit_revert">>,
            status => pending,
            body => {123, <<"RUB">>},
            created_at => 1590426777985,
            domain_revision => 123,
            party_revision => 321,
            external_id => <<"external_id">>,
            wallet_id => <<"wallet_id">>,
            source_id => <<"source_id">>
        }}
    },
    Change = {revert, Revert},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyRevert = {arr, [{str, <<"map">>}, {obj, #{
        {str, <<"id">>} => {bin, <<"deposit_revert">>},
        {str, <<"status">>} => {str, <<"pending">>},
        {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
        {str, <<"created_at">>} => {i, 1590426777985},
        {str, <<"domain_revision">>} => {i, 123},
        {str, <<"party_revision">>} => {i, 321},
        {str, <<"external_id">>} => {bin, <<"external_id">>},
        {str, <<"wallet_id">>} => {bin, <<"wallet_id">>},
        {str, <<"source_id">>} => {bin, <<"source_id">>}
    }}]},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"revert">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"id">>} => {bin, <<"id">>},
                {str, <<"payload">>} => {arr, [
                    {str, <<"tup">>},
                    {str, <<"created">>},
                    LegacyRevert
                ]}
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

-spec adjustment_v0_3_decoding_test() -> _.
adjustment_v0_3_decoding_test() ->
    CashFlowChange = #{
        old_cash_flow_inverted => #{postings => []},
        new_cash_flow => #{postings => []}
    },

    Plan = #{
        new_cash_flow => CashFlowChange,
        new_status => #{
            new_status => succeeded
        }
    },
    Adjustment = #{
        id => <<"adjustment">>,
        payload => {created, #{
            id => <<"adjustment">>,
            status => pending,
            changes_plan => Plan,
            created_at => 1590426777985,
            domain_revision => 123,
            party_revision => 321,
            operation_timestamp => 1590426777986,
            external_id => <<"external_id">>
        }}
    },
    Change = {adjustment, Adjustment},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyPlan = {arr, [{str, <<"map">>}, {obj, #{
        {str, <<"new_cash_flow">>} => {arr, [{str, <<"map">>}, {obj, #{
            {str, <<"old_cash_flow_inverted">>} =>
                {arr, [{str, <<"map">>}, {obj, #{{str, <<"postings">>} => {arr, []}}}]},
            {str, <<"new_cash_flow">>} =>
                {arr, [{str, <<"map">>}, {obj, #{{str, <<"postings">>} => {arr, []}}}]}
        }}]},
        {str, <<"new_status">>} => {arr, [{str, <<"map">>}, {obj, #{
            {str, <<"new_status">>} => {str, <<"succeeded">>}
        }}]}
    }}]},
    LegacyAdjustment = {arr, [{str, <<"map">>}, {obj, #{
        {str, <<"id">>} => {bin, <<"adjustment">>},
        {str, <<"status">>} => {str, <<"pending">>},
        {str, <<"changes_plan">>} => LegacyPlan,
        {str, <<"created_at">>} => {i, 1590426777985},
        {str, <<"domain_revision">>} => {i, 123},
        {str, <<"party_revision">>} => {i, 321},
        {str, <<"operation_timestamp">>} => {i, 1590426777986},
        {str, <<"external_id">>} => {bin, <<"external_id">>}
    }}]},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"adjustment">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"id">>} => {bin, <<"adjustment">>},
                {str, <<"payload">>} => {arr, [
                    {str, <<"tup">>},
                    {str, <<"created">>},
                    LegacyAdjustment
                ]}
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

-endif.
