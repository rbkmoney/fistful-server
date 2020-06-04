-module(ff_p2p_transfer_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([get_version/1]).
-export([marshal/2]).
-export([unmarshal/2]).

%% Constants

-define(CURRENT_EVENT_FORMAT_VERSION, 1).

%% Internal types

-type t() :: machinery_mg_schema:t().
-type v(T) :: machinery_mg_schema:v(T).
-type vt() :: machinery_mg_schema:vt().

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

-spec get_version(vt()) ->
    machinery_mg_schema:version().
get_version(event) ->
    ?CURRENT_EVENT_FORMAT_VERSION;
get_version(aux_state) ->
    undefined.

-spec marshal(t(), v(data())) ->
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

-spec unmarshal(t(), machinery_msgpack:t()) ->
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
marshal_event(1, TimestampedChange) ->
    ThriftChange = ff_p2p_transfer_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_p2p_transfer_thrift, 'TimestampedChange'}},
    {bin, ff_proto_utils:serialize(Type, ThriftChange)}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t()) ->
    event().
unmarshal_event(1, EncodedChange) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_p2p_transfer_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    ff_p2p_transfer_codec:unmarshal(timestamped_change, ThriftChange);
unmarshal_event(undefined = Version, EncodedChange) ->
    {ev, Timestamp, Change} = machinery_mg_schema_generic:unmarshal({event, Version}, EncodedChange),
    {ev, Timestamp, maybe_migrate(Change)}.

-spec maybe_migrate(any()) ->
    p2p_transfer:event().
maybe_migrate({resource_got, Sender, Receiver}) ->
    {resource_got, maybe_migrate_resource(Sender), maybe_migrate_resource(Receiver)};
maybe_migrate({created, #{version := 1} = Transfer}) ->
    #{
        version := 1,
        sender := Sender,
        receiver := Receiver
    } = Transfer,
    maybe_migrate({created, genlib_map:compact(Transfer#{
        version := 2,
        sender => maybe_migrate_participant(Sender),
        receiver => maybe_migrate_participant(Receiver)
    })});
maybe_migrate({created, #{version := 2} = Transfer}) ->
    #{
        version := 2,
        sender := Sender,
        receiver := Receiver
    } = Transfer,
    {created, genlib_map:compact(Transfer#{
        sender => maybe_migrate_participant(Sender),
        receiver => maybe_migrate_participant(Receiver)
    })};
% Other events
maybe_migrate(Ev) ->
    Ev.

maybe_migrate_resource({crypto_wallet, #{id := _ID} = CryptoWallet}) ->
    maybe_migrate_resource({crypto_wallet, #{crypto_wallet => CryptoWallet}});
maybe_migrate_resource({bank_card, #{token := _Token} = BankCard}) ->
    maybe_migrate_resource({bank_card, #{bank_card => BankCard}});
maybe_migrate_resource(Resource) ->
    Resource.

maybe_migrate_participant({raw, #{resource_params := Resource} = Participant}) ->
    {raw, Participant#{
        resource_params => maybe_migrate_resource(Resource),
        contact_info => maps:get(contact_info, Participant, #{})
    }};
maybe_migrate_participant(Resource) ->
    Resource.

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec legacy_created_v0_2_decoding_test() -> _.
legacy_created_v0_2_decoding_test() ->
    Resource = {bank_card, #{bank_card => #{
        token => <<"token">>,
        bin_data_id => {binary, <<"bin">>}
    }}},
    Participant = {raw, #{
        resource_params => Resource,
        contact_info => #{}
    }},
    P2PTransfer = #{
        version => 2,
        id => <<"transfer">>,
        status => pending,
        owner => <<"owner">>,
        body => {123, <<"RUB">>},
        created_at => 1590426777985,
        sender => Participant,
        receiver => Participant,
        quote => #{},
        domain_revision => 123,
        party_revision => 321,
        operation_timestamp => 1590426777986,
        external_id => <<"external_id">>,
        deadline => 1590426777987
    },
    Change = {created, P2PTransfer},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    ResourceParams = {arr, [
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
                            {bin, <<"bin">>}
                        ]},
                        {str, <<"token">>} => {bin, <<"token">>}
                    }}
                ]}
            }}
        ]}
    ]},
    LegacyParticipant1 = {arr, [
        {str, <<"tup">>},
        {str, <<"raw">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"contact_info">>} => {arr, [{str, <<"map">>}, {obj, #{}}]},
                {str, <<"resource_params">>} => ResourceParams
            }}
        ]}
    ]},
    LegacyParticipant2 = {arr, [
        {str, <<"tup">>},
        {str, <<"raw">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"resource_params">>} => ResourceParams
            }}
        ]}
    ]},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"created">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"version">>} => {i, 2},
                {str, <<"id">>} => {bin, <<"transfer">>},
                {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                {str, <<"created_at">>} => {i, 1590426777985},
                {str, <<"deadline">>} => {i, 1590426777987},
                {str, <<"domain_revision">>} => {i, 123},
                {str, <<"external_id">>} => {bin, <<"external_id">>},
                {str, <<"operation_timestamp">>} => {i, 1590426777986},
                {str, <<"owner">>} => {bin, <<"owner">>},
                {str, <<"party_revision">>} => {i, 321},
                {str, <<"quote">>} => {arr, [{str, <<"map">>}, {obj, #{}}]},
                {str, <<"receiver">>} => LegacyParticipant1,
                {str, <<"sender">>} => LegacyParticipant2,
                {str, <<"status">>} => {str, <<"pending">>}
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

-spec legacy_resource_got_v0_2_decoding_test() -> _.
legacy_resource_got_v0_2_decoding_test() ->
    Resource = {bank_card, #{bank_card => #{
        token => <<"token">>,
        bin_data_id => {binary, <<"bin">>}
    }}},
    Change = {resource_got, Resource, Resource},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyResource = {arr, [
        {str, <<"tup">>},
        {str, <<"bank_card">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"bank_card">>} => {arr, [
                    {str, <<"map">>},
                    {obj, #{
                        {str, <<"bin_data_id">>} => {arr, [
                            {str, <<"tup">>},
                            {str, <<"binary">>},
                            {bin, <<"bin">>}
                        ]},
                        {str, <<"token">>} => {bin, <<"token">>}
                    }}
                ]}
            }}
        ]}
    ]},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"resource_got">>},
        LegacyResource,
        LegacyResource
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

-spec legacy_risk_score_changed_v0_2_decoding_test() -> _.
legacy_risk_score_changed_v0_2_decoding_test() ->
    Change = {risk_score_changed, low},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"risk_score_changed">>},
        {str, <<"low">>}
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

-spec legacy_route_changed_v0_2_decoding_test() -> _.
legacy_route_changed_v0_2_decoding_test() ->
    Change = {route_changed, #{provider_id => 1}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"route_changed">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{{str, <<"provider_id">>} => {i, 1}}}
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

-spec legacy_p_transfer_v0_2_decoding_test() -> _.
legacy_p_transfer_v0_2_decoding_test() ->
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

-spec legacy_session_v0_2_decoding_test() -> _.
legacy_session_v0_2_decoding_test() ->
    Change = {session, {<<"session_id">>, started}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"session">>},
        {arr, [
            {str, <<"tup">>},
            {bin, <<"session_id">>},
            {str, <<"started">>}
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