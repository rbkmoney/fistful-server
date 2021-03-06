-module(ff_p2p_session_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_p2p_session_thrift.hrl").
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

-type event() :: ff_machine:timestamped_event(p2p_session:event()).
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
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange, Context);
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = ff_p2p_session_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_p2p_session_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_p2p_session_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_p2p_session_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context0) ->
    {Event, Context1} = machinery_mg_schema_generic:unmarshal({event, Version}, EncodedChange, Context0),
    {ev, Timestamp, Change} = Event,
    {{ev, Timestamp, maybe_migrate(Change)}, Context1}.

-spec maybe_migrate(any()) -> p2p_session:event().
maybe_migrate({created, #{version := 1} = Session}) ->
    #{
        version := 1,
        transfer_params := #{
            sender := Sender,
            receiver := Receiver
        } = Params
    } = Session,
    maybe_migrate(
        {created,
            genlib_map:compact(Session#{
                version => 2,
                transfer_params => Params#{
                    sender => maybe_migrate_resource(Sender),
                    receiver => maybe_migrate_resource(Receiver)
                }
            })}
    );
maybe_migrate({created, #{version := 2} = Session}) ->
    #{
        version := 2,
        provider_id := ProviderID
    } = Session,
    maybe_migrate(
        {created,
            genlib_map:compact(
                maps:without([provider_id], Session#{
                    version => 3,
                    route => #{
                        provider_id => ProviderID + 400
                    },
                    provider_id_legacy => ProviderID
                })
            )}
    );
% Other events
maybe_migrate({callback, _Ev} = Event) ->
    p2p_callback_utils:maybe_migrate(Event);
maybe_migrate({user_interaction, _Ev} = Event) ->
    p2p_user_interaction_utils:maybe_migrate(Event);
maybe_migrate(Ev) ->
    Ev.

maybe_migrate_resource({crypto_wallet, #{id := _ID} = CryptoWallet}) ->
    maybe_migrate_resource({crypto_wallet, #{crypto_wallet => CryptoWallet}});
maybe_migrate_resource({bank_card, #{token := _Token} = BankCard}) ->
    maybe_migrate_resource({bank_card, #{bank_card => BankCard}});
maybe_migrate_resource(Resource) ->
    Resource.

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

-spec created_v0_1_decoding_test() -> _.
created_v0_1_decoding_test() ->
    Fees = #{fees => #{operation_amount => {100, <<"RUB">>}}},
    TransferParams = #{
        id => <<"2">>,
        body => {10000, <<"RUB">>},
        sender =>
            {bank_card, #{
                bank_card => #{
                    bin => <<"555555">>,
                    bin_data_id => 279896,
                    card_type => credit,
                    cardholder_name => <<"sender">>,
                    exp_date => {10, 2022},
                    iso_country_code => bra,
                    masked_pan => <<"4444">>,
                    payment_system => mastercard,
                    token => <<"token">>
                }
            }},
        receiver =>
            {bank_card, #{
                bank_card => #{
                    bin => <<"424242">>,
                    bin_data_id => 279896,
                    card_type => credit,
                    cardholder_name => <<"receiver">>,
                    exp_date => {10, 2022},
                    iso_country_code => gbr,
                    masked_pan => <<"4242">>,
                    payment_system => visa,
                    token => <<"token">>
                }
            }},
        provider_fees => Fees
    },

    P2PSession = #{
        version => 3,
        id => <<"2">>,
        status => active,
        transfer_params => TransferParams,
        route => #{provider_id => 401},
        provider_id_legacy => 1,
        domain_revision => 123,
        party_revision => 321
    },
    Change = {created, P2PSession},
    Event = {ev, {{{2020, 3, 4}, {13, 27, 20}}, 136850}, Change},

    LegacyEvent =
        {arr, [
            {str, <<"tup">>},
            {str, <<"ev">>},
            {arr, [
                {str, <<"tup">>},
                {arr, [
                    {str, <<"tup">>},
                    {arr, [{str, <<"tup">>}, {i, 2020}, {i, 3}, {i, 4}]},
                    {arr, [{str, <<"tup">>}, {i, 13}, {i, 27}, {i, 20}]}
                ]},
                {i, 136850}
            ]},
            {arr, [
                {str, <<"tup">>},
                {str, <<"created">>},
                {arr, [
                    {str, <<"map">>},
                    {obj, #{
                        {str, <<"adapter">>} =>
                            {arr, [
                                {str, <<"tup">>},
                                {arr, [
                                    {str, <<"map">>},
                                    {obj, #{
                                        {str, <<"event_handler">>} =>
                                            {arr, [
                                                {str, <<"tup">>},
                                                {str, <<"scoper_woody_event_handler">>},
                                                {arr, [
                                                    {str, <<"map">>},
                                                    {obj, #{}}
                                                ]}
                                            ]},
                                        {str, <<"url">>} =>
                                            {bin, <<"http://adapter-mockapter:8022/adapter/mockapter/p2p">>}
                                    }}
                                ]},
                                {arr, [{str, <<"map">>}, {obj, #{{bin, <<"k">>} => {bin, <<"v">>}}}]}
                            ]},
                        {str, <<"domain_revision">>} => {i, 123},
                        {str, <<"id">>} => {bin, <<"2">>},
                        {str, <<"party_revision">>} => {i, 321},
                        {str, <<"provider_id">>} => {i, 1},
                        {str, <<"status">>} => {str, <<"active">>},
                        {str, <<"transfer_params">>} =>
                            {arr, [
                                {str, <<"map">>},
                                {obj, #{
                                    {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 10000}, {bin, <<"RUB">>}]},
                                    {str, <<"id">>} => {bin, <<"2">>},
                                    {str, <<"provider_fees">>} =>
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {str, <<"fees">>} =>
                                                    {arr, [
                                                        {str, <<"map">>},
                                                        {obj, #{
                                                            {str, <<"operation_amount">>} =>
                                                                {arr, [
                                                                    {str, <<"tup">>},
                                                                    {i, 100},
                                                                    {bin, <<"RUB">>}
                                                                ]}
                                                        }}
                                                    ]}
                                            }}
                                        ]},
                                    {str, <<"receiver">>} =>
                                        {arr, [
                                            {str, <<"tup">>},
                                            {str, <<"bank_card">>},
                                            {arr, [
                                                {str, <<"map">>},
                                                {obj, #{
                                                    {str, <<"bin">>} => {bin, <<"424242">>},
                                                    {str, <<"bin_data_id">>} => {i, 279896},
                                                    {str, <<"card_type">>} => {str, <<"credit">>},
                                                    {str, <<"cardholder_name">>} => {bin, <<"receiver">>},
                                                    {str, <<"exp_date">>} =>
                                                        {arr, [
                                                            {str, <<"tup">>},
                                                            {i, 10},
                                                            {i, 2022}
                                                        ]},
                                                    {str, <<"iso_country_code">>} => {str, <<"gbr">>},
                                                    {str, <<"masked_pan">>} => {bin, <<"4242">>},
                                                    {str, <<"payment_system">>} => {str, <<"visa">>},
                                                    {str, <<"token">>} => {bin, <<"token">>}
                                                }}
                                            ]}
                                        ]},
                                    {str, <<"sender">>} =>
                                        {arr, [
                                            {str, <<"tup">>},
                                            {str, <<"bank_card">>},
                                            {arr, [
                                                {str, <<"map">>},
                                                {obj, #{
                                                    {str, <<"bin">>} => {bin, <<"555555">>},
                                                    {str, <<"bin_data_id">>} => {i, 279896},
                                                    {str, <<"card_type">>} => {str, <<"credit">>},
                                                    {str, <<"cardholder_name">>} => {bin, <<"sender">>},
                                                    {str, <<"exp_date">>} =>
                                                        {arr, [
                                                            {str, <<"tup">>},
                                                            {i, 10},
                                                            {i, 2022}
                                                        ]},
                                                    {str, <<"iso_country_code">>} => {str, <<"bra">>},
                                                    {str, <<"masked_pan">>} => {bin, <<"4444">>},
                                                    {str, <<"payment_system">>} => {str, <<"mastercard">>},
                                                    {str, <<"token">>} => {bin, <<"token">>}
                                                }}
                                            ]}
                                        ]}
                                }}
                            ]},
                        {str, <<"version">>} => {i, 1}
                    }}
                ]}
            ]}
        ]},
    DecodedLegacy = unmarshal({event, undefined}, LegacyEvent),
    ModernizedBinary = marshal({event, 1}, DecodedLegacy),
    Decoded = unmarshal({event, 1}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_v0_2_decoding_test() -> _.
created_v0_2_decoding_test() ->
    Resource =
        {bank_card, #{
            bank_card => #{
                token => <<"token">>,
                bin_data_id => {binary, <<"bin">>}
            }
        }},

    Fees = #{fees => #{operation_amount => {123, <<"RUB">>}}},

    TransferParams = #{
        id => <<"transfer">>,
        body => {123, <<"RUB">>},
        sender => Resource,
        receiver => Resource,
        deadline => 1590426777987,
        merchant_fees => Fees,
        provider_fees => Fees
    },

    P2PSession = #{
        version => 3,
        id => <<"session">>,
        status => active,
        transfer_params => TransferParams,
        route => #{provider_id => 401},
        provider_id_legacy => 1,
        domain_revision => 123,
        party_revision => 321
    },
    Change = {created, P2PSession},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    ResourceParams =
        {arr, [
            {str, <<"tup">>},
            {str, <<"bank_card">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"bank_card">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"bin_data_id">>} =>
                                    {arr, [
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

    LegacyFees =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"fees">>} =>
                    {arr, [
                        {str, <<"map">>},
                        {obj, #{
                            {str, <<"operation_amount">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]}
                        }}
                    ]}
            }}
        ]},

    LegacyTransferParams =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"id">>} => {bin, <<"transfer">>},
                {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                {str, <<"deadline">>} => {i, 1590426777987},
                {str, <<"receiver">>} => ResourceParams,
                {str, <<"sender">>} => ResourceParams,
                {str, <<"merchant_fees">>} => LegacyFees,
                {str, <<"provider_fees">>} => LegacyFees
            }}
        ]},

    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"version">>} => {i, 2},
                    {str, <<"id">>} => {bin, <<"session">>},
                    {str, <<"status">>} => {str, <<"active">>},
                    {str, <<"transfer_params">>} => LegacyTransferParams,
                    {str, <<"provider_id">>} => {i, 1},
                    {str, <<"domain_revision">>} => {i, 123},
                    {str, <<"party_revision">>} => {i, 321}
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

-spec next_state_v0_2_decoding_test() -> _.
next_state_v0_2_decoding_test() ->
    Change = {next_state, <<"next_state">>},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"next_state">>},
            {bin, <<"next_state">>}
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

-spec transaction_bound_v0_2_decoding_test() -> _.
transaction_bound_v0_2_decoding_test() ->
    Change =
        {transaction_bound, #{
            id => <<"id">>,
            timestamp => <<"timestamp">>,
            extra => #{<<"key">> => <<"value">>},
            additional_info => #{
                rrn => <<"value">>,
                approval_code => <<"value">>,
                acs_url => <<"value">>,
                pareq => <<"value">>,
                md => <<"value">>,
                term_url => <<"value">>,
                pares => <<"value">>,
                eci => <<"value">>,
                cavv => <<"value">>,
                xid => <<"value">>,
                cavv_algorithm => <<"value">>,
                three_ds_verification => authentication_successful
            }
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"transaction_bound">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"id">>} => {bin, <<"id">>},
                    {str, <<"timestamp">>} => {bin, <<"timestamp">>},
                    {str, <<"extra">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {bin, <<"key">>} => {bin, <<"value">>}
                            }}
                        ]},
                    {str, <<"additional_info">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"rrn">>} => {bin, <<"value">>},
                                {str, <<"approval_code">>} => {bin, <<"value">>},
                                {str, <<"acs_url">>} => {bin, <<"value">>},
                                {str, <<"pareq">>} => {bin, <<"value">>},
                                {str, <<"md">>} => {bin, <<"value">>},
                                {str, <<"term_url">>} => {bin, <<"value">>},
                                {str, <<"pares">>} => {bin, <<"value">>},
                                {str, <<"eci">>} => {bin, <<"value">>},
                                {str, <<"cavv">>} => {bin, <<"value">>},
                                {str, <<"xid">>} => {bin, <<"value">>},
                                {str, <<"cavv_algorithm">>} => {bin, <<"value">>},
                                {str, <<"three_ds_verification">>} => {str, <<"authentication_successful">>}
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
    DecodedLegacy = unmarshal({event, undefined}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec finished_v0_2_decoding_test() -> _.
finished_v0_2_decoding_test() ->
    Change = {finished, success},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"finished">>},
            {str, <<"success">>}
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

-spec callback_created_v0_2_decoding_test() -> _.
callback_created_v0_2_decoding_test() ->
    Change =
        {callback, #{
            tag => <<"tag">>,
            payload =>
                {created, #{
                    version => 1,
                    tag => <<"tag">>
                }}
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"callback">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"tag">>} => {bin, <<"tag">>},
                    {str, <<"payload">>} =>
                        {arr, [
                            {str, <<"tup">>},
                            {str, <<"created">>},
                            {arr, [
                                {str, <<"map">>},
                                {obj, #{
                                    {str, <<"version">>} => {i, 1},
                                    {str, <<"tag">>} => {bin, <<"tag">>}
                                }}
                            ]}
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

-spec callback_finished_v0_2_decoding_test() -> _.
callback_finished_v0_2_decoding_test() ->
    Change =
        {callback, #{
            tag => <<"tag">>,
            payload =>
                {finished, #{
                    payload => <<"payload">>
                }}
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"callback">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"tag">>} => {bin, <<"tag">>},
                    {str, <<"payload">>} =>
                        {arr, [
                            {str, <<"tup">>},
                            {str, <<"finished">>},
                            {arr, [
                                {str, <<"map">>},
                                {obj, #{
                                    {str, <<"payload">>} => {bin, <<"payload">>}
                                }}
                            ]}
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

-spec callback_status_changed_v0_2_decoding_test() -> _.
callback_status_changed_v0_2_decoding_test() ->
    Change =
        {callback, #{
            tag => <<"tag">>,
            payload => {status_changed, pending}
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"callback">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"tag">>} => {bin, <<"tag">>},
                    {str, <<"payload">>} =>
                        {arr, [
                            {str, <<"tup">>},
                            {str, <<"status_changed">>},
                            {str, <<"pending">>}
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

-spec user_interaction_created_v0_2_decoding_test() -> _.
user_interaction_created_v0_2_decoding_test() ->
    Change =
        {user_interaction, #{
            id => <<"id">>,
            payload =>
                {created, #{
                    version => 1,
                    id => <<"id">>,
                    content =>
                        {redirect, #{
                            content => {get, <<"url">>}
                        }}
                }}
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"user_interaction">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"id">>} => {bin, <<"id">>},
                    {str, <<"payload">>} =>
                        {arr, [
                            {str, <<"tup">>},
                            {str, <<"created">>},
                            {arr, [
                                {str, <<"map">>},
                                {obj, #{
                                    {str, <<"version">>} => {i, 1},
                                    {str, <<"id">>} => {bin, <<"id">>},
                                    {str, <<"content">>} =>
                                        {arr, [
                                            {str, <<"tup">>},
                                            {str, <<"redirect">>},
                                            {arr, [
                                                {str, <<"map">>},
                                                {obj, #{
                                                    {str, <<"content">>} =>
                                                        {arr, [
                                                            {str, <<"tup">>},
                                                            {str, <<"get">>},
                                                            {bin, <<"url">>}
                                                        ]}
                                                }}
                                            ]}
                                        ]}
                                }}
                            ]}
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

-spec user_interaction_status_changed_v0_2_decoding_test() -> _.
user_interaction_status_changed_v0_2_decoding_test() ->
    Change =
        {user_interaction, #{
            id => <<"id">>,
            payload => {status_changed, pending}
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"user_interaction">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"id">>} => {bin, <<"id">>},
                    {str, <<"payload">>} =>
                        {arr, [
                            {str, <<"tup">>},
                            {str, <<"status_changed">>},
                            {str, <<"pending">>}
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

-spec created_v1_decoding_test() -> _.
created_v1_decoding_test() ->
    Resource =
        {bank_card, #{
            bank_card => #{
                token => <<"token">>,
                bin_data_id => {binary, <<"bin">>}
            }
        }},

    Fees = #{fees => #{operation_amount => {123, <<"RUB">>}}},

    TransferParams = #{
        id => <<"transfer">>,
        body => {123, <<"RUB">>},
        sender => Resource,
        receiver => Resource,
        deadline => 1590426777987,
        merchant_fees => Fees,
        provider_fees => Fees
    },

    P2PSession = #{
        version => 3,
        id => <<"session">>,
        status => active,
        transfer_params => TransferParams,
        route => #{provider_id => 1},
        provider_id_legacy => 1,
        domain_revision => 123,
        party_revision => 321
    },
    Change = {created, P2PSession},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAQwAAQs"
                "AAQAAAAdzZXNzaW9uDAACDAABAAAMAAMLAAEAAAAIdHJhbnNmZXIMAAIMAA"
                "EMAAELAAEAAAAFdG9rZW4MABULAAYAAAADYmluAAAAAAwAAwwAAQwAAQsAA"
                "QAAAAV0b2tlbgwAFQsABgAAAANiaW4AAAAADAAECgABAAAAAAAAAHsMAAIL"
                "AAEAAAADUlVCAAALAAUAAAAYMjAyMC0wNS0yNVQxNzoxMjo1Ny45ODdaDAA"
                "GDQABCAwAAAABAAAAAAoAAQAAAAAAAAB7DAACCwABAAAAA1JVQgAAAAwABw"
                "0AAQgMAAAAAQAAAAAKAAEAAAAAAAAAewwAAgsAAQAAAANSVUIAAAAADAAHC"
                "AABAAAAAQAKAAUAAAAAAAAAewoABgAAAAAAAAFBCAAEAAAAAQAAAAA="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec next_state_v1_decoding_test() -> _.
next_state_v1_decoding_test() ->
    Change = {next_state, <<"next_state">>},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAgsAAQAAAApuZXh0X3N0YXRlAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec transaction_bound_v1_decoding_test() -> _.
transaction_bound_v1_decoding_test() ->
    Change =
        {transaction_bound, #{
            id => <<"id">>,
            timestamp => <<"timestamp">>,
            extra => #{<<"key">> => <<"value">>},
            additional_info => #{
                rrn => <<"value">>,
                approval_code => <<"value">>,
                acs_url => <<"value">>,
                pareq => <<"value">>,
                md => <<"value">>,
                term_url => <<"value">>,
                pares => <<"value">>,
                eci => <<"value">>,
                cavv => <<"value">>,
                xid => <<"value">>,
                cavv_algorithm => <<"value">>,
                three_ds_verification => authentication_successful
            }
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwwAAQ"
                "sAAQAAAAJpZAsAAgAAAAl0aW1lc3RhbXANAAMLCwAAAAEAAAADa2V5AAAA"
                "BXZhbHVlDAAECwABAAAABXZhbHVlCwACAAAABXZhbHVlCwADAAAABXZhbH"
                "VlCwAEAAAABXZhbHVlCwAFAAAABXZhbHVlCwAGAAAABXZhbHVlCwAHAAAA"
                "BXZhbHVlCwAIAAAABXZhbHVlCwAJAAAABXZhbHVlCwAKAAAABXZhbHVlCw"
                "ALAAAABXZhbHVlCAAMAAAAAAAAAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec finished_v1_decoding_test() -> _.
finished_v1_decoding_test() ->
    Change = {finished, success},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABAwAAQwAAQAAAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec callback_created_v1_decoding_test() -> _.
callback_created_v1_decoding_test() ->
    Change =
        {callback, #{
            tag => <<"tag">>,
            payload =>
                {created, #{
                    version => 1,
                    tag => <<"tag">>
                }}
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABQsAAQAAAAN0YWc"
                "MAAIMAAEMAAELAAEAAAADdGFnAAAAAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec callback_finished_v1_decoding_test() -> _.
callback_finished_v1_decoding_test() ->
    Change =
        {callback, #{
            tag => <<"tag">>,
            payload =>
                {finished, #{
                    payload => <<"payload">>
                }}
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABQsAAQAAAAN0YWc"
                "MAAIMAAMLAAEAAAAHcGF5bG9hZAAAAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec callback_status_changed_v1_decoding_test() -> _.
callback_status_changed_v1_decoding_test() ->
    Change =
        {callback, #{
            tag => <<"tag">>,
            payload => {status_changed, pending}
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABQsAAQAAAAN0YWc"
                "MAAIMAAIMAAEMAAEAAAAAAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec user_interaction_created_v1_decoding_test() -> _.
user_interaction_created_v1_decoding_test() ->
    Change =
        {user_interaction, #{
            id => <<"id">>,
            payload =>
                {created, #{
                    version => 1,
                    id => <<"id">>,
                    content =>
                        {redirect, #{
                            content => {get, <<"url">>}
                        }}
                }}
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABgsAAQAAAAJpZAwAA"
                "gwAAQwAAQsAAQAAAAJpZAwAAgwAAQwAAQsAAQAAAAN1cmwAAAAAAAAAAAA="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec user_interaction_status_changed_v1_decoding_test() -> _.
user_interaction_status_changed_v1_decoding_test() ->
    Change =
        {user_interaction, #{
            id => <<"id">>,
            payload => {status_changed, pending}
        }},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABgsAAQAAAAJpZAwAA"
                "gwAAgwAAQwAAQAAAAAAAAA="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-endif.
