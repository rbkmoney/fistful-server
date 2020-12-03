-module(ff_p2p_transfer_machinery_schema).

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
-type context() :: machinery_mg_schema:context().

-type event() :: ff_machine:timestamped_event(p2p_transfer:event()).
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
    % TODO: Remove this clause after MSPF-561 finish
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange, Context);
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = ff_p2p_transfer_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_p2p_transfer_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_p2p_transfer_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_p2p_transfer_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context0) ->
    {Event, Context1} = machinery_mg_schema_generic:unmarshal({event, Version}, EncodedChange, Context0),
    {ev, Timestamp, Change} = Event,
    {{ev, Timestamp, maybe_migrate(Change)}, Context1}.

-spec maybe_migrate(any()) -> p2p_transfer:event().
maybe_migrate({resource_got, Sender, Receiver}) ->
    {resource_got, maybe_migrate_resource(Sender), maybe_migrate_resource(Receiver)};
maybe_migrate({route_changed, Route}) when not is_map_key(version, Route) ->
    #{
        provider_id := LegacyProviderID
    } = Route,
    maybe_migrate({route_changed, Route#{version => 1, provider_id => LegacyProviderID + 400}});
maybe_migrate({created, #{version := 1} = Transfer}) ->
    #{
        version := 1,
        sender := Sender,
        receiver := Receiver
    } = Transfer,
    maybe_migrate(
        {created,
            genlib_map:compact(Transfer#{
                version := 2,
                sender => maybe_migrate_participant(Sender),
                receiver => maybe_migrate_participant(Receiver)
            })}
    );
maybe_migrate({created, #{version := 2} = Transfer}) ->
    #{
        sender := Sender,
        receiver := Receiver
    } = Transfer,
    Quote = maps:get(quote, Transfer, undefined),
    {created,
        genlib_map:compact(Transfer#{
            version => 3,
            sender => maybe_migrate_participant(Sender),
            receiver => maybe_migrate_participant(Receiver),
            quote => maybe_migrate_quote(Quote)
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

maybe_migrate_quote(undefined) ->
    undefined;
maybe_migrate_quote(Quote) when not is_map_key(fees, Quote) ->
    #{
        created_at := CreatedAt,
        expires_on := ExpiresOn,
        sender := {bank_card, #{bin_data_id := SenderBinDataID}},
        receiver := {bank_card, #{bin_data_id := ReceiverBinDataID}}
    } = Quote,
    #{
        created_at => CreatedAt,
        expires_on => ExpiresOn,
        fees => #{fees => #{}},
        sender => {bank_card, SenderBinDataID},
        receiver => {bank_card, ReceiverBinDataID}
    };
maybe_migrate_quote(Quote) when is_map_key(fees, Quote) ->
    Quote.

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

% tests helpers

-spec marshal(type(), value(data())) -> machinery_msgpack:t().

marshal(Type, Value) ->
    {Result, _Context} = marshal(Type, Value, #{}),
    Result.

-spec unmarshal(type(), machinery_msgpack:t()) -> data().
unmarshal(Type, Value) ->
    {Result, _Context} = unmarshal(Type, Value, #{}),
    Result.

-spec created_v0_1_decoding_test() -> _.
created_v0_1_decoding_test() ->
    Resource1 =
        {crypto_wallet, #{
            crypto_wallet => #{
                id => <<"address">>,
                currency => {bitcoin_cash, #{}}
            }
        }},
    Resource2 =
        {bank_card, #{
            bank_card => #{
                token => <<"token">>,
                bin_data_id => {binary, <<"bin">>}
            }
        }},
    Participant1 =
        {raw, #{
            resource_params => Resource1,
            contact_info => #{}
        }},
    Participant2 =
        {raw, #{
            resource_params => Resource2,
            contact_info => #{}
        }},
    P2PTransfer = #{
        version => 3,
        id => <<"transfer">>,
        status => pending,
        owner => <<"owner">>,
        body => {123, <<"RUB">>},
        created_at => 1590426777985,
        sender => Participant1,
        receiver => Participant2,
        quote => #{
            fees => #{fees => #{}},
            created_at => 1590426777986,
            expires_on => 1590426777986,
            sender => {bank_card, 1},
            receiver => {bank_card, 2}
        },
        domain_revision => 123,
        party_revision => 321,
        operation_timestamp => 1590426777986,
        external_id => <<"external_id">>,
        deadline => 1590426777987
    },
    Change = {created, P2PTransfer},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    ResourceParams1 =
        {arr, [
            {str, <<"tup">>},
            {str, <<"crypto_wallet">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"currency">>} =>
                        {arr, [
                            {str, <<"tup">>},
                            {str, <<"bitcoin_cash">>},
                            {arr, [{str, <<"map">>}, {obj, #{}}]}
                        ]},
                    {str, <<"id">>} => {bin, <<"address">>}
                }}
            ]}
        ]},
    ResourceParams2 =
        {arr, [
            {str, <<"tup">>},
            {str, <<"bank_card">>},
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
        ]},
    LegacyParticipant1 =
        {arr, [
            {str, <<"tup">>},
            {str, <<"raw">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"contact_info">>} => {arr, [{str, <<"map">>}, {obj, #{}}]},
                    {str, <<"resource_params">>} => ResourceParams1
                }}
            ]}
        ]},
    LegacyParticipant2 =
        {arr, [
            {str, <<"tup">>},
            {str, <<"raw">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"resource_params">>} => ResourceParams2
                }}
            ]}
        ]},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"version">>} => {i, 1},
                    {str, <<"id">>} => {bin, <<"transfer">>},
                    {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                    {str, <<"created_at">>} => {i, 1590426777985},
                    {str, <<"deadline">>} => {i, 1590426777987},
                    {str, <<"domain_revision">>} => {i, 123},
                    {str, <<"external_id">>} => {bin, <<"external_id">>},
                    {str, <<"operation_timestamp">>} => {i, 1590426777986},
                    {str, <<"owner">>} => {bin, <<"owner">>},
                    {str, <<"party_revision">>} => {i, 321},
                    {str, <<"quote">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"created_at">>} => {i, 1590426777986},
                                {str, <<"expires_on">>} => {i, 1590426777986},
                                {str, <<"sender">>} =>
                                    {arr, [
                                        {str, <<"tup">>},
                                        {str, <<"bank_card">>},
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {str, <<"token">>} => {bin, <<"123">>},
                                                {str, <<"bin_data_id">>} => {i, 1}
                                            }}
                                        ]}
                                    ]},
                                {str, <<"receiver">>} =>
                                    {arr, [
                                        {str, <<"tup">>},
                                        {str, <<"bank_card">>},
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {str, <<"token">>} => {bin, <<"123">>},
                                                {str, <<"bin_data_id">>} => {i, 2}
                                            }}
                                        ]}
                                    ]}
                            }}
                        ]},
                    {str, <<"receiver">>} => LegacyParticipant2,
                    {str, <<"sender">>} => LegacyParticipant1,
                    {str, <<"status">>} => {str, <<"pending">>}
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

-spec created_v0_2_decoding_test() -> _.
created_v0_2_decoding_test() ->
    Resource =
        {bank_card, #{
            bank_card => #{
                token => <<"token">>,
                bin_data_id => {binary, <<"bin">>}
            }
        }},
    Participant =
        {raw, #{
            resource_params => Resource,
            contact_info => #{}
        }},
    P2PTransfer = #{
        version => 3,
        id => <<"transfer">>,
        status => pending,
        owner => <<"owner">>,
        body => {123, <<"RUB">>},
        created_at => 1590426777985,
        sender => Participant,
        receiver => Participant,
        quote => #{
            fees => #{fees => #{}},
            created_at => 1590426777986,
            expires_on => 1590426777986,
            sender => {bank_card, 1},
            receiver => {bank_card, 2}
        },
        domain_revision => 123,
        party_revision => 321,
        operation_timestamp => 1590426777986,
        external_id => <<"external_id">>,
        deadline => 1590426777987
    },
    Change = {created, P2PTransfer},
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
    LegacyParticipant1 =
        {arr, [
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
    LegacyParticipant2 =
        {arr, [
            {str, <<"tup">>},
            {str, <<"raw">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"resource_params">>} => ResourceParams
                }}
            ]}
        ]},
    LegacyChange =
        {arr, [
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
                    {str, <<"quote">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"created_at">>} => {i, 1590426777986},
                                {str, <<"expires_on">>} => {i, 1590426777986},
                                {str, <<"sender">>} =>
                                    {arr, [
                                        {str, <<"tup">>},
                                        {str, <<"bank_card">>},
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {str, <<"token">>} => {bin, <<"123">>},
                                                {str, <<"bin_data_id">>} => {i, 1}
                                            }}
                                        ]}
                                    ]},
                                {str, <<"receiver">>} =>
                                    {arr, [
                                        {str, <<"tup">>},
                                        {str, <<"bank_card">>},
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {str, <<"token">>} => {bin, <<"123">>},
                                                {str, <<"bin_data_id">>} => {i, 2}
                                            }}
                                        ]}
                                    ]}
                            }}
                        ]},
                    {str, <<"receiver">>} => LegacyParticipant1,
                    {str, <<"sender">>} => LegacyParticipant2,
                    {str, <<"status">>} => {str, <<"pending">>}
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

-spec created_v0_3_decoding_test() -> _.
created_v0_3_decoding_test() ->
    Resource =
        {bank_card, #{
            bank_card => #{
                token => <<"token">>,
                bin_data_id => {binary, <<"bin">>}
            }
        }},
    Participant =
        {raw, #{
            resource_params => Resource,
            contact_info => #{}
        }},
    P2PTransfer = #{
        version => 3,
        id => <<"transfer">>,
        status => pending,
        owner => <<"owner">>,
        body => {123, <<"RUB">>},
        created_at => 1590426777985,
        sender => Participant,
        receiver => Participant,
        quote => #{
            fees => #{
                fees => #{
                    surplus => {123, <<"RUB">>}
                }
            },
            created_at => 1590426777986,
            expires_on => 1590426777986,
            sender => {bank_card, 1},
            receiver => {bank_card, 2}
        },
        domain_revision => 123,
        party_revision => 321,
        operation_timestamp => 1590426777986,
        external_id => <<"external_id">>,
        deadline => 1590426777987
    },
    Change = {created, P2PTransfer},
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
    LegacyParticipant1 =
        {arr, [
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
    LegacyParticipant2 =
        {arr, [
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
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"version">>} => {i, 3},
                    {str, <<"id">>} => {bin, <<"transfer">>},
                    {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                    {str, <<"created_at">>} => {i, 1590426777985},
                    {str, <<"deadline">>} => {i, 1590426777987},
                    {str, <<"domain_revision">>} => {i, 123},
                    {str, <<"external_id">>} => {bin, <<"external_id">>},
                    {str, <<"operation_timestamp">>} => {i, 1590426777986},
                    {str, <<"owner">>} => {bin, <<"owner">>},
                    {str, <<"party_revision">>} => {i, 321},
                    {str, <<"quote">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"created_at">>} => {i, 1590426777986},
                                {str, <<"expires_on">>} => {i, 1590426777986},
                                {str, <<"fees">>} =>
                                    {arr, [
                                        {str, <<"map">>},
                                        {obj, #{
                                            {str, <<"fees">>} =>
                                                {arr, [
                                                    {str, <<"map">>},
                                                    {obj, #{
                                                        {str, <<"surplus">>} =>
                                                            {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]}
                                                    }}
                                                ]}
                                        }}
                                    ]},
                                {str, <<"sender">>} =>
                                    {arr, [
                                        {str, <<"tup">>},
                                        {str, <<"bank_card">>},
                                        {i, 1}
                                    ]},
                                {str, <<"receiver">>} =>
                                    {arr, [
                                        {str, <<"tup">>},
                                        {str, <<"bank_card">>},
                                        {i, 2}
                                    ]}
                            }}
                        ]},
                    {str, <<"receiver">>} => LegacyParticipant1,
                    {str, <<"sender">>} => LegacyParticipant2,
                    {str, <<"status">>} => {str, <<"pending">>}
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

-spec resource_got_v0_0_decoding_test() -> _.
resource_got_v0_0_decoding_test() ->
    Resource =
        {bank_card, #{
            bank_card => #{
                token => <<"token">>,
                bin_data_id => {binary, <<"bin">>}
            }
        }},
    Change = {resource_got, Resource, Resource},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyResource =
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
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"resource_got">>},
            LegacyResource,
            LegacyResource
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

-spec risk_score_changed_v0_0_decoding_test() -> _.
risk_score_changed_v0_0_decoding_test() ->
    Change = {risk_score_changed, low},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"risk_score_changed">>},
            {str, <<"low">>}
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

-spec route_changed_v0_0_decoding_test() -> _.
route_changed_v0_0_decoding_test() ->
    Change = {route_changed, #{version => 1, provider_id => 401}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"route_changed">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{{str, <<"provider_id">>} => {i, 1}}}
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

-spec route_changed_v0_1_decoding_test() -> _.
route_changed_v0_1_decoding_test() ->
    Change = {route_changed, #{version => 1, provider_id => 1}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"route_changed">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"provider_id">>} => {i, 1},
                    {str, <<"version">>} => {i, 1}
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

-spec p_transfer_v0_0_decoding_test() -> _.
p_transfer_v0_0_decoding_test() ->
    PTransfer = #{
        id => <<"external_id">>,
        final_cash_flow => #{
            postings => []
        }
    },
    Change = {p_transfer, {created, PTransfer}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"p_transfer">>},
            {arr, [
                {str, <<"tup">>},
                {str, <<"created">>},
                {arr, [
                    {str, <<"map">>},
                    {obj, #{
                        {str, <<"final_cash_flow">>} =>
                            {arr, [
                                {str, <<"map">>},
                                {obj, #{{str, <<"postings">>} => {arr, []}}}
                            ]},
                        {str, <<"id">>} => {bin, <<"external_id">>}
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

-spec session_v0_0_decoding_test() -> _.
session_v0_0_decoding_test() ->
    Change = {session, {<<"session_id">>, started}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"session">>},
            {arr, [
                {str, <<"tup">>},
                {bin, <<"session_id">>},
                {str, <<"started">>}
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
    Participant =
        {raw, #{
            resource_params => Resource,
            contact_info => #{}
        }},
    P2PTransfer = #{
        version => 3,
        id => <<"transfer">>,
        status => pending,
        owner => <<"owner">>,
        body => {123, <<"RUB">>},
        created_at => 1590426777985,
        sender => Participant,
        receiver => Participant,
        quote => #{
            fees => #{
                fees => #{
                    surplus => {200, <<"RUB">>}
                }
            },
            created_at => 1590426777986,
            expires_on => 1590426787986,
            sender => {bank_card, 1},
            receiver => {bank_card, 2}
        },
        domain_revision => 123,
        party_revision => 321,
        operation_timestamp => 1590426777986,
        external_id => <<"external_id">>,
        deadline => 1590426777987
    },
    Change = {created, P2PTransfer},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAQwAAQsADwAAAAh0cmFuc2ZlcgsAAQAAAAVvd2"
                "5lcgwAAgwAAQwAAQwAAQwAAQsAAQAAAAV0b2tlbgwAFQsABgAAAANiaW4AAAAADAACAAAADAADDAABDAABDAABDAAB"
                "CwABAAAABXRva2VuDAAVCwAGAAAAA2JpbgAAAAAMAAIAAAAMAAQKAAEAAAAAAAAAewwAAgsAAQAAAANSVUIAAAwABQ"
                "wAAQAACwAGAAAAGDIwMjAtMDUtMjVUMTc6MTI6NTcuOTg1WgoABwAAAAAAAAB7CgAIAAAAAAAAAUELAAkAAAAYMjAy"
                "MC0wNS0yNVQxNzoxMjo1Ny45ODZaDAAKCwABAAAAGDIwMjAtMDUtMjVUMTc6MTI6NTcuOTg2WgsAAgAAABgyMDIwLT"
                "A1LTI1VDE3OjEzOjA3Ljk4NloMAAMNAAEIDAAAAAEAAAABCgABAAAAAAAAAMgMAAILAAEAAAADUlVCAAAADAAEDAAB"
                "DAABCgADAAAAAAAAAAEAAAAMAAUMAAEMAAEKAAMAAAAAAAAAAgAAAAALAAsAAAALZXh0ZXJuYWxfaWQLAAwAAAAYMj"
                "AyMC0wNS0yNVQxNzoxMjo1Ny45ODdaAAAAAA=="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec resource_got_v1_decoding_test() -> _.
resource_got_v1_decoding_test() ->
    Resource =
        {bank_card, #{
            bank_card => #{
                token => <<"token">>,
                bin_data_id => {binary, <<"bin">>}
            }
        }},
    Change = {resource_got, Resource, Resource},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwwAAQwAAQwAAQwAAQwAAQsAAQAAAAV0b2tlbg"
                "wAFQsABgAAAANiaW4AAAAADAACDAABDAABCwABAAAABXRva2VuDAAVCwAGAAAAA2JpbgAAAAAAAAAAAA=="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec risk_score_changed_v1_decoding_test() -> _.
risk_score_changed_v1_decoding_test() ->
    Change = {risk_score_changed, low},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABAgAAQAAAAEAAAA="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec route_changed_v1_decoding_test() -> _.
route_changed_v1_decoding_test() ->
    Change = {route_changed, #{version => 1, provider_id => 1}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABQwAAQgAAgAAAAEAAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec p_transfer_v1_decoding_test() -> _.
p_transfer_v1_decoding_test() ->
    PTransfer = #{
        id => <<"external_id">>,
        final_cash_flow => #{
            postings => []
        }
    },
    Change = {p_transfer, {created, PTransfer}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABgwAAQwAAQwAAQsAAgAAAAtleHRlcm5hbF9pZA"
                "wAAQ8AAQwAAAAAAAAAAAAAAA=="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec session_v1_decoding_test() -> _.
session_v1_decoding_test() ->
    Change = {session, {<<"session_id">>, started}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABwsAAQAAAApzZXNzaW9uX2lkDAACDAABAAAAAAA="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-endif.
