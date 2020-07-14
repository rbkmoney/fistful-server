-module(ff_withdrawal_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([get_version/1]).
-export([marshal/3]).
-export([unmarshal/3]).

%% Constants

% TODO: Replace version to 3 after p2p provider migration
% see https://rbkmoney.atlassian.net/browse/MSPF-561 for details
-define(CURRENT_EVENT_FORMAT_VERSION, undefined).

%% Internal types

-type type() :: machinery_mg_schema:t().
-type value(T) :: machinery_mg_schema:v(T).
-type value_type() :: machinery_mg_schema:vt().

-type event()   :: ff_machine:timestamped_event(p2p_transfer:event()).
-type aux_state() :: ff_machine:auxst().
-type call_args() :: term().
-type call_response() :: term().
-type context() :: machinery_mg_schema:context().

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

-spec marshal_event(machinery_mg_schema:version(), event(), context()) ->
    {machinery_msgpack:t(), context()}.
marshal_event(undefined = Version, TimestampedChange, Context) ->
    % TODO: Remove this clause after MSPF-561 finish
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange, Context);
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = ff_withdrawal_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_p2p_transfer_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) ->
    {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_withdrawal_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_withdrawal_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context) ->
    {{ev, Timestamp, Change}, Context1} = machinery_mg_schema_generic:unmarshal(
        {event, Version},
        EncodedChange,
        Context
    ),
    {{ev, Timestamp, maybe_migrate(Change, Context1)}, Context1}.

maybe_migrate(Ev = {created, #{version := 3}}, _MigrateParams) ->
    Ev;
maybe_migrate({route_changed, Route}, _MigrateParams) ->
    {route_changed, maybe_migrate_route(Route)};
maybe_migrate({p_transfer, PEvent}, _MigrateParams) ->
    {p_transfer, ff_postings_transfer:maybe_migrate(PEvent, withdrawal)};
maybe_migrate({adjustment, _Payload} = Event, _MigrateParams) ->
    ff_adjustment_utils:maybe_migrate(Event);
maybe_migrate({resource_got, Resource}, _MigrateParams) ->
    {resource_got, ff_instrument:maybe_migrate_resource(Resource)};

% Old events
maybe_migrate({limit_check, {wallet, Details}}, MigrateParams) ->
    maybe_migrate({limit_check, {wallet_sender, Details}}, MigrateParams);
maybe_migrate({created, #{version := 1, handler := ff_withdrawal} = T}, MigrateParams) ->
    #{
        version     := 1,
        id          := ID,
        handler     := ff_withdrawal,
        body        := Body,
        params      := #{
            destination := DestinationID,
            source      := SourceID
        }
    } = T,
    Route = maps:get(route, T, undefined),
    maybe_migrate({created, genlib_map:compact(#{
        version       => 2,
        id            => ID,
        transfer_type => withdrawal,
        body          => Body,
        route         => maybe_migrate_route(Route),
        params        => #{
            wallet_id             => SourceID,
            destination_id        => DestinationID,
            % Fields below are required to correctly decode legacy events.
            % When decoding legacy events, the `erlang:binary_to_existing_atom/2` function is used,
            % so the code must contain atoms from the event.
            % They are not used now, so their value does not matter.
            wallet_account        => [],
            destination_account   => [],
            wallet_cash_flow_plan => []
        }
    })}, MigrateParams);
maybe_migrate({created, Withdrawal = #{version := 2, id := ID}}, MigrateParams) ->
    Ctx = maps:get(ctx, MigrateParams, undefined),
    Context = case Ctx of
        undefined ->
            {ok, State} = ff_machine:get(ff_withdrawal, 'ff/withdrawal_v2', ID, {undefined, 0, forward}),
            maps:get(ctx, State, undefined);
        Data ->
            Data
    end,
    maybe_migrate({created, genlib_map:compact(Withdrawal#{
        version => 3,
        metadata => ff_entity_context:try_get_legacy_metadata(Context)
    })}, MigrateParams);
maybe_migrate({created, T}, MigrateParams) ->
    DestinationID = maps:get(destination, T),
    SourceID = maps:get(source, T),
    ProviderID = maps:get(provider, T),
    maybe_migrate({created, T#{
        version     => 1,
        handler     => ff_withdrawal,
        route       => #{provider_id => ProviderID},
        params => #{
            destination => DestinationID,
            source      => SourceID
        }
    }}, MigrateParams);
maybe_migrate({transfer, PTransferEv}, MigrateParams) ->
    maybe_migrate({p_transfer, PTransferEv}, MigrateParams);
maybe_migrate({status_changed, {failed, LegacyFailure}}, MigrateParams) ->
    Failure = #{
        code => <<"unknown">>,
        reason => genlib:format(LegacyFailure)
    },
    maybe_migrate({status_changed, {failed, Failure}}, MigrateParams);
maybe_migrate({session_finished, SessionID}, MigrateParams) ->
    {ok, SessionMachine} = ff_withdrawal_session_machine:get(SessionID),
    Session = ff_withdrawal_session_machine:session(SessionMachine),
    {finished, Result} = ff_withdrawal_session:status(Session),
    maybe_migrate({session_finished, {SessionID, Result}}, MigrateParams);
% Other events
maybe_migrate(Ev, _MigrateParams) ->
    Ev.

maybe_migrate_route(undefined = Route) ->
    Route;
maybe_migrate_route(#{version := 1} = Route) ->
    Route;
maybe_migrate_route(Route) when not is_map_key(version, Route) ->
    LegacyIDs = #{
        <<"mocketbank">> => 1,
        <<"royalpay-payout">> => 2,
        <<"accentpay">> => 3
    },
    io:format("provider_id: ~p", [maps:get(provider_id, Route)]),
    case maps:get(provider_id, Route) of
        ProviderID when is_integer(ProviderID) ->
            Route#{
                version => 1,
                provider_id => ProviderID + 300,
                provider_id_legacy => genlib:to_binary(ProviderID)
            };
        ProviderID when is_binary(ProviderID) andalso is_map_key(ProviderID, LegacyIDs) ->
            ModernID = maps:get(ProviderID, LegacyIDs),
            Route#{
                version => 1,
                provider_id => ModernID + 300,
                provider_id_legacy => ProviderID
            };
        ProviderID when is_binary(ProviderID) ->
            Route#{
                version => 1,
                provider_id => erlang:binary_to_integer(ProviderID) + 300,
                provider_id_legacy => ProviderID
            }
    end.

get_aux_state_ctx(AuxState) when is_map(AuxState) ->
    maps:get(ctx, AuxState, undefined);
get_aux_state_ctx(_) ->
    undefined.

%% Tests

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

-spec v0_created_migration_test() -> _.
v0_created_migration_test() ->
    Withdrawal = #{
        body => {100,<<"RUB">>},
        id => <<"ID">>,
        metadata => #{<<"some key">> => <<"some val">>},
        params => #{
            destination_account => [],
            destination_id => <<"destinationID">>,
            wallet_account => [],
            wallet_cash_flow_plan => [],
            wallet_id => <<"sourceID">>
        },
        route => #{
            provider_id => 301,
            provider_id_legacy => <<"mocketbank">>,
            version => 1
        },
        transfer_type => withdrawal,
        version => 3
    },
    Change = {created, Withdrawal},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"created">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"id">>} => {bin, <<"ID">>},
                {str, <<"source">>} => {bin, <<"sourceID">>},
                {str, <<"destination">>} => {bin, <<"destinationID">>},
                {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 100}, {bin, <<"RUB">>}]},
                {str, <<"provider">>} => {bin, <<"mocketbank">>}
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

-spec v1_created_migration_test() -> _.
v1_created_migration_test() ->
    Withdrawal = #{
        body => {100,<<"RUB">>},
        id => <<"ID">>,
        metadata => #{<<"some key">> => <<"some val">>},
        params => #{
            destination_account => [],
            destination_id => <<"destinationID">>,
            wallet_account => [],
            wallet_cash_flow_plan => [],
            wallet_id => <<"walletID">>
        },
        transfer_type => withdrawal,
        version => 3
    },
    Change = {created, Withdrawal},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange = {arr, [
        {str,<<"tup">>},
        {str,<<"created">>},
        {arr, [
            {str,<<"map">>},
            {obj, #{
                {str,<<"body">>} => {arr, [
                    {str,<<"tup">>},
                    {i,100},
                    {bin,<<"RUB">>}
                ]},
                {str,<<"destination">>} => {arr, [
                    {str,<<"map">>},
                    {obj, #{
                        {str,<<"accounter_account_id">>} => {i,123},
                        {str,<<"currency">>} => {bin,<<"RUB">>},
                        {str,<<"id">>} => {bin,<<"destinationID">>},
                        {str,<<"identity">>} => {bin,<<"8FkoOxPRjbUXshllJieYV6qIjr3">>}
                    }}
                ]},
                {str,<<"handler">>} => {str,<<"ff_withdrawal">>},
                {str,<<"id">>} => {bin,<<"ID">>},
                {str,<<"params">>} => {arr, [
                    {str,<<"map">>},
                    {obj, #{
                        {str,<<"destination">>} => {bin,<<"destinationID">>},
                        {str,<<"source">>} => {bin,<<"walletID">>}
                    }}
                ]},
                {str,<<"source">>} => {arr, [
                    {str,<<"map">>},
                    {obj, #{
                        {str,<<"accounter_account_id">>} => {i,123},
                        {str,<<"currency">>} => {bin,<<"RUB">>},
                        {str,<<"id">>} => {bin,<<"walletID">>},
                        {str,<<"identity">>} => {bin,<<"Fy3g1eq99fZJBeQDHNPmCNCRu4X">>}
                    }}
                ]},
                {str,<<"version">>} => {i,1}
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


-spec v2_created_migration_test() -> _.
v2_created_migration_test() ->
    Withdrawal = #{body => {100,<<"RUB">>},
                     id => <<"ID">>,
                     metadata => #{<<"some key">> => <<"some val">>},
                     params =>
                      #{destination_account =>
                         #{accounter_account_id => 123,currency => <<"RUB">>,
                           id => <<"destinationID">>,
                           identity => <<"identityID2">>},
                        destination_id => <<"destinationID">>,
                        wallet_account =>
                         #{accounter_account_id => 123,currency => <<"RUB">>,
                           id => <<"walletID">>,identity => <<"identityID">>},
                        wallet_cash_flow_plan =>
                         #{postings =>
                            [#{receiver => {wallet,receiver_destination},
                               sender => {wallet,sender_settlement},
                               volume =>
                                {share,{{1,1},operation_amount,default}}}]},
                        wallet_id => <<"walletID">>},
                     transfer_type => withdrawal,version => 3},
    Change = {created, Withdrawal},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange = {arr, [
        {str,<<"tup">>},
        {str,<<"created">>},
        {arr, [
            {str,<<"map">>},
            {obj, #{
                {str,<<"body">>} => {arr,[{str,<<"tup">>},{i,100},{bin,<<"RUB">>}]},
                {str,<<"id">>} => {bin,<<"ID">>},
                {str,<<"params">>} => {arr, [
                    {str,<<"map">>},
                    {obj, #{
                        {str,<<"destination_account">>} => {arr, [
                            {str,<<"map">>},
                            {obj, #{
                                {str,<<"accounter_account_id">>} => {i,123},
                                {str,<<"currency">>} => {bin,<<"RUB">>},
                                {str,<<"id">>} => {bin,<<"destinationID">>},
                                {str,<<"identity">>} => {bin,<<"identityID2">>}
                            }}
                        ]},
                    {str,<<"destination_id">>} => {bin,<<"destinationID">>},
                    {str,<<"wallet_account">>} => {arr, [
                        {str,<<"map">>},
                        {obj, #{
                            {str,<<"accounter_account_id">>} => {i,123},
                            {str,<<"currency">>} => {bin,<<"RUB">>},
                            {str,<<"id">>} => {bin,<<"walletID">>},
                            {str,<<"identity">>} => {bin,<<"identityID">>}
                        }}
                    ]},
                    {str,<<"wallet_cash_flow_plan">>} => {arr, [
                        {str,<<"map">>},
                        {obj, #{
                            {str,<<"postings">>} => {arr, [
                                {str,<<"lst">>},
                                {arr, [
                                    {str,<<"map">>},
                                    {obj, #{
                                        {str,<<"receiver">>} => {arr, [
                                            {str,<<"tup">>},
                                            {str,<<"wallet">>},
                                            {str,<<"receiver_destination">>}
                                        ]},
                                        {str,<<"sender">>} => {arr, [
                                            {str,<<"tup">>},
                                            {str,<<"wallet">>},
                                            {str,<<"sender_settlement">>}
                                        ]},
                                        {str,<<"volume">>} => {arr, [
                                            {str,<<"tup">>},
                                            {str,<<"share">>},
                                            {arr, [
                                                {str,<<"tup">>},
                                                {arr,[{str,<<"tup">>},{i,1},{i,1}]},
                                                {str,<<"operation_amount">>},
                                                {str,<<"default">>}
                                            ]}
                                        ]}
                                    }}
                                ]}
                            ]}
                        }}
                    ]},
                    {str,<<"wallet_id">>} => {bin,<<"walletID">>}}
                    }]
                },
                {str,<<"transfer_type">>} => {str,<<"withdrawal">>},
                {str,<<"version">>} => {i,2}
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

-spec v3_created_migration_test() -> _.
v3_created_migration_test() ->
    Withdrawal = #{body => {100,<<"RUB">>},
                     id => <<"ID">>,
                     metadata => #{<<"some key">> => <<"some val">>},
                     params =>
                      #{destination_account =>
                         #{accounter_account_id => 123,currency => <<"RUB">>,
                           id => <<"destinationID">>,
                           identity => <<"identity2">>},
                        destination_id => <<"destinationID">>,
                        wallet_account =>
                         #{accounter_account_id => 123,currency => <<"RUB">>,
                           id => <<"walletID">>,identity => <<"identity">>},
                        wallet_cash_flow_plan =>
                         #{postings =>
                            [#{receiver => {wallet,receiver_destination},
                               sender => {wallet,sender_settlement},
                               volume =>
                                {share,{{1,1},operation_amount,default}}}]},
                        wallet_id => <<"walletID">>},
                     transfer_type => withdrawal,version => 3},
    Change = {created, Withdrawal},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange = {arr, [
        {str,<<"tup">>},
        {str,<<"created">>},
        {arr, [
            {str,<<"map">>},
            {obj, #{
                {str,<<"body">>} => {arr,[{str,<<"tup">>},{i,100},{bin,<<"RUB">>}]},
                {str,<<"id">>} => {bin,<<"ID">>},
                {str,<<"params">>} => {arr, [
                    {str,<<"map">>},
                    {obj, #{
                        {str,<<"destination_account">>} => {arr, [
                            {str,<<"map">>},
                            {obj, #{
                                {str,<<"accounter_account_id">>} => {i,123},
                                {str,<<"currency">>} => {bin,<<"RUB">>},
                                {str,<<"id">>} => {bin,<<"destinationID">>},
                                {str,<<"identity">>} => {bin,<<"identity2">>}
                            }}
                        ]},
                        {str,<<"destination_id">>} => {bin,<<"destinationID">>},
                        {str,<<"wallet_account">>} => {arr, [
                            {str,<<"map">>},
                            {obj, #{
                                {str,<<"accounter_account_id">>} => {i,123},
                                {str,<<"currency">>} => {bin,<<"RUB">>},
                                {str,<<"id">>} => {bin,<<"walletID">>},
                                {str,<<"identity">>} => {bin,<<"identity">>}
                            }}
                        ]},
                        {str,<<"wallet_cash_flow_plan">>} => {arr,  [
                            {str,<<"map">>},
                            {obj, #{
                                {str,<<"postings">>} => {arr, [
                                    {str,<<"lst">>},
                                    {arr, [
                                        {str,<<"map">>},
                                        {obj, #{
                                            {str,<<"receiver">>} => {arr, [
                                                {str,<<"tup">>},
                                                {str,<<"wallet">>},
                                                {str,<<"receiver_destination">>}
                                            ]},
                                            {str,<<"sender">>} => {arr, [
                                                {str,<<"tup">>},
                                                {str,<<"wallet">>},
                                                {str,<<"sender_settlement">>}
                                            ]},
                                            {str,<<"volume">>} => {arr, [
                                                {str,<<"tup">>},
                                                {str,<<"share">>},
                                                {arr, [
                                                    {str,<<"tup">>},
                                                    {arr,[{str,<<"tup">>},{i,1},{i,1}]},
                                                    {str,<<"operation_amount">>},
                                                    {str,<<"default">>}
                                                ]}
                                            ]}
                                        }}
                                    ]}
                                ]}
                            }}
                        ]},
                        {str,<<"wallet_id">>} => {bin,<<"walletID">>}
                    }}
                ]},
                {str,<<"transfer_type">>} => {str,<<"withdrawal">>},
                {str,<<"version">>} => {i,2}
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

-spec v0_route_changed_migration_test() -> _.
v0_route_changed_migration_test() ->
    LegacyEvent = {route_changed, #{provider_id => 5}},
    ModernEvent = {route_changed, #{
        version => 1,
        provider_id => 305,
        provider_id_legacy => <<"5">>
    }},
    ?assertEqual(ModernEvent, maybe_migrate(LegacyEvent, #{})).

-endif.
