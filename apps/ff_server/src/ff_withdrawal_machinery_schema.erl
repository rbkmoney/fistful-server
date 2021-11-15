-module(ff_withdrawal_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

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

-type event() :: ff_machine:timestamped_event(ff_withdrawal:event()).
-type aux_state() :: ff_machine:auxst().
-type call_args() :: term().
-type call_response() :: term().
-type context() :: machinery_mg_schema:context().

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
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = ff_withdrawal_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_withdrawal_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {event(), context()}.
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

maybe_migrate(Ev = {created, #{version := 4}}, _MigrateParams) ->
    Ev;
maybe_migrate({route_changed, Route}, _MigrateParams) ->
    {route_changed, maybe_migrate_route(Route)};
maybe_migrate({p_transfer, PEvent}, _MigrateParams) ->
    {p_transfer, ff_postings_transfer:maybe_migrate(PEvent, withdrawal)};
maybe_migrate({adjustment, _Payload} = Event, _MigrateParams) ->
    ff_adjustment_utils:maybe_migrate(Event);
maybe_migrate({resource_got, Resource}, _MigrateParams) ->
    {resource_got, ff_destination:maybe_migrate_resource(Resource)};
% Old events
maybe_migrate({limit_check, {wallet, Details}}, MigrateParams) ->
    maybe_migrate({limit_check, {wallet_sender, Details}}, MigrateParams);
maybe_migrate({created, #{version := 1, handler := ff_withdrawal} = T}, MigrateParams) ->
    #{
        version := 1,
        id := ID,
        handler := ff_withdrawal,
        body := Body,
        params := #{
            destination := DestinationID,
            source := SourceID
        }
    } = T,
    Route = maps:get(route, T, undefined),
    maybe_migrate(
        {created,
            genlib_map:compact(#{
                version => 2,
                id => ID,
                transfer_type => withdrawal,
                body => Body,
                route => maybe_migrate_route(Route),
                params => #{
                    wallet_id => SourceID,
                    destination_id => DestinationID,
                    % Fields below are required to correctly decode legacy events.
                    % When decoding legacy events, the `erlang:binary_to_existing_atom/2` function is used,
                    % so the code must contain atoms from the event.
                    % They are not used now, so their value does not matter.
                    wallet_account => [],
                    destination_account => [],
                    wallet_cash_flow_plan => []
                }
            })},
        MigrateParams
    );
maybe_migrate({created, Withdrawal = #{version := 2, id := ID}}, MigrateParams) ->
    Ctx = maps:get(ctx, MigrateParams, undefined),
    Context =
        case Ctx of
            undefined ->
                {ok, State} = ff_machine:get(ff_withdrawal, 'ff/withdrawal_v2', ID, {undefined, 0, forward}),
                maps:get(ctx, State, undefined);
            Data ->
                Data
        end,
    maybe_migrate(
        {created,
            genlib_map:compact(Withdrawal#{
                version => 3,
                metadata => ff_entity_context:try_get_legacy_metadata(Context)
            })},
        MigrateParams
    );
maybe_migrate({created, Withdrawal = #{version := 3}}, MigrateParams) ->
    Params = maps:get(params, Withdrawal),
    Quote = maps:get(quote, Params, undefined),
    maybe_migrate(
        {created,
            genlib_map:compact(Withdrawal#{
                version => 4,
                params => genlib_map:compact(Params#{quote => maybe_migrate_quote(Quote)})
            })},
        MigrateParams
    );
maybe_migrate({created, T}, MigrateParams) ->
    DestinationID = maps:get(destination, T),
    SourceID = maps:get(source, T),
    Route =
        case maps:get(provider, T) of
            TRoute when is_map(TRoute) ->
                TRoute;
            ProviderID when is_binary(ProviderID) ->
                #{provider_id => ProviderID}
        end,
    maybe_migrate(
        {created, T#{
            version => 1,
            handler => ff_withdrawal,
            route => Route,
            params => #{
                destination => DestinationID,
                source => SourceID
            }
        }},
        MigrateParams
    );
maybe_migrate({transfer, PTransferEv}, MigrateParams) ->
    maybe_migrate({p_transfer, PTransferEv}, MigrateParams);
maybe_migrate({status_changed, {failed, Failure}}, _MigrateParams) when is_map(Failure) ->
    {status_changed, {failed, Failure}};
maybe_migrate({status_changed, {failed, LegacyFailure}}, MigrateParams) ->
    KnownFailures = #{
        {quote, inconsistent_data} => <<"unknown">>
    },
    Failure = #{
        code => maps:get(LegacyFailure, KnownFailures, <<"unknown">>),
        reason => genlib:format(LegacyFailure)
    },
    maybe_migrate({status_changed, {failed, Failure}}, MigrateParams);
maybe_migrate({session_finished, {SessionID, Result}}, _MigrateParams) ->
    {session_finished, {SessionID, Result}};
maybe_migrate({session_finished, SessionID}, MigrateParams) ->
    {ok, SessionMachine} = ff_withdrawal_session_machine:get(SessionID),
    Session = ff_withdrawal_session_machine:session(SessionMachine),
    Result = ff_withdrawal_session:result(Session),
    maybe_migrate({session_finished, {SessionID, Result}}, MigrateParams);
% Other events
maybe_migrate(Ev, _MigrateParams) ->
    Ev.

maybe_migrate_route(undefined = Route) ->
    Route;
maybe_migrate_route(#{version := 1} = Route) ->
    Route;
maybe_migrate_route(Route) when is_map_key(provider_id, Route) andalso not is_map_key(version, Route) ->
    LegacyIDs = #{
        <<"mocketbank">> => 1,
        <<"royalpay">> => 2,
        <<"royalpay-payout">> => 2,
        <<"accentpay">> => 3
    },
    NewRoute =
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
        end,
    maybe_migrate_route(NewRoute);
maybe_migrate_route(Route) when is_map_key(adapter, Route) ->
    #{
        adapter := #{
            url := Url,
            event_handler := scoper_woody_event_handler
        },
        adapter_opts := #{}
    } = Route,
    LegacyUrls = #{
        <<"http://proxy-mocketbank:8022/proxy/mocketbank/p2p-credit">> => <<"mocketbank">>
    },
    maybe_migrate_route(#{provider_id => maps:get(Url, LegacyUrls)}).

maybe_migrate_quote(undefined) ->
    undefined;
maybe_migrate_quote(#{quote_data := #{<<"version">> := 1}} = Quote) when not is_map_key(route, Quote) ->
    #{
        cash_from := CashFrom,
        cash_to := CashTo,
        created_at := CreatedAt,
        expires_on := ExpiresOn,
        quote_data := WQuoteData
    } = Quote,
    #{
        <<"version">> := 1,
        <<"quote_data">> := QuoteData
    } = WQuoteData,
    TerminalID = maps:get(<<"terminal_id">>, WQuoteData, undefined),
    Timestamp = maps:get(<<"timestamp">>, WQuoteData, undefined),
    ResourceID = maps:get(<<"resource_id">>, WQuoteData, undefined),
    LegacyIDs = #{
        <<"mocketbank">> => 1,
        <<"royalpay">> => 2,
        <<"royalpay-payout">> => 2,
        <<"accentpay">> => 3
    },
    ModernProviderID =
        case maps:get(<<"provider_id">>, WQuoteData) of
            ProviderID when is_integer(ProviderID) andalso ProviderID > 300 ->
                ProviderID;
            ProviderID when is_integer(ProviderID) andalso ProviderID =< 300 ->
                ProviderID + 300;
            ProviderID when is_binary(ProviderID) andalso is_map_key(ProviderID, LegacyIDs) ->
                maps:get(ProviderID, LegacyIDs) + 300;
            ProviderID when is_binary(ProviderID) ->
                erlang:binary_to_integer(ProviderID) + 300
        end,
    genlib_map:compact(#{
        cash_from => CashFrom,
        cash_to => CashTo,
        created_at => CreatedAt,
        expires_on => ExpiresOn,
        quote_data => QuoteData,
        route => ff_withdrawal_routing:make_route(ModernProviderID, TerminalID),
        operation_timestamp => Timestamp,
        resource_descriptor => decode_legacy_resource_id(ResourceID)
    });
maybe_migrate_quote(Quote) when is_map_key(route, Quote) ->
    Quote.

decode_legacy_resource_id(undefined) ->
    undefined;
decode_legacy_resource_id(#{<<"bank_card">> := ID}) ->
    {bank_card, ID}.

get_aux_state_ctx(AuxState) when is_map(AuxState) ->
    maps:get(ctx, AuxState, undefined);
get_aux_state_ctx(_) ->
    undefined.

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

-spec created_v0_0_without_provider_migration_test() -> _.
created_v0_0_without_provider_migration_test() ->
    Withdrawal = #{
        body => {1000, <<"RUB">>},
        id => <<"ID">>,
        metadata => #{<<"some key">> => <<"some val">>},
        params => #{
            destination_id => <<"destinationID">>,
            wallet_id => <<"sourceID">>
        },
        route => #{
            provider_id => 301,
            provider_id_legacy => <<"mocketbank">>,
            version => 1
        },
        transfer_type => withdrawal,
        version => 4
    },
    Change = {created, Withdrawal},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
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
            {arr, [
                {str, <<"tup">>},
                {str, <<"created">>},
                {arr, [
                    {str, <<"map">>},
                    {obj, #{
                        {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 1000}, {bin, <<"RUB">>}]},
                        {str, <<"destination">>} => {bin, <<"destinationID">>},
                        {str, <<"id">>} => {bin, <<"ID">>},
                        {str, <<"provider">>} =>
                            {arr, [
                                {str, <<"map">>},
                                {obj, #{
                                    {str, <<"adapter">>} =>
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {str, <<"event_handler">>} => {str, <<"scoper_woody_event_handler">>},
                                                {str, <<"url">>} =>
                                                    {bin,
                                                        <<"http://proxy-mocketbank:8022/proxy/mocketbank/p2p-credit">>}
                                            }}
                                        ]},
                                    {str, <<"adapter_opts">>} =>
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {bin, <<"term_id">>} => {bin, <<"30001018">>},
                                                {bin, <<"version">>} => {bin, <<"109">>}
                                            }}
                                        ]}
                                }}
                            ]},
                        {str, <<"source">>} => {bin, <<"sourceID">>}
                    }}
                ]}
            ]}
        ]},
    {DecodedLegacy, _} = unmarshal(
        {event, undefined},
        LegacyEvent,
        make_legacy_context(#{
            ctx => #{
                <<"com.rbkmoney.wapi">> => #{
                    <<"metadata">> => #{
                        <<"some key">> => <<"some val">>
                    }
                }
            }
        })
    ),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_v0_0_migration_test() -> _.
created_v0_0_migration_test() ->
    Withdrawal = #{
        body => {100, <<"RUB">>},
        id => <<"ID">>,
        metadata => #{<<"some key">> => <<"some val">>},
        params => #{
            destination_id => <<"destinationID">>,
            wallet_id => <<"sourceID">>
        },
        route => #{
            provider_id => 301,
            provider_id_legacy => <<"mocketbank">>,
            version => 1
        },
        transfer_type => withdrawal,
        version => 4
    },
    Change = {created, Withdrawal},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyChange =
        {arr, [
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
    {DecodedLegacy, _} = unmarshal(
        {event, undefined},
        LegacyEvent,
        make_legacy_context(#{
            ctx => #{
                <<"com.rbkmoney.wapi">> => #{
                    <<"metadata">> => #{
                        <<"some key">> => <<"some val">>
                    }
                }
            }
        })
    ),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_v0_1_migration_test() -> _.
created_v0_1_migration_test() ->
    Withdrawal = #{
        body => {100, <<"RUB">>},
        id => <<"ID">>,
        metadata => #{<<"some key">> => <<"some val">>},
        params => #{
            destination_id => <<"destinationID">>,
            wallet_id => <<"walletID">>
        },
        transfer_type => withdrawal,
        version => 4
    },
    Change = {created, Withdrawal},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"body">>} =>
                        {arr, [
                            {str, <<"tup">>},
                            {i, 100},
                            {bin, <<"RUB">>}
                        ]},
                    {str, <<"destination">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"accounter_account_id">>} => {i, 123},
                                {str, <<"currency">>} => {bin, <<"RUB">>},
                                {str, <<"id">>} => {bin, <<"destinationID">>},
                                {str, <<"identity">>} => {bin, <<"8FkoOxPRjbUXshllJieYV6qIjr3">>}
                            }}
                        ]},
                    {str, <<"handler">>} => {str, <<"ff_withdrawal">>},
                    {str, <<"id">>} => {bin, <<"ID">>},
                    {str, <<"params">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"destination">>} => {bin, <<"destinationID">>},
                                {str, <<"source">>} => {bin, <<"walletID">>}
                            }}
                        ]},
                    {str, <<"source">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"accounter_account_id">>} => {i, 123},
                                {str, <<"currency">>} => {bin, <<"RUB">>},
                                {str, <<"id">>} => {bin, <<"walletID">>},
                                {str, <<"identity">>} => {bin, <<"Fy3g1eq99fZJBeQDHNPmCNCRu4X">>}
                            }}
                        ]},
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
    {DecodedLegacy, _} = unmarshal(
        {event, undefined},
        LegacyEvent,
        make_legacy_context(#{
            ctx => #{
                <<"com.rbkmoney.wapi">> => #{
                    <<"metadata">> => #{
                        <<"some key">> => <<"some val">>
                    }
                }
            }
        })
    ),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_v0_2_migration_test() -> _.
created_v0_2_migration_test() ->
    Withdrawal = #{
        body => {100, <<"RUB">>},
        id => <<"ID">>,
        metadata => #{<<"some key">> => <<"some val">>},
        params => #{
            destination_id => <<"destinationID">>,
            wallet_id => <<"walletID">>,
            quote => #{
                cash_from => {100, <<"RUB">>},
                cash_to => {100, <<"USD">>},
                created_at => <<"2020-01-01T01:00:00Z">>,
                expires_on => <<"2020-01-01T01:00:00Z">>,
                quote_data => nil,
                route => #{
                    version => 1,
                    provider_id => 301,
                    provider_id_legacy => <<"1">>
                }
            }
        },
        transfer_type => withdrawal,
        version => 4
    },
    Change = {created, Withdrawal},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 100}, {bin, <<"RUB">>}]},
                    {str, <<"id">>} => {bin, <<"ID">>},
                    {str, <<"params">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"destination_account">>} =>
                                    {arr, [
                                        {str, <<"map">>},
                                        {obj, #{
                                            {str, <<"accounter_account_id">>} => {i, 123},
                                            {str, <<"currency">>} => {bin, <<"RUB">>},
                                            {str, <<"id">>} => {bin, <<"destinationID">>},
                                            {str, <<"identity">>} => {bin, <<"identity2">>}
                                        }}
                                    ]},
                                {str, <<"destination_id">>} => {bin, <<"destinationID">>},
                                {str, <<"wallet_account">>} =>
                                    {arr, [
                                        {str, <<"map">>},
                                        {obj, #{
                                            {str, <<"accounter_account_id">>} => {i, 123},
                                            {str, <<"currency">>} => {bin, <<"RUB">>},
                                            {str, <<"id">>} => {bin, <<"walletID">>},
                                            {str, <<"identity">>} => {bin, <<"identity">>}
                                        }}
                                    ]},
                                {str, <<"wallet_cash_flow_plan">>} =>
                                    {arr, [
                                        {str, <<"map">>},
                                        {obj, #{
                                            {str, <<"postings">>} =>
                                                {arr, [
                                                    {str, <<"lst">>},
                                                    {arr, [
                                                        {str, <<"map">>},
                                                        {obj, #{
                                                            {str, <<"receiver">>} =>
                                                                {arr, [
                                                                    {str, <<"tup">>},
                                                                    {str, <<"wallet">>},
                                                                    {str, <<"receiver_destination">>}
                                                                ]},
                                                            {str, <<"sender">>} =>
                                                                {arr, [
                                                                    {str, <<"tup">>},
                                                                    {str, <<"wallet">>},
                                                                    {str, <<"sender_settlement">>}
                                                                ]},
                                                            {str, <<"volume">>} =>
                                                                {arr, [
                                                                    {str, <<"tup">>},
                                                                    {str, <<"share">>},
                                                                    {arr, [
                                                                        {str, <<"tup">>},
                                                                        {arr, [{str, <<"tup">>}, {i, 1}, {i, 1}]},
                                                                        {str, <<"operation_amount">>},
                                                                        {str, <<"default">>}
                                                                    ]}
                                                                ]}
                                                        }}
                                                    ]}
                                                ]}
                                        }}
                                    ]},
                                {str, <<"wallet_id">>} => {bin, <<"walletID">>},
                                {str, <<"quote">>} =>
                                    {arr, [
                                        {str, <<"map">>},
                                        {obj, #{
                                            {str, <<"cash_from">>} =>
                                                {arr, [{str, <<"tup">>}, {i, 100}, {bin, <<"RUB">>}]},
                                            {str, <<"cash_to">>} =>
                                                {arr, [{str, <<"tup">>}, {i, 100}, {bin, <<"USD">>}]},
                                            {str, <<"created_at">>} => {bin, <<"2020-01-01T01:00:00Z">>},
                                            {str, <<"expires_on">>} => {bin, <<"2020-01-01T01:00:00Z">>},
                                            {str, <<"quote_data">>} =>
                                                {arr, [
                                                    {str, <<"map">>},
                                                    {obj, #{
                                                        {bin, <<"version">>} => {i, 1},
                                                        {bin, <<"provider_id">>} => {bin, <<"mocketbank">>},
                                                        {bin, <<"quote_data">>} => {str, <<"nil">>}
                                                    }}
                                                ]}
                                        }}
                                    ]}
                            }}
                        ]},
                    {str, <<"transfer_type">>} => {str, <<"withdrawal">>},
                    {str, <<"version">>} => {i, 2}
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
    {DecodedLegacy, _} = unmarshal(
        {event, undefined},
        LegacyEvent,
        make_legacy_context(#{
            ctx => #{
                <<"com.rbkmoney.wapi">> => #{
                    <<"metadata">> => #{
                        <<"some key">> => <<"some val">>
                    }
                }
            }
        })
    ),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_v0_3_migration_test() -> _.
created_v0_3_migration_test() ->
    Withdrawal = #{
        body => {100, <<"RUB">>},
        id => <<"ID">>,
        params => #{
            destination_id => <<"destinationID">>,
            wallet_id => <<"walletID">>
        },
        transfer_type => withdrawal,
        version => 4
    },
    Change = {created, Withdrawal},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 100}, {bin, <<"RUB">>}]},
                    {str, <<"id">>} => {bin, <<"ID">>},
                    {str, <<"params">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"destination_account">>} =>
                                    {arr, [
                                        {str, <<"map">>},
                                        {obj, #{
                                            {str, <<"accounter_account_id">>} => {i, 123},
                                            {str, <<"currency">>} => {bin, <<"RUB">>},
                                            {str, <<"id">>} => {bin, <<"destinationID">>},
                                            {str, <<"identity">>} => {bin, <<"identity2">>}
                                        }}
                                    ]},
                                {str, <<"destination_id">>} => {bin, <<"destinationID">>},
                                {str, <<"wallet_account">>} =>
                                    {arr, [
                                        {str, <<"map">>},
                                        {obj, #{
                                            {str, <<"accounter_account_id">>} => {i, 123},
                                            {str, <<"currency">>} => {bin, <<"RUB">>},
                                            {str, <<"id">>} => {bin, <<"walletID">>},
                                            {str, <<"identity">>} => {bin, <<"identity">>}
                                        }}
                                    ]},
                                {str, <<"wallet_cash_flow_plan">>} =>
                                    {arr, [
                                        {str, <<"map">>},
                                        {obj, #{
                                            {str, <<"postings">>} =>
                                                {arr, [
                                                    {str, <<"lst">>},
                                                    {arr, [
                                                        {str, <<"map">>},
                                                        {obj, #{
                                                            {str, <<"receiver">>} =>
                                                                {arr, [
                                                                    {str, <<"tup">>},
                                                                    {str, <<"wallet">>},
                                                                    {str, <<"receiver_destination">>}
                                                                ]},
                                                            {str, <<"sender">>} =>
                                                                {arr, [
                                                                    {str, <<"tup">>},
                                                                    {str, <<"wallet">>},
                                                                    {str, <<"sender_settlement">>}
                                                                ]},
                                                            {str, <<"volume">>} =>
                                                                {arr, [
                                                                    {str, <<"tup">>},
                                                                    {str, <<"share">>},
                                                                    {arr, [
                                                                        {str, <<"tup">>},
                                                                        {arr, [{str, <<"tup">>}, {i, 1}, {i, 1}]},
                                                                        {str, <<"operation_amount">>},
                                                                        {str, <<"default">>}
                                                                    ]}
                                                                ]}
                                                        }}
                                                    ]}
                                                ]}
                                        }}
                                    ]},
                                {str, <<"wallet_id">>} => {bin, <<"walletID">>}
                            }}
                        ]},
                    {str, <<"transfer_type">>} => {str, <<"withdrawal">>},
                    {str, <<"version">>} => {i, 3}
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
    {DecodedLegacy, _} = unmarshal(
        {event, undefined},
        LegacyEvent,
        make_legacy_context(#{
            ctx => #{
                <<"com.rbkmoney.wapi">> => #{
                    <<"metadata">> => #{
                        <<"some key">> => <<"some val">>
                    }
                }
            }
        })
    ),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_v0_3_migration_with_quote_test() -> _.
created_v0_3_migration_with_quote_test() ->
    Withdrawal = #{
        body => {100, <<"RUB">>},
        id => <<"ID">>,
        domain_revision => 1,
        party_revision => 2,
        created_at => 123,
        params => #{
            destination_id => <<"destinationID">>,
            wallet_id => <<"walletID">>,
            quote => #{
                cash_from => {100, <<"RUB">>},
                cash_to => {100, <<"USD">>},
                created_at => <<"2020-01-01T01:00:00Z">>,
                expires_on => <<"2020-01-01T01:00:00Z">>,
                quote_data => nil,
                route => #{
                    version => 1,
                    provider_id => 302,
                    terminal_id => 1,
                    provider_id_legacy => <<"2">>
                },
                resource_descriptor => {bank_card, nil}
            }
        },
        transfer_type => withdrawal,
        version => 4
    },
    Change = {created, Withdrawal},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 100}, {bin, <<"RUB">>}]},
                    {str, <<"id">>} => {bin, <<"ID">>},
                    {str, <<"domain_revision">>} => {i, 1},
                    {str, <<"party_revision">>} => {i, 2},
                    {str, <<"created_at">>} => {i, 123},
                    {str, <<"params">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"destination_id">>} => {bin, <<"destinationID">>},
                                {str, <<"wallet_id">>} => {bin, <<"walletID">>},
                                {str, <<"quote">>} =>
                                    {arr, [
                                        {str, <<"map">>},
                                        {obj, #{
                                            {str, <<"cash_from">>} =>
                                                {arr, [{str, <<"tup">>}, {i, 100}, {bin, <<"RUB">>}]},
                                            {str, <<"cash_to">>} =>
                                                {arr, [{str, <<"tup">>}, {i, 100}, {bin, <<"USD">>}]},
                                            {str, <<"created_at">>} => {bin, <<"2020-01-01T01:00:00Z">>},
                                            {str, <<"expires_on">>} => {bin, <<"2020-01-01T01:00:00Z">>},
                                            {str, <<"quote_data">>} =>
                                                {arr, [
                                                    {str, <<"map">>},
                                                    {obj, #{
                                                        {bin, <<"version">>} => {i, 1},
                                                        {bin, <<"provider_id">>} => {i, 2},
                                                        {bin, <<"terminal_id">>} => {i, 1},
                                                        {bin, <<"domain_revision">>} => {i, 1},
                                                        {bin, <<"party_revision">>} => {i, 2},
                                                        {bin, <<"resource_id">>} =>
                                                            {arr, [
                                                                {str, <<"map">>},
                                                                {obj, #{
                                                                    {bin, <<"bank_card">>} => {str, <<"nil">>}
                                                                }}
                                                            ]},
                                                        {bin, <<"quote_data">>} => {str, <<"nil">>}
                                                    }}
                                                ]}
                                        }}
                                    ]}
                            }}
                        ]},
                    {str, <<"transfer_type">>} => {str, <<"withdrawal">>},
                    {str, <<"version">>} => {i, 3}
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

-spec created_v0_4_migration_with_quote_test() -> _.
created_v0_4_migration_with_quote_test() ->
    Withdrawal = #{
        body => {100, <<"RUB">>},
        id => <<"ID">>,
        domain_revision => 1,
        party_revision => 2,
        created_at => 123,
        params => #{
            destination_id => <<"destinationID">>,
            wallet_id => <<"walletID">>,
            quote => #{
                cash_from => {100, <<"RUB">>},
                cash_to => {100, <<"USD">>},
                created_at => <<"2020-01-01T01:00:00Z">>,
                expires_on => <<"2020-01-01T01:00:00Z">>,
                quote_data => nil,
                route => #{
                    version => 1,
                    provider_id => 2,
                    terminal_id => 1,
                    provider_id_legacy => <<"-298">>
                },
                resource_descriptor => {bank_card, nil}
            }
        },
        transfer_type => withdrawal,
        version => 4
    },
    Change = {created, Withdrawal},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 100}, {bin, <<"RUB">>}]},
                    {str, <<"id">>} => {bin, <<"ID">>},
                    {str, <<"domain_revision">>} => {i, 1},
                    {str, <<"party_revision">>} => {i, 2},
                    {str, <<"created_at">>} => {i, 123},
                    {str, <<"params">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"destination_id">>} => {bin, <<"destinationID">>},
                                {str, <<"wallet_id">>} => {bin, <<"walletID">>},
                                {str, <<"quote">>} =>
                                    {arr, [
                                        {str, <<"map">>},
                                        {obj, #{
                                            {str, <<"cash_from">>} =>
                                                {arr, [{str, <<"tup">>}, {i, 100}, {bin, <<"RUB">>}]},
                                            {str, <<"cash_to">>} =>
                                                {arr, [{str, <<"tup">>}, {i, 100}, {bin, <<"USD">>}]},
                                            {str, <<"created_at">>} => {bin, <<"2020-01-01T01:00:00Z">>},
                                            {str, <<"expires_on">>} => {bin, <<"2020-01-01T01:00:00Z">>},
                                            {str, <<"route">>} =>
                                                {arr, [
                                                    {str, <<"map">>},
                                                    {obj, #{
                                                        {str, <<"provider_id">>} => {i, 2},
                                                        {str, <<"terminal_id">>} => {i, 1},
                                                        {str, <<"version">>} => {i, 1}
                                                    }}
                                                ]},
                                            {str, <<"quote_data">>} => {str, <<"nil">>},
                                            {str, <<"resource_descriptor">>} =>
                                                {arr, [
                                                    {str, <<"tup">>},
                                                    {str, <<"bank_card">>},
                                                    {str, <<"nil">>}
                                                ]}
                                        }}
                                    ]}
                            }}
                        ]},
                    {str, <<"transfer_type">>} => {str, <<"withdrawal">>},
                    {str, <<"version">>} => {i, 4}
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

-spec route_changed_v0_0_migration_test() -> _.
route_changed_v0_0_migration_test() ->
    LegacyEvent = {route_changed, #{provider_id => 5}},
    ModernEvent =
        {route_changed, #{
            version => 1,
            provider_id => 305,
            provider_id_legacy => <<"5">>
        }},
    ?assertEqual(ModernEvent, maybe_migrate(LegacyEvent, #{})).

-spec status_changed_v0_0_migration_test() -> _.
status_changed_v0_0_migration_test() ->
    Change =
        {status_changed,
            {failed, #{
                code => <<"unknown">>,
                reason => <<"{quote,inconsistent_data}">>
            }}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"status_changed">>},
            {arr, [
                {str, <<"tup">>},
                {str, <<"failed">>},
                {arr, [
                    {str, <<"tup">>},
                    {str, <<"quote">>},
                    {str, <<"inconsistent_data">>}
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

-spec created_v1_marshaling_test() -> _.
created_v1_marshaling_test() ->
    Withdrawal = #{
        body => {100, <<"RUB">>},
        id => <<"ID">>,
        params => #{
            destination_id => <<"destinationID">>,
            wallet_id => <<"walletID">>
        },
        transfer_type => withdrawal,
        version => 4
    },
    Change = {created, Withdrawal},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(
                <<"CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAg",
                    "wAAQwAAQsABQAAAAJJRAsAAQAAAAh3YWxsZXRJRAsAAgAAAA1kZXN0aW5hdGlvbklEDAADCgABAAAAAAAAA",
                    "GQMAAILAAEAAAADUlVCAAAAAAAA">>
            )},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec status_changed_v1_marshaling_test() -> _.
status_changed_v1_marshaling_test() ->
    Change = {status_changed, {failed, #{code => <<"unknown">>, reason => <<"failure reason">>}}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(
                <<"CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAgwAAQw",
                    "AAwwAAQsAAQAAAAd1bmtub3duCwACAAAADmZhaWx1cmUgcmVhc29uAAAAAAAA">>
            )},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-endif.
