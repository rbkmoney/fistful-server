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

-type event() :: ff_machine:timestamped_event(ff_deposit:event()).
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

-spec unmarshal(type(), machinery_msgpack:t(), context()) -> {value(data()), context()}.
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
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange, Context);
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = ff_deposit_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_deposit_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_deposit_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_deposit_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context0) ->
    {Event, Context1} = machinery_mg_schema_generic:unmarshal({event, Version}, EncodedChange, Context0),
    {ev, Timestamp, Change} = Event,
    {{ev, Timestamp, maybe_migrate(Change, Context1)}, Context1}.

-spec maybe_migrate(any(), context()) -> ff_deposit:event().
maybe_migrate(Ev = {status_changed, {failed, #{code := _}}}, _MigrateParams) ->
    Ev;
maybe_migrate(Ev = {limit_check, {wallet_receiver, _Details}}, _MigrateParams) ->
    Ev;
maybe_migrate({p_transfer, PEvent}, _MigrateParams) ->
    {p_transfer, ff_postings_transfer:maybe_migrate(PEvent, deposit)};
maybe_migrate({revert, _Payload} = Event, _MigrateParams) ->
    ff_deposit_revert_utils:maybe_migrate(Event);
maybe_migrate({adjustment, _Payload} = Event, _MigrateParams) ->
    ff_adjustment_utils:maybe_migrate(Event);
% Old events
maybe_migrate({limit_check, {wallet, Details}}, MigrateParams) ->
    maybe_migrate({limit_check, {wallet_receiver, Details}}, MigrateParams);
maybe_migrate({created, #{version := 1, handler := ff_deposit} = T}, MigrateParams) ->
    #{
        version := 1,
        id := ID,
        handler := ff_deposit,
        source := _SourceAccount,
        destination := _DestinationAccount,
        body := Body,
        params := #{
            destination := DestinationID,
            source := SourceID
        }
    } = T,
    maybe_migrate(
        {created, #{
            version => 2,
            id => ID,
            transfer_type => deposit,
            body => Body,
            params => #{
                wallet_id => DestinationID,
                source_id => SourceID,
                % Fields below are required to correctly decode legacy events.
                % When decoding legacy events, the `erlang:binary_to_existing_atom/2` function is used,
                % so the code must contain atoms from the event.
                % They are not used now, so their value does not matter.
                wallet_account => [],
                source_account => [],
                wallet_cash_flow_plan => []
            }
        }},
        MigrateParams
    );
maybe_migrate({created, Deposit = #{version := 2, id := ID, params := Params}}, MigrateParams) ->
    Ctx = maps:get(ctx, MigrateParams, undefined),
    Context =
        case Ctx of
            undefined ->
                {ok, State} = ff_machine:get(ff_deposit, 'ff/deposit_v1', ID, {undefined, 0, forward}),
                maps:get(ctx, State, undefined);
            Data ->
                Data
        end,
    maybe_migrate(
        {created,
            genlib_map:compact(Deposit#{
                version => 3,
                metadata => ff_entity_context:try_get_legacy_metadata(Context),
                params => #{
                    wallet_id => maps:get(wallet_id, Params),
                    source_id => maps:get(source_id, Params)
                }
            })},
        MigrateParams
    );
maybe_migrate({created, #{version := 3}} = Ev, _MigrateParams) ->
    Ev;
maybe_migrate({transfer, PTransferEv}, MigrateParams) ->
    maybe_migrate({p_transfer, PTransferEv}, MigrateParams);
maybe_migrate({status_changed, {failed, LegacyFailure}}, MigrateParams) ->
    Failure = #{
        code => <<"unknown">>,
        reason => genlib:format(LegacyFailure)
    },
    maybe_migrate({status_changed, {failed, Failure}}, MigrateParams);
maybe_migrate(Ev, _MigrateParams) ->
    Ev.

get_aux_state_ctx(AuxState) when is_map(AuxState) ->
    maps:get(ctx, AuxState, undefined);
get_aux_state_ctx(_) ->
    undefined.

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

-spec created_v1_3_decoding_test() -> _.
created_v1_3_decoding_test() ->
    Deposit = #{
        version => 3,
        id => <<"deposit">>,
        transfer_type => deposit,
        body => {123, <<"RUB">>},
        params => #{
            wallet_id => <<"wallet_id">>,
            source_id => <<"source_id">>
        },
        metadata => #{<<"foo">> => <<"bar">>}
    },
    Change = {created, Deposit},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyAccount =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"id">>} => {bin, <<"id">>},
                {str, <<"identity">>} => {bin, <<"id">>},
                {str, <<"currency">>} => {bin, <<"id">>},
                {str, <<"accounter_account_id">>} => {bin, <<"id">>}
            }}
        ]},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"version">>} => {i, 1},
                    {str, <<"id">>} => {bin, <<"deposit">>},
                    {str, <<"handler">>} => {str, <<"ff_deposit">>},
                    {str, <<"source">>} => LegacyAccount,
                    {str, <<"destination">>} => LegacyAccount,
                    {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                    {str, <<"params">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"destination">>} => {bin, <<"wallet_id">>},
                                {str, <<"source">>} => {bin, <<"source_id">>}
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
    C = make_legacy_context(#{ctx => #{<<"com.rbkmoney.wapi">> => #{<<"metadata">> => #{<<"foo">> => <<"bar">>}}}}),
    {DecodedLegacy, _Context} = unmarshal({event, undefined}, LegacyEvent, C),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_v2_3_decoding_test() -> _.
created_v2_3_decoding_test() ->
    Deposit = #{
        version => 3,
        id => <<"deposit">>,
        transfer_type => deposit,
        body => {123, <<"RUB">>},
        created_at => 1590426777985,
        domain_revision => 123,
        party_revision => 321,
        external_id => <<"external_id">>,
        params => #{
            wallet_id => <<"wallet_id">>,
            source_id => <<"source_id">>
        },
        metadata => #{<<"foo">> => <<"bar">>}
    },
    Change = {created, Deposit},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyAccount =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"id">>} => {bin, <<"id">>},
                {str, <<"identity">>} => {bin, <<"id">>},
                {str, <<"currency">>} => {bin, <<"id">>},
                {str, <<"accounter_account_id">>} => {bin, <<"id">>}
            }}
        ]},
    LegacyWalletCashFlowPlan =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"postings">>} =>
                    {arr, [
                        {str, <<"lst">>},
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"sender">>} =>
                                    {arr, [
                                        {str, <<"tup">>},
                                        {str, <<"wallet">>},
                                        {str, <<"sender_source">>}
                                    ]},
                                {str, <<"receiver">>} =>
                                    {arr, [
                                        {str, <<"tup">>},
                                        {str, <<"wallet">>},
                                        {str, <<"receiver_settlement">>}
                                    ]},
                                {str, <<"volume">>} =>
                                    {arr, [
                                        {str, <<"tup">>},
                                        {str, <<"share">>},
                                        {arr, [
                                            {str, <<"tup">>},
                                            {arr, [
                                                {str, <<"tup">>},
                                                {i, 1},
                                                {i, 1}
                                            ]},
                                            {str, <<"operation_amount">>},
                                            {str, <<"default">>}
                                        ]}
                                    ]}
                            }}
                        ]}
                    ]}
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
                    {str, <<"id">>} => {bin, <<"deposit">>},
                    {str, <<"transfer_type">>} => {str, <<"deposit">>},
                    {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                    {str, <<"params">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"wallet_id">>} => {bin, <<"wallet_id">>},
                                {str, <<"source_id">>} => {bin, <<"source_id">>},
                                {str, <<"wallet_account">>} => LegacyAccount,
                                {str, <<"source_account">>} => LegacyAccount,
                                {str, <<"wallet_cash_flow_plan">>} => LegacyWalletCashFlowPlan
                            }}
                        ]},
                    {str, <<"domain_revision">>} => {i, 123},
                    {str, <<"party_revision">>} => {i, 321},
                    {str, <<"created_at">>} => {i, 1590426777985},
                    {str, <<"external_id">>} => {bin, <<"external_id">>}
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
    C = make_legacy_context(#{ctx => #{<<"com.rbkmoney.wapi">> => #{<<"metadata">> => #{<<"foo">> => <<"bar">>}}}}),
    {DecodedLegacy, _Context} = unmarshal({event, undefined}, LegacyEvent, C),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_v2_3_saved_metadata_decoding_test() -> _.
created_v2_3_saved_metadata_decoding_test() ->
    AuxState = #{
        ctx => #{
            <<"com.rbkmoney.wapi">> => #{
                <<"metadata">> => #{
                    <<"foo">> => <<"bar">>
                }
            }
        }
    },
    Deposit = #{
        version => 3,
        id => <<"deposit">>,
        transfer_type => deposit,
        body => {123, <<"RUB">>},
        created_at => 1590426777985,
        domain_revision => 123,
        party_revision => 321,
        external_id => <<"external_id">>,
        params => #{
            wallet_id => <<"wallet_id">>,
            source_id => <<"source_id">>
        },
        metadata => #{<<"foo">> => <<"bar">>}
    },
    Change = {created, Deposit},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyAccount =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"id">>} => {bin, <<"id">>},
                {str, <<"identity">>} => {bin, <<"id">>},
                {str, <<"currency">>} => {bin, <<"id">>},
                {str, <<"accounter_account_id">>} => {bin, <<"id">>}
            }}
        ]},
    LegacyWalletCashFlowPlan =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"postings">>} =>
                    {arr, [
                        {str, <<"lst">>},
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"sender">>} =>
                                    {arr, [
                                        {str, <<"tup">>},
                                        {str, <<"wallet">>},
                                        {str, <<"sender_source">>}
                                    ]},
                                {str, <<"receiver">>} =>
                                    {arr, [
                                        {str, <<"tup">>},
                                        {str, <<"wallet">>},
                                        {str, <<"receiver_settlement">>}
                                    ]},
                                {str, <<"volume">>} =>
                                    {arr, [
                                        {str, <<"tup">>},
                                        {str, <<"share">>},
                                        {arr, [
                                            {str, <<"tup">>},
                                            {arr, [
                                                {str, <<"tup">>},
                                                {i, 1},
                                                {i, 1}
                                            ]},
                                            {str, <<"operation_amount">>},
                                            {str, <<"default">>}
                                        ]}
                                    ]}
                            }}
                        ]}
                    ]}
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
                    {str, <<"id">>} => {bin, <<"deposit">>},
                    {str, <<"transfer_type">>} => {str, <<"deposit">>},
                    {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                    {str, <<"params">>} =>
                        {arr, [
                            {str, <<"map">>},
                            {obj, #{
                                {str, <<"wallet_id">>} => {bin, <<"wallet_id">>},
                                {str, <<"source_id">>} => {bin, <<"source_id">>},
                                {str, <<"wallet_account">>} => LegacyAccount,
                                {str, <<"source_account">>} => LegacyAccount,
                                {str, <<"wallet_cash_flow_plan">>} => LegacyWalletCashFlowPlan
                            }}
                        ]},
                    {str, <<"domain_revision">>} => {i, 123},
                    {str, <<"party_revision">>} => {i, 321},
                    {str, <<"created_at">>} => {i, 1590426777985},
                    {str, <<"external_id">>} => {bin, <<"external_id">>}
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
    C = make_legacy_context(#{}),
    {MarshalledAuxState, _Context0} = marshal({aux_state, undefined}, AuxState, C),
    {_UnmarshalledAuxState, Context0} = unmarshal({aux_state, undefined}, MarshalledAuxState, C),
    {DecodedLegacy, _Context1} = unmarshal({event, undefined}, LegacyEvent, Context0),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec p_transfer_v0_decoding_test() -> _.
p_transfer_v0_decoding_test() ->
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

-spec limit_check_v0_decoding_test() -> _.
limit_check_v0_decoding_test() ->
    Change = {limit_check, {wallet_sender, ok}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"limit_check">>},
            {arr, [
                {str, <<"tup">>},
                {str, <<"wallet_sender">>},
                {str, <<"ok">>}
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

-spec revert_v0_decoding_test() -> _.
revert_v0_decoding_test() ->
    Revert = #{
        id => <<"id">>,
        payload =>
            {created, #{
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

    LegacyRevert =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"id">>} => {bin, <<"deposit_revert">>},
                {str, <<"status">>} => {str, <<"pending">>},
                {str, <<"body">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                {str, <<"created_at">>} => {i, 1590426777985},
                {str, <<"domain_revision">>} => {i, 123},
                {str, <<"party_revision">>} => {i, 321},
                {str, <<"external_id">>} => {bin, <<"external_id">>},
                {str, <<"wallet_id">>} => {bin, <<"wallet_id">>},
                {str, <<"source_id">>} => {bin, <<"source_id">>}
            }}
        ]},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"revert">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"id">>} => {bin, <<"id">>},
                    {str, <<"payload">>} =>
                        {arr, [
                            {str, <<"tup">>},
                            {str, <<"created">>},
                            LegacyRevert
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

-spec adjustment_v0_decoding_test() -> _.
adjustment_v0_decoding_test() ->
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
        payload =>
            {created, #{
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

    LegacyPlan =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"new_cash_flow">>} =>
                    {arr, [
                        {str, <<"map">>},
                        {obj, #{
                            {str, <<"old_cash_flow_inverted">>} =>
                                {arr, [{str, <<"map">>}, {obj, #{{str, <<"postings">>} => {arr, []}}}]},
                            {str, <<"new_cash_flow">>} =>
                                {arr, [{str, <<"map">>}, {obj, #{{str, <<"postings">>} => {arr, []}}}]}
                        }}
                    ]},
                {str, <<"new_status">>} =>
                    {arr, [
                        {str, <<"map">>},
                        {obj, #{
                            {str, <<"new_status">>} => {str, <<"succeeded">>}
                        }}
                    ]}
            }}
        ]},
    LegacyAdjustment =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"id">>} => {bin, <<"adjustment">>},
                {str, <<"status">>} => {str, <<"pending">>},
                {str, <<"changes_plan">>} => LegacyPlan,
                {str, <<"created_at">>} => {i, 1590426777985},
                {str, <<"domain_revision">>} => {i, 123},
                {str, <<"party_revision">>} => {i, 321},
                {str, <<"operation_timestamp">>} => {i, 1590426777986},
                {str, <<"external_id">>} => {bin, <<"external_id">>}
            }}
        ]},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"adjustment">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"id">>} => {bin, <<"adjustment">>},
                    {str, <<"payload">>} =>
                        {arr, [
                            {str, <<"tup">>},
                            {str, <<"created">>},
                            LegacyAdjustment
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

-spec created_1_decoding_test() -> _.
created_1_decoding_test() ->
    Deposit = #{
        version => 3,
        id => <<"deposit">>,
        transfer_type => deposit,
        status => pending,
        body => {123, <<"RUB">>},
        created_at => 1590426777985,
        domain_revision => 123,
        party_revision => 321,
        external_id => <<"external_id">>,
        params => #{
            wallet_id => <<"wallet_id">>,
            source_id => <<"source_id">>
        },
        metadata => #{}
    },
    Change = {created, Deposit},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyEvent =
        {bin,
            base64:decode(
                <<
                    "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAA",
                    "gwAAQwAAQsABQAAAAdkZXBvc2l0CwABAAAACXdhbGxldF9pZAsAAgAAAAlzb3VyY2VfaWQMAAMKAAEAAAA",
                    "AAAAAewwAAgsAAQAAAANSVUIAAAwABgwAAQAACwAEAAAAC2V4dGVybmFsX2lkCwAHAAAAGDIwMjAtMDUtM",
                    "jVUMTc6MTI6NTcuOTg1WgoACAAAAAAAAAB7CgAJAAAAAAAAAUENAAoLDAAAAAAAAAAA"
                >>
            )},

    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec p_transfer_1_decoding_test() -> _.
p_transfer_1_decoding_test() ->
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
            base64:decode(
                <<
                    "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAA",
                    "wwAAQwAAQwAAQsAAgAAAAtleHRlcm5hbF9pZAwAAQ8AAQwAAAAAAAAAAAAAAA=="
                >>
            )},

    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec limit_check_1_decoding_test() -> _.
limit_check_1_decoding_test() ->
    Change = {limit_check, {wallet_sender, ok}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},

    LegacyEvent =
        {bin, base64:decode(<<"CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABg", "wAAQwAAQwAAQAAAAAAAA==">>)},

    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec revert_1_decoding_test() -> _.
revert_1_decoding_test() ->
    Revert = #{
        id => <<"id">>,
        payload =>
            {created, #{
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

    LegacyEvent =
        {bin,
            base64:decode(
                <<
                    "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABAs",
                    "AAQAAAAJpZAwAAgwAAQwAAQsAAQAAAA5kZXBvc2l0X3JldmVydAsAAgAAAAl3YWxsZXRfaWQLAAMAAAAJc291cm",
                    "NlX2lkDAAEDAABAAAMAAUKAAEAAAAAAAAAewwAAgsAAQAAAANSVUIAAAsABgAAABgyMDIwLTA1LTI1VDE3OjEyO",
                    "jU3Ljk4NVoKAAcAAAAAAAAAewoACAAAAAAAAAFBCwAKAAAAC2V4dGVybmFsX2lkAAAAAAAA"
                >>
            )},

    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec adjustment_1_decoding_test() -> _.
adjustment_1_decoding_test() ->
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
        payload =>
            {created, #{
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

    LegacyEvent =
        {bin,
            base64:decode(
                <<
                    "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwABQsAA",
                    "QAAAAphZGp1c3RtZW50DAACDAABDAABCwABAAAACmFkanVzdG1lbnQMAAIMAAEAAAwAAwwAAQwAAQ8AAQwAAAAAAA",
                    "wAAg8AAQwAAAAAAAAMAAIMAAEMAAIAAAAACwAEAAAAGDIwMjAtMDUtMjVUMTc6MTI6NTcuOTg1WgoABQAAAAAAAAB",
                    "7CgAGAAAAAAAAAUELAAcAAAALZXh0ZXJuYWxfaWQLAAgAAAAYMjAyMC0wNS0yNVQxNzoxMjo1Ny45ODZaAAAAAAAA"
                >>
            )},

    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-endif.
