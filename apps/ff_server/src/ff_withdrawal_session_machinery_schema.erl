-module(ff_withdrawal_session_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_withdrawal_session_thrift.hrl").
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
-type id() :: machinery:id().

-type event() :: ff_machine:timestamped_event(ff_withdrawal_session:event()).
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
unmarshal({aux_state, FormatVersion}, EncodedChange, Context) ->
    unmarshal_aux_state(FormatVersion, EncodedChange, Context);
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
    % TODO: Remove this clause after deploy
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange, Context);
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = ff_withdrawal_session_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_withdrawal_session_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_withdrawal_session_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_withdrawal_session_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context0) ->
    {Event, Context1} = machinery_mg_schema_generic:unmarshal({event, Version}, EncodedChange, Context0),
    {ev, Timestamp, Change} = Event,
    {{ev, Timestamp, maybe_migrate(Change, Context0)}, Context1}.

-spec unmarshal_aux_state(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {aux_state(), context()}.
unmarshal_aux_state(undefined = Version, EncodedAuxState, Context0) ->
    {AuxState, Context1} = machinery_mg_schema_generic:unmarshal({aux_state, Version}, EncodedAuxState, Context0),
    {maybe_migrate_aux_state(AuxState, Context0), Context1}.

-spec maybe_migrate(any(), context()) -> ff_withdrawal_session:event().
maybe_migrate(Event = {created, #{version := 5}}, _Context) ->
    Event;
maybe_migrate(
    {created,
        Session = #{
            version := 4,
            withdrawal := Withdrawal = #{
                id := ID
            }
        }},
    Context
) ->
    NewSession = Session#{
        version => 5,
        withdrawal => Withdrawal#{
            id => maybe_cut_withdrawal_id(ID)
        }
    },
    maybe_migrate({created, NewSession}, Context);
maybe_migrate(
    {created,
        Session = #{
            version := 3,
            withdrawal := Withdrawal = #{
                sender := Sender,
                receiver := Receiver
            }
        }},
    Context
) ->
    NewSession = maps:without([adapter], Session#{
        version => 4,
        withdrawal => Withdrawal#{
            sender => try_migrate_to_adapter_identity(Sender),
            receiver => try_migrate_to_adapter_identity(Receiver)
        }
    }),
    maybe_migrate({created, NewSession}, Context);
maybe_migrate({created, #{version := 2} = Session}, Context) when is_map_key(provider, Session) ->
    KnowndLegacyIDs = #{
        <<"mocketbank">> => 1,
        <<"royalpay-payout">> => 2,
        <<"royalpay">> => 2,
        <<"accentpay">> => 3
    },
    {LegacyProviderID, Route} =
        case maps:get(provider, Session) of
            ProviderID when is_integer(ProviderID) ->
                {genlib:to_binary(ProviderID), #{
                    provider_id => ProviderID + 300
                }};
            ProviderID when is_binary(ProviderID) andalso is_map_key(ProviderID, KnowndLegacyIDs) ->
                ModernID = maps:get(ProviderID, KnowndLegacyIDs),
                {ProviderID, #{
                    provider_id => ModernID + 300
                }};
            ProviderID when is_binary(ProviderID) ->
                {ProviderID, #{
                    provider_id => erlang:binary_to_integer(ProviderID) + 300
                }}
        end,
    NewSession = (maps:without([provider], Session))#{
        version => 3,
        route => Route,
        provider_legacy => LegacyProviderID
    },
    maybe_migrate({created, NewSession}, Context);
maybe_migrate({created, #{version := 2} = Session}, Context) when not is_map_key(provider, Session) ->
    #{
        adapter := {Client, _Opts}
    } = Session,
    #{
        url := Url,
        event_handler := scoper_woody_event_handler
    } = Client,
    LegacyUrls = #{
        <<"http://adapter-royalpay:8022/adapter/royalpay/p2p-credit">> => <<"royalpay">>,
        <<"http://proxy-mocketbank:8022/proxy/mocketbank/p2p-credit">> => <<"mocketbank">>
    },
    maybe_migrate({created, Session#{provider => maps:get(Url, LegacyUrls)}}, Context);
maybe_migrate(
    {created,
        Session = #{
            version := 1,
            withdrawal := Withdrawal = #{
                sender := Sender,
                receiver := Receiver
            }
        }},
    Context
) ->
    maybe_migrate(
        {created, Session#{
            version => 2,
            withdrawal => Withdrawal#{
                sender => try_migrate_identity_state(Sender, Context),
                receiver => try_migrate_identity_state(Receiver, Context)
            }
        }},
        Context
    );
maybe_migrate(
    {created,
        Session = #{
            withdrawal := Withdrawal = #{
                destination := #{resource := OldResource}
            }
        }},
    Context
) ->
    NewResource = ff_destination:maybe_migrate_resource(OldResource),
    FullResource = try_get_full_resource(NewResource, Context),
    NewWithdrawal0 = maps:without([destination], Withdrawal),
    NewWithdrawal1 = NewWithdrawal0#{resource => FullResource},
    maybe_migrate({created, Session#{withdrawal => NewWithdrawal1}}, Context);
maybe_migrate(
    {created,
        Session = #{
            withdrawal := Withdrawal = #{
                resource := Resource
            }
        }},
    Context
) ->
    NewResource = ff_destination:maybe_migrate_resource(Resource),
    maybe_migrate(
        {created, Session#{
            version => 1,
            withdrawal => Withdrawal#{
                resource => NewResource
            }
        }},
        Context
    );
maybe_migrate({next_state, Value}, _Context) when Value =/= undefined ->
    {next_state, try_unmarshal_msgpack(Value)};
maybe_migrate({finished, {failed, {'domain_Failure', Code, Reason, SubFailure}}}, _Context) ->
    {finished,
        {failed,
            genlib_map:compact(#{
                code => migrate_unmarshal(string, Code),
                reason => maybe_migrate_unmarshal(string, Reason),
                sub => maybe_migrate_unmarshal(sub_failure, SubFailure)
            })}};
maybe_migrate({finished, {success, {'domain_TransactionInfo', ID, Timestamp, Extra, AddInfo}}}, _Context) ->
    {finished,
        {success,
            genlib_map:compact(#{
                id => ID,
                timestamp => Timestamp,
                extra => Extra,
                additional_info => maybe_migrate_unmarshal(additional_transaction_info, AddInfo)
            })}};
maybe_migrate({finished, {success, {'domain_TransactionInfo', ID, Timestamp, Extra}}}, _Context) ->
    {finished,
        {success,
            genlib_map:compact(#{
                id => ID,
                timestamp => Timestamp,
                extra => Extra
            })}};
% Other events
maybe_migrate(Ev, _Context) ->
    Ev.

try_get_full_resource(ResourceParams, Context) ->
    % `bindata_fun` is a helper for test purposes. You shouldn't use in production code.
    case maps:find(bindata_fun, Context) of
        {ok, Fun} ->
            Fun(ResourceParams);
        error ->
            case ff_resource:create_resource(ResourceParams) of
                {ok, NewResource} ->
                    NewResource;
                {error, _Reason} ->
                    % it looks like we have met unsupported card
                    % let's construct some dummy resource
                    {bank_card, Card} = ResourceParams,
                    {bank_card, maps:merge(#{payment_system_deprecated => visa, bin_data_id => nil}, Card)}
            end
    end.

migrate_unmarshal(sub_failure, {'domain_SubFailure', Code, SubFailure}) ->
    genlib_map:compact(#{
        code => migrate_unmarshal(string, Code),
        sub => maybe_migrate_unmarshal(sub_failure, SubFailure)
    });
migrate_unmarshal(additional_transaction_info, AddInfo) ->
    {
        'domain_AdditionalTransactionInfo',
        RRN,
        ApprovalCode,
        AcsURL,
        Pareq,
        MD,
        TermURL,
        Pares,
        ECI,
        CAVV,
        XID,
        CAVVAlgorithm,
        ThreeDSVerification
    } = AddInfo,
    genlib_map:compact(#{
        rrn => maybe_migrate_unmarshal(string, RRN),
        approval_code => maybe_migrate_unmarshal(string, ApprovalCode),
        acs_url => maybe_migrate_unmarshal(string, AcsURL),
        pareq => maybe_migrate_unmarshal(string, Pareq),
        md => maybe_migrate_unmarshal(string, MD),
        term_url => maybe_migrate_unmarshal(string, TermURL),
        pares => maybe_migrate_unmarshal(string, Pares),
        eci => maybe_migrate_unmarshal(string, ECI),
        cavv => maybe_migrate_unmarshal(string, CAVV),
        xid => maybe_migrate_unmarshal(string, XID),
        cavv_algorithm => maybe_migrate_unmarshal(string, CAVVAlgorithm),
        three_ds_verification => maybe_migrate_unmarshal(
            three_ds_verification,
            ThreeDSVerification
        )
    });
migrate_unmarshal(three_ds_verification, Value) when
    Value =:= authentication_successful orelse
        Value =:= attempts_processing_performed orelse
        Value =:= authentication_failed orelse
        Value =:= authentication_could_not_be_performed
->
    Value;
migrate_unmarshal(string, V) when is_binary(V) ->
    V.

maybe_migrate_unmarshal(_Type, undefined) ->
    undefined;
maybe_migrate_unmarshal(Type, V) ->
    migrate_unmarshal(Type, V).

try_unmarshal_msgpack({nl, {'msgpack_Nil'}}) ->
    nil;
try_unmarshal_msgpack({b, V}) when is_boolean(V) ->
    V;
try_unmarshal_msgpack({i, V}) when is_integer(V) ->
    V;
try_unmarshal_msgpack({flt, V}) when is_float(V) ->
    V;
try_unmarshal_msgpack({str, V}) when is_binary(V) ->
    V;
try_unmarshal_msgpack({bin, V}) when is_binary(V) ->
    {binary, V};
try_unmarshal_msgpack({arr, V}) when is_list(V) ->
    [try_unmarshal_msgpack(ListItem) || ListItem <- V];
try_unmarshal_msgpack({obj, V}) when is_map(V) ->
    maps:fold(
        fun(Key, Value, Map) ->
            Map#{try_unmarshal_msgpack(Key) => try_unmarshal_msgpack(Value)}
        end,
        #{},
        V
    );
% Not msgpack value
try_unmarshal_msgpack(V) ->
    V.

% Вид устаревшей структуры данных для облегчения будущих миграций
% LegacyIdentity v0 = #{
%     id           := id(),
%     party        := party_id(),
%     provider     := provider_id(),
%     class        := class_id(),
%     contract     := contract_id(),
%     level        => level_id(),
%     challenges   => #{challenge_id() => challenge()},
%     effective    => challenge_id(),
%     external_id  => id(),
%     blocking     => blocking()
% }

try_migrate_identity_state(undefined, _Context) ->
    undefined;
try_migrate_identity_state(Identity, _Context) ->
    Identity#{
        version => 1,
        % Dummy time, we will dump it on one of the next steps
        created_at => 0,
        % Dummy metadata, we will dump it on one of the next steps
        metadata => #{}
    }.

try_migrate_to_adapter_identity(undefined) ->
    undefined;
try_migrate_to_adapter_identity(Identity) ->
    genlib_map:compact(#{
        id => maps:get(id, Identity),
        effective_challenge => try_get_identity_challenge(Identity)
    }).

try_get_identity_challenge(#{effective := ChallengeID, challenges := Challenges}) ->
    #{ChallengeID := Challenge} = Challenges,
    #{
        id => ChallengeID,
        proofs => maps:get(proofs, Challenge)
    };
try_get_identity_challenge(_) ->
    undefined.

-spec maybe_migrate_aux_state(aux_state() | term(), context()) -> aux_state().
maybe_migrate_aux_state(<<>>, _Context) ->
    #{ctx => #{}};
maybe_migrate_aux_state(AuxState, _Context) ->
    AuxState.

-spec maybe_cut_withdrawal_id(id()) -> id().
%% Fix leaked session_ids by converting'em back to withdrawal_id
maybe_cut_withdrawal_id(ID) ->
    hd(binary:split(ID, <<"/">>)).

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

-spec created_with_broken_withdrawal_id_test() -> _.
created_with_broken_withdrawal_id_test() ->
    WithdrawalID = <<"withdrawal">>,
    SessionID = <<WithdrawalID/binary, "/1">>,
    Resource =
        {bank_card, #{
            bank_card => #{
                token => <<"token">>,
                bin_data_id => {binary, <<"bin">>},
                payment_system_deprecated => visa
            }
        }},
    Quote = #{
        cash_from => {123, <<"RUB">>},
        cash_to => {123, <<"RUB">>},
        created_at => <<"some timestamp">>,
        expires_on => <<"some timestamp">>,
        quote_data => #{}
    },
    Identity = #{
        id => <<"ID">>
    },
    Withdrawal = #{
        id => WithdrawalID,
        session_id => SessionID,
        resource => Resource,
        cash => {123, <<"RUB">>},
        sender => Identity,
        receiver => Identity,
        quote => Quote
    },
    Session = #{
        version => 5,
        id => SessionID,
        status => active,
        withdrawal => Withdrawal,
        route => #{provider_id => 1},
        provider_legacy => <<"-299">>
    },
    Change = {created, Session},
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
                                {str, <<"token">>} => {bin, <<"token">>},
                                {str, <<"payment_system_deprecated">>} => {str, <<"visa">>}
                            }}
                        ]}
                }}
            ]}
        ]},
    LegacyQuote =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"cash_from">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                {str, <<"cash_to">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                {str, <<"created_at">>} => {bin, <<"some timestamp">>},
                {str, <<"expires_on">>} => {bin, <<"some timestamp">>},
                {str, <<"quote_data">>} => {arr, [{str, <<"map">>}, {obj, #{}}]}
            }}
        ]},
    LegacyIdentity =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"class">>} => {bin, <<"class">>},
                {str, <<"contract">>} => {bin, <<"ContractID">>},
                {str, <<"created_at">>} => {i, 1592576943762},
                {str, <<"id">>} => {bin, <<"ID">>},
                {str, <<"party">>} => {bin, <<"PartyID">>},
                {str, <<"provider">>} => {bin, <<"good-one">>},
                {str, <<"metadata">>} =>
                    {arr, [{str, <<"map">>}, {obj, #{{bin, <<"some key">>} => {bin, <<"some val">>}}}]}
            }}
        ]},
    LegacyWithdrawal =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"id">>} => {bin, SessionID},
                {str, <<"session_id">>} => {bin, SessionID},
                {str, <<"resource">>} => LegacyResource,
                {str, <<"cash">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                {str, <<"sender">>} => LegacyIdentity,
                {str, <<"receiver">>} => LegacyIdentity,
                {str, <<"quote">>} => LegacyQuote
            }}
        ]},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"version">>} => {i, 4},
                    {str, <<"id">>} => {bin, SessionID},
                    {str, <<"status">>} => {str, <<"active">>},
                    {str, <<"withdrawal">>} => LegacyWithdrawal,
                    {str, <<"route">>} => {arr, [{str, <<"map">>}, {obj, #{{str, <<"provider_id">>} => {i, 1}}}]}
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
                bin_data_id => {binary, <<"bin">>},
                payment_system_deprecated => visa
            }
        }},
    Quote = #{
        cash_from => {123, <<"RUB">>},
        cash_to => {123, <<"RUB">>},
        created_at => <<"some timestamp">>,
        expires_on => <<"some timestamp">>,
        quote_data => #{}
    },
    Identity = #{
        id => <<"ID">>
    },
    Withdrawal = #{
        id => <<"id">>,
        session_id => <<"session_id">>,
        resource => Resource,
        cash => {123, <<"RUB">>},
        sender => Identity,
        receiver => Identity,
        quote => Quote
    },
    Session = #{
        version => 5,
        id => <<"id">>,
        status => active,
        withdrawal => Withdrawal,
        route => #{provider_id => 1},
        provider_legacy => <<"-299">>
    },
    Change = {created, Session},
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
                                {str, <<"token">>} => {bin, <<"token">>},
                                {str, <<"payment_system_deprecated">>} => {str, <<"visa">>}
                            }}
                        ]}
                }}
            ]}
        ]},
    LegacyQuote =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"cash_from">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                {str, <<"cash_to">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                {str, <<"created_at">>} => {bin, <<"some timestamp">>},
                {str, <<"expires_on">>} => {bin, <<"some timestamp">>},
                {str, <<"quote_data">>} => {arr, [{str, <<"map">>}, {obj, #{}}]}
            }}
        ]},
    LegacyIdentity =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"class">>} => {bin, <<"class">>},
                {str, <<"contract">>} => {bin, <<"ContractID">>},
                {str, <<"created_at">>} => {i, 1592576943762},
                {str, <<"id">>} => {bin, <<"ID">>},
                {str, <<"party">>} => {bin, <<"PartyID">>},
                {str, <<"provider">>} => {bin, <<"good-one">>},
                {str, <<"metadata">>} =>
                    {arr, [{str, <<"map">>}, {obj, #{{bin, <<"some key">>} => {bin, <<"some val">>}}}]}
            }}
        ]},
    LegacyWithdrawal =
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"id">>} => {bin, <<"id">>},
                {str, <<"session_id">>} => {bin, <<"session_id">>},
                {str, <<"resource">>} => LegacyResource,
                {str, <<"cash">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
                {str, <<"sender">>} => LegacyIdentity,
                {str, <<"receiver">>} => LegacyIdentity,
                {str, <<"quote">>} => LegacyQuote
            }}
        ]},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"created">>},
            {arr, [
                {str, <<"map">>},
                {obj, #{
                    {str, <<"version">>} => {i, 3},
                    {str, <<"id">>} => {bin, <<"id">>},
                    {str, <<"status">>} => {str, <<"active">>},
                    {str, <<"withdrawal">>} => LegacyWithdrawal,
                    {str, <<"route">>} => {arr, [{str, <<"map">>}, {obj, #{{str, <<"provider_id">>} => {i, 1}}}]}
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

-spec created_v0_unknown_with_binary_provider_decoding_test() -> _.
created_v0_unknown_with_binary_provider_decoding_test() ->
    Session = #{
        version => 5,
        id => <<"1274">>,
        route => #{
            provider_id => 302
        },
        provider_legacy => <<"royalpay">>,
        status => active,
        withdrawal => #{
            cash => {1500000, <<"RUB">>},
            id => <<"1274">>,
            receiver => #{id => <<"receiver_id">>},
            sender => #{
                id => <<"sender_id">>
            },
            resource =>
                {bank_card, #{
                    bank_card => #{
                        bin => <<"123456">>,
                        masked_pan => <<"1234">>,
                        payment_system_deprecated => visa,
                        token => <<"token">>
                    }
                }}
        }
    },
    Change = {created, Session},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 19}}, 293305}, Change},
    LegacyEvent =
        {arr, [
            {str, <<"tup">>},
            {str, <<"ev">>},
            {arr, [
                {str, <<"tup">>},
                {arr, [
                    {str, <<"tup">>},
                    {arr, [
                        {str, <<"tup">>},
                        {i, 2020},
                        {i, 5},
                        {i, 25}
                    ]},
                    {arr, [
                        {str, <<"tup">>},
                        {i, 19},
                        {i, 19},
                        {i, 19}
                    ]}
                ]},
                {i, 293305}
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
                                        {str, <<"event_handler">>} => {str, <<"scoper_woody_event_handler">>},
                                        {str, <<"url">>} =>
                                            {bin, <<"http://adapter-royalpay:8022/adapter/royalpay/p2p-credit">>}
                                    }}
                                ]},
                                {arr, [
                                    {str, <<"map">>},
                                    {obj, #{
                                        {bin, <<"payment_system_deprecated">>} => {bin, <<"Card">>},
                                        {bin, <<"timer_timeout">>} => {bin, <<"10">>}
                                    }}
                                ]}
                            ]},
                        {str, <<"id">>} => {bin, <<"1274">>},
                        {str, <<"provider">>} => {bin, <<"royalpay">>},
                        {str, <<"status">>} => {str, <<"active">>},
                        {str, <<"withdrawal">>} =>
                            {arr, [
                                {str, <<"map">>},
                                {obj, #{
                                    {str, <<"cash">>} =>
                                        {arr, [
                                            {str, <<"tup">>},
                                            {i, 1500000},
                                            {bin, <<"RUB">>}
                                        ]},
                                    {str, <<"destination">>} =>
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {str, <<"account">>} =>
                                                    {arr, [
                                                        {str, <<"map">>},
                                                        {obj, #{
                                                            {str, <<"accounter_account_id">>} => {i, 15052},
                                                            {str, <<"currency">>} => {bin, <<"RUB">>},
                                                            {str, <<"id">>} => {bin, <<"destination_id">>},
                                                            {str, <<"identity">>} => {bin, <<"identity_id">>}
                                                        }}
                                                    ]},
                                                {str, <<"name">>} => {bin, <<"Customer #75">>},
                                                {str, <<"resource">>} =>
                                                    {arr, [
                                                        {str, <<"tup">>},
                                                        {str, <<"bank_card">>},
                                                        {arr, [
                                                            {str, <<"map">>},
                                                            {obj, #{
                                                                {str, <<"bin">>} => {bin, <<"123456">>},
                                                                {str, <<"masked_pan">>} => {bin, <<"1234">>},
                                                                {str, <<"payment_system_deprecated">>} =>
                                                                    {str, <<"visa">>},
                                                                {str, <<"token">>} => {bin, <<"token">>}
                                                            }}
                                                        ]}
                                                    ]},
                                                {str, <<"status">>} => {str, <<"authorized">>}
                                            }}
                                        ]},
                                    {str, <<"id">>} => {bin, <<"1274">>},
                                    {str, <<"receiver">>} =>
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {str, <<"class">>} => {bin, <<"company">>},
                                                {str, <<"contract">>} => {bin, <<"receiver_contract">>},
                                                {str, <<"id">>} => {bin, <<"receiver_id">>},
                                                {str, <<"level">>} => {bin, <<"identified">>},
                                                {str, <<"party">>} => {bin, <<"party">>},
                                                {str, <<"provider">>} => {bin, <<"provider">>}
                                            }}
                                        ]},
                                    {str, <<"sender">>} =>
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {str, <<"class">>} => {bin, <<"company">>},
                                                {str, <<"contract">>} => {bin, <<"sender_contract">>},
                                                {str, <<"id">>} => {bin, <<"sender_id">>},
                                                {str, <<"level">>} => {bin, <<"identified">>},
                                                {str, <<"party">>} => {bin, <<"party">>},
                                                {str, <<"provider">>} => {bin, <<"provider">>}
                                            }}
                                        ]}
                                }}
                            ]}
                    }}
                ]}
            ]}
        ]},
    {DecodedLegacy, _Context} = unmarshal(
        {event, undefined},
        LegacyEvent,
        make_legacy_context(#{bindata_fun => fun(R) -> R end})
    ),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec created_v0_unknown_without_provider_decoding_test() -> _.
created_v0_unknown_without_provider_decoding_test() ->
    Session = #{
        version => 5,
        id => <<"294">>,
        route => #{
            provider_id => 301
        },
        provider_legacy => <<"mocketbank">>,
        status => active,
        withdrawal => #{
            cash => {10000000, <<"RUB">>},
            id => <<"294">>,
            receiver => #{id => <<"receiver_id">>},
            sender => #{id => <<"sender_id">>},
            resource =>
                {bank_card, #{
                    bank_card => #{
                        bin => <<"123456">>,
                        masked_pan => <<"1234">>,
                        payment_system_deprecated => visa,
                        token => <<"token">>
                    }
                }}
        }
    },
    Change = {created, Session},
    Event = {ev, {{{2018, 9, 5}, {11, 21, 57}}, 119792}, Change},
    LegacyEvent =
        {arr, [
            {str, <<"tup">>},
            {str, <<"ev">>},
            {arr, [
                {str, <<"tup">>},
                {arr, [
                    {str, <<"tup">>},
                    {arr, [{str, <<"tup">>}, {i, 2018}, {i, 9}, {i, 5}]},
                    {arr, [{str, <<"tup">>}, {i, 11}, {i, 21}, {i, 57}]}
                ]},
                {i, 119792}
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
                                        {str, <<"event_handler">>} => {str, <<"scoper_woody_event_handler">>},
                                        {str, <<"url">>} =>
                                            {bin, <<"http://proxy-mocketbank:8022/proxy/mocketbank/p2p-credit">>}
                                    }}
                                ]},
                                {arr, [
                                    {str, <<"map">>},
                                    {obj, #{}}
                                ]}
                            ]},
                        {str, <<"id">>} => {bin, <<"294">>},
                        {str, <<"status">>} => {str, <<"active">>},
                        {str, <<"withdrawal">>} =>
                            {arr, [
                                {str, <<"map">>},
                                {obj, #{
                                    {str, <<"cash">>} => {arr, [{str, <<"tup">>}, {i, 10000000}, {bin, <<"RUB">>}]},
                                    {str, <<"destination">>} =>
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {str, <<"account">>} =>
                                                    {arr, [
                                                        {str, <<"map">>},
                                                        {obj, #{
                                                            {str, <<"accounter_account_id">>} => {i, 11895},
                                                            {str, <<"currency">>} => {bin, <<"RUB">>},
                                                            {str, <<"id">>} => {bin, <<"destination">>},
                                                            {str, <<"identity">>} => {bin, <<"destination_identity">>}
                                                        }}
                                                    ]},
                                                {str, <<"name">>} => {bin, <<"Customer">>},
                                                {str, <<"resource">>} =>
                                                    {arr, [
                                                        {str, <<"tup">>},
                                                        {str, <<"bank_card">>},
                                                        {arr, [
                                                            {str, <<"map">>},
                                                            {obj, #{
                                                                {str, <<"bin">>} => {bin, <<"123456">>},
                                                                {str, <<"masked_pan">>} => {bin, <<"1234">>},
                                                                {str, <<"payment_system_deprecated">>} =>
                                                                    {str, <<"visa">>},
                                                                {str, <<"token">>} => {bin, <<"token">>}
                                                            }}
                                                        ]}
                                                    ]},
                                                {str, <<"status">>} => {str, <<"authorized">>}
                                            }}
                                        ]},
                                    {str, <<"id">>} => {bin, <<"294">>},
                                    {str, <<"receiver">>} =>
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {str, <<"class">>} => {bin, <<"person">>},
                                                {str, <<"contract">>} => {bin, <<"123">>},
                                                {str, <<"id">>} => {bin, <<"receiver_id">>},
                                                {str, <<"level">>} => {bin, <<"anonymous">>},
                                                {str, <<"party">>} => {bin, <<"123">>},
                                                {str, <<"provider">>} => {bin, <<"test">>}
                                            }}
                                        ]},
                                    {str, <<"sender">>} =>
                                        {arr, [
                                            {str, <<"map">>},
                                            {obj, #{
                                                {str, <<"class">>} => {bin, <<"person">>},
                                                {str, <<"contract">>} => {bin, <<"123">>},
                                                {str, <<"id">>} => {bin, <<"sender_id">>},
                                                {str, <<"level">>} => {bin, <<"anonymous">>},
                                                {str, <<"party">>} => {bin, <<"321">>},
                                                {str, <<"provider">>} => {bin, <<"test">>}
                                            }}
                                        ]}
                                }}
                            ]}
                    }}
                ]}
            ]}
        ]},
    {DecodedLegacy, _Context} = unmarshal(
        {event, undefined},
        LegacyEvent,
        make_legacy_context(#{bindata_fun => fun(R) -> R end})
    ),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec next_state_v0_decoding_test() -> _.
next_state_v0_decoding_test() ->
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

-spec finished_v0_decoding_test() -> _.
finished_v0_decoding_test() ->
    Change = {finished, {failed, #{code => <<"code">>}}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange =
        {arr, [
            {str, <<"tup">>},
            {str, <<"finished">>},
            {arr, [
                {str, <<"tup">>},
                {str, <<"failed">>},
                {arr, [{str, <<"map">>}, {obj, #{{str, <<"code">>} => {bin, <<"code">>}}}]}
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
                bin_data_id => {binary, <<"bin">>},
                payment_system_deprecated => visa
            }
        }},
    Quote = #{
        cash_from => {123, <<"RUB">>},
        cash_to => {123, <<"RUB">>},
        created_at => <<"some timestamp">>,
        expires_on => <<"some timestamp">>,
        quote_data => [1, nil, #{}]
    },
    Identity = #{
        id => <<"ID">>
    },
    Withdrawal = #{
        id => <<"id">>,
        session_id => <<"session_id">>,
        resource => Resource,
        cash => {123, <<"RUB">>},
        sender => Identity,
        receiver => Identity,
        quote => Quote
    },
    Session = #{
        version => 5,
        id => <<"id">>,
        status => active,
        withdrawal => Withdrawal,
        route => #{provider_id => 1},

        % Deprecated. Remove after MSPF-560 finish
        provider_legacy => <<"-299">>
    },
    Change = {created, Session},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAQsAAQAAAAJpZAwAAwsAAQAAAAJp"
                "ZAwAAgwAAQwAAQsAAQAAAAV0b2tlbggAAgAAAAAMABULAAYAAAADYmluAAAAAAwAAwoAAQAAAAAAAAB7"
                "DAACCwABAAAAA1JVQgAADAAICwABAAAAAklEAAwACQsAAQAAAAJJRAALAAYAAAAKc2Vzc2lvbl9pZAwA"
                "BwwAAQoAAQAAAAAAAAB7DAACCwABAAAAA1JVQgAADAACCgABAAAAAAAAAHsMAAILAAEAAAADUlVCAAAL"
                "AAMAAAAOc29tZSB0aW1lc3RhbXALAAQAAAAOc29tZSB0aW1lc3RhbXAMAAYPAAgMAAAAAwoAAwAAAAAA"
                "AAABAAwAAQAADQAHDAwAAAAAAAANAAULDAAAAAAAAAwABggAAQAAAAEADAACDAABAAALAAQAAAAELTI5"
                "OQAAAA=="
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
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAgsABQAAAApuZXh0X3N0YXRlAAAA"
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec finished_v1_decoding_test() -> _.
finished_v1_decoding_test() ->
    Change = {finished, {failed, #{code => <<"code">>}}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent =
        {bin,
            base64:decode(<<
                "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwwAAgwAAQsAAQAAAARjb2RlAAAAAAA="
            >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-endif.
