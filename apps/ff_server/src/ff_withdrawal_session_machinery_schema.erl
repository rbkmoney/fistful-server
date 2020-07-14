-module(ff_withdrawal_session_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_withdrawal_session_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([get_version/1]).
-export([marshal/3]).
-export([unmarshal/3]).

%% Constants

% TODO: Replace version to 1 after p2p provider migration
% see https://rbkmoney.atlassian.net/browse/MSPF-561 for details
-define(CURRENT_EVENT_FORMAT_VERSION, undefined).

%% Internal types

-type type() :: machinery_mg_schema:t().
-type value(T) :: machinery_mg_schema:v(T).
-type value_type() :: machinery_mg_schema:vt().
-type context() :: machinery_mg_schema:context().

-type event()   :: ff_machine:timestamped_event(withdrawal_session:event()).
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
    % TODO: Remove this clause after deploy
    machinery_mg_schema_generic:marshal({event, Version}, TimestampedChange, Context);
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = ff_withdrawal_session_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {ff_proto_withdrawal_session_thrift, 'TimestampedChange'}},
    {{bin, ff_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) ->
    {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {ff_proto_withdrawal_session_thrift, 'TimestampedChange'}},
    ThriftChange = ff_proto_utils:deserialize(Type, EncodedThriftChange),
    {ff_withdrawal_session_codec:unmarshal(timestamped_change, ThriftChange), Context};
unmarshal_event(undefined = Version, EncodedChange, Context0) ->
    {Event, Context1} = machinery_mg_schema_generic:unmarshal({event, Version}, EncodedChange, Context0),
    {ev, Timestamp, Change} = Event,
    {{ev, Timestamp, maybe_migrate(Change)}, Context1}.

-spec maybe_migrate(any()) ->
    withdrawal_session:event().

maybe_migrate(Event = {created, #{version := 3}}) ->
    Event;
maybe_migrate({created, #{version := 2} = Session}) ->
    KnowndLegacyIDs = #{
        <<"mocketbank">> => 1,
        <<"royalpay-payout">> => 2,
        <<"accentpay">> => 3
    },
    {LegacyProviderID, Route} = case maps:get(provider, Session) of
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
    maybe_migrate({created, NewSession});
maybe_migrate({created, Session = #{version := 1, withdrawal := Withdrawal = #{
    sender := Sender,
    receiver := Receiver
}}}) ->
    maybe_migrate({created, Session#{
        version => 2,
        withdrawal => Withdrawal#{
            sender => try_migrate_identity_state(Sender),
            receiver => try_migrate_identity_state(Receiver)
    }}});
maybe_migrate({created, Session = #{
    withdrawal := Withdrawal = #{
        destination := #{resource := OldResource}
    }
}}) ->
    {ok, Resource} = ff_destination:process_resource_full(ff_instrument:maybe_migrate_resource(OldResource), undefined),
    NewWithdrawal0 = maps:without([destination], Withdrawal),
    NewWithdrawal1 = NewWithdrawal0#{resource => Resource},
    maybe_migrate({created, Session#{withdrawal => NewWithdrawal1}});
maybe_migrate({created, Session = #{
    withdrawal := Withdrawal = #{
        resource := Resource
    }
}}) ->
    NewResource = ff_instrument:maybe_migrate_resource(Resource),
    maybe_migrate({created, Session#{
        version => 1,
        withdrawal => Withdrawal#{
            resource => NewResource
    }}});
maybe_migrate({next_state, Value}) when Value =/= undefined ->
    {next_state, try_unmarshal_msgpack(Value)};
maybe_migrate({finished, {failed, {'domain_Failure', Code, Reason, SubFailure}}}) ->
    {finished, {failed, genlib_map:compact(#{
        code => migrate_unmarshal(string, Code),
        reason => maybe_migrate_unmarshal(string, Reason),
        sub => maybe_migrate_unmarshal(sub_failure, SubFailure)
    })}};
maybe_migrate({finished, {success, {'domain_TransactionInfo', ID, Timestamp, Extra, AddInfo}}}) ->
    {finished, {success, genlib_map:compact(#{
        id => ID,
        timestamp => Timestamp,
        extra => Extra,
        additional_info => maybe_migrate_unmarshal(additional_transaction_info, AddInfo)
    })}};
maybe_migrate({finished, {success, {'domain_TransactionInfo', ID, Timestamp, Extra}}}) ->
    {finished, {success, genlib_map:compact(#{
        id => ID,
        timestamp => Timestamp,
        extra => Extra
    })}};
% Other events
maybe_migrate(Ev) ->
    Ev.

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

try_migrate_identity_state(Identity = #{id := ID}) ->
    {ok, Machine} = ff_identity_machine:get(ID),
    NewIdentity = ff_identity_machine:identity(Machine),
    Identity#{
        version => 1,
        created_at => ff_identity:created_at(NewIdentity),
        metadata => ff_identity:metadata(NewIdentity)
    }.

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

-spec created_v0_decoding_test() -> _.
created_v0_decoding_test() ->
    Resource = {bank_card, #{bank_card => #{
        token => <<"token">>,
        bin_data_id => {binary, <<"bin">>},
        payment_system => visa
    }}},
    Quote = #{
        cash_from => {123, <<"RUB">>},
        cash_to => {123, <<"RUB">>},
        created_at => <<"some timestamp">>,
        expires_on => <<"some timestamp">>,
        quote_data => #{}
    },
    Identity = #{
        class => <<"class">>,
        contract => <<"ContractID">>,
        created_at => 1592576943762,
        id => <<"ID">>,
        party => <<"PartyID">>,
        provider => <<"good-one">>,
        metadata => #{<<"some key">> => <<"some val">>},
        version => 2
    },
    Withdrawal = #{
        id          => <<"id">>,
        session_id  => <<"session_id">>,
        resource    => Resource,
        cash        => {123, <<"RUB">>},
        sender      => Identity,
        receiver    => Identity,
        quote       => Quote
    },
    Session = #{
        version       => 3,
        id            => <<"id">>,
        status        => active,
        withdrawal    => Withdrawal,
        route         => #{provider_id => 1},

        % Deprecated. Remove after MSPF-560 finish
        provider_legacy => <<"provider_legacy">>
    },
    Change = {created, Session},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyResource = {arr, [
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
                        {str, <<"token">>} => {bin, <<"token">>},
                        {str, <<"payment_system">>} => {str, <<"visa">>}
                    }}
                ]}
            }}
        ]}
    ]},
    LegacyQuote = {arr, [
        {str, <<"map">>},
        {obj, #{
            {str, <<"cash_from">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
            {str, <<"cash_to">>} => {arr, [{str, <<"tup">>}, {i, 123}, {bin, <<"RUB">>}]},
            {str, <<"created_at">>} => {bin, <<"some timestamp">>},
            {str, <<"expires_on">>} => {bin, <<"some timestamp">>},
            {str, <<"quote_data">>} => {arr, [{str, <<"map">>}, {obj, #{}}]}
        }}
    ]},
    LegacyIdentity = {arr, [
        {str, <<"map">>},
        {obj, #{
            {str, <<"class">>} => {bin, <<"class">>},
            {str, <<"contract">>} => {bin, <<"ContractID">>},
            {str, <<"created_at">>} => {i, 1592576943762},
            {str, <<"id">>} => {bin, <<"ID">>},
            {str, <<"party">>} => {bin, <<"PartyID">>},
            {str, <<"provider">>} => {bin, <<"good-one">>},
            {str, <<"metadata">>} => {arr, [{str, <<"map">>}, {obj, #{{bin, <<"some key">>} => {bin, <<"some val">>}}}]}
        }}
    ]},
    LegacyWithdrawal = {arr, [
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
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"created">>},
        {arr, [
            {str, <<"map">>},
            {obj, #{
                {str, <<"version">>} => {i, 3},
                {str, <<"id">>} => {bin, <<"id">>},
                {str, <<"status">>} => {str, <<"active">>},
                {str, <<"withdrawal">>} => LegacyWithdrawal,
                {str, <<"route">>} => {arr, [{str, <<"map">>}, {obj, #{{str, <<"provider_id">>} => {i, 1}}}]},
                % Deprecated. Remove after MSPF-560 finish
                {str, <<"provider_legacy">>} => {bin, <<"provider_legacy">>}
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

-spec next_state_v0_decoding_test() -> _.
next_state_v0_decoding_test() ->
    Change = {next_state, <<"next_state">>},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"next_state">>},
        {bin, <<"next_state">>}
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

-spec finished_v0_decoding_test() -> _.
finished_v0_decoding_test() ->
    Change = {finished, {failed, #{code => <<"code">>}}},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyChange = {arr, [
        {str, <<"tup">>},
        {str, <<"finished">>},
        {arr, [
            {str, <<"tup">>},
            {str, <<"failed">>},
            {arr, [{str, <<"map">>}, {obj, #{{str, <<"code">>} => {bin, <<"code">>}}}]}
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

-spec created_v1_decoding_test() -> _.
created_v1_decoding_test() ->
    Resource = {bank_card, #{bank_card => #{
        token => <<"token">>,
        bin_data_id => {binary, <<"bin">>},
        payment_system => visa
    }}},
    Quote = #{
        cash_from => {123, <<"RUB">>},
        cash_to => {123, <<"RUB">>},
        created_at => <<"some timestamp">>,
        expires_on => <<"some timestamp">>,
        quote_data => #{}
    },
    Identity = #{
        class => <<"class">>,
        contract => <<"ContractID">>,
        created_at => 1592576943762,
        id => <<"ID">>,
        party => <<"PartyID">>,
        provider => <<"good-one">>,
        metadata => #{<<"some key">> => <<"some val">>},
        version => 2
    },
    Withdrawal = #{
        id          => <<"id">>,
        session_id  => <<"session_id">>,
        resource    => Resource,
        cash        => {123, <<"RUB">>},
        sender      => Identity,
        receiver    => Identity,
        quote       => Quote
    },
    Session = #{
        version       => 3,
        id            => <<"id">>,
        status        => active,
        withdrawal    => Withdrawal,
        route         => #{provider_id => 1},

        % Deprecated. Remove after MSPF-560 finish
        provider_legacy => <<"provider_legacy">>
    },
    Change = {created, Session},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent = {bin, base64:decode(<<
        "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAQsAAQAAAAJpZAwAAwsAAQAAAAJpZAwAAgwAAQw"
        "AAQsAAQAAAAV0b2tlbggAAgAAAAAMABULAAYAAAADYmluAAAAAAwAAwoAAQAAAAAAAAB7DAACCwABAAAAA1JVQgAADAAECw"
        "AGAAAAAklECwABAAAAB1BhcnR5SUQLAAIAAAAIZ29vZC1vbmULAAMAAAAFY2xhc3MLAAQAAAAKQ29udHJhY3RJRAsACgAAABgyM"
        "DIwLTA2LTE5VDE0OjI5OjAzLjc2MloNAAsLDAAAAAEAAAAIc29tZSBrZXkLAAUAAAAIc29tZSB2YWwAAAwABQsABgAAAAJJRAsA"
        "AQAAAAdQYXJ0eUlECwACAAAACGdvb2Qtb25lCwADAAAABWNsYXNzCwAEAAAACkNvbnRyYWN0SUQLAAoAAAAYMjAyMC0wNi0xOVQ"
        "xNDoyOTowMy43NjJaDQALCwwAAAABAAAACHNvbWUga2V5CwAFAAAACHNvbWUgdmFsAAALAAYAAAAKc2Vzc2lvbl9pZAwABwwAAQ"
        "oAAQAAAAAAAAB7DAACCwABAAAAA1JVQgAADAACCgABAAAAAAAAAHsMAAILAAEAAAADUlVCAAALAAMAAAAOc29tZSB0aW1lc3Rhb"
        "XALAAQAAAAOc29tZSB0aW1lc3RhbXANAAULDAAAAAAAAAwABggAAQAAAAEADAACDAABAAALAAQAAAAPcHJvdmlkZXJfbGVnYWN5AAAA"
    >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-spec next_state_v1_decoding_test() -> _.
next_state_v1_decoding_test() ->
    Change = {next_state, <<"next_state">>},
    Event = {ev, {{{2020, 5, 25}, {19, 19, 10}}, 293305}, Change},
    LegacyEvent = {bin, base64:decode(<<
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
    LegacyEvent = {bin, base64:decode(<<
        "CwABAAAAGzIwMjAtMDUtMjVUMTk6MTk6MTAuMjkzMzA1WgwAAgwAAwwAAgwAAQsAAQAAAARjb2RlAAAAAAA="
    >>)},
    DecodedLegacy = unmarshal({event, 1}, LegacyEvent),
    ModernizedBinary = marshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, DecodedLegacy),
    Decoded = unmarshal({event, ?CURRENT_EVENT_FORMAT_VERSION}, ModernizedBinary),
    ?assertEqual(Event, Decoded).

-endif.