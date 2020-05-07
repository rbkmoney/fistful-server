-module(ff_withdrawal_machinery_mg_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

-define(CONTENT_TYPE, {struct, struct, {mg_proto_state_processing_thrift, 'Content'}}).
-define(VERSION, 2).

-type t()  :: machinery_mg_schema:t().
-type v(T) :: machinery_mg_schema:v(T).

-type changes() :: [ff_withdrawal:event()].
-type context() :: ff_entity_context:context().
-type event()   :: ff_machine:timestamped_event(ff_withdrawal:event()).
-type aux_state() :: ff_machine:auxst().

-type withdrawal_data() :: {changes(), context()} | aux_state() | event().

-spec marshal(t(), v(withdrawal_data())) ->
    machinery_msgpack:t().
marshal(event, {ev, Ts, Change}) ->
    ThriftChange = ff_withdrawal_codec:marshal(change, Change),
    TimeStamp = ff_codec:marshal(timestamp, Ts),
    Data = #wthd_NoIdEvent{change = ThriftChange, occured_at = TimeStamp},
    Type = {struct, union, {ff_proto_withdrawal_thrift, 'NoIdEvent'}},
    Bin = wrap(Type, Data),
    {bin, Bin};
marshal(T, V) ->
    machinery_mg_schema_generic:marshal(T, V).

-spec unmarshal(t(), machinery_msgpack:t()) ->
    withdrawal_data().
unmarshal(event, V) ->
    Type = {struct, union, {ff_proto_withdrawal_thrift, 'NoIdEvent'}},
    {TimeStamp, Change} = unwrap(Type, V),
    {ev, ff_codec:unmarshal(timestamp, TimeStamp), ff_withdrawal_codec:unmarshal(change, Change)};

unmarshal(T, V) ->
    machinery_mg_schema_generic:unmarshal(T, V).

wrap(Type, Data) ->
    Content = #mg_stateproc_Content{
        data = {bin, ff_proto_utils:serialize(Type, Data)},
        format_version = ?VERSION
    },
    ff_proto_utils:serialize(?CONTENT_TYPE, Content).

unwrap(Type, Bin) ->
    #mg_stateproc_Content{
        data = Data,
        format_version = Version
    } = ff_proto_utils:deserialize(?CONTENT_TYPE, Bin),
    {TimeStamp, Change0} = ff_proto_utils:deserialize(Type, Data),
    {TimeStamp, case Version of
        ?VERSION ->
            Change0;
        _Other ->
            maybe_migrate(Change0, #{timestamp => TimeStamp})
    end}.


%% Migrations

maybe_migrate(Ev = {status_changed, {failed, #{code := _}}}, _MigrateParams) ->
    Ev;
maybe_migrate(Ev = {session_finished, {_SessionID, _Status}}, _MigrateParams) ->
    Ev;
maybe_migrate(Ev = {limit_check, {wallet_sender, _Details}}, _MigrateParams) ->
    Ev;
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
        route         => Route,
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

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec v0_created_migration_test() -> _.
v0_created_migration_test() ->
    ID = genlib:unique(),
    WalletID = genlib:unique(),
    DestinationID = genlib:unique(),
    ProviderID = genlib:unique(),
    Body = {100, <<"RUB">>},
    LegacyEvent = {created, #{
        id          => ID,
        source      => WalletID,
        destination => DestinationID,
        body        => Body,
        provider    => ProviderID
    }},
    {created, Withdrawal} = maybe_migrate(LegacyEvent, #{}),
    ?assertEqual(ID, id(Withdrawal)),
    ?assertEqual(WalletID, wallet_id(Withdrawal)),
    ?assertEqual(DestinationID, destination_id(Withdrawal)),
    ?assertEqual(Body, body(Withdrawal)),
    ?assertEqual(#{provider_id => ProviderID}, route(Withdrawal)).

-spec v1_created_migration_test() -> _.
v1_created_migration_test() ->
    ID = genlib:unique(),
    WalletID = genlib:unique(),
    WalletAccount = #{
        id => WalletID,
        identity => genlib:unique(),
        currency => <<"RUB">>,
        accounter_account_id => 123
    },
    DestinationID = genlib:unique(),
    DestinationAccount = #{
        id => DestinationID,
        identity => genlib:unique(),
        currency => <<"RUB">>,
        accounter_account_id => 123
    },
    Body = {100, <<"RUB">>},
    LegacyEvent = {created, #{
        version     => 1,
        id          => ID,
        handler     => ff_withdrawal,
        source      => WalletAccount,
        destination => DestinationAccount,
        body        => Body,
        params      => #{
            source => WalletID,
            destination => DestinationID
        }
    }},
    {created, Withdrawal} = maybe_migrate(LegacyEvent, #{}),
    ?assertEqual(ID, id(Withdrawal)),
    ?assertEqual(WalletID, wallet_id(Withdrawal)),
    ?assertEqual(DestinationID, destination_id(Withdrawal)),
    ?assertEqual(Body, body(Withdrawal)).

-spec v2_created_migration_test() -> _.
v2_created_migration_test() ->
    ID = genlib:unique(),
    WalletID = genlib:unique(),
    WalletAccount = #{
        id => WalletID,
        identity => genlib:unique(),
        currency => <<"RUB">>,
        accounter_account_id => 123
    },
    DestinationID = genlib:unique(),
    DestinationAccount = #{
        id => DestinationID,
        identity => genlib:unique(),
        currency => <<"RUB">>,
        accounter_account_id => 123
    },
    Body = {100, <<"RUB">>},
    LegacyEvent = {created, #{
        version       => 2,
        id            => ID,
        transfer_type => withdrawal,
        body          => Body,
        params        => #{
            wallet_id             => WalletID,
            destination_id        => DestinationID,
            wallet_account        => WalletAccount,
            destination_account   => DestinationAccount,
            wallet_cash_flow_plan => #{
                postings => [
                    #{
                        sender   => {wallet, sender_settlement},
                        receiver => {wallet, receiver_destination},
                        volume   => {share, {{1, 1}, operation_amount, default}}
                    }
                ]
            }
        }
    }},
    {created, Withdrawal} = maybe_migrate(LegacyEvent, #{}),
    ?assertEqual(ID, id(Withdrawal)),
    ?assertEqual(WalletID, wallet_id(Withdrawal)),
    ?assertEqual(DestinationID, destination_id(Withdrawal)),
    ?assertEqual(Body, body(Withdrawal)).

-endif.
