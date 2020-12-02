-module(ff_destination_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").

-export([unmarshal_destination_params/1]).
-export([marshal_destination_state/2]).

-export([marshal_event/1]).

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec unmarshal_destination_params(ff_proto_destination_thrift:'DestinationParams'()) -> ff_destination:params().
unmarshal_destination_params(Params) ->
    genlib_map:compact(#{
        id => unmarshal(id, Params#dst_DestinationParams.id),
        identity => unmarshal(id, Params#dst_DestinationParams.identity),
        name => unmarshal(string, Params#dst_DestinationParams.name),
        currency => unmarshal(string, Params#dst_DestinationParams.currency),
        resource => unmarshal(resource, Params#dst_DestinationParams.resource),
        external_id => maybe_unmarshal(id, Params#dst_DestinationParams.external_id),
        metadata => maybe_unmarshal(ctx, Params#dst_DestinationParams.metadata)
    }).

-spec marshal_destination_state(ff_destination:destination_state(), ff_entity_context:context()) ->
    ff_proto_destination_thrift:'DestinationState'().
marshal_destination_state(DestinationState, Context) ->
    Blocking =
        case ff_destination:is_accessible(DestinationState) of
            {ok, accessible} ->
                unblocked;
            _ ->
                blocked
        end,
    #dst_DestinationState{
        id = marshal(id, ff_destination:id(DestinationState)),
        name = marshal(string, ff_destination:name(DestinationState)),
        resource = maybe_marshal(resource, ff_destination:resource(DestinationState)),
        external_id = maybe_marshal(id, ff_destination:external_id(DestinationState)),
        account = maybe_marshal(account, ff_destination:account(DestinationState)),
        status = maybe_marshal(status, ff_destination:status(DestinationState)),
        created_at = maybe_marshal(timestamp_ms, ff_destination:created_at(DestinationState)),
        blocking = Blocking,
        metadata = maybe_marshal(ctx, ff_destination:metadata(DestinationState)),
        context = maybe_marshal(ctx, Context)
    }.

-spec marshal_event(ff_destination_machine:event()) -> ff_proto_destination_thrift:'Event'().
marshal_event({EventID, {ev, Timestamp, Change}}) ->
    #dst_Event{
        event_id = ff_codec:marshal(event_id, EventID),
        occured_at = ff_codec:marshal(timestamp, Timestamp),
        change = marshal(change, Change)
    }.

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) -> ff_codec:encoded_value().
marshal(timestamped_change, {ev, Timestamp, Change}) ->
    #dst_TimestampedChange{
        change = marshal(change, Change),
        occured_at = ff_codec:marshal(timestamp, Timestamp)
    };
marshal(change, {created, Destination}) ->
    {created, marshal(create_change, Destination)};
marshal(change, {account, AccountChange}) ->
    {account, marshal(account_change, AccountChange)};
marshal(change, {status_changed, StatusChange}) ->
    {status, marshal(status_change, StatusChange)};
marshal(
    create_change,
    Destination = #{
        name := Name,
        resource := Resource
    }
) ->
    #dst_Destination{
        name = Name,
        resource = marshal(resource, Resource),
        created_at = maybe_marshal(timestamp_ms, maps:get(created_at, Destination, undefined)),
        external_id = maybe_marshal(id, maps:get(external_id, Destination, undefined)),
        metadata = maybe_marshal(ctx, maps:get(metadata, Destination, undefined))
    };
marshal(status, authorized) ->
    {authorized, #dst_Authorized{}};
marshal(status, unauthorized) ->
    {unauthorized, #dst_Unauthorized{}};
marshal(status_change, unauthorized) ->
    {changed, {unauthorized, #dst_Unauthorized{}}};
marshal(status_change, authorized) ->
    {changed, {authorized, #dst_Authorized{}}};
marshal(ctx, Ctx) ->
    marshal(context, Ctx);
marshal(T, V) ->
    ff_codec:marshal(T, V).

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) -> ff_codec:decoded_value().
unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];
unmarshal(repair_scenario, {add_events, #dst_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events,
        genlib_map:compact(#{
            events => unmarshal({list, change}, Events),
            action => maybe_unmarshal(complex_action, Action)
        })};
unmarshal(timestamped_change, TimestampedChange) ->
    Timestamp = ff_codec:unmarshal(timestamp, TimestampedChange#dst_TimestampedChange.occured_at),
    Change = unmarshal(change, TimestampedChange#dst_TimestampedChange.change),
    {ev, Timestamp, Change};
unmarshal(change, {created, Destination}) ->
    {created, unmarshal(destination, Destination)};
unmarshal(change, {account, AccountChange}) ->
    {account, unmarshal(account_change, AccountChange)};
unmarshal(change, {status, StatusChange}) ->
    {status_changed, unmarshal(status_change, StatusChange)};
unmarshal(destination, Dest) ->
    genlib_map:compact(#{
        version => 3,
        resource => unmarshal(resource, Dest#dst_Destination.resource),
        name => unmarshal(string, Dest#dst_Destination.name),
        created_at => maybe_unmarshal(timestamp_ms, Dest#dst_Destination.created_at),
        external_id => maybe_unmarshal(id, Dest#dst_Destination.external_id),
        metadata => maybe_unmarshal(ctx, Dest#dst_Destination.metadata)
    });
unmarshal(status, {authorized, #dst_Authorized{}}) ->
    authorized;
unmarshal(status, {unauthorized, #dst_Unauthorized{}}) ->
    unauthorized;
unmarshal(status_change, {changed, {unauthorized, #dst_Unauthorized{}}}) ->
    unauthorized;
unmarshal(status_change, {changed, {authorized, #dst_Authorized{}}}) ->
    authorized;
unmarshal(ctx, Ctx) ->
    maybe_unmarshal(context, Ctx);
unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec destination_test() -> _.

destination_test() ->
    Resource =
        {bank_card, #{
            bank_card => #{
                token => <<"token auth">>
            }
        }},
    In = #{
        version => 3,
        name => <<"Wallet">>,
        resource => Resource
    },

    ?assertEqual(In, unmarshal(destination, marshal(create_change, In))).

-spec crypto_wallet_resource_test() -> _.
crypto_wallet_resource_test() ->
    Resource =
        {crypto_wallet, #{
            crypto_wallet => #{
                id => <<"9e6245a7a6e15f75769a4d87183b090a">>,
                currency => {bitcoin, #{}}
            }
        }},
    ?assertEqual(Resource, unmarshal(resource, marshal(resource, Resource))).

-endif.
