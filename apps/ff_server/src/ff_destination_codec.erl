-module(ff_destination_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").

-export([unmarshal_destination_params/1]).

-export([marshal_destination/2]).
-export([unmarshal_destination/1]).

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec unmarshal_destination_params(ff_proto_destination_thrift:'DestinationParams'()) ->
    ff_destination:params().

unmarshal_destination_params(Params) ->
    genlib_map:compact(#{
        id          => unmarshal(id,       Params#dst_DestinationParams.id),
        identity    => unmarshal(id,       Params#dst_DestinationParams.identity),
        name        => unmarshal(string,   Params#dst_DestinationParams.name),
        currency    => unmarshal(string,   Params#dst_DestinationParams.currency),
        resource    => unmarshal(resource, Params#dst_DestinationParams.resource),
        external_id => maybe_unmarshal(id, Params#dst_DestinationParams.external_id)
    }).

-spec marshal_destination(ff_destination:id(), ff_destination:machine()) ->
    ff_proto_destination_thrift:'DestinationState'().

marshal_destination(ID, Machine) ->
    Context     = marshal(ctx, ff_destination:ctx(Machine)),
    Destination = ff_destination:get(Machine),
    Blocking    = case ff_destination:is_accessible(Destination) of
        {ok, accessible} ->
            unblocked;
        _ ->
            blocked
    end,
    #dst_DestinationState{
        id = ID,
        name = marshal(string, ff_destination:name(Destination)),
        resource = marshal(resource, ff_destination:resource(Destination)),
        external_id = marshal(id, ff_destination:external_id(Destination)),
        account = marshal(account, ff_destination:account(Destination)),
        status = marshal(status, ff_destination:status(Destination)),
        created_at = marshal(timestamp, ff_machine:created(Machine)),
        blocking = Blocking,
        metadata = marshal(ctx, ff_destination:metadata(Destination)),
        context = Context
    }.

-spec unmarshal_destination(ff_proto_destination_thrift:'DestinationState'()) ->
    ff_destination:destination().

unmarshal_destination(Dest) ->
    genlib_map:compact(#{
        resource => unmarshal(resource, Dest#dst_DestinationState.resource),
        name => unmarshal(string, Dest#dst_DestinationState.name),
        account => maybe_unmarshal(account, Dest#dst_DestinationState.account),
        status => maybe_unmarshal(status, Dest#dst_DestinationState.status),
        external_id => maybe_unmarshal(external_id, Dest#dst_DestinationState.external_id),
        metadata => maybe_unmarshal(ctx, Dest#dst_DestinationState.metadata)
    }).

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal(change, {created, Destination}) ->
    {created, marshal(create_change, Destination)};
marshal(change, {account, AccountChange}) ->
    {account, marshal(account_change, AccountChange)};
marshal(change, {status_changed, StatusChange}) ->
    {status, marshal(status_change, StatusChange)};

marshal(create_change, Destination = #{
    name := Name,
    resource := Resource
}) ->
    #dst_DestinationState{
        name = Name,
        resource = Resource,
        external_id = maybe_marshal(id, maps:get(external_id, Destination,  undefined))
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

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #dst_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, change}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(change, {created, Destination}) ->
    {created, unmarshal(destination, Destination)};
unmarshal(change, {account, AccountChange}) ->
    {account, unmarshal(account_change, AccountChange)};
unmarshal(change, {status, StatusChange}) ->
    {status_changed, unmarshal(status_change, StatusChange)};

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
    Resource = {bank_card, #{bank_card => #{
        token => <<"token auth">>
    }}},
    AAID = 12345,
    AccountID = genlib:unique(),
    In = #{
        id => AccountID,
        account => #{
            id       => AccountID,
            identity => genlib:unique(),
            currency => <<"RUN">>,
            accounter_account_id => AAID
        },
        name        => <<"Wallet">>,
        status      => unauthorized,
        resource    => Resource,
        external_id => genlib:unique()
    },

    ?assertEqual(In, unmarshal_destination(marshal_destination(In))).

-spec crypto_wallet_resource_test() -> _.
crypto_wallet_resource_test() ->
    Resource = {crypto_wallet, #{crypto_wallet => #{
        id => <<"9e6245a7a6e15f75769a4d87183b090a">>,
        currency => {bitcoin, #{}}
    }}},
    ?assertEqual(Resource, unmarshal(resource, marshal(resource, Resource))).

-endif.
