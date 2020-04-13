-module(ff_wallet_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

-export([marshal_wallet_state/3]).
-export([unmarshal_wallet_params/1]).

-export([marshal/2]).
-export([unmarshal/2]).

%% API
-spec marshal_wallet_state(ff_wallet:wallet_state(), ff_wallet:id(), ff_entity_context:context()) ->
    ff_proto_wallet_thrift:'WalletState'().

marshal_wallet_state(WalletState, ID, Context) ->
    #wlt_WalletState{
        id = marshal(id, ID),
        name = marshal(string, ff_wallet:name(WalletState)),
        blocking = marshal(blocking, ff_wallet:blocking(WalletState)),
        account = maybe_marshal(account, ff_wallet:account(WalletState)),
        external_id = maybe_marshal(id, ff_wallet:external_id(WalletState)),
        created_at = maybe_marshal(timestamp_ms, ff_wallet:created_at(WalletState)),
        metadata = maybe_marshal(ctx, ff_wallet:metadata(WalletState)),
        context = marshal(ctx, Context)
    }.

-spec unmarshal_wallet_params(ff_proto_wallet_thrift:'WalletParams'()) ->
    ff_wallet_machine:params().

unmarshal_wallet_params(#wlt_WalletParams{
    id = ID,
    account_params = AccountParams,
    name = Name,
    external_id = ExternalID
}) ->
    {IdentityID, Currency} = unmarshal(account_params, AccountParams),
    genlib_map:compact(#{
        id          => unmarshal(id, ID),
        name        => unmarshal(string, Name),
        identity    => IdentityID,
        currency    => Currency,
        external_id => maybe_unmarshal(id, ExternalID)
    }).

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal(change, {created, Wallet}) ->
    {created, marshal(wallet, Wallet)};
marshal(change, {account, AccountChange}) ->
    {account, marshal(account_change, AccountChange)};

marshal(wallet, Wallet) ->
    #wlt_Wallet{
        name = marshal(string, maps:get(name, Wallet, <<>>)),
        blocking = marshal(blocking, maps:get(blocking, Wallet)),
        external_id = maybe_marshal(id, maps:get(external_id, Wallet, undefined)),
        created_at = maybe_marshal(timestamp_ms, maps:get(created_at, Wallet, undefined)),
        metadata = maybe_marshal(ctx, maps:get(metadata, Wallet, undefined))
    };

marshal(ctx, Ctx) ->
    marshal(context, Ctx);

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #wlt_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, change}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(change, {created, Wallet}) ->
    {created, unmarshal(wallet, Wallet)};
unmarshal(change, {account, AccountChange}) ->
    {account, unmarshal(account_change, AccountChange)};

unmarshal(wallet, #wlt_Wallet{
    name = Name,
    external_id = ExternalID,
    created_at = CreatedAt,
    metadata = Metadata
}) ->
    genlib_map:compact(#{
        name => unmarshal(string, Name),
        created_at => maybe_unmarshal(timestamp_ms, CreatedAt),
        external_id => maybe_unmarshal(id, ExternalID),
        metadata => maybe_unmarshal(ctx, Metadata)
    });

unmarshal(account_params, #account_AccountParams{
    identity_id   = IdentityID,
    symbolic_code = SymbolicCode
}) ->
    {unmarshal(id, IdentityID), unmarshal(string, SymbolicCode) };

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

