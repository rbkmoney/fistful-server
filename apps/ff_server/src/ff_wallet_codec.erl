-module(ff_wallet_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

-export([marshal_wallet/1]).
-export([unmarshal_wallet/1]).
-export([unmarshal_wallet_params/1]).

-export([marshal/2]).
-export([unmarshal/2]).

%% API
-spec marshal_wallet(ff_wallet:wallet()) ->
    ff_proto_wallet_thrift:'Wallet'().

marshal_wallet(Wallet) ->
    #wlt_Wallet{
        name        = marshal(string,   ff_wallet:name(Wallet)),
        blocking    = marshal(blocking,  ff_wallet:blocking(Wallet)),
        account     = marshal(account,  ff_wallet:account(Wallet)),
        external_id = marshal(id,       ff_wallet:external_id(Wallet))
    }.

-spec unmarshal_wallet(ff_proto_wallet_thrift:'Wallet'()) ->
    ff_wallet:wallet().

unmarshal_wallet(#wlt_Wallet{
    name = Name,
    external_id = ExternalID
}) ->
    genlib_map:compact(#{
        name => unmarshal(string, Name),
        external_id => unmarshal(id, ExternalID)
    }).

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
    {created, marshal_wallet(Wallet)};
marshal(change, {account, AccountChange}) ->
    {account, marshal(account_change, AccountChange)};

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

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).
