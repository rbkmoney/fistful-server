-module(ff_wallet_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal(event, {created, Wallet}) ->
    {created, marshal(wallet, Wallet)};
marshal(event, {account, AccountChange}) ->
    {account, marshal(account_change, AccountChange)};

marshal(wallet, Wallet) ->
    #wlt_Wallet{
        id          = maybe_marshal(id,       ff_wallet:id(Wallet)),
        name        = maybe_marshal(string,   ff_wallet:name(Wallet)),
        blocking    = maybe_marshal(blocking, ff_wallet:blocking(Wallet)),
        account     = maybe_marshal(account,  ff_wallet:account(Wallet)),
        external_id = maybe_marshal(id,       ff_wallet:external_id(Wallet))
    };

marshal(blocking, blocked) ->
    blocked;
marshal(blocking, unblocked) ->
    unblocked;

marshal(ctx, Ctx) ->
    maybe_marshal(context, Ctx);

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #wlt_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, event}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(event, {created, Wallet}) ->
    {created, unmarshal(wallet, Wallet)};
unmarshal(event, {account, AccountChange}) ->
    {account, unmarshal(account_change, AccountChange)};

unmarshal(wallet, #wlt_Wallet{
    name = Name,
    external_id = ExternalID
}) ->
    genlib_map:compact(#{
        name => unmarshal(string, Name),
        external_id => unmarshal(id, ExternalID)
    });

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
