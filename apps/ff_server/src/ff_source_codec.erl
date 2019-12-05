-module(ff_source_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_source_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal(change, {created, Source}) ->
    {created, marshal(source, Source)};
marshal(change, {account, AccountChange}) ->
    {account, marshal(account_change, AccountChange)};
marshal(change, {status_changed, Status}) ->
    {status, #src_StatusChange{status = marshal(status, Status)}};

marshal(source, Params = #{
    name := Name,
    resource := Resource
}) ->
    ExternalID = maps:get(external_id, Params, undefined),
    #src_Source{
        id = marshal(id, ff_source:id(Params)),
        status = maybe_marshal(status, ff_source:status(Params)),
        name = marshal(string, Name),
        resource = marshal(resource, Resource),
        external_id = marshal(id, ExternalID)
    };
marshal(resource, #{type := internal} = Internal) ->
    {internal, marshal(internal, Internal)};
marshal(internal, Internal) ->
    Details = maps:get(details, Internal, undefined),
    #src_Internal{
        details = marshal(string, Details)
    };

marshal(status, unauthorized) ->
    {unauthorized, #src_Unauthorized{}};
marshal(status, authorized) ->
    {authorized, #src_Authorized{}};

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #src_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, change}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(change, {created, Source}) ->
    {created, unmarshal(source, Source)};
unmarshal(change, {account, AccountChange}) ->
    {account, unmarshal(account_change, AccountChange)};
unmarshal(change, {status, #src_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};

unmarshal(source, #src_Source{
    name = Name,
    resource = Resource,
    external_id = ExternalID
}) ->
    genlib_map:compact(#{
        name => unmarshal(string, Name),
        resource => unmarshal(resource, Resource),
        external_id => unmarshal(id, ExternalID)
    });
unmarshal(resource, {internal, #src_Internal{details = Details}}) ->
    genlib_map:compact(#{
        type => internal,
        details => unmarshal(string, Details)
    });

unmarshal(status, {unauthorized, #src_Unauthorized{}}) ->
    unauthorized;
unmarshal(status, {authorized, #src_Authorized{}}) ->
    authorized;

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).
