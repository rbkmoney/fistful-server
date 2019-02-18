-module(ff_source_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_source_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal(event, {created, Source}) ->
    {created, marshal(source, Source)};
marshal(event, {account, AccountChange}) ->
    {account, marshal(account_change, AccountChange)};
marshal(event, {status_changed, StatusChange}) ->
    {status, marshal(status_change, StatusChange)};

marshal(source, Params = #{
    name := Name,
    resource := Resource
}) ->
    ExternalID = maps:get(external_id, Params, undefined),
    #src_Source{
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

marshal(status_change, unauthorized) ->
    {changed, {unauthorized, #src_Unauthorized{}}};
marshal(status_change, authorized) ->
    {changed, {authorized, #src_Authorized{}}};

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #src_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, event}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(event, {created, Source}) ->
    {created, unmarshal(source, Source)};
unmarshal(event, {account, AccountChange}) ->
    {account, unmarshal(account_change, AccountChange)};
unmarshal(event, {status, StatusChange}) ->
    {status_changed, unmarshal(status_change, StatusChange)};

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
        type => unmarshal(string, Details)
    });

unmarshal(status_change, {unauthorized, #src_Unauthorized{}}) ->
    {changed, unauthorized};
unmarshal(status_change, {authorized, #src_Authorized{}}) ->
    {changed, authorized};

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).
