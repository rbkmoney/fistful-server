-module(ff_source_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_source_thrift.hrl").

-export([unmarshal_source_params/1]).
-export([marshal_source_state/2]).
-export([marshal_event/1]).

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec unmarshal_source_params(ff_proto_source_thrift:'SourceParams'()) ->
    ff_source:params().

unmarshal_source_params(Params) ->
    genlib_map:compact(#{
        id          => unmarshal(id, Params#src_SourceParams.id),
        identity    => unmarshal(id, Params#src_SourceParams.identity_id),
        name        => unmarshal(string, Params#src_SourceParams.name),
        currency    => unmarshal(currency_ref, Params#src_SourceParams.currency),
        resource    => unmarshal(resource, Params#src_SourceParams.resource),
        external_id => maybe_unmarshal(id, Params#src_SourceParams.external_id),
        metadata    => maybe_unmarshal(ctx, Params#src_SourceParams.metadata)
    }).

-spec marshal_source_state(ff_source:source_state(), ff_entity_context:context()) ->
    ff_proto_source_thrift:'SourceState'().

marshal_source_state(SourceState, Context) ->
    Blocking = case ff_source:is_accessible(SourceState) of
        {ok, accessible} ->
            unblocked;
        _ ->
            blocked
    end,
    #src_SourceState{
        id = marshal(id, ff_source:id(SourceState)),
        name = marshal(string, ff_source:name(SourceState)),
        resource = marshal(resource, ff_source:resource(SourceState)),
        external_id = marshal(id, ff_source:external_id(SourceState)),
        account = marshal(account, ff_source:account(SourceState)),
        status = marshal(status, ff_source:status(SourceState)),
        created_at = marshal(timestamp_ms, ff_source:created_at(SourceState)),
        blocking = Blocking,
        metadata = marshal(ctx, ff_source:metadata(SourceState)),
        context = marshal(ctx, Context)
    }.

-spec marshal_event(ff_source:timestamped_event()) ->
    ff_proto_source_thrift:'Event'().

marshal_event({EventID, {ev, Timestamp, Change}}) ->
    #src_Event{
        event_id = ff_codec:marshal(event_id, EventID),
        occured_at = ff_codec:marshal(timestamp, Timestamp),
        change = marshal(change, Change)
    }.

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal(timestamped_change, {ev, Timestamp, Change}) ->
    #src_TimestampedChange{
        change = marshal(change, Change),
        occured_at = ff_codec:marshal(timestamp, Timestamp)
    };

marshal(change, {created, Source}) ->
    {created, marshal(source, Source)};
marshal(change, {account, AccountChange}) ->
    {account, marshal(account_change, AccountChange)};
marshal(change, {status_changed, Status}) ->
    {status, #src_StatusChange{status = marshal(status, Status)}};

marshal(source, Source = #{
    name := Name,
    resource := Resource
}) ->
    #src_Source{
        id = marshal(id, ff_source:id(Source)),
        status = maybe_marshal(status, ff_source:status(Source)),
        name = marshal(string, Name),
        resource = marshal(resource, Resource),
        external_id = maybe_marshal(id, maps:get(external_id, Source, undefined)),
        created_at = maybe_marshal(timestamp_ms, maps:get(created_at, Source,  undefined)),
        metadata = maybe_marshal(context, maps:get(metadata, Source, undefined))
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

marshal(ctx, Ctx) ->
    marshal(context, Ctx);

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #src_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, change}, Events),
        action => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(timestamped_change, TimestampedChange) ->
    Timestamp = ff_codec:unmarshal(timestamp, TimestampedChange#src_TimestampedChange.occured_at),
    Change = unmarshal(change, TimestampedChange#src_TimestampedChange.change),
    {ev, Timestamp, Change};

unmarshal(change, {created, Source}) ->
    {created, unmarshal(source, Source)};
unmarshal(change, {account, AccountChange}) ->
    {account, unmarshal(account_change, AccountChange)};
unmarshal(change, {status, #src_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};

unmarshal(source, #src_Source{
    name = Name,
    resource = Resource,
    external_id = ExternalID,
    created_at = CreatedAt,
    metadata = Metadata
}) ->
    genlib_map:compact(#{
        version => 3,
        name => unmarshal(string, Name),
        resource => unmarshal(resource, Resource),
        external_id => maybe_unmarshal(id, ExternalID),
        created_at => maybe_unmarshal(timestamp_ms, CreatedAt),
        metadata => maybe_unmarshal(context, Metadata)
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

unmarshal(ctx, Ctx) ->
    maybe_unmarshal(context, Ctx);

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
