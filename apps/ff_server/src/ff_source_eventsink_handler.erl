-module(ff_source_eventsink_handler).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-behaviour(ff_eventsink_publisher).

-export([publish_events/1]).

-include_lib("fistful_proto/include/ff_proto_source_thrift.hrl").

-type event() :: ff_eventsink_publisher:event(ff_source:event()).
-type sinkevent() :: ff_eventsink_publisher:sinkevent(ff_proto_source_thrift:'SinkEvent'()).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Context, Opts) ->
    scoper:scope(wallet_eventsink, #{function => Func},
        fun() ->
            ok = ff_woody_ctx:set(Context),
            try
                ff_eventsink_handler:handle_function(
                    Func, Args, Context, Opts#{
                        handler => ff_source_eventsink_handler
                    }
                )
            after
                ff_woody_ctx:unset()
            end
        end
    ).

-spec publish_events(list(event())) ->
    list(sinkevent()).

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

-spec publish_event(event()) ->
    sinkevent().

publish_event(#{
    id          := ID,
    source_id   := SourceID,
    event       := {
        EventID,
        Dt,
        {ev, EventDt, Payload}
    }
}) ->
    #'src_SinkEvent'{
        'id'            = ff_eventsink_handler:marshal(event_id, ID),
        'created_at'    = ff_eventsink_handler:marshal(timestamp, Dt),
        'source'        = ff_eventsink_handler:marshal(id, SourceID),
        'payload'       = #'src_Event'{
            'sequence'   = ff_eventsink_handler:marshal(event_id, EventID),
            'occured_at' = ff_eventsink_handler:marshal(timestamp, EventDt),
            'changes'    = [marshal(event, Payload)]
        }
    }.

%%
%% Internals
%%

-spec marshal(term(), term()) -> term().

marshal(event, {created, Source}) ->
    {created, marshal(source, Source)};
marshal(event, {account, AccountChange}) ->
    {account, ff_eventsink_handler:marshal(account_change, AccountChange)};
marshal(event, {status_changed, StatusChange}) ->
    {status, marshal(status_change, StatusChange)};

marshal(source, #{
    name := Name,
    resource := Resource
}) ->
    #'src_Source'{
        name = ff_eventsink_handler:marshal(string, Name),
        resource = marshal(resource, Resource)
    };
marshal(resource, #{type := internal} = Internal) ->
    {internal, marshal(internal, Internal)};
marshal(internal, Internal) ->
    Details = maps:get(details, Internal, undefined),
    #'src_Internal'{
        'details' = ff_eventsink_handler:marshal(string, Details)
    };

marshal(status_change, unauthorized) ->
    {changed, {unauthorized, #'src_Unauthorized'{}}};
marshal(status_change, authorized) ->
    {changed, {authorized, #'src_Authorized'{}}};

% Catch this up in thrift validation
marshal(_, Other) ->
    Other.

