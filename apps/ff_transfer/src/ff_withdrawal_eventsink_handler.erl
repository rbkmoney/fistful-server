-module(ff_withdrawal_eventsink_handler).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Context, Opts) ->
    scoper:scope(fistful, #{function => Func},
        fun() ->
            ok = ff_woody_ctx:set(Context),
            try
                handle_function_(Func, Args, Context, Opts)
            after
                ff_woody_ctx:unset()
            end
        end
    ).

%%
%% Internals
%%

handle_function_('GetEvents', [#'evsink_EventRange'{'after' = After, limit = Limit}],
    _Context, #{schema := Schema}) ->
    {ok, Events} = machinery_eventsink:get_events(<<"wthd">>, After, Limit),
    publish_events(Events, Schema);
handle_function_('GetLastEventID', _Params, _Context, _Opts) ->
    case machinery_eventsink:get_last_event_id(<<"wthd">>) of
        {ok, ID} ->
            ID;
        {error, no_last_event} ->
            throw(#'evsink_NoLastEvent'{})
    end.

publish_events(Events, Schema) ->
    [publish_event(Event, Schema) || Event <- Events].

publish_event({ID, _Ns, SourceID, {_EventID, Dt, Payload}}, Schema) ->
    #'wthd_SinkEvent'{
        'sequence' = machinery_eventsink:unmarshal(id, ID),
        'created_at' = machinery_eventsink:unmarshal(timestamp, Dt),
        'source' = machinery_eventsink:unmarshal(id, SourceID),
        'payload' = machinery_eventsink:unmarshal({schema, Schema, event}, Payload)
    }.

