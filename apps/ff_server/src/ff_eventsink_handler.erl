-module(ff_eventsink_handler).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-include_lib("fistful_proto/include/ff_proto_eventsink_thrift.hrl").

-type options() :: #{
    schema      := module(),
    client      := woody_client:options(),
    ns          := binary(),
    publisher   := module()
}.

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Context, Opts) ->
    scoper:scope(eventsink_handler, #{function => Func},
        fun() ->
            ok = ff_woody_ctx:set(Context),
            try
                handle_function_(Func, Args, Context, Opts)
            after
                ff_woody_ctx:unset()
            end
        end
    ).

handle_function_(
    'GetEvents', [#'evsink_EventRange'{'after' = After0, limit = Limit}],
    Context, #{
        schema := Schema,
        client := Client,
        ns := NS,
        publisher := Publisher,
        start_event := StartEvent}
) ->
    After = erlang:max(After0, StartEvent),
    {ok, Events} = machinery_mg_eventsink:get_events(NS, After, Limit,
        #{client => {Client, Context}, schema => Schema}),
    ff_eventsink_publisher:publish_events(Events, #{publisher => Publisher});
handle_function_(
    'GetLastEventID', _Params, Context,
    #{schema := Schema, client := Client, ns := NS}
) ->
    Opts = #{client => {Client, Context}, schema => Schema},
    case machinery_mg_eventsink:get_last_event_id(NS, Opts) of
        {ok, _} = Result ->
            Result;
        {error, no_last_event} ->
            woody_error:raise(business, #'evsink_NoLastEvent'{})
    end.
