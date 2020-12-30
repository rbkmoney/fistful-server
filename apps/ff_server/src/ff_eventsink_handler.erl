-module(ff_eventsink_handler).

-behaviour(ff_woody_wrapper).

-export([handle_function/3]).

-include_lib("fistful_proto/include/ff_proto_eventsink_thrift.hrl").

-type options() :: #{
    schema := module(),
    client := woody_client:options(),
    ns := binary(),
    publisher := module()
}.

%%
%% ff_woody_wrapper callbacks
%%

-spec handle_function(woody:func(), woody:args(), options()) -> {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(
        eventsink_handler,
        #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

handle_function_('GetEvents', [#'evsink_EventRange'{'after' = After0, limit = Limit}], Options) ->
    #{
        schema := Schema,
        client := Client,
        ns := NS,
        publisher := Publisher,
        start_event := StartEvent
    } = Options,
    After = erlang:max(After0, StartEvent),
    WoodyContext = ff_context:get_woody_context(ff_context:load()),
    {ok, Events} = machinery_mg_eventsink:get_events(
        NS,
        After,
        Limit,
        #{client => {Client, WoodyContext}, schema => Schema}
    ),
    ff_eventsink_publisher:publish_events(Events, #{publisher => Publisher});
handle_function_('GetLastEventID', _Params, #{schema := Schema, client := Client, ns := NS}) ->
    WoodyContext = ff_context:get_woody_context(ff_context:load()),
    Opts = #{client => {Client, WoodyContext}, schema => Schema},
    case machinery_mg_eventsink:get_last_event_id(NS, Opts) of
        {ok, _} = Result ->
            Result;
        {error, no_last_event} ->
            woody_error:raise(business, #'evsink_NoLastEvent'{})
    end.
