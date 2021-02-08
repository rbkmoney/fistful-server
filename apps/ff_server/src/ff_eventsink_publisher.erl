%%%
%%% Publisher - he comes to publish all eventsinks
%%%

-module(ff_eventsink_publisher).

%% API

-type event(T) ::
    machinery_mg_eventsink:evsink_event(
        ff_machine:timestamped_event(T)
    ).

-type sinkevent(T) :: T.
-type options() :: #{publisher := module()}.

%% Behaviour definition

-export_type([event/1]).
-export_type([sinkevent/1]).
-export_type([options/0]).

-callback publish_events(list(event(_))) -> list(sinkevent(_)).

%% API

-export([publish_events/2]).

-spec publish_events(list(event(_)), options()) -> {ok, list(sinkevent(_))}.
publish_events(Events, Opts) ->
    {ok, handler_publish_events(Events, Opts)}.

get_publicher(#{publisher := Publisher}) ->
    Publisher.

%% Publisher calls

handler_publish_events(Events, Opts) ->
    Publisher = get_publicher(Opts),
    Publisher:publish_events(Events).
