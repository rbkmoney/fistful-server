%%
%% P2PCallback management helpers
%%

-module(p2p_callback_utils).

-opaque index() :: #{
    callbacks := #{tag() => callback()}
}.

-type wrapped_event() :: {callback, #{
    tag := tag(),
    payload := event()
}}.

-type unknown_callback_error() :: {unknown_callback, tag()}.

-export_type([index/0]).
-export_type([wrapped_event/0]).
-export_type([unknown_callback_error/0]).

%% API

-export([new_index/0]).
-export([wrap_event/2]).
-export([wrap_events/2]).
-export([unwrap_event/1]).
-export([apply_event/2]).
-export([maybe_migrate/1]).
-export([get_by_tag/2]).
-export([process_callback/2]).

%% Internal types

-type tag() :: p2p_callback:tag().
-type callback() :: p2p_callback:callback().
-type response() :: p2p_callback:response().
-type event() :: p2p_callback:event().
-type action() :: machinery:action() | undefined.

%% API

-spec new_index() -> index().
new_index() ->
    #{
        callbacks => #{}
    }.

-spec wrap_events(tag(), [event()]) -> [wrapped_event()].
wrap_events(Tag, Events) ->
    [wrap_event(Tag, Ev) || Ev <- Events].

-spec unwrap_event(wrapped_event()) -> {tag(), event()}.
unwrap_event({callback, #{tag := Tag, payload := Event}}) ->
    {Tag, Event}.

-spec wrap_event(tag(), event()) -> wrapped_event().
wrap_event(Tag, Event) ->
    {callback, #{tag => Tag, payload => Event}}.

-spec get_by_tag(tag(), index()) ->
    {ok, callback()} | {error, unknown_callback_error()}.
get_by_tag(Tag, #{callbacks := P2PCallbacks}) ->
    case maps:find(Tag, P2PCallbacks) of
        {ok, P2PCallback} ->
            {ok, P2PCallback};
        error ->
            {error, {unknown_callback, Tag}}
    end.

-spec apply_event(wrapped_event(), index()) -> index().
apply_event(WrappedEvent, #{callbacks := P2PCallbacks} = Index) ->
    {Tag, Event} = unwrap_event(WrappedEvent),
    P2PCallback0 = maps:get(Tag, P2PCallbacks, undefined),
    P2PCallback1 = p2p_callback:apply_event(Event, P2PCallback0),
    Index#{callbacks := P2PCallbacks#{Tag => P2PCallback1}}.

-spec maybe_migrate(wrapped_event() | any()) -> wrapped_event().
maybe_migrate(Event) ->
    {Tag, P2PCallbackEvent} = unwrap_event(Event),
    Migrated = p2p_callback:maybe_migrate(P2PCallbackEvent),
    wrap_event(Tag, Migrated).

-spec process_callback(tag(), callback()) ->
    {response(), {action(), [wrapped_event()]}}.
process_callback(Tag, P2PCallback) ->
    {Response, Events} = p2p_callback:process_callback(P2PCallback),
    WrappedEvents = wrap_events(Tag, Events),
    {Response, {undefined, WrappedEvents}}.
