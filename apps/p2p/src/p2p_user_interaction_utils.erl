%%
%% UserInteraction management helpers
%%

-module(p2p_user_interaction_utils).

-opaque index() :: #{
    user_interactions := #{id() => user_interaction()}
}.

-type wrapped_event() ::
    {user_interaction, #{
        id := id(),
        payload := event()
    }}.

-type unknown_user_interaction_error() :: {unknown_user_interaction, id()}.

-export_type([index/0]).
-export_type([wrapped_event/0]).
-export_type([unknown_user_interaction_error/0]).

%% API

-export([new_index/0]).
-export([wrap_event/2]).
-export([wrap_events/2]).
-export([unwrap_event/1]).
-export([apply_event/2]).
-export([maybe_migrate/1]).
-export([get_by_id/2]).
-export([finish/2]).

%% Internal types

-type id() :: p2p_user_interaction:id().
-type user_interaction() :: p2p_user_interaction:user_interaction().
-type event() :: p2p_user_interaction:event().

%% API

-spec new_index() -> index().
new_index() ->
    #{
        user_interactions => #{}
    }.

-spec wrap_events(id(), [event()]) -> [wrapped_event()].
wrap_events(ID, Events) ->
    [wrap_event(ID, Ev) || Ev <- Events].

-spec unwrap_event(wrapped_event()) -> {id(), event()}.
unwrap_event({user_interaction, #{id := ID, payload := Event}}) ->
    {ID, Event}.

-spec wrap_event(id(), event()) -> wrapped_event().
wrap_event(ID, Event) ->
    {user_interaction, #{id => ID, payload => Event}}.

-spec get_by_id(id(), index()) -> {ok, user_interaction()} | {error, unknown_user_interaction_error()}.
get_by_id(ID, #{user_interactions := UserInteractions}) ->
    case maps:find(ID, UserInteractions) of
        {ok, UserInteraction} ->
            {ok, UserInteraction};
        error ->
            {error, {unknown_user_interaction, ID}}
    end.

-spec apply_event(wrapped_event(), index()) -> index().
apply_event(WrappedEvent, #{user_interactions := UserInteractions} = Index) ->
    {ID, Event} = unwrap_event(WrappedEvent),
    UserInteraction0 = maps:get(ID, UserInteractions, undefined),
    UserInteraction1 = p2p_user_interaction:apply_event(Event, UserInteraction0),
    Index#{user_interactions := UserInteractions#{ID => UserInteraction1}}.

-spec maybe_migrate(wrapped_event() | any()) -> wrapped_event().
maybe_migrate(Event) ->
    {ID, UserInteractionEvent} = unwrap_event(Event),
    Migrated = p2p_user_interaction:maybe_migrate(UserInteractionEvent),
    wrap_event(ID, Migrated).

-spec finish(id(), user_interaction()) -> [wrapped_event()].
finish(ID, UserInteraction) ->
    Events = p2p_user_interaction:finish(UserInteraction),
    WrappedEvents = wrap_events(ID, Events),
    WrappedEvents.
