%%
%% Index reverts management helpers
%%

-module(ff_deposit_revert_utils).

-opaque index() :: #{
    reverts := #{id() => revert()},
    % Стек идентифкаторов возвратов. Голова списка точно является незавершенным ревертом.
    % Остальные реверты могут быть как завершенными, так и нет. Элементы могут повторяться.
    % На практике, если машина не подвергалась починке, в стеке будут идентификаторы
    % только активных возвратов без повторений.
    active  := [id()]
}.

-type wrapped_event() :: {revert, #{
    id      := id(),
    payload := event()
}}.

-type unknown_revert_error() :: {unknown_revert, id()}.

-export_type([index/0]).
-export_type([wrapped_event/0]).
-export_type([unknown_revert_error/0]).

%% API

-export([new_index/0]).
-export([reverts/1]).
-export([is_active/1]).
-export([is_finished/1]).
-export([get_not_finished/1]).
-export([wrap_event/2]).
-export([wrap_events/2]).
-export([unwrap_event/1]).
-export([apply_event/2]).
-export([get_by_id/2]).
-export([process_reverts/1]).

%% Internal types

-type id()              :: ff_adjustment:id().
-type revert()          :: ff_deposit_revert:revert().
-type event()           :: ff_deposit_revert:event().
-type action()          :: machinery:action() | undefined.

%% API

-spec new_index() -> index().
new_index() ->
    #{
        reverts => #{},
        active => []
    }.

-spec is_active(index()) -> boolean().
is_active(Index) ->
    active_revert_id(Index) =/= undefined.

-spec is_finished(index()) -> boolean().
is_finished(Index) ->
    lists:all(fun ff_deposit_revert:is_finished/1, reverts(Index)).

-spec get_not_finished(index()) -> {ok, id()} | error.
get_not_finished(Index) ->
    do_get_not_finished(reverts(Index)).

-spec reverts(index()) -> [revert()].
reverts(Index) ->
    #{reverts := Reverts} = Index,
    maps:values(Reverts).

-spec get_by_id(id(), index()) -> {ok, revert()} | {error, unknown_revert_error()}.
get_by_id(RevertID, Index) ->
    #{reverts := Reverts} = Index,
    case maps:find(RevertID, Reverts) of
        {ok, Revert} ->
            {ok, Revert};
        error ->
            {error, {unknown_revert, RevertID}}
    end.

-spec unwrap_event(wrapped_event()) -> {id(), event()}.
unwrap_event({revert, #{id := ID, payload := Event}}) ->
    {ID, Event}.

-spec wrap_event(id(), event()) -> wrapped_event().
wrap_event(ID, Event) ->
    {revert, #{id => ID, payload => Event}}.

-spec wrap_events(id(), [event()]) -> [wrapped_event()].
wrap_events(ID, Events) ->
    [wrap_event(ID, Ev) || Ev <- Events].

-spec apply_event(wrapped_event(), index()) -> index().
apply_event(WrappedEvent, Index0) ->
    {RevertID, Event} = unwrap_event(WrappedEvent),
    #{reverts := Reverts} = Index0,
    Revert0 = maps:get(RevertID, Reverts, undefined),
    Revert1 = ff_deposit_revert:apply_event(Event, Revert0),
    Index1 = Index0#{reverts := Reverts#{RevertID => Revert1}},
    Index2 = update_active(Revert1, Index1),
    Index2.

-spec process_reverts(index()) ->
    {action(), [wrapped_event()]}.
process_reverts(Index) ->
    RevertID = active_revert_id(Index),
    #{reverts := #{RevertID := Revert}} = Index,
    {RevertAction, Events} = ff_deposit_revert:process_transfer(Revert),
    WrappedEvents = wrap_events(RevertID, Events),
    NextIndex = lists:foldl(fun(E, Acc) -> ff_deposit_revert_utils:apply_event(E, Acc) end, Index, WrappedEvents),
    NextActiveRevert = active_revert_id(NextIndex),
    Action = case NextActiveRevert of
        undefined ->
            RevertAction;
        _Other ->
            continue
    end,
    {Action, WrappedEvents}.

%% Internals

-spec update_active(revert(), index()) -> index().
update_active(Revert, Index) ->
    #{active := Active} = Index,
    IsRevertActive = ff_deposit_revert:is_active(Revert),
    RevertID = ff_deposit_revert:id(Revert),
    NewActive = case {IsRevertActive, RevertID, Active} of
        {false, RevertID, [RevertID | ActiveTail]} ->
            drain_inactive_revert(ActiveTail, Index);
        {false, _RevertID, _} ->
            Active;
        {true, RevertID, [RevertID | _]} ->
            Active;
        {true, RevertID, _} ->
            [RevertID | Active]
    end,
    Index#{active => NewActive}.

-spec drain_inactive_revert([id()], index()) -> [id()].
drain_inactive_revert(RevertIDs, RevertsIndex) ->
    #{reverts := Reverts} = RevertsIndex,
    lists:dropwhile(
        fun(RevertID) ->
            #{RevertID := Revert} = Reverts,
            not ff_deposit_revert:is_active(Revert)
        end,
        RevertIDs
    ).

-spec active_revert_id(index()) -> id() | undefined.
active_revert_id(Index) ->
    #{active := Active} = Index,
    case Active of
        [RevertID | _] ->
            RevertID;
        [] ->
            undefined
    end.

-spec do_get_not_finished([revert()]) -> {ok, id()} | error.
do_get_not_finished([]) ->
    error;
do_get_not_finished([Revert | Tail]) ->
    case ff_deposit_revert:is_finished(Revert) of
        true ->
            do_get_not_finished(Tail);
        false ->
            {ok, ff_deposit_revert:id(Revert)}
    end.
