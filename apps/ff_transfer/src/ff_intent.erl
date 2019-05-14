%%%
%%% Соглашение между разными уровнями абстракции трансфера
%%%

-module(ff_intent).

-type intent(T)                 :: T.
-type action() :: ff_transfer_machine_new:action().
-type events(T) :: [ff_transfer_machine_new:event(T)].

-type intent_response(E, I)    :: #{
    action    := action(),
    events    := events(E),
    intent    => intent(I)
}.
-type maybe(T)                  :: ff_maybe:maybe(T).

-export_type([intent/1]).
-export_type([intent_response/2]).

%% Accessors

-export([action/1]).
-export([events/1]).
-export([intent/1]).

%%

-export([make_response/2]).
-export([make_response/3]).

%%

-spec action(intent_response(_, _))  -> action().
action(#{action := V}) ->
    V.

-spec events(intent_response(E, _))  -> events(E).
events(#{events := V}) ->
    V.

-spec intent(intent_response(_, I))  -> maybe(intent(I)).
intent(#{intent := V}) ->
    V;
intent(_) ->
    undefined.

%%

-spec make_response(ff_transfer_machine_new:action(), [ff_transfer_machine_new:event(E)]) ->
    intent_response(E, _).

make_response(Action, Events) ->
    make_response(Action, Events, undefined).

-spec make_response(
    ff_transfer_machine_new:action(),
    [ff_transfer_machine_new:event(E)],
    maybe(intent(I))
) ->
    intent_response(E, I).

make_response(Action, Events, undefined) ->
    #{
        action => Action,
        events => Events
    };
make_response(Action, Events, Intent) ->
    #{
        action => Action,
        events => Events,
        intent => Intent
    }.
