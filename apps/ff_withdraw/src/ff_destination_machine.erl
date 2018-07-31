%%%
%%% Destination machine
%%%

-module(ff_destination_machine).

%% API

-type id()          :: machinery:id().
-type ctx()         :: ff_ctx:ctx().
-type destination() :: ff_destination:destination().

-type st() ::
    ff_machine:st(destination()).

-export_type([id/0]).

-export([create/3]).
-export([get/1]).

%% Accessors

-export([destination/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-define(NS, 'ff/destination').

-type params() :: #{
    identity := ff_identity:id(),
    name     := binary(),
    currency := ff_currency:id(),
    resource := ff_destination:resource()
}.

-spec create(id(), params(), ctx()) ->
    ok |
    {error,
        _DestinationCreateError |
        exists
    }.

create(ID, #{identity := IdentityID, name := Name, currency := CurrencyID, resource := Resource}, Ctx) ->
    do(fun () ->
        Events = unwrap(ff_destination:create(ID, IdentityID, Name, CurrencyID, Resource)),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, fistful:backend(?NS)))
    end).

-spec get(id()) ->
    {ok, st()}        |
    {error, notfound} .

get(ID) ->
    ff_machine:get(ff_destination, ?NS, ID).

%% Accessors

-spec destination(st()) ->
    destination().

destination(St) ->
    ff_machine:model(St).

%% Machinery

-type event() ::
    ff_destination:event().

-type machine()      :: ff_machine:machine(event()).
-type result()       :: ff_machine:result(event()).
-type handler_opts() :: machinery:handler_opts(_).

-spec init({[event()], ctx()}, machine(), _, handler_opts()) ->
    result().

init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events    => ff_machine:emit_events(Events),
        action    => continue,
        aux_state => #{ctx => Ctx}
    }.

%%

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

process_timeout(Machine, _, _Opts) ->
    St = ff_machine:collapse(ff_destination, Machine),
    process_timeout(deduce_activity(ff_machine:model(St)), St).

process_timeout(authorize, St) ->
    D0 = destination(St),
    case ff_destination:authorize(D0) of
        {ok, Events} ->
            #{
                events => ff_machine:emit_events(Events)
            }
    end.

deduce_activity(#{status := unauthorized}) ->
    authorize;
deduce_activity(#{}) ->
    undefined.

%%

-spec process_call(_CallArgs, machine(), _, handler_opts()) ->
    {ok, result()}.

process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.
