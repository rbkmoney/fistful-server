%%%
%%% Instrument machine
%%%

-module(ff_instrument_machine).

%% API

-type id()          :: machinery:id().
-type ns()          :: machinery:ns().
-type ctx()         :: ff_ctx:ctx().
-type instrument(T) :: ff_instrument:instrument(T).

-type st(T) ::
    ff_machine:st(instrument(T)).

-export_type([id/0]).
-export_type([st/1]).
-export_type([events/1]).

-export([create/4]).
-export([get/2]).
-export([events/3]).

%% Accessors

-export([instrument/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-type params(T) :: #{
    identity := ff_identity:id(),
    name     := binary(),
    currency := ff_currency:id(),
    resource := ff_instrument:resource(T)
}.

-spec create(ns(), id(), params(_), ctx()) ->
    {ok, st(_)} |
    {error,
        _InstrumentCreateError |
        {conflict, id()}
    }.

create(NS, ID, #{identity := IdentityID, name := Name, currency := CurrencyID, resource := Resource}, Ctx) ->
    do(fun () ->
        Events = unwrap(ff_instrument:create(ID, IdentityID, Name, CurrencyID, Resource)),
        {ok, Result} = case machinery:start(NS, ID, {Events, Ctx}, backend(NS)) of
            ok ->
                ff_machine:get(ff_instrument, NS, ID);
            {error, exists} ->
                compare_events(NS, ID, Events)
        end,
        Result
    end).

-spec get(ns(), id()) ->
    {ok, st(_)}       |
    {error, notfound} .

get(NS, ID) ->
    ff_machine:get(ff_instrument, NS, ID).

%% Accessors

-spec instrument(st(T)) ->
    instrument(T).

instrument(St) ->
    ff_machine:model(St).

%% Machinery

-type event(T)       :: ff_instrument:event(T).
-type events(T)      :: [{integer(), ff_machine:timestamped_event(event(T))}].

-type machine()      :: ff_machine:machine(event(_)).
-type result()       :: ff_machine:result(event(_)).
-type handler_opts() :: machinery:handler_opts(_).

-spec init({[event(_)], ctx()}, machine(), _, handler_opts()) ->
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
    St = ff_machine:collapse(ff_instrument, Machine),
    process_timeout(deduce_activity(ff_machine:model(St)), St).

process_timeout(authorize, St) ->
    D0 = instrument(St),
    case ff_instrument:authorize(D0) of
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

-spec events(ns(), id(), machinery:range()) ->
    {ok, events(_)} |
    {error, notfound}.

events(NS, ID, Range) ->
    do(fun () ->
        #{history := History} = unwrap(machinery:get(NS, ID, Range, backend(NS))),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

backend(NS) ->
    fistful:backend(NS).

compare_events(NS, ID, NewEv) ->
    Limit = length(NewEv),
    {ok, OldEv} = events(NS, ID, {undefined, Limit, forward}),
    compare_events_(NS, ID, NewEv, [Ev || {_, {ev, _, Ev}} <- OldEv]).

compare_events_(NS, ID, NewEv, OldEv) when NewEv =:= OldEv ->
    ff_machine:get(ff_instrument, NS, ID);
compare_events_(_NS, ID, _NewEv, _Ev) ->
    {error, {conflict, ID}}.
