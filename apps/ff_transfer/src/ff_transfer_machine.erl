%%%
%%% Transfer machine
%%%
%%% TODO
%%%  - add support for custom activity callbacls for
%%%    particular tranfer machines (withdrawal, deposit, etc).
%%%

-module(ff_transfer_machine).

%% API

-type id()        :: machinery:id().
-type ns()        :: machinery:namespace().
-type ctx()       :: ff_ctx:ctx().
-type transfer(T) :: ff_transfer:transfer(T).
-type account()   :: ff_account:account().
-type events() :: [{integer(), ff_machine:timestamped_event(event())}].

-type activity() ::
    prepare_transfer         |
    create_session           |
    await_session_completion |
    commit_transfer          |
    cancel_transfer          |
    undefined                .

-type st(T) ::
    ff_machine:st(transfer(T)).

-export_type([id/0]).
-export_type([ns/0]).
-export_type([st/1]).
-export_type([events/0]).

-export([create/4]).
-export([get/2]).
-export([events/3]).

%% Accessors

-export([transfer/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-type params() :: #{
    type        := ff_transfer:type(),
    source      := account(),
    destination := account(),
    body        := ff_transaction:body(),
    params      := ff_transfer:params()
}.

-spec create(ns(), id(), params(), ctx()) ->
    ok |
    {error,
        _TransferError |
        exists
    }.

create(NS, ID, #{type := Type, source := Source, destination := Destination, body := Body, params := Params}, Ctx) ->
    do(fun () ->
        Events = unwrap(ff_transfer:create(Type, ID, Source, Destination, Body, Params)),
        unwrap(machinery:start(NS, ID, {Events, Ctx}, backend(NS)))
    end).

-spec get(ns(), id()) ->
    {ok, st(_)}      |
    {error, notfound}.

get(NS, ID) ->
    ff_machine:get(ff_transfer, NS, ID).

-spec events(ns(), id(), machinery:range()) ->
    {ok, events()} |
    {error, notfound}.

events(NS, ID, Range) ->
    do(fun () ->
        #{history := History} = unwrap(machinery:get(NS, ID, Range, backend(NS))),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

backend(NS) ->
    fistful:backend(NS).

%% Accessors

-spec transfer(st(T)) ->
    transfer(T).

transfer(St) ->
    ff_machine:model(St).

%% Machinery

-type event() ::
    ff_transfer:event().

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

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

process_timeout(Machine, _, _Opts) ->
    St = ff_machine:collapse(ff_transfer, Machine),
    process_activity(deduce_activity(transfer(St)), St).

process_activity(prepare_transfer, St) ->
    case ff_transfer:prepare(transfer(St)) of
        {ok, Events} ->
            #{
                events => ff_machine:emit_events(Events),
                action => continue
            };
        {error, Reason} ->
            #{
                events => emit_failure(Reason)
            }
    end;

process_activity(create_session, St) ->
    case ff_transfer:create_session(transfer(St)) of
        {ok, Events} ->
            #{
                events => ff_machine:emit_events(Events),
                action => set_poll_timer(St)
            };
        {error, Reason} ->
            #{
                events => emit_failure(Reason)
            }
    end;

process_activity(await_session_completion, St) ->
    case ff_transfer:poll_session_completion(transfer(St)) of
        {ok, Events} when length(Events) > 0 ->
            #{
                events => ff_machine:emit_events(Events),
                action => continue
            };
        {ok, []} ->
            #{
                action => set_poll_timer(St)
            }
    end;

process_activity(commit_transfer, St) ->
    {ok, Events} = ff_transfer:commit(transfer(St)),
    #{
        events => ff_machine:emit_events(Events)
    };

process_activity(cancel_transfer, St) ->
    {ok, Events} = ff_transfer:cancel(transfer(St)),
    #{
        events => ff_machine:emit_events(Events)
    }.

set_poll_timer(St) ->
    Now = machinery_time:now(),
    Timeout = erlang:max(1, machinery_time:interval(Now, ff_machine:updated(St)) div 1000),
    {set_timer, {timeout, Timeout}}.

-spec process_call(_CallArgs, machine(), _, handler_opts()) ->
    {ok, result()}.

process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.

emit_failure(Reason) ->
    ff_machine:emit_event({status_changed, {failed, Reason}}).

%%

-spec deduce_activity(transfer(_)) ->
    activity().

deduce_activity(#{status := {failed, _}}) ->
    cancel_transfer;
deduce_activity(#{status := succeeded}) ->
    commit_transfer;
deduce_activity(#{session := _}) ->
    await_session_completion;
deduce_activity(#{p_transfer := #{status := prepared}}) ->
    create_session;
deduce_activity(#{p_transfer := #{status := created}}) ->
    prepare_transfer;
deduce_activity(_) ->
    undefined.
