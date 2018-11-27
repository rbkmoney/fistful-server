%%%
%%% Transfer machine
%%%

-module(ff_transfer_machine).

%% API

-type id()        :: machinery:id().
-type ns()        :: machinery:namespace().
-type transfer(T) :: ff_transfer:transfer(T).
-type event(T)    :: T.
-type events(T)   :: [{integer(), ff_machine:timestamped_event(event(T))}].
-type params() :: #{
    handler     := ff_transfer:handler(),
    body        := ff_transaction:body(),
    params      := ff_transfer:params()
}.

%% Behaviour definition

-type st(T)    :: ff_machine:st(transfer(T)).
-type action() :: poll | continue | undefined.

-export_type([id/0]).
-export_type([ns/0]).
-export_type([action/0]).
-export_type([st/1]).
-export_type([event/1]).
-export_type([events/1]).
-export_type([params/0]).

-callback process_transfer(transfer(_)) ->
    {ok, {action(), [event(_)]}} |
    {error, _Reason}.

-callback process_call(_CallArgs, transfer(_)) ->
    {ok, {action(), [event(_)]}} |
    {error, _Reason}.

-callback process_failure(_Reason, transfer(_)) ->
    {ok, {action(), [event(_)]}} |
    {error, _Reason}.

-optional_callbacks([process_call/2]).

%% API

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

%% Internal types

-type ctx()           :: ff_ctx:ctx().
-type transfer()      :: ff_transfer:transfer().
-type transfer_type() :: ff_transfer:transfer_type().

%% API

-spec create(ns(), id(), params(), ctx()) ->
    {ok, st(_)} |
    {error,
        _TransferError |
        {conflict, id()} |
        {compare_error, id()}
    }.

create(NS, ID,
    #{handler := Handler, body := Body, params := Params},
Ctx)
->
    do(fun () ->
        Events = unwrap(ff_transfer:create(handler_to_type(Handler), ID, Body, Params)),
        case machinery:start(NS, ID, {Events, Ctx}, backend(NS)) of
            ok ->
                {ok, ff_machine:get(ff_transfer, NS, ID)};
            {error, exists} ->
                compare_events(NS, ID, Events)
        end
    end).

-spec get(ns(), id()) ->
    {ok, st(_)}      |
    {error, notfound}.

get(NS, ID) ->
    ff_machine:get(ff_transfer, NS, ID).

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

%% Accessors

-spec transfer(st(T)) ->
    transfer(T).

transfer(St) ->
    ff_machine:model(St).

%% Machinery

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

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

process_timeout(Machine, _, _Opts) ->
    St = ff_machine:collapse(ff_transfer, Machine),
    Transfer = transfer(St),
    process_result(handler_process_transfer(Transfer), St).

-spec process_call(_CallArgs, machine(), _, handler_opts()) ->
    {ok, result()}.

process_call(CallArgs, Machine, _, _Opts) ->
    St = ff_machine:collapse(ff_transfer, Machine),
    Transfer = transfer(St),
    {ok, process_result(handler_process_call(CallArgs, Transfer), St)}.

process_result({ok, {Action, Events}}, St) ->
    genlib_map:compact(#{
        events => set_events(Events),
        action => set_action(Action, St)
    });
process_result({error, Reason}, St) ->
    {ok, {Action, Events}} = handler_process_failure(Reason, transfer(St)),
    genlib_map:compact(#{
        events => set_events(Events),
        action => set_action(Action, St)
    }).

set_events([]) ->
    undefined;
set_events(Events) ->
    ff_machine:emit_events(Events).

set_action(continue, _St) ->
    continue;
set_action(undefined, _St) ->
    undefined;
set_action(poll, St) ->
    Now = machinery_time:now(),
    Timeout = erlang:max(1, machinery_time:interval(Now, ff_machine:updated(St)) div 1000),
    {set_timer, {timeout, Timeout}}.


%% Handler convertors

-spec handler_to_type(module()) ->
    transfer_type().
handler_to_type(ff_deposit) ->
    deposit;
handler_to_type(ff_withdrawal) ->
    withdrawal.

-spec type_to_handler(transfer_type()) ->
    module().
type_to_handler(deposit) ->
    ff_deposit;
type_to_handler(withdrawal) ->
    ff_withdrawal.

-spec transfer_handler(transfer()) ->
    module().
transfer_handler(Transfer) ->
    TransferType = ff_transfer:transfer_type(Transfer),
    type_to_handler(TransferType).

%% Handler calls

handler_process_call(CallArgs, Transfer) ->
    Handler = transfer_handler(Transfer),
    Handler:process_call(CallArgs, Transfer).

handler_process_transfer(Transfer) ->
    Handler = transfer_handler(Transfer),
    Handler:process_transfer(Transfer).

handler_process_failure(Reason, Transfer) ->
    Handler = transfer_handler(Transfer),
    Handler:process_failure(Reason, Transfer).

compare_events(NS, ID, NewEv) ->
    Limit = length(NewEv),
    case events(NS, ID, {undefined, Limit, forward}) of
        {ok, OldEv} ->
            compare_events_(NS, ID, NewEv, [Ev || {_, {ev, _, Ev}} <- OldEv]);
        {error, notfound} ->
            {error, {compare_error, ID}}
    end.

compare_events_(NS, ID, NewEv, OldEv) when NewEv =:= OldEv ->
    {ok, ff_machine:get(ff_transfer, NS, ID)};
compare_events_(_NS, ID, _NewEv, _Ev) ->
    {error, {conflict, ID}}.
