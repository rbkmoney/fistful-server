%%%
%%% Transfer machine
%%%

-module(ff_transfer_machine_new).

%% API

-type id()          :: machinery:id().
-type external_id() :: id() | undefined.
-type ns()          :: machinery:namespace().
-type transfer(T)   :: ff_transfer_new:transfer(T).
-type event(T)      :: T.
-type events(T)     :: [{integer(), ff_machine:timestamped_event(event(T))}].
-type params()      :: #{
    handler         := ff_transfer_new:handler(),
    body            := ff_transfer_new:body(),
    params          := ff_transfer_new:params(),
    session_type    := ff_transfer_new:session_type(),
    external_id => external_id()
}.

-type revert_params()      :: #{
    id      := id(),
    body    := ff_transaction:body(),
    reason  := binary() | undefined
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
-export_type([revert_params/0]).

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
% -export([revert/2]).

%% Accessors

-export([transfer/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%% Internal types

-type ctx()           :: ff_ctx:ctx().
-type transfer()      :: ff_transfer_new:transfer().
-type transfer_type() :: ff_transfer_new:transfer_type().

%% API

-spec create(ns(), id(), params(), ctx()) ->
    ok |
    {error,
        _TransferError |
        exists
    }.

create(
    NS,
    ID,
    Args = #{handler := Handler, body := Body, params := Params, session_type := SessionType},
    Ctx
) ->
    do(fun () ->
        Events = unwrap(ff_transfer_new:create(#{
            handler         => handler_to_type(Handler),
            id              => ID,
            body            => Body,
            params          => Params,
            session_type    => SessionType,
            external_id     => maps:get(external_id, Args, undefined)
        })),
        unwrap(machinery:start(NS, ID, {Events, Ctx}, backend(NS)))
    end).

-spec get(ns(), id()) ->
    {ok, st(_)}      |
    {error, notfound}.

get(NS, ID) ->
    ff_machine:get(ff_transfer_new, NS, ID).

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

% -spec revert(ns(), revert_params()) ->
%     {ok, ff_reposit:reposit()}             |
%     {error,
%         _TransferError |
%         notfound       |
%         not_implemented
%     }.

% % revert(ff_withdrawal, _ID) ->
% %     {error, not_implemented};
% revert(NS, #{id := ID, body := Body, reason := Reason}) ->
%     case machinery:call(NS, ID, {revert, Body, Reason}, backend(NS)) of
%         {ok, _} ->
%             do(fun () ->
%                 Transfer = transfer(unwrap(get(NS, ID))),
%                 ff_transfer_new:reposit(Transfer)
%             end);
%         {error, _} = Result ->
%             Result
%     end.

%% Accessors

-spec transfer(st(T)) ->
    transfer(T).

transfer(St) ->
    ff_machine:model(St).

%% Machinery

-define(MAX_SESSION_POLL_TIMEOUT, 4 * 60 * 60).

-type machine()      :: ff_machine:machine(event(_)).
-type result()       :: ff_machine:result(event(_)).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

-spec init({[event(_)], ctx()}, machine(), handler_args(), handler_opts()) ->
    result().

init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events    => ff_machine:emit_events(Events),
        action    => continue,
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), handler_args(), handler_opts()) ->
    result().

process_timeout(Machine, _, _Opts) ->
    St = ff_machine:collapse(ff_transfer_new, Machine),
    Transfer = transfer(St),
    process_result(handler_process_transfer(Transfer), St).

-spec process_call(_CallArgs, machine(), handler_args(), handler_opts()) ->
    {ok, result()}.

process_call(CallArgs, Machine, _, _Opts) ->
    St = ff_machine:collapse(ff_transfer_new, Machine),
    Transfer = transfer(St),
    CallRes = handler_process_call(CallArgs, Transfer),
    Result = process_result(CallRes, St),
    {ok, Result}.

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    result().

process_repair(Scenario, Machine, _Args, _Opts) ->
    ff_repair:apply_scenario(ff_transfer_new, Machine, Scenario).

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
    {set_timer, {timeout, compute_poll_timeout(Now, St)}}.

compute_poll_timeout(Now, St) ->
    MaxTimeout = genlib_app:env(ff_transfer, max_session_poll_timeout, ?MAX_SESSION_POLL_TIMEOUT),
    Timeout0 = machinery_time:interval(Now, ff_machine:updated(St)) div 1000,
    erlang:min(MaxTimeout, erlang:max(1, Timeout0)).

%% Handler convertors

-spec handler_to_type(module()) ->
    transfer_type().
handler_to_type(ff_deposit_new) ->
    deposit;
handler_to_type(ff_withdrawal_new) ->
    withdrawal.

-spec type_to_handler(transfer_type()) ->
    module().
type_to_handler(deposit) ->
    ff_deposit_new;
type_to_handler(withdrawal) ->
    ff_withdrawal_new.

-spec transfer_handler(transfer()) ->
    module().
transfer_handler(Transfer) ->
    TransferType = ff_transfer_new:transfer_type(Transfer),
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
