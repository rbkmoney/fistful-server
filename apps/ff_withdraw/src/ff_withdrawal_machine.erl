%%%
%%% Withdrawal machine
%%%

-module(ff_withdrawal_machine).

%% API

-type id()        :: machinery:id().
-type ctx()       :: ff_ctx:ctx().
-type withdrawal() :: ff_withdrawal:withdrawal().

-type activity() ::
    prepare_transfer         |
    create_session           |
    await_session_completion |
    commit_transfer          |
    cancel_transfer          |
    undefined                .

-type st() ::
    ff_machine:st(withdrawal()).

-export_type([id/0]).

-export([create/3]).
-export([get/1]).
-export([events/2]).

%% Accessors

-export([withdrawal/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-define(NS, 'ff/withdrawal').

-type params() :: #{
    source      := ff_wallet_machine:id(),
    destination := ff_destination_machine:id(),
    body        := ff_transaction:body()
}.

-spec create(id(), params(), ctx()) ->
    ok |
    {error,
        _WithdrawalError |
        exists
    }.

create(ID, #{source := SourceID, destination := DestinationID, body := Body}, Ctx) ->
    do(fun () ->
        Events = unwrap(ff_withdrawal:create(ID, SourceID, DestinationID, Body)),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, st()}       |
    {error, notfound}.

get(ID) ->
    ff_machine:get(ff_withdrawal, ?NS, ID).

-spec events(id(), machinery:range()) ->
    {ok, [{integer(), ff_machine:timestamped_event(event())}]} |
    {error, notfound}.

events(ID, Range) ->
    do(fun () ->
        #{history := History} = unwrap(machinery:get(?NS, ID, Range, backend())),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

backend() ->
    fistful:backend(?NS).

%% Accessors

-spec withdrawal(st()) ->
    withdrawal().

withdrawal(St) ->
    ff_machine:model(St).

%% Machinery

-type event() ::
    ff_withdrawal:event().

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
    St = ff_machine:collapse(ff_withdrawal, Machine),
    process_activity(deduce_activity(withdrawal(St)), St).

process_activity(prepare_transfer, St) ->
    case ff_withdrawal:prepare_transfer(withdrawal(St)) of
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
    case ff_withdrawal:create_session(withdrawal(St)) of
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
    case ff_withdrawal:poll_session_completion(withdrawal(St)) of
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
    {ok, Events} = ff_withdrawal:commit_transfer(withdrawal(St)),
    #{
        events => ff_machine:emit_events(Events)
    };

process_activity(cancel_transfer, St) ->
    {ok, Events} = ff_withdrawal:cancel_transfer(withdrawal(St)),
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

-spec deduce_activity(withdrawal()) ->
    activity().

deduce_activity(#{status := {failed, _}}) ->
    cancel_transfer;
deduce_activity(#{status := succeeded}) ->
    commit_transfer;
deduce_activity(#{session := _}) ->
    await_session_completion;
deduce_activity(#{transfer := #{status := prepared}}) ->
    create_session;
deduce_activity(#{transfer := #{status := created}}) ->
    prepare_transfer;
deduce_activity(_) ->
    undefined.
