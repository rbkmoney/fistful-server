%%%
%%% Withdrawal machine
%%%

-module(ff_withdrawal_machine).

%% API

-type id()        :: machinery:id().
-type timestamp() :: machinery:timestamp().
-type ctx()       :: ff_ctx:ctx().

-type withdrawal() :: ff_withdrawal:withdrawal().
-type status()     ::
    succeeded |
    failed    .

-type activity() ::
    prepare_transfer         |
    create_session           |
    await_session_completion |
    commit_transfer          |
    cancel_transfer          |
    undefined                .

-type st()        :: #{
    activity      := activity(),
    status        => status(),
    withdrawal    := withdrawal(),
    times         := {timestamp(), timestamp()},
    ctx           := ctx()
}.

-export_type([id/0]).

-export([create/3]).
-export([get/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%%

-define(NS, withdrawal).

-type params() :: #{
    source      := ff_wallet_machine:id(),
    destination := ff_destination_machine:id(),
    body        := ff_transaction:body()
}.

-spec create(id(), params(), ctx()) ->
    ok |
    {error,
        {source, notfound} |
        {destination, notfound | unauthorized} |
        {provider, notfound} |
        _TransferError |
        exists
    }.

create(ID, #{source := SourceID, destination := DestinationID, body := Body}, Ctx) ->
    do(fun () ->
        Source       = unwrap(source, ff_wallet_machine:get(SourceID)),
        Destination  = unwrap(destination, ff_destination_machine:get(DestinationID)),
        authorized   = unwrap(destination, valid(authorized, ff_destination:status(Destination))),
        Provider     = unwrap(provider, ff_withdrawal_provider:choose(Destination, Body)),
        Withdrawal   = unwrap(ff_withdrawal:create(Source, Destination, ID, Body, Provider)),
        {Events1, _} = unwrap(ff_withdrawal:create_transfer(Withdrawal)),
        Events       = [{created, Withdrawal} | Events1],
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, withdrawal()} |
    {error, notfound}.

get(ID) ->
    do(fun () ->
        withdrawal(collapse(unwrap(machinery:get(?NS, ID, backend()))))
    end).

backend() ->
    ff_withdraw:backend(?NS).

%% Machinery

-type ts_ev(T) ::
    {ev, timestamp(), T}.

-type ev() :: ts_ev(
    {created, withdrawal()}    |
    {status_changed, status()} |
    ff_withdrawal:ev()
).

-type auxst() ::
    #{ctx => ctx()}.

-type machine()      :: machinery:machine(ev(), auxst()).
-type result()       :: machinery:result(ev(), auxst()).
-type handler_opts() :: machinery:handler_opts(_).

-spec init({[ev()], ctx()}, machine(), _, handler_opts()) ->
    result().

init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events    => emit_events(Events),
        action    => continue,
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

process_timeout(Machine, _, _Opts) ->
    St = collapse(Machine),
    process_activity(activity(St), St).

process_activity(prepare_transfer, St) ->
    case ff_withdrawal:prepare_transfer(withdrawal(St)) of
        {ok, {Events, _W1}} ->
            #{events => emit_events(Events), action => continue};
        {error, Reason} ->
            #{events => emit_failure(Reason)}
    end;

process_activity(create_session, St) ->
    case ff_withdrawal:create_session(withdrawal(St)) of
        {ok, Session} ->
            #{
                events => emit_event({session_created, Session}),
                action => {set_timer, {timeout, 1}}
            };
        {error, Reason} ->
            #{events => emit_failure(Reason)}
    end;

process_activity(await_session_completion, St) ->
    case ff_withdrawal:poll_session_completion(withdrawal(St)) of
        {ok, {Events, _W1}} when length(Events) > 0 ->
            #{events => emit_events(Events), action => continue};
        {ok, {[], _W0}} ->
            Now     = machinery_time:now(),
            Timeout = erlang:max(1, machinery_time:interval(Now, updated(St))),
            #{action => {set_timer, {timeout, Timeout}}}
    end;

process_activity(commit_transfer, St) ->
    {ok, {Events, _W1}} = ff_withdrawal:commit_transfer(withdrawal(St)),
    #{
        events => emit_events(Events ++ [{status_changed, succeeded}])
    };

process_activity(cancel_transfer, St) ->
    {ok, {Events, _W1}} = ff_withdrawal:cancel_transfer(withdrawal(St)),
    #{
        events => emit_events(Events ++ [{status_changed, failed}])
    }.

-spec process_call(_CallArgs, machine(), _, handler_opts()) ->
    {ok, result()}.

process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.

emit_failure(Reason) ->
    emit_event({status_changed, {failed, Reason}}).

%%

collapse(#{history := History, aux_state := #{ctx := Ctx}}) ->
    collapse_history(History, #{activity => idle, ctx => Ctx}).

collapse_history(History, St) ->
    lists:foldl(fun merge_event/2, St, History).

merge_event({_ID, _Ts, TsEv}, St0) ->
    {EvBody, St1} = merge_ts_event(TsEv, St0),
    apply_event(EvBody, St1).

apply_event({created, Withdrawal}, St) ->
    St#{withdrawal => Withdrawal};
apply_event({status_changed, Status}, St) ->
    St#{status => Status};
apply_event(Ev, St) ->
    W1 = ff_withdrawal:apply_event(Ev, withdrawal(St)),
    St#{
        activity   => deduce_activity(Ev),
        withdrawal => W1
    }.

deduce_activity({transfer_created, _}) ->
    prepare_transfer;
deduce_activity({transfer, {status_changed, prepared}}) ->
    create_session;
deduce_activity({session_created, _}) ->
    await_session_completion;
deduce_activity({session, {status_changed, succeeded}}) ->
    commit_transfer;
deduce_activity({session, {status_changed, {failed, _}}}) ->
    cancel_transfer;
deduce_activity({transfer, {status_changed, committed}}) ->
    undefined;
deduce_activity({transfer, {status_changed, cancelled}}) ->
    undefined;
deduce_activity({status_changed, _}) ->
    undefined.

activity(#{activity := V}) ->
    V.

withdrawal(#{withdrawal := V}) ->
    V.

updated(#{times := {_, V}}) ->
    V.

%%

emit_event(E) ->
    emit_events([E]).

emit_events(Es) ->
    emit_events(Es, machinery_time:now()).

emit_events(Es, Ts) ->
    [{ev, Ts, Body} || Body <- Es].

merge_ts_event({ev, Ts, Body}, St = #{times := {Created, _Updated}}) ->
    {Body, St#{times => {Created, Ts}}};
merge_ts_event({ev, Ts, Body}, St = #{}) ->
    {Body, St#{times => {Ts, Ts}}}.
