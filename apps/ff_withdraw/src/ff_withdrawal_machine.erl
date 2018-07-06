%%%
%%% Withdrawal machine
%%%

-module(ff_withdrawal_machine).

%% API

-type id()        :: machinery:id().
-type timestamp() :: machinery:timestamp().
-type ctx()       :: ff_ctx:ctx().
-type withdrawal() :: ff_withdrawal:withdrawal().

-type activity() ::
    prepare_transfer         |
    create_session           |
    await_session_completion |
    commit_transfer          |
    cancel_transfer          |
    undefined                .

-type st()        :: #{
    activity      := activity(),
    withdrawal    := withdrawal(),
    ctx           := ctx(),
    times         => {timestamp(), timestamp()}
}.

-export_type([id/0]).

-export([create/3]).
-export([get/1]).
-export([get_status_events/2]).

%% Accessors

-export([withdrawal/1]).
-export([activity/1]).
-export([ctx/1]).
-export([status/1]).
-export([created/1]).
-export([updated/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

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
        {source, notfound} |
        {destination, notfound | unauthorized} |
        {provider, notfound} |
        _TransferError |
        exists
    }.

create(ID, #{source := SourceID, destination := DestinationID, body := Body}, Ctx) ->
    do(fun () ->
        Source       = ff_wallet_machine:wallet(unwrap(source,ff_wallet_machine:get(SourceID))),
        Destination  = ff_destination_machine:destination(
            unwrap(destination, ff_destination_machine:get(DestinationID))
        ),
        ok           = unwrap(destination, valid(authorized, ff_destination:status(Destination))),
        Provider     = unwrap(provider, ff_withdrawal_provider:choose(Destination, Body)),
        Events1      = unwrap(ff_withdrawal:create(ID, Source, Destination, Body, Provider)),
        Events2      = unwrap(ff_withdrawal:create_transfer(ff_withdrawal:collapse_events(Events1))),
        unwrap(machinery:start(?NS, ID, {Events1 ++ Events2, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, st()} |
    {error, notfound}.

get(ID) ->
    do(fun () ->
        collapse(unwrap(machinery:get(?NS, ID, backend())))
    end).

-type event_cursor() :: non_neg_integer() | undefined.

-spec get_status_events(id(), event_cursor()) ->
    {ok, [ev()]}      |
    {error, notfound} .

get_status_events(ID, Cursor) ->
    do(fun () ->
        unwrap(machinery:get(?NS, ID, {Cursor, undefined, forward}, backend()))
    end).

backend() ->
    fistful:backend(?NS).

%% Accessors

-spec withdrawal(st()) -> withdrawal().
-spec activity(st())   -> activity().
-spec ctx(st())        -> ctx().
-spec created(st())    -> timestamp() | undefined.
-spec updated(st())    -> timestamp() | undefined.

withdrawal(#{withdrawal := V}) -> V.
activity(#{activity := V})     -> V.
ctx(#{ctx := V})               -> V.
status(St)                     -> genlib_map:get(status, St).
created(St)                    -> erlang:element(1, times(St)).
updated(St)                    -> erlang:element(2, times(St)).

times(St) ->
    genlib_map:get(times, St, {undefined, undefined}).

%% Machinery

-type ts_ev(T) ::
    {ev, timestamp(), T}.

-type ev() ::
    ff_withdrawal:ev().

-type auxst() ::
    #{ctx => ctx()}.

-type machine()      :: machinery:machine(ts_ev(ev()), auxst()).
-type result()       :: machinery:result(ts_ev(ev()), auxst()).
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
    lager:log(info, self(), "process_activity:~p w/ ~p", [activity(St), St]),
    process_activity(activity(St), St).

process_activity(prepare_transfer, St) ->
    case ff_withdrawal:prepare_transfer(withdrawal(St)) of
        {ok, Events} ->
            #{events => emit_events(Events), action => continue};
        {error, Reason} ->
            #{events => emit_failure(Reason)}
    end;

process_activity(create_session, St) ->
    case ff_withdrawal:create_session(withdrawal(St)) of
        {ok, Events} ->
            #{
                events => emit_events(Events),
                action => set_poll_timer(St)
            };
        {error, Reason} ->
            #{events => emit_failure(Reason)}
    end;

process_activity(await_session_completion, St) ->
    case ff_withdrawal:poll_session_completion(withdrawal(St)) of
        {ok, Events} when length(Events) > 0 ->
            #{events => emit_events(Events), action => continue};
        {ok, []} ->
            #{action => set_poll_timer(St)}
    end;

process_activity(commit_transfer, St) ->
    {ok, Events} = ff_withdrawal:commit_transfer(withdrawal(St)),
    #{
        events => emit_events(Events ++ [{status_changed, succeeded}])
    };

process_activity(cancel_transfer, St) ->
    {ok, Events} = ff_withdrawal:cancel_transfer(withdrawal(St)),
    #{
        events => emit_events(Events ++ [{status_changed, {failed, <<"transfer cancelled">>}}])
    }.

set_poll_timer(St) ->
    Now = machinery_time:now(),
    Timeout = erlang:max(1, machinery_time:interval(Now, updated(St)) div 1000),
    {set_timer, {timeout, Timeout}}.

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
    apply_event(ff_withdrawal:hydrate(EvBody, maps:get(withdrawal, St1, undefined)), St1).

apply_event(Ev, St) ->
    W1 = ff_withdrawal:apply_event(Ev, maps:get(withdrawal, St, undefined)),
    St#{
        activity   => deduce_activity(Ev),
        withdrawal => W1
    }.

deduce_activity({created, _}) ->
    undefined;
deduce_activity({transfer, {created, _}}) ->
    undefined;
deduce_activity({transfer, {status_changed, created}}) ->
    prepare_transfer;
deduce_activity({transfer, {status_changed, prepared}}) ->
    create_session;
deduce_activity({session_started, _}) ->
    await_session_completion;
deduce_activity({session_finished, _}) ->
    undefined;
deduce_activity({status_changed, succeeded}) ->
    commit_transfer;
deduce_activity({status_changed, {failed, _}}) ->
    cancel_transfer;
deduce_activity({transfer, {status_changed, committed}}) ->
    undefined;
deduce_activity({transfer, {status_changed, cancelled}}) ->
    undefined;
deduce_activity({status_changed, _}) ->
    undefined.

%%

emit_event(E) ->
    emit_events([E]).

emit_events(Es) ->
    emit_events(Es, machinery_time:now()).

emit_events(Es, Ts) ->
    [{ev, Ts, ff_withdrawal:dehydrate(Body)} || Body <- Es].

merge_ts_event({ev, Ts, Body}, St = #{times := {Created, _Updated}}) ->
    {Body, St#{times => {Created, Ts}}};
merge_ts_event({ev, Ts, Body}, St = #{}) ->
    {Body, St#{times => {Ts, Ts}}}.
