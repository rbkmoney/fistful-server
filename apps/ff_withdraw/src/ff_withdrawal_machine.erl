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
    {failed, _}.

-type activity() ::
    prepare_destination_transfer |
    commit_destination_transfer  |
    idle                         .

-type st()        :: #{
    activity      := activity(),
    status        => status(),
    withdrawal    := withdrawal(),
    times         := {timestamp(), timestamp()},
    ctx           := ctx()
}.

-export([create/4]).
-export([get/2]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%%

-define(NS, withdrawal).

-type backend() :: machinery:backend(_).

-type params() :: #{
    source      := ff_wallet_machine:id(),
    destination := ff_destination_machine:id(),
    body        := ff_transaction:body()
}.

-spec create(id(), params(), ctx(), backend()) ->
    {ok, withdrawal()} |
    {error,
        {source, notfound} |
        {destination, notfound | unauthorized} |
        {provider, notfound} |
        _TransferError |
        exists
    }.

create(ID, #{source := SourceID, destination := DestinationID, body := Body}, Ctx, Be) ->
    do(fun () ->
        Source      = unwrap(source, ff_wallet_machine:get(SourceID, Be)),
        Destination = unwrap(destination, ff_destination_machine:get(DestinationID, Be)),
        authorized  = unwrap(destination, valid(authorized, ff_destination:status(Destination))),
        Provider    = unwrap(provider, ff_withdrawal_provider:choose(Destination, Body)),
        Withdrawal0 = unwrap(ff_withdrawal:create(Source, Destination, ID, Body, Provider)),
        Withdrawal1 = unwrap(ff_withdrawal:setup_transfer(Withdrawal0)),
        ok          = unwrap(machinery:start(?NS, ID, {Withdrawal1, Ctx}, Be)),
        unwrap(get(ID, Be))
    end).

-spec get(id(), backend()) ->
    {ok, withdrawal()} |
    {error, notfound}.

get(ID, Be) ->
    do(fun () ->
        withdrawal(collapse(unwrap(machinery:get(?NS, ID, Be))))
    end).

%% Machinery

-type ts_ev(T) ::
    {ev, timestamp(), T}.

-type ev() :: ts_ev(
    {created, withdrawal()}                               |
    {status_changed, status()}                            |
    transfer_prepared                                     |
    transfer_committed                                    |
    {session_created, session()}                          |
    {session_started, session()}                          |
    {session_status_changed, session(), session_status()}
).

-type machine()      :: machinery:machine(ev()).
-type result()       :: machinery:result(ev()).
-type handler_opts() :: machinery:handler_opts().

-spec init({withdrawal(), ctx()}, machine(), _, handler_opts()) ->
    result().

init({Withdrawal, Ctx}, #{}, _, _Opts) ->
    #{
        events    => emit_ts_event({created, Withdrawal}),
        action    => continue,
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

process_timeout(Machine, _, _Opts) ->
    St = collapse(Machine),
    process_activity(activity(St), withdrawal(St)).

process_activity(prepare_destination_transfer, W0) ->
    case ff_withdrawal:prepare_destination_transfer(W0) of
        {ok, _W1} ->
            #{
                events => emit_ts_event({transfer_status_changed, prepared}),
                action => continue
            };
        {error, Reason} ->
            #{
                events => emit_ts_event({status_changed, {failed, {transfer, Reason}}})
            }
    end;

process_activity(commit_destination_transfer, W0) ->
    {ok, T0} = ff_withdrawal:transfer(W0),
    {ok, T1} = ff_transfer:commit(T0),
    #{
        events => emit_ts_event({transfer_status_changed, ff_transfer:status(T1)}),
        action => continue
    };

process_activity(create_withdrawal_session, W0) ->
    ok.

-spec process_call(none(), machine(), _, handler_opts()) ->
    {_, result()}.

process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.

%%

collapse(#{history := History, aux_state := #{ctx := Ctx}}) ->
    collapse_history(History, #{activity => idle, ctx => Ctx}).

collapse_history(History, St) ->
    lists:foldl(fun merge_event/2, St, History).

merge_event({_ID, _Ts, TsEv}, St0) ->
    {EvBody, St1} = merge_ts_event(TsEv, St0),
    merge_event_body(EvBody, St1).

merge_event_body({created, Withdrawal}, St) ->
    St#{
        activity   => prepare_destination_transfer,
        withdrawal => Withdrawal
    };

merge_event_body({transfer_status_changed, S}, St) ->
    W0 = withdrawal(St),
    W1 = ff_withdrawal:mod_transfer(fun (T) -> ff_transfer:set_status(S, T) end, W0),
    St#{
        activity => case S of
            prepared  -> commit_destination_transfer;
            committed -> create_withdrawal_session
        end,
        withdrawal => W1
    }.

activity(#{activity := V}) ->
    V.

withdrawal(#{withdrawal := V}) ->
    V.

%%

emit_ts_event(E) ->
    emit_ts_events([E]).

emit_ts_events(Es) ->
    emit_ts_events(Es, machinery_time:now()).

emit_ts_events(Es, Ts) ->
    [{ev, Ts, Body} || Body <- Es].

merge_ts_event({ev, Ts, Body}, St = #{times := {Created, _Updated}}) ->
    {Body, St#{times => {Created, Ts}}};
merge_ts_event({ev, Ts, Body}, St = #{}) ->
    {Body, St#{times => {Ts, Ts}}}.
