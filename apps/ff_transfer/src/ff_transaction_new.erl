%%%
%%% Transaction between 2 accounts
%%%

-module(ff_transaction_new).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-define(ACTUAL_FORMAT_VERSION, 1).

-opaque transaction() :: #{
    version             := ?ACTUAL_FORMAT_VERSION,
    id                  := id(),
    body                := body(),
    session_data        := session_data(),
    final_cash_flow     := final_cash_flow(),
    activity            => activity(),
    p_transfer          => maybe(p_transfer()),
    session_id          => session_id(),
    status              => status()
}.

-type status() ::
    pending          |
    succeeded        |
    {failed, _TODO}  .

-type event() ::
    {created, transaction()}                    |
    {p_transfer, ff_postings_transfer:event()}  |
    {session_started, session_id()}             |
    {session_finished, session_id()}            |
    {status_changed, status()}              .

-type maybe(T)              :: ff_maybe:maybe(T).
-type body()                :: ff_transaction:body().
-type activity()            ::
    new                      |
    p_transfer_starting      |
    p_transfer_preparing     |
    session_starting         |
    session_polling          |
    p_transfer_finishing     .

-type action()              ::
    next                     |
    undefined                .

-type session_data(T) :: #{
    type := ff_session:session_type(),
    params := T
}.

-type session_data() :: session_data(any()).
-type create_params() :: create_params(any()).
-type create_params(T) :: #{
    id              := id(),
    body            := body(),
    final_cash_flow := final_cash_flow(),
    session_data    := session_data(T)
}.

-export_type([transaction/0]).
-export_type([event/0]).
-export_type([status/0]).
-export_type([body/0]).
-export_type([action/0]).
-export_type([create_params/0]).
-export_type([session_data/1]).
-export_type([create_params/1]).

-export([id/1]).
-export([session_type/1]).
-export([body/1]).
-export([p_transfer/1]).
-export([status/1]).
-export([session_id/1]).
-export([activity/1]).
-export([final_cash_flow/1]).

-export([get_empty_session_type/0]).
-export([get_session_type/1]).

-export([create/1]).
-export([process_transaction/1]).
-export([process_failure/2]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, with/3]).

%% Internal types

-type id()              :: binary().
-type session_id()      :: id().
-type session_type()    :: ff_session:session_type().
-type p_transfer()      :: ff_postings_transfer:transaction().
-type final_cash_flow() :: ff_cash_flow:final_cash_flow().

%% Accessors

-spec id(transaction()) -> id().
id(#{id := V}) ->
    V.

-spec session_type(transaction()) -> session_type().
session_type(#{session_data := #{type := V}}) ->
    V.

-spec session_data(transaction()) -> session_data().
session_data(#{session_data := V}) ->
    V.

-spec body(transaction()) -> body().
body(#{body := V}) ->
    V.

-spec status(transaction()) -> status().
status(#{status := V}) ->
    V.

-spec p_transfer(transaction())  -> maybe(p_transfer()).
p_transfer(#{p_transfer := V}) ->
    V;
p_transfer(_Other) ->
    undefined.

-spec session_id(transaction())  -> maybe(session_id()).
session_id(#{session_id := V}) ->
    V;
session_id(_Other) ->
    undefined.

-spec activity(transaction())  -> maybe(activity()).
activity(#{activity := V}) ->
    V;
activity(_Other) ->
    undefined.

-spec final_cash_flow(transaction())  -> final_cash_flow().
final_cash_flow(#{final_cash_flow := V}) ->
    V.

-spec get_empty_session_type() ->
    session_type().

get_empty_session_type() ->
    ff_session:get_empty_session_type().

-spec get_session_type(withdrawal) ->
    session_type().

get_session_type(Type) ->
    ff_session:get_session_type(Type).

%% API

-spec create(create_params()) ->
    {ok, [event()]} |
    {error,
        _PostingsTransferError
    }.

create(#{
    id              := ID,
    body            := Body,
    final_cash_flow := FinalCashFlow,
    session_data    := SessionData
}) ->
    do(fun () ->
        [
            {created, #{
                version         => ?ACTUAL_FORMAT_VERSION,
                id              => ID,
                body            => Body,
                final_cash_flow => FinalCashFlow,
                session_data    => SessionData
            }},
            {status_changed, pending}
        ]
    end).

%% ff_transfer_machine_new behaviour

-spec process_transaction(transaction()) ->
    {ok, {ff_transfer_machine_new:action(), [ff_transfer_machine_new:event(event())]}} |
    {error, _Reason}.

process_transaction(Transaction) ->
    process_activity(activity(Transaction), Transaction).

-spec process_failure(any(), transaction()) ->
    {ok, {ff_transfer_machine_new:action(), [ff_transfer_machine_new:event(event())]}} |
    {error, _Reason}.

process_failure(Reason, Transaction) ->
    {ok, ShutdownEvents} = do_process_failure(Reason, Transaction),
    {ok, {undefined,
        ShutdownEvents ++
        [{status_changed, {failed, Reason}}]
    }}.

do_process_failure(_Reason, #{status := pending, activity := Status}) when
    Status =:= p_transfer_starting orelse
    Status =:= p_transfer_preparing
->
    {ok, []};
do_process_failure(_Reason, #{status := pending, activity := session_starting} = Transaction) ->
    do(fun () ->
        unwrap(with(
            p_transfer,
            Transaction,
            fun ff_postings_transfer:cancel/1))
    end);
do_process_failure(Reason, #{status := pending, activity := Status}) when
    Status =:= session_polling orelse
    Status =:= p_transfer_finishing
->
    erlang:error({unprocessable_failure, {step, Status}, Reason});
do_process_failure(_Reason, Transaction) ->
    no_p_transfer = maps:get(p_transfer, Transaction, no_p_transfer),
    {ok, []}.

process_activity(p_transfer_starting, Transaction) ->
    do(fun () ->
        create_p_transfer(Transaction)
    end);
process_activity(p_transfer_preparing, Transaction) ->
    do(fun () ->
        {continue, unwrap(with(
            p_transfer,
            Transaction,
            fun ff_postings_transfer:prepare/1)
        )}
    end);
process_activity(session_starting, Transaction = #{session_data := #{
    type := Type,
    params := Params
}}) when Type =/= empty ->
    SessionID = construct_session_id(id(Transaction)),
    do(fun () ->
        ok = unwrap(ff_session:create(Type, SessionID, Params)),
        {continue, [{session_started, SessionID}]}
    end);
process_activity(session_starting, _Transaction) ->
    do(fun () ->
        {continue, [{status_changed, succeeded}]}
    end);
process_activity(session_polling, Transaction) ->
    SessionID = session_id(Transaction),
    #{type := Type} = session_data(Transaction),
    {ok, SessionMachine} = ff_session:get(Type, SessionID),
    Session = ff_session_machine:session(SessionMachine),
    do(fun () ->
        case ff_session:status(Session) of
            active ->
                {poll, []};
            {finished, {success, _}} ->
                {continue, [
                    {session_finished, SessionID},
                    {status_changed, succeeded}
                ]};
            {finished, {failed, Failure}} ->
                {continue, [
                    {session_finished, SessionID},
                    {status_changed, {failed, Failure}}
                ]}
        end
    end);
process_activity(p_transfer_finishing, Transaction) ->
    do(fun () ->
        Fun = get_finish_fun(status(Transaction)),
        {undefined, unwrap(with(
            p_transfer,
            Transaction,
            Fun
        ))}
    end).

get_finish_fun(succeeded) ->
    fun ff_postings_transfer:commit/1;
get_finish_fun({failed, _}) ->
    fun ff_postings_transfer:cancel/1.

create_p_transfer(Transaction) ->
    FinalCashFlow = final_cash_flow(Transaction),
    PTransferID = construct_p_transfer_id(id(Transaction)),
    PostingsTransferEvents = unwrap(p_transfer, ff_postings_transfer:create(PTransferID, FinalCashFlow)),
    {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}.

construct_session_id(ID) ->
    ID.

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/transaction/", ID/binary>>.

%%

-spec apply_event(event(), ff_maybe:maybe(transaction())) ->
    {action(), transaction()}.
apply_event(Ev, T) ->
    Transaction = apply_event_(Ev, T),
    {get_action_from_activity(activity(Transaction)), Transaction}.

-spec apply_event_(event(), ff_maybe:maybe(transaction())) ->
    transaction().
apply_event_({created, T}, undefined) ->
    set_activity(p_transfer_starting, T);
apply_event_({status_changed, S}, T) ->
    set_activity(get_activity_from_status(S), maps:put(status, S, T));
apply_event_({p_transfer, Ev}, T = #{p_transfer := PT}) ->
    PTransfer = ff_postings_transfer:apply_event(Ev, PT),
    set_activity(get_activity_from_p_status(PTransfer), T#{p_transfer := PTransfer});
apply_event_({p_transfer, Ev}, T0) ->
    apply_event_({p_transfer, Ev}, T0#{p_transfer => undefined});
apply_event_({session_started, S}, T) ->
    set_activity(session_polling, maps:put(session_id, S, T));
apply_event_({session_finished, S}, T = #{session_id := S}) ->
    T.

set_activity(undefined, T) ->
    T;
set_activity(Activity, T) ->
    maps:put(activity, Activity, T).

get_activity_from_p_status(#{status := created}) ->
    p_transfer_preparing;
get_activity_from_p_status(#{status := prepared}) ->
    session_starting;
get_activity_from_p_status(_) ->
    undefined.

get_activity_from_status(succeeded) ->
    p_transfer_finishing;
get_activity_from_status({failed, _}) ->
    p_transfer_finishing;
get_activity_from_status(_) ->
    undefined.

get_action_from_activity(p_transfer_starting) ->
    next;
get_action_from_activity(_) ->
    undefined.
