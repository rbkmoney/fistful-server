%%%
%%% Transfer - сущность, описывающая процесс передачи средств
%%%

-module(ff_transfer_new).

-type handler()     :: module().

-define(ACTUAL_FORMAT_VERSION, 3).

-opaque transfer(T) :: #{
    version             := ?ACTUAL_FORMAT_VERSION,
    id                  := id(),
    transfer_type       := transfer_type(),
    body                := body(),
    params              := params(T),
    session_type        := session_type(),
    activity            => activity(),
    route               => route(),
    transaction         => transaction(),
    status              => status(),
    external_id         => id(),

    reverts             => list(revert()),
    adjustments         => list(adjustment())
}.

% Глобальное состояние трансфера
-type status() :: {cmd(), cmd_status()}.
-type route(T) :: T.

-type cmd() ::
    base_flow        |
    revert           |
    adjustment       .

% Состояние выполняемой в данный момент операции
-type cmd_status() ::
    pending          |
    succeeded        |
    {failed, _TODO}  .

-type event(Params, Route) ::
    {created, transfer(Params)}             |
    {route_changed, route(Route)}           |
    {status_changed, status()}              |
    {cmd, cmd_event()}                      .

-type cmd_event() ::
    {transaction, transaction_event()}      |
    {revert, revert_event()}                |
    {adjustment, adjustment_event()}        .

-type event()               :: event(Params :: any(), Route :: any()).
-type maybe(T)              :: ff_maybe:maybe(T).
-type body()                :: ff_transaction:body().
-type transaction()         :: ff_transaction_new:transaction().
-type revert()              :: any().
-type adjustment()          :: any().
-type transaction_event()   :: ff_transaction_new:event().
-type revert_event()        :: any().
-type adjustment_event()    :: any().
-type session_type()        :: ff_transaction_new:session_type().
-type route()               :: route(any()).
-type activity()            ::
    new                      |
    routing                  |
    transaction_starting     |
    transaction_polling      .

-export_type([transfer/1]).
-export_type([handler/0]).
-export_type([params/1]).
-export_type([event/0]).
-export_type([event/2]).
-export_type([status/0]).
-export_type([body/0]).
-export_type([session_type/0]).

-export([id/1]).
-export([transfer_type/1]).
-export([body/1]).
-export([params/1]).
-export([external_id/1]).
-export([transaction/1]).
-export([activity/1]).
-export([route/1]).
-export([status/1]).
-export([get_empty_session_type/0]).
-export([get_session_type/1]).

-export([create/1]).
-export([process_failure/2]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%% Internal types

-type id()              :: binary().
-type external_id()     :: id() | undefined.
-type params()          :: params(any()).
-type params(T)         :: T.
-type transfer()        :: transfer(any()).
-type legacy_event()    :: any().
-type transfer_type()   :: atom().

%% Accessors

-spec id(transfer()) -> id().
id(#{id := V}) ->
    V.

-spec transfer_type(transfer()) -> transfer_type().
transfer_type(#{transfer_type := V}) ->
    V.

-spec body(transfer()) -> body().
body(#{body := V}) ->
    V.

-spec params(transfer(T)) -> params(T).
params(#{params := V}) ->
    V.

-spec external_id(transfer()) ->
    external_id().

external_id(#{external_id := ExternalID}) ->
    ExternalID;
external_id(_Transfer) ->
    undefined.

-spec transaction(transfer()) ->
    maybe(transaction()).

transaction(#{transaction := Transaction}) ->
    Transaction;
transaction(_Transfer) ->
    undefined.

-spec activity(transfer()) ->
    maybe(activity()).

activity(#{activity := Activity}) ->
    Activity;
activity(_Transfer) ->
    undefined.

-spec route(transfer())  -> maybe(route()).
route(#{route := V}) ->
    V;
route(_Other) ->
    undefined.

-spec status(transfer())  -> maybe(status()).
status(#{status := V}) ->
    V;
status(_Other) ->
    undefined.

-spec get_empty_session_type() ->
    session_type().

get_empty_session_type() ->
    ff_transaction_new:get_empty_session_type().

-spec get_session_type(deposit | withdrawal) ->
    session_type().

get_session_type(Type) ->
    ff_transaction_new:get_session_type(Type).

%% API

-type create_params() :: #{
    handler         := handler(),
    id              := id(),
    body            := body(),
    params          := params(),
    session_type    := session_type(),
    external_id     := maybe(id())
}.

-spec create(create_params()) ->
    {ok, [event()]} |
    {error,
        _PostingsTransferError
    }.

create(#{
    handler := TransferType,
    id := ID,
    body := Body,
    params := Params,
    session_type := SessionType,
    external_id := ExternalID
}) ->
    do(fun () ->
        [
            {created, add_external_id(ExternalID, #{
                version       => ?ACTUAL_FORMAT_VERSION,
                id            => ID,
                transfer_type => TransferType,
                body          => Body,
                params        => Params,
                session_type  => SessionType
            })},
            {status_changed, {base_flow, pending}}
        ]
    end).

add_external_id(undefined, Event) ->
    Event;
add_external_id(ExternalID, Event) ->
    Event#{external_id => ExternalID}.

%%

-spec process_failure(any(), transfer()) ->
    {ok, {ff_transfer_machine_new:action(), [ff_transfer_machine_new:event(event())]}} |
    {error, _Reason}.

process_failure(Reason, #{activity := Activity}) when
    Activity =:= routing orelse
    Activity =:= transaction_starting
->
    {ok, {undefined, [{status_changed, {failed, Reason}}]}};
process_failure(Reason, Transfer = #{activity := transaction_polling}) ->
    {_Action, TransactionEvents} = unwrap(
        ff_transaction_new:process_failure(Reason, transaction(Transfer))),
    {ok, {undefined,
        TransactionEvents ++
        [{status_changed, {failed, Reason}}]
    }}.

%%

-spec apply_event(event() | legacy_event(), ff_maybe:maybe(transfer(T))) ->
    transfer(T).
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev, maybe_transfer_type(T)), T).

-spec apply_event_(event(), ff_maybe:maybe(transfer(T))) ->
    transfer(T).
apply_event_({created, T}, undefined) ->
    set_activity(routing, T);
apply_event_({status_changed, S}, T) ->
    maps:put(status, S, T);
apply_event_({route_changed, R}, T) ->
    set_activity(transaction_starting, maps:put(route, R, T));
apply_event_({cmd, {transaction, Ev}}, T) ->
    {Action, Transaction} = ff_transaction_new:apply_event(Ev, transaction(T)),
    Activity = action_to_activity(transaction, Action),
    set_activity(Activity, maps:put(transaction, Transaction, T)).

maybe_transfer_type(undefined) ->
    undefined;
maybe_transfer_type(T) ->
    transfer_type(T).

set_activity(undefined, T) ->
    T;
set_activity(Activity, T) ->
    maps:put(activity, Activity, T).

-spec action_to_activity(transaction, undefined | next) ->
    undefined | transaction_polling.

action_to_activity(transaction, undefined) ->
    undefined;
action_to_activity(transaction, next) ->
    transaction_polling.

-spec maybe_migrate(event() | legacy_event(), transfer_type() | undefined) ->
    event().
% Actual events
maybe_migrate(Ev = {created, #{version := ?ACTUAL_FORMAT_VERSION}}, _) ->
    Ev;
maybe_migrate(Ev = {p_transfer, _PEvent}, EventType) ->
    {cmd, {transaction, ff_transaction_new:maybe_migrate(Ev, EventType)}};
maybe_migrate(Ev = {status_changed, {_, _}}, _) ->
    Ev;
% Old events
maybe_migrate({status_changed, Status}, EventType) ->
    maybe_migrate({status_changed, {base_flow, Status}}, EventType);
maybe_migrate({created, #{version := 2, handler := ff_withdrawal} = T}, EventType) ->
    maybe_migrate({created, T#{
        version       => 3,
        session_type  => ff_transaction_new:get_session_type(withdrawal)
        }}, EventType);
maybe_migrate({created, #{version := 2, handler := ff_deposit} = T}, EventType) ->
    maybe_migrate({created, T#{
        version       => 3,
        session_type  => ff_transaction_new:get_empty_session_type()
    }}, EventType);
maybe_migrate({created, #{version := 1, handler := ff_withdrawal} = T}, EventType) ->
    #{
        version     := 1,
        id          := ID,
        handler     := ff_withdrawal,
        source      := SourceAccount,
        destination := DestinationAccount,
        body        := Body,
        params      := #{
            destination := DestinationID,
            source      := SourceID
        }
    } = T,
    maybe_migrate({created, #{
        version       => 2,
        id            => ID,
        transfer_type => withdrawal,
        body          => Body,
        params        => #{
            wallet_id             => SourceID,
            destination_id        => DestinationID,
            wallet_account        => SourceAccount,
            destination_account   => DestinationAccount,
            wallet_cash_flow_plan => #{
                postings => [
                    #{
                        sender   => {wallet, sender_settlement},
                        receiver => {wallet, receiver_destination},
                        volume   => {share, {{1, 1}, operation_amount, default}}
                    }
                ]
            }
        }
    }}, EventType);
maybe_migrate({created, #{version := 1, handler := ff_deposit} = T}, EventType) ->
    #{
        version     := 1,
        id          := ID,
        handler     := ff_deposit,
        source      := SourceAccount,
        destination := DestinationAccount,
        body        := Body,
        params      := #{
            destination := DestinationID,
            source      := SourceID
        }
    } = T,
    maybe_migrate({created, #{
        version       => 2,
        id            => ID,
        transfer_type => deposit,
        body          => Body,
        params        => #{
            wallet_id             => DestinationID,
            source_id             => SourceID,
            wallet_account        => DestinationAccount,
            source_account        => SourceAccount,
            wallet_cash_flow_plan => #{
                postings => [
                    #{
                        sender   => {wallet, sender_source},
                        receiver => {wallet, receiver_settlement},
                        volume   => {share, {{1, 1}, operation_amount, default}}
                    }
                ]
            }
        }
    }}, EventType);
maybe_migrate({created, T}, EventType) ->
    DestinationID = maps:get(destination, T),
    {ok, DestinationSt} = ff_destination:get_machine(DestinationID),
    DestinationAcc = ff_destination:account(ff_destination:get(DestinationSt)),
    SourceID = maps:get(source, T),
    {ok, SourceSt} = ff_wallet_machine:get(SourceID),
    SourceAcc = ff_wallet:account(ff_wallet_machine:wallet(SourceSt)),
    maybe_migrate({created, T#{
        version     => 1,
        handler     => ff_withdrawal,
        source      => SourceAcc,
        destination => DestinationAcc,
        params => #{
            destination => DestinationID,
            source      => SourceID
        }
    }}, EventType);
maybe_migrate({transfer, PTransferEv}, EventType) ->
    maybe_migrate({p_transfer, PTransferEv}, EventType);
% Other events
maybe_migrate(Ev, _) ->
    Ev.
