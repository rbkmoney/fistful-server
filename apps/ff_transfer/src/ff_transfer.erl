%%%
%%% Transfer between 2 accounts
%%%

-module(ff_transfer).

-type handler()     :: module().

-define(ACTUAL_FORMAT_VERSION, 2).

-opaque transfer(T) :: #{
    version             := ?ACTUAL_FORMAT_VERSION,
    id                  := id(),
    transfer_type       := transfer_type(),
    body                := body(),
    params              := params(T),
    p_transfer          => maybe(p_transfer()),
    session_id          => session_id(),
    route               => any(),
    status              => status(),
    external_id         => id(),
    p_transfer_count    => integer(),
    reposits            => list(ff_reposit:reposit()),
    current_reposit     => ff_reposit:reposit(),
    action              => maybe(action())
}.

-type route(T) :: T.

-type action() ::
    revert.

-type reverted_params() :: #{
    reposit_id  := id(),
    details     => binary()
}.

-type status() ::
    pending                         |
    succeeded                       |
    {failed, _TODO}                 |
    {reverted, reverted_params()}   .

-type event(Params, Route) ::
    {created, transfer(Params)}             |
    {route_changed, route(Route)}           |
    {p_transfer, ff_postings_transfer:event()} |
    {session_started, session_id()}         |
    {session_finished, session_id()}        |
    {status_changed, status()}              |
    {reposit, ff_reposit:event()}           .

-type event()  :: event(Params :: any(), Route :: any()).
-type maybe(T) :: ff_maybe:maybe(T).
-type body()   :: ff_transaction:body().
-type route()  :: route(any()).

-export_type([transfer/1]).
-export_type([handler/0]).
-export_type([params/1]).
-export_type([event/0]).
-export_type([event/2]).
-export_type([status/0]).
-export_type([route/1]).
-export_type([body/0]).
-export_type([action/0]).

-export([id/1]).
-export([transfer_type/1]).
-export([body/1]).
-export([params/1]).
-export([p_transfer/1]).
-export([status/1]).
-export([session_id/1]).
-export([route/1]).
-export([external_id/1]).
-export([reposit/1]).
-export([reposits/1]).
-export([action/1]).

-export([create/5]).

%% ff_transfer_machine behaviour
-behaviour(ff_transfer_machine).
-export([process_transfer/1]).
-export([process_failure/2]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/2]).
-export([wrap_events/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, with/3]).

%% Internal types

-type id() :: binary().
-type external_id() :: id() | undefined.
-type params() :: params(any()).
-type params(T) :: T.
-type transfer() :: transfer(any()).
-type session_id() :: id().
-type p_transfer() :: ff_postings_transfer:transfer().
-type legacy_event() :: any().
-type transfer_type() :: atom().

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

-spec status(transfer()) -> status().
status(#{status := V}) ->
    V.

-spec p_transfer(transfer())  -> maybe(p_transfer()).
p_transfer(#{p_transfer := V}) ->
    V;
p_transfer(_Other) ->
    undefined.

-spec session_id(transfer())  -> maybe(session_id()).
session_id(#{session_id := V}) ->
    V;
session_id(_Other) ->
    undefined.

-spec route(transfer())  -> maybe(route()).
route(#{route := V}) ->
    V;
route(_Other) ->
    undefined.

-spec external_id(transfer()) ->
    external_id().

external_id(#{external_id := ExternalID}) ->
    ExternalID;
external_id(_Transfer) ->
    undefined.

-spec reposit(transfer())  -> maybe(ff_reposit:reposit()).
reposit(#{current_reposit := V}) ->
    V;
reposit(_Other) ->
    undefined.

-spec reposits(transfer())  -> maybe(list(ff_reposit:reposit())).
reposits(#{reposits := V}) ->
    V;
reposits(_Other) ->
    undefined.

-spec action(transfer())  -> maybe(action()).
action(#{action := V}) ->
    V;
action(_Other) ->
    undefined.

%% API

-spec create(handler(), id(), body(), params(), external_id()) ->
    {ok, [event()]} |
    {error,
        _PostingsTransferError
    }.

create(TransferType, ID, Body, Params, ExternalID) ->
    do(fun () ->
        [
            {created, add_external_id(ExternalID, #{
                version       => ?ACTUAL_FORMAT_VERSION,
                id            => ID,
                transfer_type => TransferType,
                body          => Body,
                params        => Params
            })},
            {status_changed, pending}
        ]
    end).

%% ff_transfer_machine behaviour

-spec process_transfer(transfer()) ->
    {ok, {ff_transfer_machine:action(), [ff_transfer_machine:event(event())]}} |
    {error, _Reason}.

process_transfer(Transfer) ->
    process_activity(deduce_activity(Transfer), Transfer).

-spec process_failure(any(), transfer()) ->
    {ok, {ff_transfer_machine:action(), [ff_transfer_machine:event(event())]}} |
    {error, _Reason}.

process_failure(Reason, Transfer) ->
    {ok, ShutdownEvents} = do_process_failure(Reason, Transfer),
    ActionEvents = get_fail_events_for_action(action(Transfer), Reason),
    {ok, {undefined,
        ShutdownEvents ++
        [{status_changed, {failed, Reason}}] ++
        ActionEvents
    }}.

do_process_failure(_Reason, #{status := pending, p_transfer := #{status := created}}) ->
    {ok, []};
do_process_failure(_Reason, #{status := pending, p_transfer := #{status := prepared}} = Transfer) ->
    do(fun () ->
        unwrap(with(
            p_transfer,
            Transfer,
            wrap_process_callback(fun ff_postings_transfer:cancel/2, Transfer)))
    end);
do_process_failure(Reason, #{status := pending, p_transfer := #{status := committed}}) ->
    erlang:error({unprocessable_failure, committed_p_transfer, Reason});
do_process_failure(_Reason, Transfer) ->
    no_p_transfer = maps:get(p_transfer, Transfer, no_p_transfer),
    {ok, []}.

get_fail_events_for_action(undefined, _Reason) ->
    [];
get_fail_events_for_action(revert, Reason) ->
    ff_transfer:wrap_events(reposit, ff_reposit:update_status({failed, Reason})).

-type activity() ::
    prepare_transfer         |
    commit_transfer          |
    cancel_transfer          .

-spec deduce_activity(transfer()) ->
    activity().
deduce_activity(#{status := {failed, _}, p_transfer := #{status := prepared}}) ->
    cancel_transfer;
deduce_activity(#{status := succeeded, p_transfer := #{status := prepared}}) ->
    commit_transfer;
deduce_activity(#{status := pending, p_transfer := #{status := created}}) ->
    prepare_transfer.

process_activity(prepare_transfer, Transfer) ->
    do(fun () ->
        {continue, unwrap(with(
            p_transfer,
            Transfer,
            wrap_process_callback(fun ff_postings_transfer:prepare/2, Transfer)))}
    end);
process_activity(commit_transfer, Transfer) ->
    do(fun () ->
        {undefined, unwrap(with(
            p_transfer,
            Transfer,
            wrap_process_callback(fun ff_postings_transfer:commit/2, Transfer)))}
    end);
process_activity(cancel_transfer, Transfer) ->
    do(fun () ->
        {undefined, unwrap(with(
            p_transfer,
            Transfer,
            wrap_process_callback(fun ff_postings_transfer:cancel/2, Transfer)))}
    end).

add_external_id(undefined, Event) ->
    Event;
add_external_id(ExternalID, Event) ->
    Event#{external_id => ExternalID}.

wrap_process_callback(Fun, Transfer) ->
    Count = maps:get(p_transfer_count, Transfer),
    fun(State) ->
        Fun(Count, State)
    end.

%%

-spec apply_event(event() | legacy_event(), ff_maybe:maybe(transfer(T))) ->
    transfer(T).
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev, maybe_transfer_type(T)), T).

-spec apply_event_(event(), ff_maybe:maybe(transfer(T))) ->
    transfer(T).
apply_event_({created, T}, undefined) ->
    init_action(T);
apply_event_({status_changed, S}, T) ->
    maps:put(status, S, T);
apply_event_({p_transfer, Ev}, T0 = #{p_transfer := PT}) ->
    T = increment_if_new(ff_postings_transfer:is_new_transfer(Ev, PT), T0),
    T#{p_transfer := ff_postings_transfer:apply_event(Ev, PT)};
apply_event_({p_transfer, Ev}, T) ->
    apply_event({p_transfer, Ev}, T#{p_transfer => undefined});
apply_event_({reposit, Ev}, T) ->
    Reposits = ff_reposit:apply_event(Ev, reposits(T)),
    CurrentReposit = ff_reposit:get_current(Reposits),
    NewT = T#{
        reposits => Reposits,
        current_reposit => CurrentReposit
    },
    set_action(revert, NewT);
apply_event_({session_started, S}, T) ->
    maps:put(session_id, S, T);
apply_event_({session_finished, S}, T = #{session_id := S}) ->
    T;
apply_event_({route_changed, R}, T) ->
    maps:put(route, R, T).

maybe_transfer_type(undefined) ->
    undefined;
maybe_transfer_type(T) ->
    transfer_type(T).

increment_if_new({_, true}, T) ->
    increment_transfer_count(T);
increment_if_new(_, T) ->
    T.

increment_transfer_count(T = #{p_transfer_count := Count}) ->
    T#{p_transfer_count := Count + 1};
increment_transfer_count(T) ->
    T#{p_transfer_count => 1}.

init_action(T) ->
    set_action(undefined, T).

set_action(Action, Transfer) ->
    maps:put(action, Action, Transfer).

-spec maybe_migrate(event() | legacy_event(), transfer_type() | undefined) ->
    event().
% Actual events
maybe_migrate(Ev = {created, #{version := ?ACTUAL_FORMAT_VERSION}}, _) ->
    Ev;
maybe_migrate({p_transfer, PEvent}, EventType) ->
    {p_transfer, ff_postings_transfer:maybe_migrate(PEvent, EventType)};
% Old events
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

-spec wrap_events(atom(), list()) ->
    list(event()).

wrap_events(reposit, Events) ->
    [{reposit, Ev} || Ev <- Events];
wrap_events(Type, Events) ->
    erlang:error({unknown_event_type, {Type, Events}}).
