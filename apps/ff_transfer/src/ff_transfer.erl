%%%
%%% Transfer between 2 accounts
%%%

-module(ff_transfer).

-type handler()     :: module().

-define(ACTUAL_FORMAT_VERSION, 2).

-opaque transfer(T) :: #{
    version       := ?ACTUAL_FORMAT_VERSION,
    id            := id(),
    transfer_type := transfer_type(),
    body          := body(),
    params        := params(T),
    p_transfer    => maybe(p_transfer()),
    session_id    => session_id(),
    route         => any(),
    resource      => any(),
    status        => status(),
    external_id   => id()
}.

-type route(T) :: T.
-type resource(T) :: T.

-type status() ::
    pending         |
    succeeded       |
    {failed, _TODO} .

-type event(Params, Route) :: event(Params, Route, Resource :: any()).

-type event(Params, Route, Resource) ::
    {created, transfer(Params)}             |
    {route_changed, route(Route)}           |
    {p_transfer, ff_postings_transfer:event()} |
    {session_started, session_id()}         |
    {session_finished, session_id()}        |
    {resource_got, resource(Resource)}     |
    {status_changed, status()}              .

-type event() :: event(Params :: any(), Route :: any(), Resource :: any()).

-type args() :: #{
    id            := id(),
    body          := body(),
    params        := params(),
    transfer_type := transfer_type(),

    status        => status(),
    external_id   => external_id()
}.

-export_type([args/0]).
-export_type([transfer/1]).
-export_type([handler/0]).
-export_type([params/1]).
-export_type([event/0]).
-export_type([event/2]).
-export_type([event/3]).
-export_type([status/0]).
-export_type([route/1]).
-export_type([body/0]).
-export_type([id/0]).
-export_type([legacy_event/0]).
-export_type([params/0]).
-export_type([transfer/0]).
-export_type([transfer_type/0]).

-export([gen/1]).
-export([id/1]).
-export([transfer_type/1]).
-export([body/1]).
-export([params/1]).
-export([p_transfer/1]).
-export([status/1]).
-export([session_id/1]).
-export([route/1]).
-export([resource/1]).
-export([external_id/1]).

-export([create/6]).

%% ff_transfer_machine behaviour
-behaviour(ff_transfer_machine).
-export([process_transfer/1]).
-export([process_failure/2]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/2]).

%% Pipeline

-compile({parse_transform, ff_pipeline}).
-import(ff_pipeline, [do/1, unwrap/1, with/3]).

%% Internal types

-type id() :: binary().
-type external_id() :: id() | undefined.
-type body() :: ff_transaction:body().
-type route() :: route(any()).
-type maybe(T) :: ff_maybe:maybe(T).
-type params() :: params(any()).
-type params(T) :: T.
-type transfer() :: transfer(any()).
-type session_id() :: id().
-type p_transfer() :: ff_postings_transfer:transfer().
-type legacy_event() :: any().
-type transfer_type() :: atom().

%% Constructor

-spec gen(args()) -> transfer().
gen(Args) ->
    TypeKeys = [id, transfer_type, body, params, status, external_id],
    genlib_map:compact(maps:with(TypeKeys, Args)).

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

-spec status(transfer()) -> maybe(status()).
status(#{status := V}) ->
    V;
status(_Other) ->
    undefined.

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

-spec resource(transfer())  -> maybe(resource(any())).
resource(#{resource := V}) ->
    V;
resource(_Other) ->
    undefined.

-spec external_id(transfer()) ->
    external_id().

external_id(#{external_id := ExternalID}) ->
    ExternalID;
external_id(_Transfer) ->
    undefined.

%% API

-spec create(handler(), id(), body(), params(), external_id(), list(event())) ->
    {ok, [event()]}.

create(TransferType, ID, Body, Params, ExternalID, AddEvents) ->
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
        ] ++ AddEvents
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
    {ok, {undefined, ShutdownEvents ++ [{status_changed, {failed, Reason}}]}}.

do_process_failure(_Reason, #{status := pending, p_transfer := #{status := created}}) ->
    {ok, []};
do_process_failure(_Reason, #{status := pending, p_transfer := #{status := prepared}} = Transfer) ->
    do(fun () ->
        unwrap(with(p_transfer, Transfer, fun ff_postings_transfer:cancel/1))
    end);
do_process_failure(Reason, #{status := pending, p_transfer := #{status := committed}}) ->
    erlang:error({unprocessable_failure, committed_p_transfer, Reason});
do_process_failure(_Reason, Transfer) ->
    no_p_transfer = maps:get(p_transfer, Transfer, no_p_transfer),
    {ok, []}.

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
        {continue, unwrap(with(p_transfer, Transfer, fun ff_postings_transfer:prepare/1))}
    end);
process_activity(commit_transfer, Transfer) ->
    do(fun () ->
        {undefined, unwrap(with(p_transfer, Transfer, fun ff_postings_transfer:commit/1))}
    end);
process_activity(cancel_transfer, Transfer) ->
    do(fun () ->
        {undefined, unwrap(with(p_transfer, Transfer, fun ff_postings_transfer:cancel/1))}
    end).

add_external_id(undefined, Event) ->
    Event;
add_external_id(ExternalID, Event) ->
    Event#{external_id => ExternalID}.

%%

-spec apply_event(event() | legacy_event(), ff_maybe:maybe(transfer(T))) ->
    transfer(T).
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev, maybe_transfer_type(T)), T).

-spec apply_event_(event(), ff_maybe:maybe(transfer(T))) ->
    transfer(T).
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, S}, T) ->
    maps:put(status, S, T);
apply_event_({p_transfer, Ev}, T = #{p_transfer := PT}) ->
    T#{p_transfer := ff_postings_transfer:apply_event(Ev, PT)};
apply_event_({p_transfer, Ev}, T) ->
    apply_event({p_transfer, Ev}, T#{p_transfer => undefined});
apply_event_({session_started, S}, T) ->
    maps:put(session_id, S, T);
apply_event_({session_finished, S}, T = #{session_id := S}) ->
    T;
apply_event_({route_changed, R}, T) ->
    maps:put(route, R, T);
apply_event_({resource_got, R}, T) ->
    maps:put(resource, R, T).

maybe_transfer_type(undefined) ->
    undefined;
maybe_transfer_type(T) ->
    transfer_type(T).

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
