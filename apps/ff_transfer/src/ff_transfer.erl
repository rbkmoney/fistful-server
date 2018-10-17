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
    status        => status()
}.

-type route(T) :: T.

-type status() ::
    pending         |
    succeeded       |
    {failed, _TODO} .

-type event(Params, Route) ::
    {created, transfer(Params)}             |
    {route_changed, route(Route)}           |
    {p_transfer, ff_postings_transfer:ev()} |
    {session_started, session_id()}         |
    {session_finished, session_id()}        |
    {status_changed, status()}              .

-type event() :: event(Params :: any(), Route :: any()).

-export_type([transfer/1]).
-export_type([handler/0]).
-export_type([params/1]).
-export_type([event/0]).
-export_type([event/2]).
-export_type([status/0]).
-export_type([route/1]).

-export([id/1]).
-export([transfer_type/1]).
-export([body/1]).
-export([params/1]).
-export([p_transfer/1]).
-export([status/1]).
-export([session_id/1]).
-export([route/1]).

-export([create/4]).

%% ff_transfer_machine behaviour
-behaviour(ff_transfer_machine).
-export([process_transfer/1]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, with/3]).

%% Internal types

-type id() :: binary().
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

%% API

-spec create(handler(), id(), body(), params()) ->
    {ok, [event()]} |
    {error,
        _PostingsTransferError
    }.

create(TransferType, ID, Body, Params) ->
    do(fun () ->
        [
            {created, #{
                version       => ?ACTUAL_FORMAT_VERSION,
                id            => ID,
                transfer_type => TransferType,
                body          => Body,
                params        => Params
            }},
            {status_changed, pending}
        ]
    end).

%% ff_transfer_machine behaviour

-spec process_transfer(transfer()) ->
    {ok, {ff_transfer_machine:action(), [ff_transfer_machine:event(event())]}} |
    {error, _Reason}.

process_transfer(Transfer) ->
    process_activity(deduce_activity(Transfer), Transfer).

-type activity() ::
    prepare_transfer         |
    commit_transfer          |
    cancel_transfer          |
    undefined                .

-spec deduce_activity(transfer()) ->
    activity().
deduce_activity(#{status := {failed, _}, p_transfer := #{status := prepared}}) ->
    cancel_transfer;
deduce_activity(#{status := succeeded, p_transfer := #{status := prepared}}) ->
    commit_transfer;
deduce_activity(#{status := pending, p_transfer := #{status := created}}) ->
    prepare_transfer;
deduce_activity(_) ->
    undefined.

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

%%

-spec apply_event(event() | legacy_event(), ff_maybe:maybe(transfer(T))) ->
    transfer(T).
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev), T).

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
    maps:remove(session_id, T).

-spec maybe_migrate(event() | legacy_event()) ->
    event().
% Actual events
maybe_migrate(Ev = {created, #{version := ?ACTUAL_FORMAT_VERSION}}) ->
    Ev;
maybe_migrate({p_transfer, PEvent}) ->
    {p_transfer, ff_postings_transfer:maybe_migrate(PEvent)};
% Old events
maybe_migrate({created, #{version := 1, handler := ff_withdrawal} = T}) ->
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
    }});
maybe_migrate({created, #{version := 1, handler := ff_deposit} = T}) ->
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
    }});
maybe_migrate({created, T}) ->
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
    }});
maybe_migrate({transfer, PTransferEv}) ->
    maybe_migrate({p_transfer, PTransferEv});
% Other events
maybe_migrate(Ev) ->
    Ev.
