%%%
%%% Transfer between 2 accounts
%%%

-module(ff_transfer).

-type id(T)         :: T.
-type handler()     :: module().
-type account()     :: ff_account:account().
-type body()        :: ff_transaction:body().
-type params(T)     :: T.
-type p_transfer()  :: ff_postings_transfer:transfer().

-type transfer(T) :: #{
    version     := non_neg_integer(),
    id          := id(binary()),
    handler     := handler(),
    source      := account(),
    destination := account(),
    body        := body(),
    params      := params(T),
    p_transfer  := ff_maybe:maybe(p_transfer()),
    session     => session(),
    status      => status()
}.

-type session() ::
    id(_).

-type status() ::
    pending         |
    succeeded       |
    {failed, _TODO} .

-type event() ::
    {created, transfer(_)}                  |
    {p_transfer, ff_postings_transfer:ev()} |
    {session_started, session()}            |
    {session_finished, session()}           |
    {status_changed, status()}              .

-export_type([transfer/1]).
-export_type([handler/0]).
-export_type([params/1]).
-export_type([event/0]).

-export([id/1]).
-export([handler/1]).
-export([source/1]).
-export([destination/1]).
-export([body/1]).
-export([params/1]).
-export([p_transfer/1]).
-export([status/1]).

-export([create/6]).

%% ff_transfer_machine behaviour
-behaviour(ff_transfer_machine).
-export([process_transfer/1]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, with/3]).

%% Accessors

-spec id(transfer(_))          -> id(binary()).
-spec handler(transfer(_))     -> handler().
-spec source(transfer(_))      -> account().
-spec destination(transfer(_)) -> account().
-spec body(transfer(_))        -> body().
-spec params(transfer(T))      -> params(T).
-spec status(transfer(_))      -> status().
-spec p_transfer(transfer(_))  -> p_transfer().

id(#{id := V})                   -> V.
handler(#{handler := V})         -> V.
source(#{source := V})           -> V.
destination(#{destination := V}) -> V.
body(#{body := V})               -> V.
params(#{params := V})           -> V.
status(#{status := V})           -> V.
p_transfer(#{p_transfer := V})   -> V.

%%

-spec create(handler(), id(_), account(), account(), body(), params(_)) ->
    {ok, [event()]} |
    {error,
        _PostingsTransferError
    }.

create(Handler, ID, Source, Destination, Body, Params) ->
    do(fun () ->
        PTransferID = construct_p_transfer_id(ID),
        PostingsTransferEvents = unwrap(ff_postings_transfer:create(PTransferID, [{Source, Destination, Body}])),
        [{created, #{
            version     => 1,
            id          => ID,
            handler     => Handler,
            source      => Source,
            destination => Destination,
            body        => Body,
            params      => Params
        }}] ++
        [{p_transfer, Ev} || Ev <- PostingsTransferEvents] ++
        [{status_changed, pending}]
    end).

construct_p_transfer_id(ID) ->
    ID.

%% ff_transfer_machine behaviour

-spec process_transfer(transfer(_)) ->
    {ok, [ff_transfer_machine:event(event())] | poll} |
    {error, _Reason}.

process_transfer(Transfer) ->
    process_activity(deduce_activity(Transfer), Transfer).

-type activity() ::
    prepare_transfer         |
    commit_transfer          |
    cancel_transfer          |
    undefined                .

-spec deduce_activity(transfer(_)) ->
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
    with(p_transfer, Transfer, fun ff_postings_transfer:prepare/1);

process_activity(commit_transfer, Transfer) ->
    with(p_transfer, Transfer, fun ff_postings_transfer:commit/1);

process_activity(cancel_transfer, Transfer) ->
    with(p_transfer, Transfer, fun ff_postings_transfer:cancel/1).

%%

-spec apply_event(event(), ff_maybe:maybe(transfer(T))) ->
    transfer(T).

apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev), T).

apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, S}, T) ->
    maps:put(status, S, T);
apply_event_({p_transfer, Ev}, T = #{p_transfer := PT}) ->
    T#{p_transfer := ff_postings_transfer:apply_event(Ev, PT)};
apply_event_({p_transfer, Ev}, T) ->
    apply_event({p_transfer, Ev}, T#{p_transfer => undefined});
apply_event_({session_started, S}, T) ->
    maps:put(session, S, T);
apply_event_({session_finished, S}, T = #{session := S}) ->
    maps:remove(session, T).

maybe_migrate(Ev = {created, #{version := 1}}) ->
    Ev;
maybe_migrate({created, T}) ->
    DestinationID = maps:get(destination, T),
    {ok, DestinationSt} = ff_destination:get_machine(DestinationID),
    DestinationAcc = ff_destination:account(ff_destination:get(DestinationSt)),
    SourceID = maps:get(source, T),
    {ok, SourceSt} = ff_wallet_machine:get(SourceID),
    SourceAcc = ff_wallet:account(ff_wallet_machine:wallet(SourceSt)),
    {created, T#{
        version     => 1,
        handler     => ff_withdrawal,
        source      => SourceAcc,
        destination => DestinationAcc,
        params => #{
            destination => DestinationID,
            source      => SourceID
        }
    }};
maybe_migrate({transfer, PTransferEv}) ->
    {p_transfer, PTransferEv};
maybe_migrate(Ev) ->
    Ev.
