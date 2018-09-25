%%%
%%% Withdrawal
%%%

-module(ff_withdrawal).

-type id(T)         :: T.
-type wallet()      :: ff_wallet:id(_).
-type destination() :: ff_destination:id(_).
-type body()        :: ff_transaction:body().
-type provider()    :: ff_withdrawal_provider:id().
-type transfer()    :: ff_transfer:transfer().

-type withdrawal() :: #{
    id          := id(binary()),
    source      := wallet(),
    destination := destination(),
    body        := body(),
    provider    := provider(),
    transfer    := ff_maybe:maybe(transfer()),
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
    {created, withdrawal()}         |
    {transfer, ff_transfer:ev()}    |
    {session_started, session()}    |
    {session_finished, session()}   |
    {status_changed, status()}      .

-export_type([withdrawal/0]).
-export_type([event/0]).

-export([id/1]).
-export([source/1]).
-export([destination/1]).
-export([body/1]).
-export([provider/1]).
-export([transfer/1]).
-export([status/1]).

-export([create/4]).
-export([prepare_transfer/1]).
-export([commit_transfer/1]).
-export([cancel_transfer/1]).
-export([create_session/1]).
-export([poll_session_completion/1]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, with/3, valid/2]).

%% Accessors

-spec id(withdrawal())          -> id(binary()).
-spec source(withdrawal())      -> wallet().
-spec destination(withdrawal()) -> destination().
-spec body(withdrawal())        -> body().
-spec provider(withdrawal())    -> provider().
-spec status(withdrawal())      -> status().
-spec transfer(withdrawal())    -> transfer().

id(#{id := V})                  -> V.
source(#{source := V})           -> V.
destination(#{destination := V}) -> V.
body(#{body := V})               -> V.
provider(#{provider := V})       -> V.
status(#{status := V})           -> V.
transfer(#{transfer := V})       -> V.

%%

-spec create(id(_), wallet(), destination(), body()) ->
    {ok, [event()]} |
    {error,
        {source, notfound} |
        {destination, notfound | unauthorized} |
        {provider, notfound} |
        _TransferError
    }.

create(ID, SourceID, DestinationID, Body) ->
    do(fun () ->
        Source = ff_wallet_machine:wallet(
            unwrap(source, ff_wallet_machine:get(SourceID))
        ),
        Destination = ff_destination_machine:destination(
            unwrap(destination, ff_destination_machine:get(DestinationID))
        ),
        ok = unwrap(destination, valid(authorized, ff_destination:status(Destination))),
        ProviderID = unwrap(provider, ff_withdrawal_provider:choose(Source, Destination, Body)),
        TransferEvents = unwrap(ff_transfer:create(
            construct_transfer_id(ID),
            [{ff_wallet:account(Source), ff_destination:account(Destination), Body}]
        )),
        [{created, #{
            id          => ID,
            source      => SourceID,
            destination => DestinationID,
            body        => Body,
            provider    => ProviderID
        }}] ++
        [{transfer, Ev} || Ev <- TransferEvents] ++
        [{status_changed, pending}]
    end).

construct_transfer_id(ID) ->
    ID.

-spec prepare_transfer(withdrawal()) ->
    {ok, [event()]} |
    {error, _TransferError}.

prepare_transfer(Withdrawal) ->
    with(transfer, Withdrawal, fun ff_transfer:prepare/1).

-spec commit_transfer(withdrawal()) ->
    {ok, [event()]} |
    {error, _TransferError}.

commit_transfer(Withdrawal) ->
    with(transfer, Withdrawal, fun ff_transfer:commit/1).

-spec cancel_transfer(withdrawal()) ->
    {ok, [event()]} |
    {error, _TransferError}.

cancel_transfer(Withdrawal) ->
    with(transfer, Withdrawal, fun ff_transfer:cancel/1).

-spec create_session(withdrawal()) ->
    {ok, [event()]} |
    {error, _SessionError}.

create_session(Withdrawal) ->
    ID = construct_session_id(id(Withdrawal)),
    {ok, SourceSt} = ff_wallet_machine:get(source(Withdrawal)),
    Source = ff_wallet_machine:wallet(SourceSt),
    {ok, DestinationSt} = ff_destination_machine:get(destination(Withdrawal)),
    Destination = ff_destination_machine:destination(DestinationSt),
    {ok, Provider} = ff_withdrawal_provider:get(provider(Withdrawal)),
    {ok, SenderSt} = ff_identity_machine:get(ff_wallet:identity(Source)),
    {ok, ReceiverSt} = ff_identity_machine:get(ff_destination:identity(Destination)),
    WithdrawalParams = #{
        id          => ID,
        destination => Destination,
        cash        => body(Withdrawal),
        sender      => ff_identity_machine:identity(SenderSt),
        receiver    => ff_identity_machine:identity(ReceiverSt)
    },
    do(fun () ->
        ok = unwrap(ff_withdrawal_provider:create_session(ID, WithdrawalParams, Provider)),
        [{session_started, ID}]
    end).

construct_session_id(ID) ->
    ID.

-spec poll_session_completion(withdrawal()) ->
    {ok, [event()]}.

poll_session_completion(_Withdrawal = #{session := SID}) ->
    {ok, Session} = ff_withdrawal_session_machine:get(SID),
    do(fun () ->
        case ff_withdrawal_session_machine:status(Session) of
            active ->
                [];
            {finished, {success, _}} ->
                [
                    {session_finished, SID},
                    {status_changed, succeeded}
                ];
            {finished, {failed, Failure}} ->
                [
                    {session_finished, SID},
                    {status_changed, {failed, Failure}}
                ]
        end
    end);
poll_session_completion(_Withdrawal) ->
    {error, {session, notfound}}.

%%

-spec apply_event(event(), ff_maybe:maybe(withdrawal())) ->
    withdrawal().

apply_event({created, W}, undefined) ->
    W;
apply_event({status_changed, S}, W) ->
    maps:put(status, S, W);
apply_event({transfer, Ev}, W = #{transfer := T}) ->
    W#{transfer := ff_transfer:apply_event(Ev, T)};
apply_event({transfer, Ev}, W) ->
    apply_event({transfer, Ev}, W#{transfer => undefined});
apply_event({session_started, S}, W) ->
    maps:put(session, S, W);
apply_event({session_finished, S}, W = #{session := S}) ->
    maps:remove(session, W).
