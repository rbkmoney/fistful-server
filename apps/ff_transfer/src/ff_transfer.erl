%%%
%%% Transfer between 2 accounts
%%%

-module(ff_transfer).

-type id(T)         :: T.
-type type()        :: withdrawal. %% | deposit | transfer.
-type account()     :: ff_account:account().
-type body()        :: ff_transaction:body().
-type params(T)     :: T.
-type p_transfer()  :: ff_postings_transfer:transfer().

-type transfer(T) :: #{
    version     := non_neg_integer(),
    id          := id(binary()),
    type        := type(),
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
-export_type([type/0]).
-export_type([params/1]).
-export_type([event/0]).

-export([id/1]).
-export([type/1]).
-export([source/1]).
-export([destination/1]).
-export([body/1]).
-export([params/1]).
-export([p_transfer/1]).
-export([status/1]).

-export([create/6]).
-export([prepare/1]).
-export([create_session/1]).
-export([poll_session_completion/1]).
-export([commit/1]).
-export([cancel/1]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, with/3]).

%% Accessors

-spec id(transfer(_))          -> id(binary()).
-spec type(transfer(_))        -> type().
-spec source(transfer(_))      -> account().
-spec destination(transfer(_)) -> account().
-spec body(transfer(_))        -> body().
-spec params(transfer(T))      -> params(T).
-spec status(transfer(_))      -> status().
-spec p_transfer(transfer(_))  -> p_transfer().

id(#{id := V})                   -> V.
type(#{type := V})               -> V.
source(#{source := V})           -> V.
destination(#{destination := V}) -> V.
body(#{body := V})               -> V.
params(#{params := V})           -> V.
status(#{status := V})           -> V.
p_transfer(#{p_transfer := V})   -> V.

%%

-spec create(type(), id(_), account(), account(), body(), params(_)) ->
    {ok, [event()]} |
    {error,
        _PostingsTransferError
    }.

create(Type, ID, Source, Destination, Body, Params) ->
    do(fun () ->
        PTransferID = construct_p_transfer_id(ID),
        PostingsTransferEvents = unwrap(ff_postings_transfer:create(PTransferID, [{Source, Destination, Body}])),
        [{created, #{
            version     => 1,
            id          => ID,
            type        => Type,
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

-spec prepare(transfer(_)) ->
    {ok, [event()]} |
    {error, _PostingsTransferError}.

prepare(Transfer) ->
    with(p_transfer, Transfer, fun ff_postings_transfer:prepare/1).

-spec commit(transfer(_)) ->
    {ok, [event()]} |
    {error, _PostingsTransferError}.

commit(Transfer) ->
    with(p_transfer, Transfer, fun ff_postings_transfer:commit/1).

-spec cancel(transfer(_)) ->
    {ok, [event()]} |
    {error, _PostingsTransferError}.

cancel(Transfer) ->
    with(p_transfer, Transfer, fun ff_postings_transfer:cancel/1).

-spec create_session(transfer(_)) ->
    {ok, [event()]} |
    {error, _SessionError}.

create_session(Transfer) ->
    ID = construct_session_id(id(Transfer)),
    {ok, SenderSt} = ff_identity_machine:get(ff_account:identity(source(Transfer))),
    {ok, ReceiverSt} = ff_identity_machine:get(ff_account:identity(destination(Transfer))),
    TransferData = #{
        id          => ID,
        cash        => body(Transfer),
        sender      => ff_identity_machine:identity(SenderSt),
        receiver    => ff_identity_machine:identity(ReceiverSt)
    },
    do(fun () ->
        ok = unwrap(ff_transfer_session:create(type(Transfer), ID, TransferData, params(Transfer))),
        [{session_started, ID}]
    end).

construct_session_id(ID) ->
    ID.

-spec poll_session_completion(transfer(_)) ->
    {ok, [event()]}.

poll_session_completion(_Transfer = #{type := Type, session := SID}) ->
    {ok, Session} = ff_transfer_session:get(Type, SID),
    do(fun () ->
        case ff_transfer_session:status(Type, Session) of
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
poll_session_completion(_Transfer) ->
    {error, {session, notfound}}.

%%

-spec apply_event(event(), ff_maybe:maybe(transfer(T))) ->
    transfer(T).

apply_event({created, T}, undefined) ->
    migrate(T);
apply_event({status_changed, S}, T) ->
    maps:put(status, S, T);
apply_event({p_transfer, Ev}, T = #{p_transfer := PT}) ->
    T#{p_transfer := ff_postings_transfer:apply_event(Ev, PT)};
apply_event({p_transfer, Ev}, T) ->
    apply_event({p_transfer, Ev}, T#{p_transfer => undefined});
apply_event({session_started, S}, T) ->
    maps:put(session, S, T);
apply_event({session_finished, S}, T = #{session := S}) ->
    maps:remove(session, T).

%%

migrate(T = #{version := 1}) ->
    T;
migrate(T) ->
    DestinationID = maps:get(destination, T),
    {ok, DestinationSt} = ff_destination_machine:get(DestinationID),
    DestinationAcc = ff_destination:account(ff_destination_machine:destination(DestinationSt)),
    SourceID = maps:get(source, T),
    {ok, SourceSt} = ff_wallet_machine:get(SourceID),
    SourceAcc = ff_wallet:account(ff_wallet_machine:wallet(SourceSt)),
    T#{
        version     => 1,
        type        => withdrawal,
        source      => SourceAcc,
        destination => DestinationAcc,
        params => #{
            destination => DestinationID,
            source      => SourceID
        }
    }.
