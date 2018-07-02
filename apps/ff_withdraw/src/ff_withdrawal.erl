%%%
%%% Withdrawal
%%%

-module(ff_withdrawal).

-type id()          :: machinery:id().
-type wallet()      :: ff_wallet:wallet().
-type destination() :: ff_destination:destination().
-type trxid()       :: ff_transaction:id().
-type body()        :: ff_transaction:body().
-type provider()    :: ff_provider:provider().
-type transfer()    :: ff_transfer:transfer().

-type withdrawal() :: #{
    id          := id(),
    source      := wallet(),
    destination := destination(),
    trxid       := trxid(),
    body        := body(),
    provider    := provider(),
    transfer    => transfer(),
    session     => session()
}.

-type session() ::
    {_ID, session_status()}.

-type session_status() ::
    succeeded       |
    {failed, _TODO} .

-type ev() ::
    {transfer_created, transfer()}                |
    {transfer, ff_transfer:ev()}                  |
    {session_created, session()}                  |
    {session, {status_changed, session_status()}} .

-export_type([withdrawal/0]).
-export_type([ev/0]).

-export([id/1]).
-export([source/1]).
-export([destination/1]).
-export([trxid/1]).
-export([body/1]).
-export([provider/1]).
-export([transfer/1]).

-export([create/6]).
-export([create_transfer/1]).
-export([prepare_transfer/1]).
-export([commit_transfer/1]).
-export([cancel_transfer/1]).
-export([create_session/1]).
-export([poll_session_completion/1]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, with/3]).

%% Accessors

-spec id(withdrawal())          -> id().
-spec source(withdrawal())      -> wallet().
-spec destination(withdrawal()) -> destination().
-spec trxid(withdrawal())       -> trxid().
-spec body(withdrawal())        -> body().
-spec provider(withdrawal())    -> provider().
-spec transfer(withdrawal())    -> {ok, transfer()} | {error | notfound}.

id(#{id := V})                  -> V.
source(#{source := V})           -> V.
destination(#{destination := V}) -> V.
trxid(#{trxid := V})             -> V.
body(#{body := V})               -> V.
provider(#{provider := V})       -> V.
transfer(W)                      -> ff_map:find(transfer, W).

%%

-spec create(id(), wallet(), destination(), trxid(), body(), provider()) ->
    {ok, withdrawal()}.

create(ID, Source, Destination, TrxID, Body, Provider) ->
    do(fun () ->
        #{
            id          => ID,
            source      => Source,
            destination => Destination,
            trxid       => TrxID,
            body        => Body,
            provider    => Provider
        }
    end).

create_transfer(Withdrawal) ->
    Source = source(Withdrawal),
    Destination = ff_destination:wallet(destination(Withdrawal)),
    TrxID = construct_transfer_id(trxid(Withdrawal)),
    Posting = {Source, Destination, body(Withdrawal)},
    do(fun () ->
        Transfer = unwrap(transfer, ff_transfer:create(TrxID, [Posting])),
        [{transfer_created, Transfer}]
    end).

construct_transfer_id(TrxID) ->
    ff_string:join($/, [TrxID, transfer]).

prepare_transfer(Withdrawal) ->
    with(transfer, Withdrawal, fun ff_transfer:prepare/1).

commit_transfer(Withdrawal) ->
    with(transfer, Withdrawal, fun ff_transfer:commit/1).

cancel_transfer(Withdrawal) ->
    with(transfer, Withdrawal, fun ff_transfer:cancel/1).

create_session(Withdrawal) ->
    SID         = construct_session_id(trxid(Withdrawal)),
    Source      = source(Withdrawal),
    Destination = destination(Withdrawal),
    Provider    = provider(Withdrawal),
    WithdrawalParams = #{
        id          => SID,
        destination => Destination,
        cash        => body(Withdrawal),
        sender      => ff_wallet:identity(Source),
        receiver    => ff_wallet:identity(ff_destination:wallet(Destination))
    },
    do(fun () ->
        ok = unwrap(ff_withdrawal_provider:create_session(SID, WithdrawalParams, Provider)),
        [{session_created, {SID, active}}]
    end).

construct_session_id(TrxID) ->
    TrxID.

poll_session_completion(Withdrawal) ->
    with(session, Withdrawal, fun
        ({SID, active}) ->
            {ok, Session} = ff_withdrawal_session_machine:get(SID),
            case ff_withdrawal_session_machine:status(Session) of
                active ->
                    {ok, []};
                {finished, {success, _}} ->
                    {ok, [{status_changed, succeeded}]};
                {finished, {failed, Failure}} ->
                    {ok, [{status_changed, {failed, Failure}}]}
            end;
        ({_SID, _Completed}) ->
            {ok, []}
    end).

%%

apply_event({transfer_created, T}, W) ->
    maps:put(transfer, T, W);
apply_event({transfer, Ev}, W) ->
    maps:update(transfer, fun (T) -> ff_transfer:apply_event(Ev, T) end, W);
apply_event({session_created, S}, W) ->
    maps:put(session, S, W);
apply_event({session, {status_changed, S}}, W) ->
    maps:update(session, fun ({SID, _}) -> {SID, S} end, W).
