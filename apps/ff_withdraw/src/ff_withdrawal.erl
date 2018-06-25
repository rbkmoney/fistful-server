%%%
%%% Withdrawal
%%%

-module(ff_withdrawal).

-type withdrawal() :: #{
    source      := ff_wallet:wallet(),
    destination := ff_destination:destination(),
    trxid       := ff_transaction:id(),
    body        := ff_transaction:body(),
    provider    := ff_provider:provider(),
    transfer    => ff_transfer:transfer(),
    session     => session()
}.

-type session() ::
    {_ID, session_status()}.

-type session_status() ::
    succeeded       |
    {failed, _TODO} .

-type ev() ::
    {transfer_created, ff_transfer:transfer()}    |
    {transfer, ff_transfer:ev()}                  |
    {session_created, session()}                  |
    {session, {status_changed, session_status()}} .

-export_type([withdrawal/0]).
-export_type([ev/0]).

-export([source/1]).
-export([destination/1]).
-export([trxid/1]).
-export([body/1]).
-export([provider/1]).
-export([transfer/1]).

-export([create/5]).
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

%%

source(#{source := V}) -> V.
destination(#{destination := V}) -> V.
trxid(#{trxid := V}) -> V.
body(#{body := V}) -> V.
provider(#{provider := V}) -> V.
transfer(W) -> ff_map:find(transfer, W).

%%

create(Source, Destination, TrxID, Body, Provider) ->
    do(fun () ->
        #{
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
