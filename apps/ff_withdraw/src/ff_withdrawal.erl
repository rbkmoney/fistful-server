%%%
%%% Withdrawal
%%%

-module(ff_withdrawal).

-type id(T)         :: T.
-type wallet()      :: ff_wallet:wallet().
-type destination() :: ff_destination:destination().
-type body()        :: ff_transaction:body().
-type provider()    :: ff_withdrawal_provider:provider().
-type transfer()    :: ff_transfer:transfer().

-type withdrawal() :: #{
    id          := id(_),
    source      := wallet(),
    destination := destination(),
    body        := body(),
    provider    := provider(),
    transfer    => transfer(),
    session     => session(),
    status      => status()
}.

-type session() ::
    id(_).

-type status() ::
    pending         |
    succeeded       |
    {failed, _TODO} .

-type ev() ::
    {created, withdrawal()}         |
    {transfer, ff_transfer:ev()}    |
    {session_started, session()}    |
    {session_finished, session()}   |
    {status_changed, status()}      .

-type outcome() :: [ev()].

-export_type([withdrawal/0]).
-export_type([ev/0]).

-export([id/1]).
-export([source/1]).
-export([destination/1]).
-export([body/1]).
-export([provider/1]).
-export([transfer/1]).
-export([status/1]).

-export([create/5]).
-export([create_transfer/1]).
-export([prepare_transfer/1]).
-export([commit_transfer/1]).
-export([cancel_transfer/1]).
-export([create_session/1]).
-export([poll_session_completion/1]).

%% Event source

-export([collapse_events/1]).
-export([apply_event/2]).

-export([dehydrate/1]).
-export([hydrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, with/3]).

%% Accessors

-spec id(withdrawal())          -> id(_).
-spec source(withdrawal())      -> wallet().
-spec destination(withdrawal()) -> destination().
-spec body(withdrawal())        -> body().
-spec provider(withdrawal())    -> provider().
-spec status(withdrawal())      -> status().
-spec transfer(withdrawal())    -> {ok, transfer()} | {error | notfound}.

id(#{id := V})                  -> V.
source(#{source := V})           -> V.
destination(#{destination := V}) -> V.
body(#{body := V})               -> V.
provider(#{provider := V})       -> V.
status(#{status := V})           -> V.
transfer(W)                      -> ff_map:find(transfer, W).

%%

-spec create(id(_), wallet(), destination(), body(), provider()) ->
    {ok, outcome()}.

create(ID, Source, Destination, Body, Provider) ->
    do(fun () ->
        [
            {created, #{
                id          => ID,
                source      => Source,
                destination => Destination,
                body        => Body,
                provider    => Provider
            }},
            {status_changed,
                pending
            }
        ]
    end).

create_transfer(Withdrawal) ->
    Source = source(Withdrawal),
    Destination = ff_destination:wallet(destination(Withdrawal)),
    TrxID = construct_transfer_id(id(Withdrawal)),
    Posting = {Source, Destination, body(Withdrawal)},
    do(fun () ->
        Events = unwrap(transfer, ff_transfer:create(TrxID, [Posting])),
        [{transfer, Ev} || Ev <- Events]
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
    SID         = construct_session_id(id(Withdrawal)),
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
        [{session_started, SID}]
    end).

construct_session_id(TrxID) ->
    TrxID.

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

-spec collapse_events([ev(), ...]) ->
    withdrawal().

collapse_events(Evs) when length(Evs) > 0 ->
    apply_events(Evs, undefined).

-spec apply_events([ev()], undefined | withdrawal()) ->
    undefined | withdrawal().

apply_events(Evs, Identity) ->
    lists:foldl(fun apply_event/2, Identity, Evs).

-spec apply_event(ev(), undefined | withdrawal()) ->
    withdrawal().

apply_event({created, W}, undefined) ->
    W;
apply_event({status_changed, S}, W) ->
    maps:put(status, S, W);
apply_event({transfer, Ev}, W) ->
    maps:update_with(transfer, fun (T) -> ff_transfer:apply_event(Ev, T) end, maps:merge(#{transfer => undefined}, W));
apply_event({session_started, S}, W) ->
    maps:put(session, S, W);
apply_event({session_finished, S}, W = #{session := S}) ->
    maps:remove(session, W).

%%

-spec dehydrate(ev()) ->
    term().

-spec hydrate(term(), undefined | withdrawal()) ->
    ev().

dehydrate({created, W}) ->
    {created, #{
        id          => id(W),
        source      => ff_wallet:id(source(W)),
        destination => ff_destination:id(destination(W)),
        body        => body(W),
        provider    => ff_withdrawal_provider:id(provider(W))
    }};
dehydrate({status_changed, S}) ->
    {status_changed, S};
dehydrate({transfer, Ev}) ->
    % TODO
    %  - `ff_transfer:dehydrate(Ev)`
    {transfer, Ev};
dehydrate({session_started, SID}) ->
    {session_started, SID};
dehydrate({session_finished, SID}) ->
    {session_finished, SID}.

hydrate({created, V}, undefined) ->
    {ok, SourceSt}      = ff_wallet_machine:get(maps:get(source, V)),
    {ok, DestinationSt} = ff_destination_machine:get(maps:get(destination, V)),
    {ok, Provider}      = ff_withdrawal_provider:get(maps:get(provider, V)),
    {created, #{
        id          => maps:get(id, V),
        source      => ff_wallet_machine:wallet(SourceSt),
        destination => ff_destination_machine:destination(DestinationSt),
        body        => maps:get(body, V),
        provider    => Provider
    }};
hydrate({status_changed, S}, _) ->
    {status_changed, S};
hydrate({transfer, Ev}, _) ->
    % TODO
    %  - `ff_transfer:hydrate(Ev)`
    {transfer, Ev};
hydrate({session_started, SID}, _) ->
    {session_started, SID};
hydrate({session_finished, SID}, _) ->
    {session_finished, SID}.
