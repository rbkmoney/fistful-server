%%%
%%% Reposit
%%%

-module(ff_reposit).

-type id()              :: ff_transfer_machine:id().
-type deposit_id()      :: ff_transfer_machine:id().
-type source_id()       :: ff_source:id().
-type wallet_id()       :: ff_wallet:id().
-type body()            :: ff_transaction:body().
-type domain_revision() :: integer().
-type party_revision()  :: integer().
-type maybe(T)          :: ff_maybe:maybe(T).
-type timestamp()       :: binary().

-type reposit() :: #{
    id              := id(),
    deposit_id      := deposit_id(),
    source_id       := source_id(),
    wallet_id       := wallet_id(),
    body            := body(),
    create_at       := timestamp(),
    reason          => maybe(binary()),
    status          => status(),
    domain_revision => domain_revision(),
    party_revision  => party_revision()
}.

-type status() ::
    pending         |
    succeeded       |
    {failed, _TODO} .

-type event() ::
    {created, reposit()}        |
    {status_changed, status()}.

-export_type([reposit/0]).
-export_type([event/0]).
-export_type([id/0]).

%% Accessors

-export([deposit_id/1]).
-export([wallet_id/1]).
-export([source_id/1]).
-export([create_at/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([reason/1]).
-export([domain_revision/1]).
-export([party_revision/1]).

%% API
-export([create/2]).
-export([get/2]).
-export([next_id/1]).
-export([collapse/1]).
-export([construct_p_transfer_id/1]).
-export([update_status/1]).
-export([get_current/1]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1]).

%% Accessors

-spec deposit_id(reposit())      -> deposit_id().
-spec wallet_id(reposit())       -> source_id().
-spec source_id(reposit())       -> wallet_id().
-spec create_at(reposit())       -> timestamp().
-spec id(reposit())              -> id().
-spec body(reposit())            -> body().
-spec status(reposit())          -> status().
-spec reason(reposit())          -> binary().
-spec domain_revision(reposit()) -> domain_revision().
-spec party_revision(reposit())  -> party_revision().

deposit_id(#{deposit_id := V})              -> V.
wallet_id(#{wallet_id := V})                -> V.
source_id(#{source_id := V})                -> V.
create_at(#{create_at := V})                -> V.
id(#{id := V})                              -> V.
body(#{body := V})                          -> V.
status(#{status := V})                      -> V.

reason(#{reason := V})                      -> V;
reason(_)                                   -> undefined.
domain_revision(#{domain_revision := V})    -> V;
domain_revision(_)                          -> undefined.
party_revision(#{party_revision := V})      -> V;
party_revision(_)                           -> undefined.
%%

-type params() :: #{
    deposit_id      := deposit_id(),
    source_id       := source_id(),
    wallet_id       := wallet_id(),
    body            := body(),
    reason          := maybe(binary())
}.

-spec create(id(), params()) ->
    {ok, [event()]}.

create(ID, #{
    deposit_id := DepositID,
    source_id  := SourceID,
    wallet_id  := WalletID,
    body       := Body,
    reason     := Reason
}) ->
    do(fun () ->
        Timestamp = ff_time:now_rfc3339(),
        [
            {created, genlib_map:compact(#{
                id              => ID,
                deposit_id      => DepositID,
                source_id       => SourceID,
                wallet_id       => WalletID,
                body            => Body,
                create_at       => Timestamp,
                reason          => Reason
            })},
            {status_changed, pending}
        ]
    end).

-spec get(id(), maybe(list(reposit()))) ->
    {error, notfound} | {ok, reposit()}.
get(_, undefined) ->
    {error, notfound};
get(RepositID, Reposits) ->
    case lists:filter(fun(#{id := ID}) -> ID =:= RepositID end, Reposits) of
        [] ->
            {error, notfound};
        [Reposit | _Rest] ->
            {ok ,Reposit}
    end.

-spec next_id(maybe(list(reposit()))) ->
    id().
next_id(undefined) ->
    <<"reposit_0">>;
next_id(Reposits) ->
    ID = erlang:integer_to_binary(length(Reposits)),
    <<"reposit_", ID/binary>>.

-spec collapse(list(event())) ->
    reposit().
collapse(Events) ->
    Reposits = lists:foldl(fun (Ev, St) -> apply_event(Ev, St) end, undefined, Events),
    get_current(Reposits).

-spec construct_p_transfer_id(reposit()) -> id().
construct_p_transfer_id(Reposit) ->
    ID = id(Reposit),
    DepositID = deposit_id(Reposit),
    <<"ff/reposit/", DepositID/binary, "/", ID/binary>>.

-spec update_status(status()) ->
    list(event()).
update_status(Status) ->
    [{status_changed, Status}].

-spec apply_event(event(), maybe(list(reposit()))) ->
    list(reposit()).
apply_event({created, R}, undefined) ->
    [R];
apply_event({created, R}, Reposits) ->
    [R | Reposits];
apply_event({status_changed, S}, [R | Reposits]) ->
    [maps:put(status, S, R) | Reposits].

-spec get_current(maybe(list(reposit()))) ->
    reposit().
get_current(undefined) ->
    undefined;
get_current([Reposit | _Rest]) ->
    Reposit.
