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

-type reposit() :: #{
    id              := id(),
    deposit_id      := deposit_id(),
    source_id       := source_id(),
    wallet_id       := wallet_id(),
    body            := body(),
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
-export([source_id/1]).
-export([wallet_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).

%% API
-export([create/2]).
-export([get/1]).
-export([construct_p_transfer_id/1]).
-export([update_status/1]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1]).

%% Accessors

-spec deposit_id(reposit())      -> deposit_id().
-spec wallet_id(reposit())       -> source_id().
-spec source_id(reposit())       -> wallet_id().
-spec id(reposit())              -> id().
-spec body(reposit())            -> body().
-spec status(reposit())          -> status().

deposit_id(#{deposit_id := V}) -> V.
wallet_id(#{wallet_id := V})   -> V.
source_id(#{source_id := V})   -> V.
id(#{id := V})                 -> V.
body(#{body := V})             -> V.
status(#{status := V})         -> V.

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
        [
            {created, #{
                id              => ID,
                deposit_id      => DepositID,
                source_id       => SourceID,
                wallet_id       => WalletID,
                body            => Body,
                reason          => Reason
            }},
            {status_changed, pending}
        ]
    end).

-spec get(list(event())) ->
    reposit().
get(Events) ->
    lists:foldl(fun (Ev, St) -> apply_event(Ev, St) end, undefined, Events).

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/reposit/", ID/binary>>.

-spec update_status(status()) ->
    list(event()).
update_status(Status) ->
    [{status_changed, Status}].

-spec apply_event(event(), maybe(reposit())) ->
    reposit().
apply_event({created, R}, undefined) ->
    R;
apply_event({status_changed, S}, R) ->
    maps:put(status, S, R).
