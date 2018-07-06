%%%
%%% Tranfer
%%%
%%% TODOs
%%%
%%%  - We must synchronise any transfers on wallet machine, as one may request
%%%    us to close wallet concurrently. Moreover, we should probably check any
%%%    limits there too.
%%%  - Well, we will need `cancel` soon too.
%%%  - What if we get rid of some failures in `prepare`, specifically those
%%%    which related to wallet blocking / suspension? It would be great to get
%%%    rid of the `wallet closed` failure but I see no way to do so.
%%%

-module(ff_transfer).

-type wallet()   :: ff_wallet:wallet().
-type body()     :: ff_transaction:body().
-type trxid()    :: ff_transaction:id().
-type posting()  :: {wallet(), wallet(), body()}.

-type status() ::
    created   |
    prepared  |
    committed |
    cancelled .

-type transfer() :: #{
    trxid       := trxid(),
    postings    := [posting()],
    status      => status()
}.

-type ev() ::
    {created, transfer()} |
    {status_changed, status()}.

-type outcome() ::
    [ev()].

-export_type([transfer/0]).
-export_type([posting/0]).
-export_type([status/0]).
-export_type([ev/0]).

-export([trxid/1]).
-export([postings/1]).
-export([status/1]).

-export([create/2]).
-export([prepare/1]).
-export([commit/1]).
-export([cancel/1]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%%

-spec trxid(transfer()) -> trxid().
-spec postings(transfer()) -> [posting()].
-spec status(transfer()) -> status().

trxid(#{trxid := V}) -> V.
postings(#{postings := V}) -> V.
status(#{status := V}) -> V.

%%

-spec create(trxid(), [posting()]) ->
    {ok, outcome()} |
    {error,
        empty |
        {wallet,
            {inaccessible, blocked | suspended} |
            {currency, invalid} |
            {provider, invalid}
        }
    }.

create(TrxID, Postings = [_ | _]) ->
    do(fun () ->
        Wallets = gather_wallets(Postings),
        accessible = unwrap(wallet, validate_accessible(Wallets)),
        valid      = unwrap(wallet, validate_currencies(Wallets)),
        valid      = unwrap(wallet, validate_identities(Wallets)),
        [
            {created, #{
                trxid       => TrxID,
                postings    => Postings,
                status      => created
            }},
            {status_changed,
                created
            }
        ]
    end);
create(_TrxID, []) ->
    {error, empty}.

gather_wallets(Postings) ->
    lists:usort(lists:flatten([[S, D] || {S, D, _} <- Postings])).

validate_accessible(Wallets) ->
    do(fun () ->
        _ = [accessible = unwrap(ff_wallet:is_accessible(W)) || W <- Wallets],
        accessible
    end).

validate_currencies([W0 | Wallets]) ->
    do(fun () ->
        Currency = ff_wallet:currency(W0),
        _ = [ok = unwrap(currency, valid(Currency, ff_wallet:currency(W))) || W <- Wallets],
        valid
    end).

validate_identities([W0 | Wallets]) ->
    do(fun () ->
        Provider = ff_identity:provider(ff_wallet:identity(W0)),
        _ = [
            ok = unwrap(provider, valid(Provider, ff_identity:provider(ff_wallet:identity(W)))) ||
                W <- Wallets
        ],
        valid
    end).

%%

-spec prepare(transfer()) ->
    {ok, outcome()} |
    {error,
        balance |
        {status, committed | cancelled} |
        {wallet, {inaccessible, blocked | suspended}}
    }.

prepare(Transfer = #{status := created}) ->
    TrxID = trxid(Transfer),
    Postings = postings(Transfer),
    do(fun () ->
        accessible = unwrap(wallet, validate_accessible(gather_wallets(Postings))),
        _Affected  = unwrap(ff_transaction:prepare(TrxID, construct_trx_postings(Postings))),
        [{status_changed, prepared}]
    end);
prepare(_Transfer = #{status := prepared}) ->
    {ok, []};
prepare(#{status := Status}) ->
    {error, {status, Status}}.

%% TODO
% validate_balances(Affected) ->
%     {ok, valid}.

%%

-spec commit(transfer()) ->
    {ok, outcome()} |
    {error, {status, created | cancelled}}.

commit(Transfer = #{status := prepared}) ->
    TrxID = trxid(Transfer),
    Postings  = postings(Transfer),
    do(fun () ->
        _Affected = unwrap(ff_transaction:commit(TrxID, construct_trx_postings(Postings))),
        [{status_changed, committed}]
    end);
commit(_Transfer = #{status := committed}) ->
    {ok, []};
commit(#{status := Status}) ->
    {error, Status}.

%%

-spec cancel(transfer()) ->
    {ok, outcome()} |
    {error, {status, created | committed}}.

cancel(Transfer = #{status := prepared}) ->
    do(fun () ->
        Postings  = construct_trx_postings(postings(Transfer)),
        _Affected = unwrap(ff_transaction:cancel(trxid(Transfer), Postings)),
        [{status_changed, cancelled}]
    end);
cancel(_Transfer = #{status := cancelled}) ->
    {ok, []};
cancel(#{status := Status}) ->
    {error, {status, Status}}.

%%

apply_event({created, Transfer}, undefined) ->
    Transfer;
apply_event({status_changed, S}, Transfer) ->
    Transfer#{status := S}.

%%

construct_trx_postings(Postings) ->
    [
        {unwrap(ff_wallet:account(Source)), unwrap(ff_wallet:account(Destination)), Body} ||
            {Source, Destination, Body} <- Postings
    ].
