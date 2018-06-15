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
    status      := status()
}.

-type ev() ::
    {status_changed, status()}.

-type outcome() ::
    {[ev()], transfer()}.

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
    {ok, transfer()} |
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
        #{
            trxid       => TrxID,
            postings    => Postings,
            status      => created
        }
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
        _ = [Currency = unwrap(currency, valid(Currency, ff_wallet:currency(W))) || W <- Wallets],
        valid
    end).

validate_identities([W0 | Wallets]) ->
    do(fun () ->
        Provider = ff_identity:provider(ff_wallet:identity(W0)),
        _ = [
            Provider = unwrap(provider, valid(Provider, ff_identity:provider(ff_wallet:identity(W)))) ||
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
    Postings = construct_trx_postings(postings(Transfer)),
    roll(Transfer, do(fun () ->
        accessible = unwrap(wallet, validate_accessible(gather_wallets(Postings))),
        Affected   = unwrap(ff_transaction:prepare(TrxID, Postings)),
        case validate_balances(Affected) of
            {ok, valid} ->
                [{status_changed, prepared}];
            {error, invalid} ->
                _ = ff_transaction:cancel(TrxID, Postings),
                throw(balance)
        end
    end));
prepare(Transfer = #{status := prepared}) ->
    {ok, Transfer};
prepare(#{status := Status}) ->
    {error, {status, Status}}.

validate_balances(Affected) ->
    % TODO
    {ok, valid}.

%%

-spec commit(transfer()) ->
    {ok, outcome()} |
    {error, {status, created | cancelled}}.

commit(Transfer = #{status := prepared}) ->
    roll(Transfer, do(fun () ->
        Postings  = construct_trx_postings(postings(Transfer)),
        _Affected = unwrap(ff_transaction:commit(trxid(Transfer), Postings)),
        [{status_changed, committed}]
    end));
commit(Transfer = #{status := committed}) ->
    {ok, roll(Transfer)};
commit(#{status := Status}) ->
    {error, Status}.

%%

-spec cancel(transfer()) ->
    {ok, outcome()} |
    {error, {status, created | committed}}.

cancel(Transfer = #{status := prepared}) ->
    roll(Transfer, do(fun () ->
        Postings  = construct_trx_postings(postings(Transfer)),
        _Affected = unwrap(ff_transaction:cancel(trxid(Transfer), Postings)),
        [{status_changed, cancelled}]
    end));
cancel(Transfer = #{status := cancelled}) ->
    {ok, roll(Transfer)};
cancel(#{status := Status}) ->
    {error, {status, Status}}.

%%

apply_event({status_changed, S}, Transfer) ->
    Transfer#{status := S}.

%% TODO

-type result(V, R) :: {ok, V} | {error, R}.

-spec roll(St, result(Evs, Reason)) ->
    result({Evs, St}, Reason) when
        Evs :: [_].

roll(St, {ok, Events}) when is_list(Events) ->
    {ok, {Events, lists:foldl(fun apply_event/2, St, Events)}};
roll(_St, {error, _} = Error) ->
    Error.

roll(St) ->
    {[], St}.

%%

construct_trx_postings(Postings) ->
    [
        {ff_wallet:account(Source), ff_wallet:account(Destination), Body} ||
            {Source, Destination, Body} <- Postings
    ].
