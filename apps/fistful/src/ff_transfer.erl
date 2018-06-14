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

-export_type([transfer/0]).
-export_type([posting/0]).
-export_type([status/0]).

-export([trxid/1]).
-export([postings/1]).
-export([status/1]).

-export([create/2]).
-export([prepare/1]).
-export([commit/1]).

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
        {ok, Identity} = ff_identity_machine:get(ff_wallet:identity(W0)),
        Provider       = ff_identity:provider(Identity),
        _ = [
            Provider = unwrap(provider, valid(Provider, ff_identity:provider(I))) ||
                W       <- Wallets,
                {ok, I} <- [ff_identity_machine:get(ff_wallet:identity(W))]
        ],
        valid
    end).

%%

-spec prepare(transfer()) ->
    {ok, transfer()} |
    {error,
        status() |
        balance |
        {wallet, {inaccessible, blocked | suspended}}
    }.

prepare(Transfer = #{status := created}) ->
    do(fun () ->
        Postings   = postings(Transfer),
        accessible = unwrap(wallet, validate_accessible(gather_wallets(Postings))),
        Affected   = ff_transaction:prepare(trxid(Transfer), construct_trx_postings(Postings)),
        case validate_balances(Affected) of
            {ok, valid} ->
                Transfer#{status := prepared};
            {error, invalid} ->
                throw(balance)
        end
    end);
prepare(Transfer = #{status := prepared}) ->
    {ok, Transfer};
prepare(#{status := Status}) ->
    {error, Status}.

validate_balances(Affected) ->
    % TODO
    {ok, valid}.

%%

-spec commit(transfer()) ->
    {ok, transfer()} |
    {error, status()}.

commit(Transfer = #{status := prepared}) ->
    do(fun () ->
        Postings   = postings(Transfer),
        _Affected  = ff_transaction:commit(trxid(Transfer), construct_trx_postings(Postings)),
        Transfer#{status := committed}
    end);
commit(Transfer = #{status := committed}) ->
    {ok, Transfer};
commit(#{status := Status}) ->
    {error, Status}.

%%

construct_trx_postings(Postings) ->
    [
        {ff_wallet:account(Source), ff_wallet:account(Destination), Body} ||
            {Source, Destination, Body} <- Postings
    ].
