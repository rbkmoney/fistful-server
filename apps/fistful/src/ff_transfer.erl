%%%
%%% Tranfer
%%%
%%% TODOs
%%%
%%%  - We must synchronise any transfers on wallet machine, as one may request
%%%    us to close wallet concurrently. Moreover, we should probably check any
%%%    limits there too.
%%%  - What if we get rid of some failures in `prepare`, specifically those
%%%    which related to wallet blocking / suspension? It would be great to get
%%%    rid of the `wallet closed` failure but I see no way to do so.
%%%

-module(ff_transfer).

-type posting()  :: {account(), account(), body()}.

-type status() ::
    created   |
    prepared  |
    committed |
    cancelled .

-type transfer() :: #{
    id          := id(),
    postings    := [posting()],
    status      => status()
}.

-type event() ::
    {created, transfer()} |
    {status_changed, status()}.

-export_type([transfer/0]).
-export_type([posting/0]).
-export_type([status/0]).
-export_type([event/0]).

-export([id/1]).
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

%% Internal types

-type id()       :: ff_transaction:id().
-type account()  :: {Tag :: atom(), ff_wallet:id() | ff_destination:id()}.
-type body()     :: ff_transaction:body().

%%

-spec id(transfer()) ->
    id().
-spec postings(transfer()) ->
    [posting()].
-spec status(transfer()) ->
    status().

id(#{id := V}) ->
    V.
postings(#{postings := V}) ->
    V.
status(#{status := V}) ->
    V.

%%

-spec create(id(), [posting()]) ->
    {ok, [event()]} |
    {error,
        empty |
        {account, notfound} |
        {account, ff_party:inaccessibility()} |
        {currency, invalid} |
        {provider, invalid}
    }.

create(ID, Postings = [_ | _]) ->
    do(fun () ->
        Accounts = maps:values(gather_accounts(Postings)),
        valid      = validate_currencies(Accounts),
        valid      = validate_identities(Accounts),
        accessible = validate_accessible(Accounts),
        [
            {created, #{
                id       => ID,
                postings    => Postings
            }},
            {status_changed,
                created
            }
        ]
    end);
create(_TrxID, []) ->
    {error, empty}.

gather_accounts(Postings) ->
    maps:from_list([
        {AccountID, get_account(AccountID)} ||
            AccountID <- lists:usort(lists:flatten([[S, D] || {S, D, _} <- Postings]))
    ]).

%% TODO
%%  - Not the right place.
get_account({wallet, ID}) ->
    St = unwrap(account, ff_wallet_machine:get(ID)),
    ff_wallet:account(ff_wallet_machine:wallet(St));
get_account({destination, ID}) ->
    St = unwrap(account, ff_destination_machine:get(ID)),
    ff_destination:account(ff_destination_machine:destination(St)).

validate_accessible(Accounts) ->
    _ = [accessible = unwrap(account, ff_account:is_accessible(A)) || A <- Accounts],
    accessible.

validate_currencies([A0 | Accounts]) ->
    Currency = ff_account:currency(A0),
    _ = [ok = unwrap(currency, valid(Currency, ff_account:currency(A))) || A <- Accounts],
    valid.

validate_identities([A0 | Accounts]) ->
    {ok, IdentitySt} = ff_identity_machine:get(ff_account:identity(A0)),
    Identity0 = ff_identity_machine:identity(IdentitySt),
    ProviderID0 = ff_identity:provider(Identity0),
    _ = [
        ok = unwrap(provider, valid(ProviderID0, ff_identity:provider(ff_identity_machine:identity(Identity)))) ||
            Account <- Accounts,
            {ok, Identity} <- [ff_identity_machine:get(ff_account:identity(Account))]
    ],
    valid.

%%

-spec prepare(transfer()) ->
    {ok, [event()]} |
    {error,
        {status, committed | cancelled}
    }.

prepare(Transfer = #{status := created}) ->
    ID = id(Transfer),
    Postings = postings(Transfer),
    do(fun () ->
        _Affected = unwrap(ff_transaction:prepare(ID, construct_trx_postings(Postings))),
        [{status_changed, prepared}]
    end);
prepare(#{status := prepared}) ->
    {ok, []};
prepare(#{status := Status}) ->
    {error, Status}.

%% TODO
% validate_balances(Affected) ->
%     {ok, valid}.

%%

-spec commit(transfer()) ->
    {ok, [event()]} |
    {error, {status, created | cancelled}}.

commit(Transfer = #{status := prepared}) ->
    ID = id(Transfer),
    Postings = postings(Transfer),
    do(fun () ->
        _Affected = unwrap(ff_transaction:commit(ID, construct_trx_postings(Postings))),
        [{status_changed, committed}]
    end);
commit(#{status := committed}) ->
    {ok, []};
commit(#{status := Status}) ->
    {error, Status}.

%%

-spec cancel(transfer()) ->
    {ok, [event()]} |
    {error, {status, created | committed}}.

cancel(Transfer = #{status := prepared}) ->
    ID = id(Transfer),
    Postings = postings(Transfer),
    do(fun () ->
        _Affected = unwrap(ff_transaction:cancel(ID, construct_trx_postings(Postings))),
        [{status_changed, cancelled}]
    end);
cancel(#{status := cancelled}) ->
    {ok, []};
cancel(#{status := Status}) ->
    {error, {status, Status}}.

%%

-spec apply_event(event(), ff_maybe:maybe(account())) ->
    account().

apply_event({created, Transfer}, undefined) ->
    Transfer;
apply_event({status_changed, S}, Transfer) ->
    Transfer#{status => S}.

%%

construct_trx_postings(Postings) ->
    Accounts = gather_accounts(Postings),
    [
        {SourceAccount, DestinationAccount, Body} ||
            {Source, Destination, Body} <- Postings,
            SourceAccount               <- [ff_account:accounter_id(maps:get(Source, Accounts))],
            DestinationAccount          <- [ff_account:accounter_id(maps:get(Destination, Accounts))]
    ].
