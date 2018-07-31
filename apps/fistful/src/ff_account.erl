%%%
%%% Account
%%%
%%% Responsible for, at least:
%%%  - managing partymgmt-related wallet stuff,
%%%  - acknowledging transfer postings,
%%%  - accounting and checking limits.
%%%

-module(ff_account).

-type id(T) :: T.
-type identity() :: ff_identity:id().
-type currency() :: ff_currency:id().

-type account() :: #{
    id := id(binary()),
    identity := identity(),
    currency := currency(),
    pm_wallet := ff_party:wallet_id()
}.

-type event() ::
    {created, account()}.

-export_type([account/0]).
-export_type([event/0]).

-export([id/1]).
-export([identity/1]).
-export([currency/1]).
-export([pm_wallet/1]).

-export([pm_account/1]).

-export([create/3]).
-export([is_accessible/1]).

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec id(account()) ->
    id(binary()).
-spec identity(account()) ->
    identity().
-spec currency(account()) ->
    currency().
-spec pm_wallet(account()) ->
    ff_party:wallet_id().

id(#{id := ID}) ->
    ID.
identity(#{identity := IdentityID}) ->
    IdentityID.
currency(#{currency := CurrencyID}) ->
    CurrencyID.
pm_wallet(#{pm_wallet := PMWalletID}) ->
    PMWalletID.

-spec pm_account(account()) ->
    ff_transaction:account().

pm_account(Account) ->
    {ok, Identity} = ff_identity_machine:get(identity(Account)),
    {ok, PMAccount} = ff_party:get_wallet_account(
        ff_identity:party(ff_identity_machine:identity(Identity)),
        pm_wallet(Account)
    ),
    PMAccount.

%% Actuators

-spec create(id(_), identity(), currency()) ->
    {ok, [event()]} |
    {error,
        {identity, notfound} |
        {currency, notfound} |
        {contract, notfound} |
        ff_party:inaccessibility() |
        invalid
    }.

create(ID, IdentityID, CurrencyID) ->
    do(fun () ->
        Identity = ff_identity_machine:identity(unwrap(identity, ff_identity_machine:get(IdentityID))),
        _Currency = unwrap(currency, ff_currency:get(CurrencyID)),
        PMWalletID = unwrap(ff_party:create_wallet(
            ff_identity:party(Identity),
            ff_identity:contract(Identity),
            #{
                name     => ff_string:join($/, [<<"ff/account">>, ID]),
                currency => CurrencyID
            }
        )),
        [{created, #{
            id       => ID,
            identity => IdentityID,
            currency => CurrencyID,
            pm_wallet => PMWalletID
        }}]
    end).

-spec is_accessible(account()) ->
    {ok, accessible} |
    {error, ff_party:inaccessibility()}.

is_accessible(Account) ->
    do(fun () ->
        Identity   = get_identity(Account),
        accessible = unwrap(ff_identity:is_accessible(Identity)),
        accessible = unwrap(ff_party:is_wallet_accessible(ff_identity:party(Identity), pm_wallet(Account)))
    end).

get_identity(Account) ->
    {ok, V} = ff_identity_machine:get(identity(Account)),
    ff_identity_machine:identity(V).

%% State

-spec apply_event(event(), ff_maybe:maybe(account())) ->
    account().

apply_event({created, Account}, undefined) ->
    Account.
