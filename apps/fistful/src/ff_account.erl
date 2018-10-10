%%%
%%% Account
%%%
%%% Responsible for, at least:
%%%  - managing partymgmt-related wallet stuff,
%%%  - acknowledging transfer postings,
%%%  - accounting and checking limits.
%%%

-module(ff_account).

-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").

-type id() :: binary().
-type account() :: #{
    id := id(),
    identity := identity_id(),
    currency := currency_id(),
    accounter_account_id := accounter_account_id()
}.

-type event() ::
    {created, account()}.

-export_type([id/0]).
-export_type([account/0]).
-export_type([event/0]).

-export([id/1]).
-export([identity/1]).
-export([currency/1]).
-export([accounter_account_id/1]).

-export([create/3]).
-export([is_accessible/1]).

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Internal types

-type identity() :: ff_identity:identity().
-type currency() :: ff_currency:currency().
-type identity_id() :: ff_identity:id().
-type currency_id() :: ff_currency:id().
-type accounter_account_id() :: dmsl_accounter_thrift:'AccountID'().

%% Accessors

-spec id(account()) ->
    id().
-spec identity(account()) ->
    identity_id().
-spec currency(account()) ->
    currency_id().
-spec accounter_account_id(account()) ->
    accounter_account_id().

id(#{id := ID}) ->
    ID.
identity(#{identity := IdentityID}) ->
    IdentityID.
currency(#{currency := CurrencyID}) ->
    CurrencyID.
accounter_account_id(#{accounter_account_id := AccounterID}) ->
    AccounterID.

%% Actuators

-spec create(id(), identity(), currency()) ->
    {ok, [event()]} |
    {error,
        {identity, notfound} |
        {currency, notfound} |
        {contract, notfound} |
        {accounter, any()} |
        {party, ff_party:inaccessibility()} |
        invalid
    }.

create(ID, Identity, Currency) ->
    do(fun () ->
        Contract = ff_identity:contract(Identity),
        Party = ff_identity:party(Identity),
        Contract = ff_identity:contract(Identity),
        accessible = unwrap(party, ff_party:is_accessible(Party)),
        valid = unwrap(contract, ff_party:validate_account_creation(
            Party, Contract, ID, ff_currency:id(Currency), ff_time:now()
        )),
        AccounterID = unwrap(accounter, create_account(ID, Currency)),
        [{created, #{
            id       => ID,
            identity => ff_identity:id(Identity),
            currency => ff_currency:id(Currency),
            accounter_account_id => AccounterID
        }}]
    end).

-spec is_accessible(account()) ->
    {ok, accessible} |
    {error, ff_party:inaccessibility()}.

is_accessible(Account) ->
    do(fun () ->
        Identity   = get_identity(Account),
        accessible = unwrap(ff_identity:is_accessible(Identity))
    end).

get_identity(Account) ->
    {ok, V} = ff_identity_machine:get(identity(Account)),
    ff_identity_machine:identity(V).

%% State

-spec apply_event(event(), ff_maybe:maybe(account())) ->
    account().

apply_event({created, Account}, undefined) ->
    Account.

%% Accounter client

-spec create_account(id(), currency()) ->
    {ok, accounter_account_id()} |
    {error, {exception, any()}}.

create_account(ID, Currency) ->
    CurrencyCode = ff_currency:symcode(Currency),
    Description = ff_string:join($/, [<<"ff/account">>, ID]),
    case call_accounter('CreateAccount', [construct_prototype(CurrencyCode, Description)]) of
        {ok, Result} ->
            {ok, Result};
        {exception, Exception} ->
            {error, {exception, Exception}}
    end.

construct_prototype(CurrencyCode, Description) ->
    #accounter_AccountPrototype{
        currency_sym_code = CurrencyCode,
        description = Description
    }.

call_accounter(Function, Args) ->
    Service = {dmsl_accounter_thrift, 'Accounter'},
    ff_woody_client:call(accounter, {Service, Function, Args}).
