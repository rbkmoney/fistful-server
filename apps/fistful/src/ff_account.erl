%%%
%%% Account
%%%
%%% Responsible for, at least:
%%%  - managing partymgmt-related wallet stuff,
%%%  - acknowledging transfer postings,
%%%  - accounting and checking limits.
%%%

-module(ff_account).

-include_lib("shumpune_proto/include/shumpune_shumpune_thrift.hrl").

-type id() :: binary().
-type accounter_account_id() :: shumpune_shumpune_thrift:'AccountID'().
-type account() :: #{
    id := id(),
    identity := identity_id(),
    currency := currency_id(),
    accounter_account_id := accounter_account_id()
}.

-type event() ::
    {created, account()}.

-type create_error() ::
    {terms, ff_party:validate_account_creation_error()} |
    {party, ff_party:inaccessibility()}.

-export_type([id/0]).
-export_type([accounter_account_id/0]).
-export_type([account/0]).
-export_type([event/0]).
-export_type([create_error/0]).

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
    {ok, [event()]} | {error, create_error()}.

create(ID, Identity, Currency) ->
    do(fun () ->
        ContractID = ff_identity:contract(Identity),
        PartyID = ff_identity:party(Identity),
        accessible = unwrap(party, ff_party:is_accessible(PartyID)),
        TermVarset = #{
            wallet_id => ID,
            currency => ff_currency:to_domain_ref(Currency)
        },
        {ok, PartyRevision} = ff_party:get_revision(PartyID),
        DomainRevision = ff_domain_config:head(),
        {ok, Terms} = ff_party:get_contract_terms(
            PartyID, ContractID, TermVarset, ff_time:now(), PartyRevision, DomainRevision
        ),
        CurrencyID = ff_currency:id(Currency),
        valid = unwrap(terms, ff_party:validate_account_creation(Terms, CurrencyID)),
        {ok, AccounterID} = create_account(ID, Currency),
        [{created, #{
            id       => ID,
            identity => ff_identity:id(Identity),
            currency => CurrencyID,
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
    #shumpune_AccountPrototype{
        currency_sym_code = CurrencyCode,
        description = Description
    }.

call_accounter(Function, Args) ->
    Service = {shumpune_shumpune_thrift, 'Accounter'},
    ff_woody_client:call(accounter, {Service, Function, Args}).
