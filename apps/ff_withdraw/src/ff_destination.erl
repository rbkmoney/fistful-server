%%%
%%% Destination
%%%
%%% TODOs
%%%
%%%  - We must consider withdrawal provider terms ensure that the provided
%%%    Resource is ok to withdraw to.
%%%

-module(ff_destination).

-type account()  :: ff_account:account().
-type resource() ::
    {bank_card, resource_bank_card()}.

-type id() :: binary().
-type identity() :: ff_identity:id().
-type currency() :: ff_currency:id().

-type resource_bank_card() :: #{
    token          := binary(),
    payment_system => atom(), % TODO
    bin            => binary(),
    masked_pan     => binary()
}.

-type status() ::
    unauthorized |
    authorized.

-type destination() :: #{
    account  := account() | undefined,
    resource := resource(),
    name     := binary(),
    status   := status()
}.

-type event() ::
    {created, destination()} |
    {account, ff_account:ev()} |
    {status_changed, status()}.

-export_type([id/0]).
-export_type([destination/0]).
-export_type([status/0]).
-export_type([resource/0]).
-export_type([event/0]).

-export([account/1]).

-export([id/1]).
-export([name/1]).
-export([identity/1]).
-export([currency/1]).
-export([resource/1]).
-export([status/1]).

-export([create/5]).
-export([authorize/1]).

-export([is_accessible/1]).

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec account(destination()) ->
    account().

account(#{account := V}) ->
    V.

-spec id(destination()) ->
    id().
-spec name(destination()) ->
    binary().
-spec identity(destination()) ->
    identity().
-spec currency(destination()) ->
    currency().
-spec resource(destination()) ->
    resource().
-spec status(destination()) ->
    status().

id(Destination) ->
    ff_account:id(account(Destination)).
name(#{name := V}) ->
    V.
identity(Destination) ->
    ff_account:identity(account(Destination)).
currency(Destination) ->
    ff_account:currency(account(Destination)).
resource(#{resource := V}) ->
    V.
status(#{status := V}) ->
    V.

%%

-spec create(id(), identity(), binary(), currency(), resource()) ->
    {ok, [event()]} |
    {error, _WalletError}.

create(ID, IdentityID, Name, CurrencyID, Resource) ->
    do(fun () ->
        Identity = ff_identity_machine:identity(unwrap(identity, ff_identity_machine:get(IdentityID))),
        Party = ff_identity:party(Identity),
        Contract = ff_identity:contract(Identity),
        Currency = unwrap(currency, ff_currency:get(CurrencyID)),
        accessible = unwrap(party, ff_party:is_accessible(Party)),
        valid = unwrap(contract, ff_party:validate_wallet_creation(
            Party, Contract, ID, Currency, ff_time:now()
        )),
        Events = unwrap(ff_account:create(ID, Identity, Currency)),
        [{created, #{name => Name, resource => Resource}}] ++
        [{account, Ev} || Ev <- Events] ++
        [{status_changed, unauthorized}]
    end).

-spec authorize(destination()) ->
    {ok, [event()]} |
    {error, _TODO}.

authorize(#{status := unauthorized}) ->
    % TODO
    %  - Do the actual authorization
    {ok, [{status_changed, authorized}]};
authorize(#{status := authorized}) ->
    {ok, []}.

-spec is_accessible(destination()) ->
    {ok, accessible} |
    {error, ff_party:inaccessibility()}.

is_accessible(Destination) ->
    ff_account:is_accessible(account(Destination)).

%%

-spec apply_event(event(), ff_maybe:maybe(destination())) ->
    destination().

apply_event({created, Destination}, undefined) ->
    Destination;
apply_event({status_changed, S}, Destination) ->
    Destination#{status => S};
apply_event({account, Ev}, Destination = #{account := Account}) ->
    Destination#{account => ff_account:apply_event(Ev, Account)};
apply_event({account, Ev}, Destination) ->
    apply_event({account, Ev}, Destination#{account => undefined}).
