%%%
%%% Wallet
%%%

-module(ff_wallet).

-type id()          :: ff_account:id().
-type external_id() :: id() | undefined.

-type wallet() :: #{
    name        := binary(),
    blocking    := blocking(),
    account     => account(),
    external_id => id()
}.

-type event() ::
    {created, wallet()} |
    {account, ff_account:event()}.

-type create_error() ::
    {identity, notfound} |
    {currency, notfound} |
    ff_account:create_error().

-export_type([id/0]).
-export_type([wallet/0]).
-export_type([event/0]).
-export_type([create_error/0]).

-type inaccessibility() ::
    {inaccessible, blocked}.

-export_type([inaccessibility/0]).

-export([account/1]).
-export([id/1]).
-export([identity/1]).
-export([name/1]).
-export([currency/1]).
-export([blocking/1]).
-export([external_id/1]).

-export([create/5]).
-export([is_accessible/1]).
-export([close/1]).

-export([apply_event/2]).

%% Internal types

-type account()     :: ff_account:account().
-type identity()    :: ff_identity:id().
-type currency()    :: ff_currency:id().
-type blocking()    :: unblocked | blocked.

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec account(wallet()) -> account().

-spec id(wallet()) ->
    id().
-spec identity(wallet()) ->
    identity().
-spec name(wallet()) ->
    binary().
-spec currency(wallet()) ->
    currency().
-spec blocking(wallet()) ->
    blocking().

account(Wallet) ->
    maps:get(account, Wallet, undefined).

id(Wallet) ->
    ff_account:id(account(Wallet)).
identity(Wallet) ->
    ff_account:identity(account(Wallet)).
name(Wallet) ->
    maps:get(name, Wallet, <<>>).
currency(Wallet) ->
    ff_account:currency(account(Wallet)).
blocking(#{blocking := Blocking}) ->
    Blocking.

-spec external_id(wallet()) ->
    external_id().

external_id(#{external_id := ExternalID}) ->
    ExternalID;
external_id(_Wallet) ->
    undefined.

%%

-spec create(id(), identity(), binary(), currency(), external_id()) ->
    {ok, [event()]} |
    {error, create_error()}.

create(ID, IdentityID, Name, CurrencyID, ExternalID) ->
    do(fun () ->
        IdentityMachine = unwrap(identity, ff_identity_machine:get(IdentityID)),
        Identity = ff_identity_machine:identity(IdentityMachine),
        Currency = unwrap(currency, ff_currency:get(CurrencyID)),
        Wallet = #{
            name => Name,
            blocking => unblocked
        },
        [{created, add_external_id(ExternalID, Wallet)}] ++
        [{account, Ev} || Ev <- unwrap(ff_account:create(ID, Identity, Currency))]
    end).

-spec is_accessible(wallet()) ->
    {ok, accessible} |
    {error, inaccessibility()}.

is_accessible(Wallet) ->
    do(fun () ->
        accessible = unwrap(check_accessible(Wallet)),
        accessible = unwrap(ff_account:is_accessible(account(Wallet)))
    end).

-spec close(wallet()) ->
    {ok, [event()]} |
    {error,
        inaccessibility() |
        {account, pending}
    }.

close(Wallet) ->
    do(fun () ->
        accessible = unwrap(is_accessible(Wallet)),
        % TODO
        []
    end).

add_external_id(undefined, Event) ->
    Event;
add_external_id(ExternalID, Event) ->
    Event#{external_id => ExternalID}.

%%

-spec apply_event(event(), undefined | wallet()) ->
    wallet().

apply_event({created, Wallet}, undefined) ->
    Wallet;
apply_event({account, Ev}, Wallet) ->
    Account = maps:get(account, Wallet, undefined),
    Wallet#{account => ff_account:apply_event(Ev, Account)}.

%% Internal functions

-spec check_accessible(wallet()) ->
    {ok, accessible} |
    {error, inaccessibility()}.

check_accessible(Wallet) ->
    case blocking(Wallet) of
        unblocked ->
            {ok, accessible};
        blocked ->
            {error, blocked}
    end.
