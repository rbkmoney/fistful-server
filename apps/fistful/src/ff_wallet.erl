%%%
%%% Wallet
%%%

-module(ff_wallet).

-type id() :: ff_account:id().

-type wallet() :: #{
    name       := binary(),
    contract   := contract(),
    blocking   := blocking(),
    account    => account()
}.

-type event() ::
    {created, wallet()} |
    {account, ff_account:event()}.

-export_type([id/0]).
-export_type([wallet/0]).
-export_type([event/0]).

-type inaccessibility() ::
    {inaccessible, blocked}.

-export_type([inaccessibility/0]).

-export([account/1]).
-export([id/1]).
-export([identity/1]).
-export([name/1]).
-export([currency/1]).
-export([blocking/1]).

-export([create/4]).
-export([is_accessible/1]).
-export([close/1]).

-export([apply_event/2]).

%% Internal types

-type account() :: ff_account:account().
-type contract() :: ff_party:contract().
-type identity() :: ff_identity:id().
-type currency() :: ff_currency:id().
-type blocking() :: unblocked | blocked.

%% Pipeline

-compile({parse_transform, ff_pipeline}).
-import(ff_pipeline, [unwrap/1, unwrap/2]).

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

account(#{account := V}) ->
    V.

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

%%

-spec create(id(), identity(), binary(), currency()) ->
    {ok, [event()]} |
    {error, _Reason}.

create(ID, IdentityID, Name, CurrencyID) ->
    ff_pipeline:do(fun () ->
        Identity = ff_identity_machine:identity(unwrap(identity, ff_identity_machine:get(IdentityID))),
        Contract = ff_identity:contract(Identity),
        Contract = ff_identity:contract(Identity),
        Currency = unwrap(currency, ff_currency:get(CurrencyID)),
        Wallet = #{
            name => Name,
            contract => Contract,
            blocking => unblocked
        },
        [{created, Wallet}] ++
        [{account, Ev} || Ev <- unwrap(ff_account:create(ID, Identity, Currency))]
    end).

-spec is_accessible(wallet()) ->
    {ok, accessible} |
    {error, inaccessibility()}.

is_accessible(Wallet) ->
    ff_pipeline:do(fun () ->
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
    ff_pipeline:do(fun () ->
        accessible = unwrap(is_accessible(Wallet)),
        % TODO
        []
    end).

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
