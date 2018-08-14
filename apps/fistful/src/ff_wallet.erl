%%%
%%% Wallet
%%%

-module(ff_wallet).

-type id() :: ff_account:id().

-type wallet() :: #{
    name       := binary(),
    contract   := contract(),
    created_at := timestamp(),
    blocking   := blocking(),
    suspension := suspension(),
    account    => account()
}.

-type event() ::
    {created, wallet()} |
    {account, ff_account:event()}.

-export_type([id/0]).
-export_type([wallet/0]).
-export_type([event/0]).

-type inaccessibility() ::
    {inaccessible, blocked | suspended}.

-export_type([inaccessibility/0]).

-export([account/1]).
-export([id/1]).
-export([identity/1]).
-export([name/1]).
-export([currency/1]).
-export([created_at/1]).
-export([blocking/1]).
-export([suspension/1]).

-export([create/4]).
-export([is_accessible/1]).
-export([close/1]).

-export([apply_event/2]).

%% Internal types

-type account() :: ff_account:account().
-type contract() :: ff_party:contract().
-type identity() :: ff_identity:id().
-type currency() :: ff_currency:id().

-type timestamp() :: ff_time:timestamp_ms().
-type blocking() ::
    {unblocked, {Reason :: binary(), Since :: timestamp()}} |
    {blocked, {Reason :: binary(), Since :: timestamp()}}.
-type suspension() ::
    {active, Since :: timestamp()} |
    {suspended, Since :: timestamp()}.

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
-spec created_at(wallet()) ->
    timestamp().
-spec blocking(wallet()) ->
    blocking().
-spec suspension(wallet()) ->
    suspension().

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
created_at(#{created_at := CreatedAt}) ->
    CreatedAt.
blocking(#{blocking := Blocking}) ->
    Blocking.
suspension(#{suspension := Suspension}) ->
    Suspension.

%%

-spec create(id(), identity(), binary(), currency()) ->
    {ok, [event()]} |
    {error, {identity, notfound} | {account, _AccountReason}}.

create(ID, IdentityID, Name, CurrencyID) ->
    do(fun () ->
        Identity = ff_identity_machine:identity(unwrap(identity, ff_identity_machine:get(IdentityID))),
        Contract = ff_identity:contract(Identity),
        CreatedAt = ff_time:now(),
        Wallet = #{
            name => Name,
            contract => Contract,
            created_at => CreatedAt,
            suspension => {active, CreatedAt},
            blocking => {unblocked, {<<"init">>, CreatedAt}}
        },
        [{created, Wallet}] ++
        [{account, Ev} || Ev <- unwrap(account, ff_account:create(ID, Identity, CurrencyID))]
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

%%

-spec apply_event(event(), undefined | wallet()) ->
    wallet().

apply_event({created, Wallet}, undefined) ->
    Wallet;
apply_event({account, Ev}, Wallet) ->
    Account = maps:get(account, Wallet, undefined),
    Wallet#{account := ff_account:apply_event(Ev, Account)}.

%% Internal functions

-spec check_accessible(wallet()) ->
    {ok, accessible} |
    {error, inaccessibility()}.

check_accessible(Wallet) ->
    do(fun () ->
        accessible = unwrap(check_active(Wallet)),
        accessible = unwrap(check_unblocked(Wallet))
    end).

-spec check_active(wallet()) ->
    {ok, accessible} |
    {error, suspended}.

check_active(Wallet) ->
    case suspension(Wallet) of
        {active, _Details} ->
            {ok, accessible};
        {suspended, _Details} ->
            {error, suspended}
    end.

-spec check_unblocked(wallet()) ->
    {ok, accessible} |
    {error, blocked}.

check_unblocked(Wallet) ->
    case blocking(Wallet) of
        {unblocked, _Details} ->
            {ok, accessible};
        {blocked, _Details} ->
            {error, blocked}
    end.

