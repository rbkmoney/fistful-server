%%%
%%% Wallet
%%%

-module(ff_wallet).

-type identity() :: ff_identity:identity().
-type currency() :: ff_currency:id().
-type wid()      :: ff_party:wallet().

-type wallet() :: #{
    identity := identity(),
    name     := binary(),
    currency := currency(),
    wid      := wid()
}.

-export_type([wallet/0]).

-export([identity/1]).
-export([name/1]).
-export([currency/1]).
-export([account/1]).

-export([create/3]).
-export([setup_wallet/1]).
-export([is_accessible/1]).
-export([close/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec identity(wallet())   -> identity().
-spec name(wallet())       -> binary().
-spec currency(wallet())   -> currency().
-spec wid(wallet())        -> wid().

identity(#{identity := V}) -> V.
name(#{name := V})         -> V.
currency(#{currency := V}) -> V.
wid(#{wid := V})           -> V.

-spec set_wid(wid(), wallet()) -> wallet().

set_wid(V, W = #{}) -> W#{wid => V}.

-spec account(wallet()) ->
    {ok, ff_transaction:account()} |
    {error, notfound}.

account(Wallet) ->
    do(fun () ->
        unwrap(ff_party:get_wallet_account(
            ff_identity:party(identity(Wallet)),
            wid(Wallet)
        ))
    end).

%%

-spec create(identity(), binary(), currency()) ->
    {ok, wallet()}.

create(Identity, Name, Currency) ->
    do(fun () ->
        #{
            identity => Identity,
            name     => Name,
            currency => Currency
        }
    end).

-spec setup_wallet(wallet()) ->
    {ok, wallet()} |
    {error,
        {inaccessible, blocked | suspended} |
        {contract, notfound} |
        invalid
    }.

setup_wallet(Wallet) ->
    do(fun () ->
        Identity   = identity(Wallet),
        accessible = unwrap(ff_identity:is_accessible(Identity)),
        Contract   = unwrap(contract, ff_identity:contract(Identity)),
        Prototype  = #{
            name     => name(Wallet),
            currency => currency(Wallet)
        },
        WID        = unwrap(ff_party:create_wallet(ff_identity:party(Identity), Contract, Prototype)),
        set_wid(WID, Wallet)
    end).

-spec is_accessible(wallet()) ->
    {ok, accessible} |
    {error, {inaccessible, suspended | blocked}}.

is_accessible(Wallet) ->
    do(fun () ->
        Identity   = identity(Wallet),
        accessible = unwrap(ff_identity:is_accessible(Identity)),
        accessible = unwrap(ff_party:is_wallet_accessible(ff_identity:party(Identity), wid(Wallet))),
        accessible
    end).

-spec close(wallet()) ->
    ok |
    {error,
        {inaccessible, blocked | suspended} |
        {account, pending}
    }.

close(Wallet) ->
    do(fun () ->
        accessible = unwrap(is_accessible(Wallet)),
        % TODO
        ok
    end).
