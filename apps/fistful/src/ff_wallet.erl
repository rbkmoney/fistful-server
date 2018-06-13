%%%
%%% Wallet
%%%

-module(ff_wallet).

-type wallet() :: #{
    identity := ff_identity:id(),
    name     := binary(),
    currency := ff_currency:id(),
    wid      := ff_party:wallet_id()
}.

-type prototype() :: #{
    name     := binary(),
    currency := ff_currency:id()
}.

-export_type([wallet/0]).
-export_type([prototype/0]).

-export([identity/1]).
-export([name/1]).
-export([currency/1]).

-export([create/2]).
-export([is_accessible/1]).
-export([close/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec identity(wallet())   -> binary().
-spec name(wallet())       -> binary().
-spec currency(wallet())   -> ff_currency:id().
-spec wid(wallet())        -> ff_party:wallet_id().

identity(#{identity := V}) -> V.
name(#{name := V})         -> V.
currency(#{currency := V}) -> V.
wid(#{wid := V})           -> V.

%%

-spec create(ff_identity:id(), prototype()) ->
    {ok, wallet()} |
    {error,
        {identity,
            notfound |
            {inaccessible, blocked | suspended}
        } |
        {contract,
            notfound
        } |
        invalid
    }.

create(IdentityID, Prototype) ->
    do(fun () ->
        #{
            name     := Name,
            currency := CurrencyID
        } = Prototype,
        Identity   = unwrap(identity, ff_identity_machine:get(IdentityID)),
        accessible = unwrap(identity, ff_identity:is_accessible(Identity)),
        _Currency  = unwrap(currency, ff_currency:get(CurrencyID)),
        PartyID    = ff_identity:party(Identity),
        ContractID = unwrap(contract, ff_identity:contract(Identity)),
        WalletID   = unwrap(ff_party:create_wallet(PartyID, ContractID, Prototype)),
        #{
            identity => IdentityID,
            name     => Name,
            currency => CurrencyID,
            wid      => WalletID
        }
    end).

-spec is_accessible(wallet()) ->
    {ok, accessible} |
    {error, {inaccessible, suspended | blocked}}.

is_accessible(Wallet) ->
    do(fun () ->
        {ok, Identity} = ff_identity_machine:get(identity(Wallet)),
        accessible     = unwrap(ff_identity:is_accessible(identity(Wallet))),
        accessible     = unwrap(ff_party:is_wallet_accessible(ff_identity:party(Identity), wid(Wallet))),
        accessible
    end).

-spec close(wallet()) ->
    {ok, wallet()} |
    {error,
        {inaccessible, blocked | suspended} |
        {account, pending}
    }.

close(Wallet) ->
    do(fun () ->
        accessible = unwrap(is_accessible(Wallet)),
        % TODO
        unwrap({error, pending})
    end).
