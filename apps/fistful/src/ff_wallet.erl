%%%
%%% Wallet
%%%

-module(ff_wallet).

-type identity() :: ff_identity:identity().
-type currency() :: ff_currency:id().
-type wid()      :: ff_party:wallet().
-type id(T)      :: T.

-type wallet() :: #{
    id       := id(_),
    identity := identity(),
    name     := binary(),
    currency := currency(),
    wid      => wid()
}.

-type ev() ::
    {created, wallet()} |
    {wid_set, wid()}.

-type outcome() :: [ev()].

-export_type([wallet/0]).
-export_type([ev/0]).

-export([id/1]).
-export([identity/1]).
-export([name/1]).
-export([currency/1]).
-export([account/1]).

-export([create/4]).
-export([setup_wallet/1]).
-export([is_accessible/1]).
-export([close/1]).

-export([collapse_events/1]).
-export([apply_events/2]).
-export([apply_event/2]).

-export([dehydrate/1]).
-export([hydrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec id(wallet())         -> id(_).
-spec identity(wallet())   -> identity().
-spec name(wallet())       -> binary().
-spec currency(wallet())   -> currency().

id(#{id := V})             -> V.
identity(#{identity := V}) -> V.
name(#{name := V})         -> V.
currency(#{currency := V}) -> V.

-spec wid(wallet()) -> ff_map:result(wid()).

wid(Wallet) ->
    ff_map:find(wid, Wallet).

-spec account(wallet()) ->
    {ok, ff_transaction:account()} |
    {error, notfound}.

account(Wallet) ->
    do(fun () ->
        WID = unwrap(wid(Wallet)),
        unwrap(ff_party:get_wallet_account(ff_identity:party(identity(Wallet)), WID))
    end).

%%

-spec create(id(_), identity(), binary(), currency()) ->
    {ok, outcome()}.

create(ID, Identity, Name, Currency) ->
    do(fun () ->
        [{created, #{
            id       => ID,
            identity => Identity,
            name     => Name,
            currency => Currency
        }}]
    end).

-spec setup_wallet(wallet()) ->
    {ok, outcome()} |
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
        % TODO
        %  - There is an opportunity for a race where someone can block party
        %    right before we create a party wallet.
        WID        = unwrap(ff_party:create_wallet(ff_identity:party(Identity), Contract, Prototype)),
        [{wid_set, WID}]
    end).

-spec is_accessible(wallet()) ->
    {ok, accessible} |
    {error,
        {wid, notfound} |
        {inaccessible, suspended | blocked}
    }.

is_accessible(Wallet) ->
    do(fun () ->
        Identity   = identity(Wallet),
        WID        = unwrap(wid, wid(Wallet)),
        accessible = unwrap(ff_identity:is_accessible(Identity)),
        accessible = unwrap(ff_party:is_wallet_accessible(ff_identity:party(Identity), WID)),
        accessible
    end).

-spec close(wallet()) ->
    {ok, outcome()} |
    {error,
        {inaccessible, blocked | suspended} |
        {account, pending}
    }.

close(Wallet) ->
    do(fun () ->
        accessible = unwrap(is_accessible(Wallet)),
        % TODO
        []
    end).

%%

-spec collapse_events([ev(), ...]) ->
    wallet().

collapse_events(Evs) when length(Evs) > 0 ->
    apply_events(Evs, undefined).

-spec apply_events([ev()], undefined | wallet()) ->
    undefined | wallet().

apply_events(Evs, Identity) ->
    lists:foldl(fun apply_event/2, Identity, Evs).

-spec apply_event(ev(), undefined | wallet()) ->
    wallet().

apply_event({created, Wallet}, undefined) ->
    Wallet;
apply_event({wid_set, WID}, Wallet) ->
    Wallet#{wid => WID}.

%%

-spec dehydrate(ev()) ->
    term().

-spec hydrate(term(), undefined | wallet()) ->
    ev().

dehydrate({created, Wallet}) ->
    {created, #{
        id       => id(Wallet),
        name     => name(Wallet),
        identity => ff_identity:id(identity(Wallet)),
        currency => currency(Wallet)
    }};
dehydrate({wid_set, WID}) ->
    {wid_set, WID}.

hydrate({created, V}, undefined) ->
    {ok, IdentitySt} = ff_identity_machine:get(maps:get(identity, V)),
    {created, #{
        id       => maps:get(id, V),
        name     => maps:get(name, V),
        identity => ff_identity_machine:identity(IdentitySt),
        currency => maps:get(currency, V)
    }};
hydrate({wid_set, WID}, _) ->
    {wid_set, WID}.
