%%%
%%% Wallet
%%%

-module(ff_wallet).

-type account() :: ff_account:id().

-type id(T) :: T.
-type identity() :: ff_identity:id().
-type currency() :: ff_currency:id().

-type wallet() :: #{
    account  := account(),
    name     := binary()
}.

-type event() ::
    {created, wallet()} |
    {account, ff_account:event()}.

-export_type([wallet/0]).
-export_type([event/0]).

-export([account/1]).
-export([id/1]).
-export([identity/1]).
-export([name/1]).
-export([currency/1]).

-export([create/4]).
-export([is_accessible/1]).
-export([close/1]).

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%% Accessors

-spec account(wallet()) -> account().

-spec id(wallet()) ->
    id(_).
-spec identity(wallet()) ->
    identity().
-spec name(wallet()) ->
    binary().
-spec currency(wallet()) ->
    currency().

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

%%

-spec create(id(_), identity(), binary(), currency()) ->
    {ok, [event()]}.

create(ID, IdentityID, Name, CurrencyID) ->
    do(fun () ->
        [{created, #{name => Name}}] ++
        [{account, Ev} || Ev <- unwrap(ff_account:create(ID, IdentityID, CurrencyID))]
    end).

-spec is_accessible(wallet()) ->
    {ok, accessible} |
    {error, ff_party:inaccessibility()}.

is_accessible(Wallet) ->
    ff_account:is_accessible(account(Wallet)).

-spec close(wallet()) ->
    {ok, [event()]} |
    {error,
        ff_party:inaccessibility() |
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
apply_event({account, Ev}, Wallet = #{account := Account}) ->
    Wallet#{account := ff_account:apply_event(Ev, Account)};
apply_event({account, Ev}, Wallet) ->
    apply_event({account, Ev}, Wallet#{account => undefined}).
