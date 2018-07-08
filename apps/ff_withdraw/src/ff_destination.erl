%%%
%%% Destination
%%%
%%% TODOs
%%%
%%%  - We must consider withdrawal provider terms ensure that the provided
%%%    Resource is ok to withdraw to.
%%%

-module(ff_destination).

-type id()       :: machinery:id().
-type wallet()   :: ff_wallet:wallet().
-type resource() ::
    {bank_card, resource_bank_card()}.

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
    id       := id(),
    resource := resource(),
    wallet   => wallet(),
    status   => status()
}.

-type ev() ::
    {created, destination()} |
    {wallet, ff_wallet:ev()} |
    {status_changed, status()}.

-type outcome() :: [ev()].

-export_type([destination/0]).
-export_type([status/0]).
-export_type([resource/0]).
-export_type([ev/0]).

-export([id/1]).
-export([wallet/1]).
-export([resource/1]).
-export([status/1]).

-export([create/5]).
-export([authorize/1]).

-export([apply_event/2]).

-export([dehydrate/1]).
-export([hydrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec id(destination())       -> id().
-spec wallet(destination())   -> wallet().
-spec resource(destination()) -> resource().
-spec status(destination())   -> status().

id(#{id := V})                -> V.
wallet(#{wallet := V})        -> V.
resource(#{resource := V})    -> V.
status(#{status := V})        -> V.

%%

-spec create(id(), ff_identity:identity(), binary(), ff_currency:id(), resource()) ->
    {ok, outcome()} |
    {error, _WalletError}.

create(ID, Identity, Name, Currency, Resource) ->
    do(fun () ->
        WalletEvents1 = unwrap(ff_wallet:create(ID, Identity, Name, Currency)),
        WalletEvents2 = unwrap(ff_wallet:setup_wallet(ff_wallet:collapse_events(WalletEvents1))),
        [
            {created, #{
                id       => ID,
                resource => Resource
            }}
        ] ++
        [{wallet, Ev} || Ev <- WalletEvents1 ++ WalletEvents2] ++
        [{status_changed, unauthorized}]
    end).

-spec authorize(destination()) ->
    {ok, outcome()} |
    {error, _TODO}.

authorize(#{status := unauthorized}) ->
    % TODO
    %  - Do the actual authorization
    {ok, [{status_changed, authorized}]};
authorize(#{status := authorized}) ->
    {ok, []}.

%%

-spec apply_event(ev(), undefined | destination()) ->
    destination().

apply_event({created, D}, undefined) ->
    D;
apply_event({status_changed, S}, D) ->
    D#{status => S};
apply_event({wallet, Ev}, D) ->
    D#{wallet => ff_wallet:apply_event(Ev, genlib_map:get(wallet, D))}.

-spec dehydrate(ev()) ->
    term().

-spec hydrate(term(), undefined | destination()) ->
    ev().

dehydrate({created, D}) ->
    {created, #{
        id       => id(D),
        resource => resource(D)
    }};
dehydrate({wallet, Ev}) ->
    {wallet, ff_wallet:dehydrate(Ev)};
dehydrate({status_changed, S}) ->
    {status_changed, S}.

hydrate({created, V}, undefined) ->
    {created, #{
        id       => maps:get(id, V),
        resource => maps:get(resource, V)
    }};
hydrate({wallet, Ev}, D) ->
    {wallet, ff_wallet:hydrate(Ev, genlib_map:get(wallet, D))};
hydrate({status_changed, S}, _) ->
    {status_changed, S}.
