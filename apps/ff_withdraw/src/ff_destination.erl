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
    wallet   := wallet(),
    resource := resource(),
    status   := status()
}.

-export_type([destination/0]).
-export_type([status/0]).
-export_type([resource/0]).

-export([id/1]).
-export([wallet/1]).
-export([resource/1]).
-export([status/1]).

-export([set_status/2]).

-export([create/5]).
-export([authorize/1]).

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

-spec set_status(status(), destination()) -> destination().

set_status(V, D = #{})        -> D#{status := V}.

%%

-spec create(id(), ff_identity:identity(), binary(), ff_currency:id(), resource()) ->
    {ok, destination()} |
    {error, _WalletError}.

create(ID, Identity, Name, Currency, Resource) ->
    WalletID = woody_context:new_req_id(),
    do(fun () ->
        Wallet = unwrap(ff_wallet:create(WalletID, Identity, Name, Currency)),
        #{
            id       => ID,
            wallet   => Wallet,
            resource => Resource,
            status   => unauthorized
        }
    end).

-spec authorize(destination()) ->
    {ok, destination()} |
    {error, _TODO}.

authorize(Destination = #{status := unauthorized}) ->
    % TODO
    %  - Do the actual authorization
    {ok, set_status(authorized, Destination)};
authorize(Destination = #{status := authorized}) ->
    {ok, Destination}.
