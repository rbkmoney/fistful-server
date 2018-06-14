%%%
%%% Destination
%%%
%%% TODOs
%%%
%%%  - We must consider withdrawal provider terms ensure that the provided
%%%    Resource is ok to withdraw to.
%%%

-module(ff_destination).

-type wallet()   :: ff_wallet:wallet().
-type resource() ::
    {bank_card, resource_bank_card()}.

-type resource_bank_card() :: #{
    token          := binary(),
    payment_system => atom(), % TODO
    bin            => binary()
}.

-type status() ::
    unauthorized |
    authorized.

-type destination() :: #{
    wallet   := wallet(),
    resource := resource(),
    status   := status()
}.

-export_type([destination/0]).
-export_type([status/0]).
-export_type([resource/0]).

-export([wallet/1]).
-export([resource/1]).
-export([status/1]).

-export([set_status/2]).

-export([create/3]).
-export([authorize/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec wallet(destination())   -> wallet().
-spec resource(destination()) -> resource().
-spec status(destination())   -> status().

wallet(#{wallet := V})        -> V.
resource(#{resource := V})    -> V.
status(#{status := V})        -> V.

-spec set_status(status(), destination()) -> destination().

set_status(V, D = #{})        -> D#{status := V}.

%%

-spec create(ff_identity:id(), ff_wallet:prototype(), resource()) ->
    {ok, destination()} |
    {error, _WalletError}.

create(IdentityID, Prototype, Resource) ->
    do(fun () ->
        Wallet = unwrap(ff_wallet:create(IdentityID, Prototype)),
        #{
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
