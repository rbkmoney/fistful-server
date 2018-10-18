%%%
%%% Withdrawal provider
%%%
%%% TODOs
%%%
%%%  - Anything remotely similar to routing!
%%%

-module(ff_withdrawal_provider).

-export([get/1]).
-export([choose/2]).

-export([id/1]).
-export([adapter/1]).
-export([account/1]).
-export([adapter_opts/1]).

%% Types

-type id() :: binary().
-opaque provider() :: #{
    id           := id(),
    adapter      := adapter(),
    adapter_opts := adapter_opts(),
    account      := account()
}.
-type adapter() :: ff_adapter:adapter().
-type adapter_opts() :: map().

-export_type([id/0]).
-export_type([adapter/0]).
-export_type([provider/0]).
-export_type([adapter_opts/0]).

%% Internal types

-type account() :: ff_account:account().

%% Accessors

-spec id(provider()) ->
    id().
id(#{id := V}) ->
    V.

-spec adapter(provider()) ->
    adapter().
adapter(#{adapter := V}) ->
    V.

-spec account(provider()) ->
    account().
account(#{account := V}) ->
    V.

-spec adapter_opts(provider()) ->
    adapter_opts().
adapter_opts(P) ->
    maps:get(adapter_opts, P, #{}).

%% API

-spec get(id()) ->
    ff_map:result(provider()).

get(ID) ->
    case genlib_map:get(ID, genlib_map:get(provider, genlib_app:env(ff_transfer, withdrawal, #{}))) of
        V when V /= undefined ->
            {ok, V};
        undefined ->
            {error, notfound}
    end.

-spec choose(ff_destination:destination(), ff_transaction:body()) ->
    {ok, id()} |
    {error, notfound}.

choose(Destination, Body) ->
    ID = route(Destination, Body),
    case ?MODULE:get(ID) of
        {ok, _}        -> {ok, ID};
        E = {error, _} -> E
    end.

%%

route(Destination, _Body) ->
    {ok, IdentitySt} = ff_identity_machine:get(ff_account:identity(ff_destination:account(Destination))),
    {ok, Provider} = ff_provider:get(ff_identity:provider(ff_identity_machine:identity(IdentitySt))),
    [ID | _] = ff_provider:routes(Provider),
    ID.
