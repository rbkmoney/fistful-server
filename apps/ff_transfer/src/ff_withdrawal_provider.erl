%%%
%%% Withdrawal provider
%%%
%%% TODOs
%%%
%%%  - Anything remotely similar to routing!
%%%

-module(ff_withdrawal_provider).

-export([id/1]).
-export([get/1]).
-export([get_adapter/1]).
-export([get_account/1]).
-export([choose/2]).

%%

-type id() :: binary().
-type provider() :: #{
    _ => _ % TODO
}.

-type adapter() :: {ff_adapter:adapter(), ff_adapter:opts()}.
-type account() :: ff_account:account().
-export_type([adapter/0]).


%%

-spec id(provider()) ->
    id().

id(_) ->
    <<>>.

-spec get(id()) ->
    ff_map:result(provider()).

get(ID) ->
    case genlib_map:get(ID, genlib_map:get(provider, genlib_app:env(ff_transfer, withdrawal, #{}))) of
        V when V /= undefined ->
            {ok, V};
        undefined ->
            {error, notfound}
    end.

-spec get_adapter(id()) ->
    {ok, adapter()} |
    {error, notfound}.

get_adapter(ID) ->
    case ?MODULE:get(ID) of
        {ok, Provider} ->
            {ok, {adapter(Provider), adapter_opts(Provider)}};
        Error = {error, _} ->
            Error
    end.

-spec get_account(id()) ->
    {ok, account()} |
    {error, notfound}.

get_account(ID) ->
    case ?MODULE:get(ID) of
        {ok, Provider} ->
            {ok, account(Provider)};
        Error = {error, _} ->
            Error
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

adapter(#{adapter := V}) ->
    V.

account(#{account := V}) ->
    V.

adapter_opts(P) ->
    maps:get(adapter_opts, P, #{}).
