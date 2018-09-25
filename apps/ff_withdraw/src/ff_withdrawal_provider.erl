%%%
%%% Withdrawal provider
%%%
%%% TODOs
%%%
%%%  - Anything remotely similar to routing!
%%%

-module(ff_withdrawal_provider).

-type id() :: binary().
-type provider() :: #{
    _ => _ % TODO
}.

-export([id/1]).

-export([get/1]).
-export([choose/3]).
-export([create_session/3]).

%%

adapter(#{adapter := V}) ->
    V.
adapter_opts(P) ->
    maps:get(adapter_opts, P, #{}).

%%

-spec id(provider()) ->
    id().

id(_) ->
    <<>>.

-spec get(id()) ->
    ff_map:result(provider()).

get(ID) ->
    case genlib_map:get(ID, genlib_app:env(ff_withdraw, provider, #{})) of
        V when V /= undefined ->
            {ok, V};
        undefined ->
            {error, notfound}
    end.

-spec choose(ff_wallet:wallet(), ff_destination:destination(), ff_transaction:body()) ->
    {ok, id()} |
    {error, notfound}.

choose(_Source, Destination, _Body) ->
    {ok, IdentitySt} = ff_identity_machine:get(ff_account:identity(ff_destination:account(Destination))),
    {ok, Provider} = ff_provider:get(ff_identity:provider(ff_identity_machine:identity(IdentitySt))),
    [ID | _] = ff_provider:routes(Provider),
    case ?MODULE:get(ID) of
        {ok, _}        -> {ok, ID};
        E = {error, _} -> E
    end.

%%

-spec create_session(id(), ff_adapter_withdrawal:withdrawal(), provider()) ->
    ok | {error, exists}.

create_session(ID, Withdrawal, Provider) ->
    Adapter = {adapter(Provider), adapter_opts(Provider)},
    ff_withdrawal_session_machine:create(ID, Adapter, Withdrawal).
