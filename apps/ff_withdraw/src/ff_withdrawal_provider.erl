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
-export([choose/2]).
-export([create_session/3]).

%%

adapter(#{adapter := V}) -> V.
adapter_opts(P) -> maps:get(adapter_opts, P, #{}).

%%

-spec id(provider()) ->
    id().

id(_) ->
    <<>>.

-spec get(id()) ->
    ff_map:result(provider()).

get(_) ->
    case genlib_app:env(ff_withdraw, provider) of
        V when V /= undefined ->
            {ok, V};
        undefined ->
            {error, notfound}
    end.

-spec choose(ff_destination:destination(), ff_transaction:body()) ->
    {ok, provider()} |
    {error, notfound}.

choose(_Destination, _Body) ->
    case genlib_app:env(ff_withdraw, provider) of
        V when V /= undefined ->
            {ok, V};
        undefined ->
            {error, notfound}
    end.

%%

create_session(ID, Withdrawal, Provider) ->
    Adapter = {adapter(Provider), adapter_opts(Provider)},
    ff_withdrawal_session_machine:create(ID, Adapter, Withdrawal).
