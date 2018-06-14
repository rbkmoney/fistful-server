%%%
%%% Withdrawal provider
%%%
%%% TODOs
%%%
%%%  - Anything remotely similar to routing!
%%%

-module(ff_withdrawal_provider).

-type provider() :: {ff_party:id(), ff_party:shop_id()}.

-export([choose/2]).

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
