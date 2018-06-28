%%%
%%% Withdrawal processing
%%%

-module(ff_withdraw).

-type namespace() :: machinery:namespace().
-type backend()   :: machinery:backend(_).

-export([backend/1]).

%%

-spec backend(namespace()) ->
    backend().

backend(NS) ->
    maps:get(NS, genlib_app:env(?MODULE, backends, #{})).
