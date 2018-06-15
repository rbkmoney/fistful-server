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

backend(_NS) ->
    genlib_app:env(?MODULE, backend).
