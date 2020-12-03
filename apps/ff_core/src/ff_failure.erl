%%%
%%% Errors

-module(ff_failure).

-type code() :: binary().
-type reason() :: binary().

-type failure() :: #{
    code := code(),
    reason => reason(),
    sub => sub_failure()
}.

-type sub_failure() :: #{
    code := code(),
    sub => sub_failure()
}.

-export_type([code/0]).
-export_type([reason/0]).
-export_type([failure/0]).
-export_type([sub_failure/0]).

-export([code/1]).
-export([reason/1]).
-export([sub_failure/1]).

%% API

-spec code(failure() | sub_failure()) -> code().
code(#{code := Code}) ->
    Code.

-spec reason(failure()) -> reason() | undefined.
reason(Failure) ->
    maps:get(reason, Failure, undefined).

-spec sub_failure(failure() | sub_failure()) -> sub_failure() | undefined.
sub_failure(Failure) ->
    maps:get(sub, Failure, undefined).
