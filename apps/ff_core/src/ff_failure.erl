%%%
%%% Errors

-module(ff_failure).

-type code() :: binary().
-type reason() :: binary().

-type failure() :: #{
    code   := code(),
    reason => reason(),
    sub    => sub_failure()
}.

-type sub_failure() :: #{
    code := code(),
    sub  => sub_failure()
}.

-export_type([code/0]).
-export_type([reason/0]).
-export_type([failure/0]).
-export_type([sub_failure/0]).
