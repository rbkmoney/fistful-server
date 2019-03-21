%%%
%%% A matter of time.

-module(ff_time).

-export([now/0]).
-export([to_rfc3339/1]).
-export([now_rfc3339/0]).

-export_type([timestamp_ms/0]).

-type timestamp_ms() :: integer().

%% API
-spec now() -> timestamp_ms().
now() ->
    erlang:system_time(millisecond).

-spec to_rfc3339(timestamp_ms()) -> binary().
to_rfc3339(Timestamp) ->
    {ok, BTimestamp} = rfc3339:format(Timestamp, millisecond),
    BTimestamp.

-spec now_rfc3339() -> binary().
now_rfc3339() ->
    {ok, BTimestamp} = rfc3339:format(ff_time:now(), millisecond),
    BTimestamp.
