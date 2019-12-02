%%%
%%% A matter of time.

-module(ff_time).

-export([now/0]).
-export([from_rfc3339/1]).
-export([to_rfc3339/1]).

-export_type([timestamp_ms/0]).

-type timestamp_ms() :: integer().

%% API
-spec now() -> timestamp_ms().
now() ->
    erlang:system_time(millisecond).

-spec from_rfc3339(binary()) -> timestamp_ms().
from_rfc3339(BTimestamp) ->
    {ok, Timestamp} = rfc3339:to_time(BTimestamp, millisecond),
    Timestamp.

-spec to_rfc3339(timestamp_ms()) -> binary().
to_rfc3339(Timestamp) ->
    {ok, BTimestamp} = rfc3339:format(Timestamp, millisecond),
    BTimestamp.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec rfc3339_symmetry_test() -> _.
rfc3339_symmetry_test() ->
    TimestampStr = <<"2000-01-01T00:00:00Z">>,
    ?assertEqual(TimestampStr, to_rfc3339(from_rfc3339(TimestampStr))).

-endif.
