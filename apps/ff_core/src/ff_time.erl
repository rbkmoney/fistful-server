%%%
%%% A matter of time.

-module(ff_time).

-export([now/0]).
-export([to_rfc3339/1]).
-export([from_rfc3339/1]).
-export([add_interval/2]).

-export_type([timestamp_ms/0]).

-type timestamp_ms() :: integer().
-type year() :: integer().
-type month() :: integer().
-type day() :: integer().
-type hour() :: integer().
-type minute() :: integer().
-type second() :: integer().
-type date() :: {year(), month(), day()}.
-type time() :: {hour(), minute(), second()}.
-type datetime_interval() :: {date(), time()}.

%% API

-spec now() -> timestamp_ms().
now() ->
    erlang:system_time(millisecond).

-spec to_rfc3339(timestamp_ms()) -> binary().
to_rfc3339(Timestamp) ->
    genlib_rfc3339:format_relaxed(Timestamp, millisecond).

-spec from_rfc3339(binary()) -> timestamp_ms().
from_rfc3339(BTimestamp) ->
    genlib_rfc3339:parse(BTimestamp, millisecond).

-spec add_interval(timestamp_ms(), datetime_interval()) -> timestamp_ms().
add_interval(Timestamp, {Date, Time}) ->
    Ms = Timestamp rem 1000,
    TSSeconds = erlang:convert_time_unit(Timestamp, millisecond, second),
    {D, T} = genlib_time:unixtime_to_daytime(TSSeconds),
    NewDate = genlib_time:daytime_to_unixtime({genlib_time:shift_date(D, Date), T}),
    DateTime = genlib_time:add_duration(NewDate, Time),
    DateTime * 1000 + Ms.

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec rfc3339_symmetry_test() -> _.

rfc3339_symmetry_test() ->
    TimestampStr = <<"2000-01-01T00:00:00Z">>,
    ?assertEqual(TimestampStr, to_rfc3339(from_rfc3339(TimestampStr))).

-spec add_second_interval_test() -> _.
add_second_interval_test() ->
    Timestamp = ff_time:now(),
    NewTimestamp = add_interval(Timestamp, {{0, 0, 0}, {0, 0, 1}}),
    ?assertEqual(Timestamp + 1000, NewTimestamp).

-spec add_minute_interval_test() -> _.
add_minute_interval_test() ->
    Timestamp = ff_time:now(),
    NewTimestamp = add_interval(Timestamp, {{0, 0, 0}, {0, 1, 0}}),
    ?assertEqual(Timestamp + 60 * 1000, NewTimestamp).

-spec add_hour_interval_test() -> _.
add_hour_interval_test() ->
    Timestamp = ff_time:now(),
    NewTimestamp = add_interval(Timestamp, {{0, 0, 0}, {1, 0, 0}}),
    ?assertEqual(Timestamp + 60 * 60 * 1000, NewTimestamp).

-spec add_day_interval_test() -> _.
add_day_interval_test() ->
    Timestamp = ff_time:now(),
    NewTimestamp = add_interval(Timestamp, {{0, 0, 1}, {0, 0, 0}}),
    ?assertEqual(Timestamp + 24 * 60 * 60 * 1000, NewTimestamp).

-endif.
