%%%
%%% A matter of time.

-module(ff_time).

-export([now/0]).
-export([to_rfc3339/1]).
-export([add_interval/2]).

-export_type([timestamp_ms/0]).

-type timestamp_ms()      :: integer().
-type interval()          :: {integer(), integer(), integer()}.
-type datetime_interval() :: {interval(), interval()}.

%% API

-spec now() -> timestamp_ms().
now() ->
    erlang:system_time(millisecond).

-spec to_rfc3339(timestamp_ms()) -> binary().
to_rfc3339(Timestamp) ->
    {ok, BTimestamp} = rfc3339:format(Timestamp, millisecond),
    BTimestamp.

-spec add_interval(timestamp_ms(), datetime_interval()) ->
    timestamp_ms().
add_interval(Timestamp, {Date, Time}) ->
    Ms = Timestamp rem 1000,
    {D, T} = calendar:system_time_to_local_time(Timestamp, millisecond),
    NewDate = genlib_time:daytime_to_unixtime({genlib_time:shift_date(D, Date), T}),
    DateTime = genlib_time:add_duration(NewDate, Time),
    DateTime*1000 + Ms.
