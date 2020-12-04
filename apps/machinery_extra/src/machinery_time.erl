%%%
%%%

-module(machinery_time).

-type timestamp() :: machinery:timestamp().

-export([now/0]).
-export([add_seconds/2]).
-export([interval/2]).

%%

-spec now() -> timestamp().
now() ->
    Now = {_, _, USec} = os:timestamp(),
    {calendar:now_to_universal_time(Now), USec}.

-spec add_seconds(integer(), timestamp()) -> timestamp().
add_seconds(V, {Dt, USec}) ->
    {gs2dt(dt2gs(Dt) + V), USec}.

-spec interval(timestamp(), timestamp()) -> timeout().
interval({Dt1, USec1}, {Dt2, USec2}) ->
    (dt2gs(Dt1) - dt2gs(Dt2)) * 1000 + (USec1 - USec2) div 1000.

%%

dt2gs(Dt) ->
    calendar:datetime_to_gregorian_seconds(Dt).

gs2dt(Dt) ->
    calendar:gregorian_seconds_to_datetime(Dt).
