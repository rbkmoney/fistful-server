-module(wapi_utils).

-export([base64url_to_map/1]).
-export([map_to_base64url/1]).

-export([to_universal_time/1]).

-export([redact/2]).
-export([mask_and_keep/4]).
-export([mask/4]).

-export([unwrap/1]).
-export([define/2]).

-export([get_path/2]).
-export([get_url/2]).
-export([get_url/3]).

-export([get_last_pan_digits/1]).

-export([parse_deadline/1]).

-export([get_unique_id/0]).

-type binding_value() :: binary().
-type url()           :: binary().
-type path()          :: binary().
-type route_match()   :: '_' | iodata(). % cowoby_router:route_match()

-export_type([route_match/0]).

%% API

-spec base64url_to_map(binary()) -> map() | no_return().
base64url_to_map(Base64) when is_binary(Base64) ->
    try jsx:decode(base64url:decode(Base64), [return_maps])
    catch
        Class:Reason ->
            _ = logger:debug("decoding base64 ~p to map failed with ~p:~p", [Base64, Class, Reason]),
            erlang:error(badarg)
    end.

-spec map_to_base64url(map()) -> binary() | no_return().
map_to_base64url(Map) when is_map(Map) ->
    try base64url:encode(jsx:encode(Map))
    catch
        Class:Reason ->
            _ = logger:debug("encoding map ~p to base64 failed with ~p:~p", [Map, Class, Reason]),
            erlang:error(badarg)
    end.

-spec redact(Subject :: binary(), Pattern :: binary()) -> Redacted :: binary().
redact(Subject, Pattern) ->
    case re:run(Subject, Pattern, [global, {capture, all_but_first, index}]) of
        {match, Captures} ->
            lists:foldl(fun redact_match/2, Subject, Captures);
        nomatch ->
            Subject
    end.

redact_match({S, Len}, Subject) ->
    <<Pre:S/binary, _:Len/binary, Rest/binary>> = Subject,
    <<Pre/binary, (binary:copy(<<"*">>, Len))/binary, Rest/binary>>;
redact_match([Capture], Message) ->
    redact_match(Capture, Message).

%% TODO Switch to this sexy code after the upgrade to Erlang 20+
%%
%% -spec mask(leading|trailing, non_neg_integer(), char(), binary()) ->
%%     binary().
%% mask(Dir = trailing, MaskLen, MaskChar, Str) ->
%%     mask(Dir, 0, string:length(Str) - MaskLen, MaskChar, Str);
%% mask(Dir = leading, MaskLen, MaskChar, Str) ->
%%     mask(Dir, MaskLen, string:length(Str), MaskChar, Str).

%% mask(Dir, KeepStart, KeepLen, MaskChar, Str) ->
%%     unicode:characters_to_binary(
%%         string:pad(string:slice(Str, KeepStart, KeepLen), string:length(Str), Dir, MaskChar)
%%     ).

-spec mask_and_keep(leading|trailing, non_neg_integer(), char(), binary()) ->
    binary().
mask_and_keep(trailing, KeepLen, MaskChar, Chardata) ->
    StrLen = erlang:length(unicode:characters_to_list(Chardata)),
    mask(leading, StrLen - KeepLen, MaskChar, Chardata);
mask_and_keep(leading, KeepLen, MaskChar, Chardata) ->
    StrLen = erlang:length(unicode:characters_to_list(Chardata)),
    mask(trailing, StrLen - KeepLen, MaskChar, Chardata).

-spec mask(leading|trailing, non_neg_integer(), char(), binary()) ->
    binary().
mask(trailing, MaskLen, MaskChar, Chardata) ->
    Str = unicode:characters_to_list(Chardata),
    unicode:characters_to_binary(
        string:left(string:substr(Str, 1, erlang:length(Str) - MaskLen), erlang:length(Str), MaskChar)
    );
mask(leading, MaskLen, MaskChar, Chardata) ->
    Str = unicode:characters_to_list(Chardata),
    unicode:characters_to_binary(
        string:right(string:substr(Str, MaskLen + 1), erlang:length(Str), MaskChar)
    ).

-spec to_universal_time(Timestamp :: binary()) -> TimestampUTC :: binary().
to_universal_time(Timestamp) ->
    TimestampMS = genlib_rfc3339:parse(Timestamp, microsecond),
    genlib_rfc3339:format_relaxed(TimestampMS, microsecond).

-spec unwrap(ok | {ok, Value} | {error, _Error}) ->
    Value | no_return().
unwrap(ok) ->
    ok;
unwrap({ok, Value}) ->
    Value;
unwrap({error, Error}) ->
    erlang:error({unwrap_error, Error}).

-spec define(undefined | T, T) -> T.
define(undefined, V) ->
    V;
define(V, _Default) ->
    V.

-spec get_path(route_match(), [binding_value()]) ->
    path().
get_path(PathSpec, Params) when is_list(PathSpec) ->
    get_path(genlib:to_binary(PathSpec), Params);
get_path(Path, []) ->
    Path;
get_path(PathSpec, [Value | Rest]) ->
    [P1, P2] = split(PathSpec),
    P3       = get_next(P2),
    get_path(<<P1/binary, Value/binary, P3/binary>>, Rest).

split(PathSpec) ->
    case binary:split(PathSpec, <<":">>) of
        Res = [_, _] -> Res;
        [_]          -> erlang:error(param_mismatch)
    end.

get_next(PathSpec) ->
    case binary:split(PathSpec, <<"/">>) of
        [_, Next] -> <<"/", Next/binary>>;
        [_]       -> <<>>
    end.

-spec get_url(url(), path()) ->
    url().
get_url(BaseUrl, Path) ->
    <<BaseUrl/binary, Path/binary>>.

-spec get_url(url(), route_match(), [binding_value()]) ->
    url().
get_url(BaseUrl, PathSpec, Params) ->
    get_url(BaseUrl, get_path(PathSpec, Params)).

-define(MASKED_PAN_MAX_LENGTH, 4).

-spec get_last_pan_digits(binary()) ->
    binary().
get_last_pan_digits(MaskedPan) when byte_size(MaskedPan) > ?MASKED_PAN_MAX_LENGTH ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH});
get_last_pan_digits(MaskedPan) ->
    MaskedPan.

-spec parse_deadline
    (binary()) -> {ok, woody:deadline()} | {error, bad_deadline};
    (undefined) -> {ok, undefined}.
parse_deadline(undefined) ->
    {ok, undefined};
parse_deadline(DeadlineStr) ->
    Parsers = [
        fun try_parse_woody_default/1,
        fun try_parse_relative/1
    ],
    try_parse_deadline(DeadlineStr, Parsers).

%%
%% Internals
%%
try_parse_deadline(_DeadlineStr, []) ->
    {error, bad_deadline};
try_parse_deadline(DeadlineStr, [P | Parsers]) ->
    case P(DeadlineStr) of
        {ok, _Deadline} = Result ->
            Result;
        {error, bad_deadline} ->
            try_parse_deadline(DeadlineStr, Parsers)
    end.
try_parse_woody_default(DeadlineStr) ->
    try
        Deadline = woody_deadline:from_binary(to_universal_time(DeadlineStr)),
        NewDeadline = clamp_max_request_deadline(woody_deadline:to_timeout(Deadline)),
        {ok, woody_deadline:from_timeout(NewDeadline)}
    catch
        error:{bad_deadline, _Reason} ->
            {error, bad_deadline};
        error:{badmatch, _} ->
            {error, bad_deadline};
        error:deadline_reached ->
            {error, bad_deadline}
    end.
try_parse_relative(DeadlineStr) ->
    %% deadline string like '1ms', '30m', '2.6h' etc
    case re:split(DeadlineStr, <<"^(\\d+\\.\\d+|\\d+)([a-z]+)$">>) of
        [<<>>, NumberStr, Unit, <<>>] ->
            Number = genlib:to_float(NumberStr),
            try_parse_relative(Number, Unit);
        _Other ->
            {error, bad_deadline}
    end.
try_parse_relative(Number, Unit) ->
    case unit_factor(Unit) of
        {ok, Factor} ->
            Timeout = erlang:round(Number * Factor),
            {ok, woody_deadline:from_timeout(clamp_max_request_deadline(Timeout))};
        {error, _Reason} ->
            {error, bad_deadline}
    end.
unit_factor(<<"ms">>) ->
    {ok, 1};
unit_factor(<<"s">>) ->
    {ok, 1000};
unit_factor(<<"m">>) ->
    {ok, 1000 * 60};
unit_factor(_Other) ->
    {error, unknown_unit}.

-define(MAX_REQUEST_DEADLINE_TIME, timer:minutes(1)). % 1 min

clamp_max_request_deadline(Value) when is_integer(Value)->
    MaxDeadline = genlib_app:env(wapi, max_request_deadline, ?MAX_REQUEST_DEADLINE_TIME),
    case Value > MaxDeadline of
        true ->
            MaxDeadline;
        false ->
            Value
    end.

-spec get_unique_id() -> binary().
get_unique_id() ->
    ff_id:generate_snowflake_id().
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec to_universal_time_test() -> _.
to_universal_time_test() ->
    ?assertEqual(<<"2017-04-19T13:56:07Z">>, to_universal_time(<<"2017-04-19T13:56:07Z">>)),
    ?assertEqual(<<"2017-04-19T13:56:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53Z">>)),
    ?assertEqual(<<"2017-04-19T10:36:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53+03:20">>)),
    ?assertEqual(<<"2017-04-19T17:16:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53-03:20">>)).

-spec redact_test() -> _.
redact_test() ->
    P1 = <<"^\\+\\d(\\d{1,10}?)\\d{2,4}$">>,
    ?assertEqual(<<"+7******3210">>, redact(<<"+79876543210">>, P1)),
    ?assertEqual(       <<"+1*11">>, redact(<<"+1111">>, P1)).

-spec get_path_test() -> _.
get_path_test() ->
    ?assertEqual(<<"/wallet/v0/deposits/11/events/42">>, get_path(
        <<"/wallet/v0/deposits/:depositID/events/:eventID">>, [<<"11">>, <<"42">>]
    )),
    ?assertEqual(<<"/wallet/v0/deposits/11/events/42">>, get_path(
        "/wallet/v0/deposits/:depositID/events/:eventID", [<<"11">>, <<"42">>]
    )),
    ?assertError(param_mismatch, get_path(
        "/wallet/v0/deposits/:depositID/events/:eventID", [<<"11">>, <<"42">>, <<"0">>]
    )).

-spec mask_test() -> _.
mask_test() ->
    ?assertEqual(<<"Хуй">>, mask(leading, 0, $*, <<"Хуй">>)),
    ?assertEqual(<<"*уй">>, mask(leading, 1, $*, <<"Хуй">>)),
    ?assertEqual(<<"**й">>, mask(leading, 2, $*, <<"Хуй">>)),
    ?assertEqual(<<"***">>, mask(leading, 3, $*, <<"Хуй">>)),
    ?assertEqual(<<"Хуй">>, mask(trailing, 0, $*, <<"Хуй">>)),
    ?assertEqual(<<"Ху*">>, mask(trailing, 1, $*, <<"Хуй">>)),
    ?assertEqual(<<"Х**">>, mask(trailing, 2, $*, <<"Хуй">>)),
    ?assertEqual(<<"***">>, mask(trailing, 3, $*, <<"Хуй">>)).

-spec mask_and_keep_test() -> _.
mask_and_keep_test() ->
    ?assertEqual(<<"***">>, mask_and_keep(leading, 0, $*, <<"Хуй">>)),
    ?assertEqual(<<"Х**">>, mask_and_keep(leading, 1, $*, <<"Хуй">>)),
    ?assertEqual(<<"Ху*">>, mask_and_keep(leading, 2, $*, <<"Хуй">>)),
    ?assertEqual(<<"Хуй">>, mask_and_keep(leading, 3, $*, <<"Хуй">>)),
    ?assertEqual(<<"***">>, mask_and_keep(trailing, 0, $*, <<"Хуй">>)),
    ?assertEqual(<<"**й">>, mask_and_keep(trailing, 1, $*, <<"Хуй">>)),
    ?assertEqual(<<"*уй">>, mask_and_keep(trailing, 2, $*, <<"Хуй">>)),
    ?assertEqual(<<"Хуй">>, mask_and_keep(trailing, 3, $*, <<"Хуй">>)).

-spec parse_deadline_test() -> _.
parse_deadline_test() ->
    Deadline = woody_deadline:from_timeout(3000),
    BinDeadline = woody_deadline:to_binary(Deadline),
    {ok, {_, _}} = parse_deadline(BinDeadline),
    ?assertEqual({error, bad_deadline}, parse_deadline(<<"2017-04-19T13:56:07.53Z">>)),
    {ok, {_, _}} = parse_deadline(<<"15s">>),
    {ok, {_, _}} = parse_deadline(<<"15m">>),
    {error, bad_deadline} = parse_deadline(<<"15h">>).

-endif.
