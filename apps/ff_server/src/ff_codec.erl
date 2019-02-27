-module(ff_codec).

-include_lib("fistful_proto/include/ff_proto_repairer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_account_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_msgpack_thrift.hrl").

-export([unmarshal/2]).
-export([unmarshal/3]).

-export([marshal/2]).
-export([marshal/3]).

%% Types

-type type_name() :: atom() | {list, atom()}.
-type codec() :: module().

-type encoded_value() :: encoded_value(any()).
-type encoded_value(T) :: T.

-type decoded_value() :: decoded_value(any()).
-type decoded_value(T) :: T.

-export_type([codec/0]).
-export_type([type_name/0]).
-export_type([encoded_value/0]).
-export_type([encoded_value/1]).
-export_type([decoded_value/0]).
-export_type([decoded_value/1]).

%% Callbacks

-callback unmarshal(type_name(), encoded_value()) ->
    decoded_value().
-callback marshal(type_name(), decoded_value()) ->
    encoded_value().

%% API

-spec unmarshal(codec(), type_name(), encoded_value()) ->
    decoded_value().
unmarshal(Codec, Type, Value) ->
    Codec:unmarshal(Type, Value).

-spec marshal(codec(), type_name(), decoded_value()) ->
    encoded_value().
marshal(Codec, Type, Value) ->
    Codec:marshal(Type, Value).

%% Generic codec

-spec marshal(type_name(), decoded_value()) ->
    encoded_value().
marshal(id, V) ->
    marshal(string, V);
marshal(event_id, V) ->
    marshal(integer, V);

marshal(account_change, {created, Account}) ->
    {created, marshal(account, Account)};
marshal(account, #{
    id                   := ID,
    identity             := IdentityID,
    currency             := CurrencyID,
    accounter_account_id := AAID
}) ->
    #'account_Account'{
        id = marshal(id, ID),
        identity = marshal(id, IdentityID),
        currency = marshal(currency_ref, CurrencyID),
        accounter_account_id = marshal(event_id, AAID)
    };

marshal(cash, {Amount, CurrencyRef}) ->
    #'Cash'{
        amount   = marshal(amount, Amount),
        currency = marshal(currency_ref, CurrencyRef)
    };
marshal(currency_ref, CurrencyID) when is_binary(CurrencyID) ->
    #'CurrencyRef'{
        symbolic_code = CurrencyID
    };
marshal(amount, V) ->
    marshal(integer, V);

marshal(timestamp, {{Date, Time}, USec} = V) ->
    case rfc3339:format({Date, Time, USec, 0}) of
        {ok, R} when is_binary(R) ->
            R;
        Error ->
            error({bad_timestamp, Error}, [timestamp, V])
    end;
marshal(string, V) when is_binary(V) ->
    V;
marshal(integer, V) when is_integer(V) ->
    V;

% Catch this up in thrift validation
marshal(_, Other) ->
    Other.

-spec unmarshal(type_name(), encoded_value()) ->
    decoded_value().
unmarshal(id, V) ->
    unmarshal(string, V);
unmarshal(event_id, V) ->
    unmarshal(integer, V);

unmarshal(complex_action, #ff_repairer_ComplexAction{
    timer = TimerAction,
    remove = RemoveAction
}) ->
    unmarshal(timer_action, TimerAction) ++ unmarshal(remove_action, RemoveAction);
unmarshal(timer_action, undefined) ->
    [];
unmarshal(timer_action, {set_timer, SetTimer}) ->
    [{set_timer, unmarshal(set_timer_action, SetTimer)}];
unmarshal(timer_action, {unset_timer, #ff_repairer_UnsetTimerAction{}}) ->
    [unset_timer];
unmarshal(remove_action, undefined) ->
    [];
unmarshal(remove_action, #ff_repairer_RemoveAction{}) ->
    [remove];

unmarshal(set_timer_action, {timeout, Timeout}) ->
    {timeout, unmarshal(integer, Timeout)};
unmarshal(set_timer_action, {deadline, Deadline}) ->
    {deadline, unmarshal(timestamp, Deadline)};

unmarshal(account_change, {created, Account}) ->
    {created, unmarshal(account, Account)};
unmarshal(account, #'account_Account'{
    id = ID,
    identity = IdentityID,
    currency = CurrencyRef,
    accounter_account_id = AAID
}) ->
    #{
        id => unmarshal(id, ID),
        identity => unmarshal(id, IdentityID),
        currency => unmarshal(currency_ref, CurrencyRef),
        accounter_account_id => unmarshal(accounter_account_id, AAID)
    };
unmarshal(accounter_account_id, V) ->
    unmarshal(integer, V);

unmarshal(cash, #'Cash'{
    amount   = Amount,
    currency = CurrencyRef
}) ->
    {unmarshal(amount, Amount), unmarshal(currency_ref, CurrencyRef)};
unmarshal(currency_ref, #{
    symbolic_code := SymbolicCode
}) ->
    #'CurrencyRef'{
        symbolic_code = unmarshal(string, SymbolicCode)
    };
unmarshal(currency_ref, #'CurrencyRef'{
    symbolic_code = SymbolicCode
}) ->
    unmarshal(string, SymbolicCode);
unmarshal(amount, V) ->
    unmarshal(integer, V);

unmarshal(timestamp, Timestamp) when is_binary(Timestamp) ->
    parse_timestamp(Timestamp);
unmarshal(string, V) when is_binary(V) ->
    V;
unmarshal(integer, V) when is_integer(V) ->
    V.

%% Suppress dialyzer warning until rfc3339 spec will be fixed.
%% see https://github.com/talentdeficit/rfc3339/pull/5
-dialyzer([{nowarn_function, [parse_timestamp/1]}, no_match]).
-spec parse_timestamp(binary()) ->
    machinery:timestamp().
parse_timestamp(Bin) ->
    case rfc3339:parse(Bin) of
        {ok, {_Date, _Time, _Usec, TZ}} when TZ =/= 0 andalso TZ =/= undefined ->
            erlang:error({bad_deadline, not_utc}, [Bin]);
        {ok, {Date, Time, undefined, _TZ}} ->
            {to_calendar_datetime(Date, Time), 0};
        {ok, {Date, Time, Usec, _TZ}} ->
            {to_calendar_datetime(Date, Time), Usec div 1000};
        {error, Error} ->
            erlang:error({bad_timestamp, Error}, [Bin])
    end.

to_calendar_datetime(Date, Time = {H, _, S}) when H =:= 24 orelse S =:= 60 ->
    %% Type specifications for hours and seconds differ in calendar and rfc3339,
    %% so make a proper calendar:datetime() here.
    Sec = calendar:datetime_to_gregorian_seconds({Date, Time}),
    calendar:gregorian_seconds_to_datetime(Sec);
to_calendar_datetime(Date, Time) ->
    {Date, Time}.
