%%%
%%% Publisher - he comes to publish all eventsinks
%%%

-module(ff_eventsink_publisher).

-include_lib("fistful_proto/include/ff_proto_account_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_msgpack_thrift.hrl").
%% API

-type event(T) :: machinery_mg_eventsink:evsink_event(
    ff_machine:timestamped_event(T)
).

-type sinkevent(T) :: T.
-type options() :: #{publisher := module()}.

%% Behaviour definition

-export_type([event/1]).
-export_type([sinkevent/1]).
-export_type([options/0]).

-callback publish_events(list(event(_))) ->
    list(sinkevent(_)).

%% API

-export([publish_events/2]).
-export([marshal/2]).

-spec publish_events(list(event(_)), options()) ->
    {ok, list(sinkevent(_))}.

publish_events(Events, Opts) ->
    {ok, handler_publish_events(Events, Opts)}.

get_publicher(#{publisher := Publisher}) ->
    Publisher.

%% Publisher calls

handler_publish_events(Events, Opts) ->
    Publisher = get_publicher(Opts),
    Publisher:publish_events(Events).

-spec marshal(atom() | tuple(), term()) ->
    any().

marshal(id, V) ->
    marshal(string, V);
marshal(event_id, V) ->
    marshal(integer, V);

marshal(failure, Params = #{
    code := Code
}) ->
    Reason = maps:get(reason, Params, undefined),
    SubFailure = maps:get(sub, Params, undefined),
    #'Failure'
    {
        code = marshal(string, Code),
        reason = marshal(string, Reason),
        sub = marshal(sub_failure, SubFailure)
    };
marshal(sub_failure, Params = #{
    code := Code
}) ->
    SubFailure = maps:get(sub, Params, undefined),
    #'SubFailure'
    {
        code = marshal(string, Code),
        sub = marshal(sub_failure, SubFailure)
    };

marshal(transaction_info, Params = #{
    id := TransactionID,
    extra := Extra
}) ->
    Timestamp = maps:get(timestamp, Params, undefined),
    #'TransactionInfo'
    {
        id = marshal(id, TransactionID),
        timestamp = marshal(timestamp, Timestamp),
        extra = Extra
    };

marshal(account_change, {created, Account}) ->
    {created, marshal(account, Account)};
marshal(account, #{
    id                   := ID,
    identity             := IdentityID,
    currency             := Currency,
    accounter_account_id := AAID
}) ->
    #'account_Account'{
        id = marshal(id, ID),
        identity = marshal(id, IdentityID),
        currency = marshal(currency_ref, #{symbolic_code => Currency}),
        accounter_account_id = marshal(event_id, AAID)
    };

marshal(cash, #{
        amount   := Amount,
        currency := Currency
}) ->
    #'Cash'{
        amount   = marshal(amount, Amount),
        currency = marshal(currency_ref, Currency)
    };
marshal(currency_ref, #{
        symbolic_code   := SymbolicCode
}) ->
    #'CurrencyRef'{
        symbolic_code    = marshal(string, SymbolicCode)
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

marshal(msgpack, V) when is_integer(V) ->
    wrap(V);

% Catch this up in thrift validation
marshal(_, Other) ->
    Other.

wrap(nil) ->
    {nl, #msgp_Nil{}};
wrap(V) when is_boolean(V) ->
    {b, V};
wrap(V) when is_integer(V) ->
    {i, V};
wrap(V) when is_float(V) ->
    V;
wrap(V) when is_binary(V) ->
    % Assuming well-formed UTF-8 bytestring.
    {str, V};
wrap({binary, V}) when is_binary(V) ->
    {bin, V};
wrap(V) when is_list(V) ->
    {arr, V};
wrap(V) when is_map(V) ->
    {obj, V}.

