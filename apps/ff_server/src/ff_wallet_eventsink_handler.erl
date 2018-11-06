-module(ff_wallet_eventsink_handler).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Context, Opts) ->
    scoper:scope(wallet_eventsink, #{function => Func},
        fun() ->
            ok = ff_woody_ctx:set(Context),
            try
                handle_function_(Func, Args, Context, Opts)
            after
                ff_woody_ctx:unset()
            end
        end
    ).

%%
%% Internals
%%

handle_function_(
    'GetEvents', [#'evsink_EventRange'{'after' = After, limit = Limit}],
    Context, #{schema := Schema, client := Client, ns := NS}
) ->
    {ok, Events} = machinery_mg_eventsink:get_events(NS, After, Limit,
        #{client => {Client, Context}, schema => Schema}),
    {ok, publish_events(Events)};
handle_function_(
    'GetLastEventID', _Params, Context,
    #{schema := Schema, client := Client, ns := NS}
) ->
    case machinery_mg_eventsink:get_last_event_id(NS,
        #{client => {Client, Context}, schema => Schema}) of
        {ok, _} = Result ->
            Result;
        {error, no_last_event} ->
            woody_error:raise(business, #'evsink_NoLastEvent'{})
    end.

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

-spec publish_event(machinery_mg_eventsink:evsink_event(
    ff_machine:timestamped_event(ff_wallet:event())
)) -> ff_proto_wallet_thrift:'SinkEvent'().

publish_event(#{
    id          := ID,
    source_id   := SourceID,
    event       := {
        EventID,
        Dt,
        {ev, EventDt, Payload}
    }
}) ->
    #'wlt_SinkEvent'{
        'id'            = marshal(event_id, ID),
        'created_at'    = marshal(timestamp, Dt),
        'source'        = marshal(id, SourceID),
        'payload'       = #'wlt_Event'{
            'sequence'   = marshal(event_id, EventID),
            'occured_at' = marshal(timestamp, EventDt),
            'changes'    = [marshal(event, Payload)]
        }
    }.

%%

marshal(id, V) ->
    marshal(string, V);
marshal(event_id, V) ->
    marshal(integer, V);
marshal(event, {created, Wallet}) ->
    {created, marshal(wallet, Wallet)};
marshal(event, {account, AccountChange}) ->
    {account, marshal(account_change, AccountChange)};

marshal(wallet, Wallet) ->
    Name = maps:get(name, Wallet, undefined),
    #'wlt_Wallet'{
        name = marshal(string, Name)
    };

marshal(account_change, {created, Account}) ->
    {created, marshal(account, Account)};
marshal(account, #{
        identity := Identity,
        currency := Currency
}) ->
    #'wlt_Account'{
        identity = ff_identity_eventsink_handler:marshal(identity, Identity),
        currency = marshal(currency_ref, #{symbolic_code => Currency})
    };
marshal(currency_ref, #{
        symbolic_code   := SymbolicCode
}) ->
    #'CurrencyRef'{
        symbolic_code    = marshal(string, SymbolicCode)
    };

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

