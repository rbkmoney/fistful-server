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
    scoper:scope(fistful, #{function => Func},
        fun() ->
            ok = ff_woody_ctx:set(Context),
            try
                Opts = genlib_app:env(eventsinks, 'wallet', #{}),
                NS = maps:get(namespace, Opts, ff_wallet_machine:get_ns()),
                Client = ff_woody_client:get_service_client(eventsink),
                handle_function_(Func, Args, {NS, Client, Context}, Opts)
            after
                ff_woody_ctx:unset()
            end
        end
    ).

%%
%% Internals
%%

handle_function_('GetEvents', [#'evsink_EventRange'{'after' = After, limit = Limit}],
    {NS, Client, Context}, #{schema := Schema}) ->
    {ok, Events} = machinery_mg_eventsink:get_events(NS, After, Limit,
        #{client => {Client, Context}, schema => Schema}),
    publish_events(Events);
handle_function_('GetLastEventID', _Params, {NS, Client, Context}, #{schema := Schema}) ->
    case machinery_mg_eventsink:get_last_event_id(NS,
        #{client => {Client, Context}, schema => Schema}) of
        {ok, ID} ->
            ID;
        {error, no_last_event} ->
            throw(#'evsink_NoLastEvent'{})
    end.

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

publish_event({ID, _Ns, SourceID, {_EventID, Dt, Payload}}) ->
    #'wlt_SinkEvent'{
        'sequence'      = marshal(event_id, ID),
        'created_at'    = marshal(timestamp, Dt),
        'source'        = marshal(id, SourceID),
        'payload'       = marshal({list, event}, Payload)
    }.

%%

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

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
            error(badarg, {timestamp, V, Error})
    end;
marshal(string, V) when is_binary(V) ->
    V;
marshal(integer, V) when is_integer(V) ->
    V;
marshal(_, Other) ->
    Other.

