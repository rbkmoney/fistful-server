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
                handle_function_(Func, Args, Context, Opts)
            after
                ff_woody_ctx:unset()
            end
        end
    ).

%%
%% Internals
%%

handle_function_('GetEvents', [#'evsink_EventRange'{'after' = After, limit = Limit}],
    _Context, #{schema := Schema}) ->
    {ok, Events} = machinery_eventsink:get_events(ff_wallet_machine:get_ns(),
        After, Limit, Schema),
    publish_events(Events);
handle_function_('GetLastEventID', _Params, _Context, _Opts) ->
    case machinery_eventsink:get_last_event_id(ff_wallet_machine:get_ns()) of
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
        identity = marshal(identity, Identity),
        currency = marshal(currency_ref, Currency)
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
marshal(atom, V) when is_atom(V) ->
    atom_to_binary(V, utf8);
marshal(string, V) when is_binary(V) ->
    V;
marshal(integer, V) when is_integer(V) ->
    V;
marshal(_, Other) ->
    Other.

