-module(ff_wallet_eventsink_handler).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export([publish_events/1]).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

-type event() :: machinery_mg_eventsink:evsink_event(
    ff_machine:timestamped_event(ff_wallet:event())
).

-type sinkevent() :: ff_proto_wallet_thrift:'SinkEvent'().

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
                ff_eventsink_handler:handle_function(
                    Func, Args, Context, Opts,
                    ff_wallet_eventsink_handler
                )
            after
                ff_woody_ctx:unset()
            end
        end
    ).

-spec publish_events(list(event())) ->
    list(sinkevent()).

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

-spec publish_event(event()) -> sinkevent().

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
        'id'            = ff_eventsink_handler:marshal(event_id, ID),
        'created_at'    = ff_eventsink_handler:marshal(timestamp, Dt),
        'source'        = ff_eventsink_handler:marshal(id, SourceID),
        'payload'       = #'wlt_Event'{
            'sequence'   = ff_eventsink_handler:marshal(event_id, EventID),
            'occured_at' = ff_eventsink_handler:marshal(timestamp, EventDt),
            'changes'    = [marshal(event, Payload)]
        }
    }.

%%
%% Internals
%%

marshal(event, {created, Wallet}) ->
    {created, marshal(wallet, Wallet)};
marshal(event, {account, AccountChange}) ->
    {account, marshal(account_change, AccountChange)};

marshal(wallet, Wallet) ->
    Name = maps:get(name, Wallet, undefined),
    #'wlt_Wallet'{
        name = ff_eventsink_handler:marshal(string, Name)
    };

marshal(account_change, {created, Account}) ->
    {created, marshal(account, Account)};
marshal(account, #{
        id                   := ID,
        identity             := Identity,
        currency             := Currency,
        accounter_account_id := AAID
}) ->
    #'account_Account'{
        id = ff_eventsink_handler:marshal(id, ID),
        identity = ff_identity_eventsink_handler:marshal(identity, Identity),
        currency = marshal(currency_ref, #{symbolic_code => Currency}),
        accounter_account_id = ff_eventsink_handler:marshal(event_id, AAID)
    };
marshal(currency_ref, #{
        symbolic_code   := SymbolicCode
}) ->
    #'CurrencyRef'{
        symbolic_code    = ff_eventsink_handler:marshal(string, SymbolicCode)
    };

% Catch this up in thrift validation
marshal(_, Other) ->
    Other.

