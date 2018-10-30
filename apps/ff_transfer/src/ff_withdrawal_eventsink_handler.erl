-module(ff_withdrawal_eventsink_handler).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_cashflow_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

%% Data transform

-define(transaction_body_to_cash(Amount, SymCode),
    #{amount => Amount, currency => #{symbolic_code => SymCode}}).

final_account_to_final_cash_flow_account(#{
    account := #{id := AccountID},
    type := AccountType}) ->
    #{account_type => AccountType, account_id => AccountID}.

-define(to_session_event(SessionID, Payload),
    {session, #{id => SessionID, payload => Payload}}).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Context, Opts) ->
    scoper:scope(ff_transfer, #{function => Func},
        fun() ->
            ok = ff_woody_ctx:set(Context),
            try
                NS = get_ns(ff_withdrawal:get_ns()),
                Client = ff_woody_client:get_service_client(eventsink),
                handle_function_(Func, Args, {NS, Client, Context}, Opts)
            after
                ff_woody_ctx:unset()
            end
        end
    ).

get_ns(DefNS) ->
    RouteList = genlib_app:env(ff_server, eventsink, []),
    case lists:keyfind('withdrawal', 1, RouteList) of
        false ->
            DefNS;
        Opts ->
            maps:get(namespace, Opts, DefNS)
    end.

%%
%% Internals
%%

handle_function_('GetEvents', [#'evsink_EventRange'{'after' = After, limit = Limit}],
    {NS, Client, Context}, #{schema := Schema}) ->
    {ok, Events} = machinery_mg_eventsink:get_events(NS, After, Limit,
        #{client => {Client, Context}, schema => Schema}),
    {ok, publish_events(Events)};
handle_function_('GetLastEventID', _Params, {NS, Client, Context}, #{schema := Schema}) ->
    case machinery_mg_eventsink:get_last_event_id(NS,
        #{client => {Client, Context}, schema => Schema}) of
        {ok, _} = Result ->
            Result;
        {error, no_last_event} ->
            woody_error:raise(business, #'evsink_NoLastEvent'{})
    end.

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

publish_event({ID, _Ns, SourceID, {EventID, Dt, {ev, _, Payload}}}) ->
    #'wthd_SinkEvent'{
        'sequence'      = marshal(event_id, ID),
        'created_at'    = marshal(timestamp, Dt),
        'source'        = marshal(id, SourceID),
        'payload'        = #'wthd_Event'{
            'id'         = marshal(event_id, EventID),
            'occured_at' = marshal(timestamp, Dt),
            'changes'    = [marshal(event, Payload)]
        }
    }.

%%

marshal(T, V) when is_list(V) ->
    [marshal(T, E) || E <- V];

marshal(id, V) ->
    marshal(string, V);
marshal(event_id, V) ->
    marshal(integer, V);
marshal(event, {created, Withdrawal}) ->
    {created, marshal(withdrawal, Withdrawal)};
marshal(event, {status_changed, WithdrawalStatus}) ->
    {status_changed, marshal(withdrawal_status_changed, WithdrawalStatus)};
marshal(event, {p_transfer, TransferChange}) ->
    {transfer, marshal(withdrawal_transfer_change, TransferChange)};
marshal(event, {session_started, SessionID}) ->
    marshal(session, ?to_session_event(SessionID, started));
marshal(event, {session_finished, SessionID}) ->
    marshal(session, ?to_session_event(SessionID, finished));
marshal(session, {session, SessionChange}) ->
    {session, marshal(withdrawal_session_change, SessionChange)};
marshal(event, {route_changed, Route}) ->
    {route, marshal(withdrawal_route_changed, Route)};

marshal(withdrawal, Withdrawal = #{
        body := {Amount, SymCode}
    }) ->
    WalletID = maps:get(wallet_id, Withdrawal, undefined),
    DestinationID = maps:get(destination_id, Withdrawal, undefined),
    #'wthd_Withdrawal'{
        body = marshal(cash, ?transaction_body_to_cash(Amount, SymCode)),
        source = marshal(id, WalletID),
        destination = marshal(id, DestinationID)
};

marshal(withdrawal_status_changed, pending) ->
    {pending, #'wthd_WithdrawalPending'{}};
marshal(withdrawal_status_changed, succeeded) ->
    {succeeded, #'wthd_WithdrawalSucceeded'{}};
% marshal(withdrawal_status_changed, {failed, #{
%         failure := Failure
%     }}) ->
%     {failed, #'wthd_WithdrawalFailed'{failure = marshal(failure, Failure)}};
marshal(withdrawal_status_changed, {failed, _}) ->
    {failed, #'wthd_WithdrawalFailed'{failure = marshal(failure, dummy)}};
marshal(failure, _) ->
    #'wthd_Failure'{};

marshal(withdrawal_transfer_change, {created, Transfer}) ->
    {created, marshal(transfer, Transfer)};
marshal(withdrawal_transfer_change, {status_changed, TransferStatus}) ->
    {status_changed, marshal(transfer_status, TransferStatus)};
marshal(transfer, #{
        final_cash_flow := Cashflow
    }) ->
    #'wthd_Transfer'{
        cashflow = marshal(cashflow, Cashflow)
};
marshal(cashflow, #{
        postings := Postings
    }) ->
    #'cashflow_FinalCashFlow'{
        postings = marshal(postings, Postings)
};
marshal(postings, Postings = #{
        sender := Source,
        receiver := Destination,
        volume := {Amount, SymCode}
    }) ->
    Details = maps:get(details, Postings, undefined),
    #'cashflow_FinalCashFlowPosting'{
        source      = marshal(final_cash_flow_account,
            final_account_to_final_cash_flow_account(Source)),
        destination = marshal(final_cash_flow_account,
            final_account_to_final_cash_flow_account(Destination)),
        volume      = marshal(cash, ?transaction_body_to_cash(Amount, SymCode)),
        details     = marshal(string, Details)
};
marshal(final_cash_flow_account, #{
        account_type   := AccountType,
        account_id     := AccountID
    }) ->
    #'cashflow_FinalCashFlowAccount'{
        account_type   = marshal(account_type, AccountType),
        account_id     = marshal(id, AccountID)
};

marshal(account_type, {provider, Provider}) ->
    {provider, marshal(provider, Provider)};
marshal(account_type, {system, System}) ->
    {system, marshal(system, System)};
marshal(account_type, {wallet, Wallet}) ->
    {wallet, marshal(wallet, Wallet)};
marshal(provider, settlement) ->
    settlement;
marshal(system, settlement) ->
    settlement;
% How to convert here?
marshal(wallet, sender_source) ->
    payout;
marshal(wallet, sender_settlement) ->
    settlement;
marshal(wallet, receiver_settlement) ->
    settlement;
marshal(wallet, receiver_destination) ->
    payout;
marshal(transfer_status, created) ->
    {created, #'wthd_TransferCreated'{}};
marshal(transfer_status, prepared) ->
    {prepared, #'wthd_TransferPrepared'{}};
marshal(transfer_status, committed) ->
    {committed, #'wthd_TransferCommitted'{}};
marshal(transfer_status, cancelled) ->
    {cancelled, #'wthd_TransferCancelled'{}};

marshal(withdrawal_session_change, #{
        id      := SessionID,
        payload := Payload
    }) ->
    #'wthd_SessionChange'{
        id      = marshal(id, SessionID),
        payload = marshal(withdrawal_session_payload, Payload)
};
marshal(withdrawal_session_payload, started) ->
    {started, #'wthd_SessionStarted'{}};
marshal(withdrawal_session_payload, finished) ->
    {finished, #'wthd_SessionFinished'{}};

marshal(withdrawal_route_changed, #{
        provider_id := ProviderID
    }) ->
    #'wthd_RouteChange'{
        id = marshal(id, ProviderID)
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
