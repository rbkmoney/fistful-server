-module(ff_withdrawal_eventsink_handler).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_cashflow_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
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
    {ok, Events} = machinery_eventsink:get_events(ff_withdrawal_session_machine:get_ns(),
        After, Limit, Schema),
    publish_events(Events);
handle_function_('GetLastEventID', _Params, _Context, _Opts) ->
    case machinery_eventsink:get_last_event_id(ff_withdrawal_session_machine:get_ns()) of
        {ok, ID} ->
            ID;
        {error, no_last_event} ->
            throw(#'evsink_NoLastEvent'{})
    end.

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

publish_event({ID, _Ns, SourceID, {_EventID, Dt, Payload}}) ->
    #'wthd_SinkEvent'{
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
marshal(event, {created, Withdrawal}) ->
    {created, marshal(withdrawal, Withdrawal)};
marshal(event, {status_changed, WithdrawalStatus}) ->
    {status_changed, marshal(withdrawal_status_changed, WithdrawalStatus)};
marshal(event, {transfer, TransferChange}) ->
    {transfer, marshal(withdrawal_transfer_change, TransferChange)};
marshal(event, {session, SessionChange}) ->
    {session, marshal(withdrawal_session_change, SessionChange)};

marshal(withdrawal, Withdrawal = #{
        body := Body
    }) ->
    WalletID = maps:get(source, Withdrawal, undefined),
    DestinationID = maps:get(destination, Withdrawal, undefined),
    #'wthd_Withdrawal'{
        body = marshal(cash, Body),
        source = marshal(id, WalletID),
        destination = marshal(id, DestinationID)
};

marshal(withdrawal_status_changed, {pending, _}) ->
    {pending, #'wthd_WithdrawalPending'{}};
marshal(withdrawal_status_changed, {succeeded, _}) ->
    {succeeded, #'wthd_WithdrawalSucceeded'{}};
marshal(withdrawal_status_changed, {failed, #{
        failure := Failure
    }}) ->
    {failed, #'wthd_WithdrawalFailed'{failure = marshal(failure, Failure)}};
marshal(failure, _) ->
    #'wthd_Failure'{};

marshal(withdrawal_transfer_change, {created, Transfer}) ->
    {created, marshal(transfer, Transfer)};
marshal(withdrawal_transfer_change, {status_changed, TransferStatus}) ->
    {status_changed, marshal(transfer_status, TransferStatus)};
marshal(transfer, #{
        cashflow := Cashflow
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
        source := Source,
        destination := Destination,
        volume := Volume
    }) ->
    Details = maps:get(details, Postings, undefined),
    #'cashflow_FinalCashFlowPosting'{
        source      = marshal(final_cash_flow_account, Source),
        destination = marshal(final_cash_flow_account, Destination),
        volume      = marshal(cash, Volume),
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
marshal(account_type, {merchant, Merchant}) ->
    {merchant, marshal(atom, Merchant)};
marshal(account_type, {provider, Provider}) ->
    {provider, marshal(atom, Provider)};
marshal(account_type, {system, System}) ->
    {system, marshal(atom, System)};
marshal(account_type, {external, External}) ->
    {external, marshal(atom, External)};
marshal(account_type, {wallet, Wallet}) ->
    {wallet, marshal(atom, Wallet)};
marshal(transfer_status, {created, _}) ->
    {created, #'wthd_TransferCreated'{}};
marshal(transfer_status, {prepared, _}) ->
    {prepared, #'wthd_TransferPrepared'{}};
marshal(transfer_status, {committed, _}) ->
    {committed, #'wthd_TransferCommitted'{}};
marshal(transfer_status, {cancelled, _}) ->
    {cancelled, #'wthd_TransferCancelled'{}};

marshal(withdrawal_session_change, #{
        id      := SessionID,
        payload := Payload
    }) ->
    #'wthd_SessionChange'{
        id      = marshal(id, SessionID),
        payload = marshal(withdrawal_session_payload, Payload)
};
marshal(transfer_status, {started, _}) ->
    {started, #'wthd_SessionStarted'{}};
marshal(transfer_status, {finished, _}) ->
    {finished, #'wthd_SessionFinished'{}};

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
