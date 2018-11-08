-module(ff_withdrawal_eventsink_handler).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-behaviour(ff_eventsink_publisher).

-export([publish_events/1]).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_cashflow_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type event() :: ff_eventsink_publisher:event(ff_withdrawal:event()).
-type sinkevent() :: ff_eventsink_publisher:sinkevent(ff_proto_withdrawal_thrift:'SinkEvent'()).


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
    scoper:scope(withdrawal_eventsink, #{function => Func},
        fun() ->
            ok = ff_woody_ctx:set(Context),
            try
                ff_eventsink_handler:handle_function(
                    Func, Args, Context, Opts#{
                        handler => ff_withdrawal_eventsink_handler
                    }
                )
            after
                ff_woody_ctx:unset()
            end
        end
    ).

%%
%% Internals
%%

-spec publish_events(list(event())) ->
    list(sinkevent()).

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

-spec publish_event(event()) ->
    sinkevent().

publish_event(#{
    id          := ID,
    source_id   := SourceID,
    event       := {
        EventID,
        Dt,
        {ev, EventDt, Payload}
    }
}) ->
    #'wthd_SinkEvent'{
        'id'            = ff_eventsink_handler:marshal(event_id, ID),
        'created_at'    = ff_eventsink_handler:marshal(timestamp, Dt),
        'source'        = ff_eventsink_handler:marshal(id, SourceID),
        'payload'       = #'wthd_Event'{
            'sequence'   = ff_eventsink_handler:marshal(event_id, EventID),
            'occured_at' = ff_eventsink_handler:marshal(timestamp, EventDt),
            'changes'    = [marshal(event, Payload)]
        }
    }.
%%

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(event, {created, Withdrawal}) ->
    {created, marshal(withdrawal, Withdrawal)};
marshal(event, {status_changed, WithdrawalStatus}) ->
    {status_changed, marshal(withdrawal_status_changed, WithdrawalStatus)};
marshal(event, {p_transfer, TransferChange}) ->
    {transfer, marshal(postings_transfer_change, TransferChange)};
marshal(event, {session_started, SessionID}) ->
    marshal(session, ?to_session_event(SessionID, started));
marshal(event, {session_finished, SessionID}) ->
    marshal(session, ?to_session_event(SessionID, finished));
marshal(session, {session, SessionChange}) ->
    {session, marshal(withdrawal_session_change, SessionChange)};
marshal(event, {route_changed, Route}) ->
    {route, marshal(withdrawal_route_changed, Route)};

marshal(withdrawal, #{
        body := {Amount, SymCode},
        params := Params
}) ->
    WalletID = maps:get(wallet_id, Params),
    DestinationID = maps:get(destination_id, Params),
    #'wthd_Withdrawal'{
        body = marshal(cash, ?transaction_body_to_cash(Amount, SymCode)),
        source = ff_eventsink_handler:marshal(id, WalletID),
        destination = ff_eventsink_handler:marshal(id, DestinationID)
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

marshal(postings_transfer_change, {created, Transfer}) ->
    {created, marshal(transfer, Transfer)}; % not ff_transfer :) see thrift
marshal(postings_transfer_change, {status_changed, TransferStatus}) ->
    {status_changed, marshal(transfer_status, TransferStatus)};
marshal(transfer, #{
        final_cash_flow := Cashflow
}) ->
    #'wthd_Transfer'{
        cashflow = marshal(final_cash_flow, Cashflow)
    };
marshal(final_cash_flow, #{
        postings := Postings
}) ->
    #'cashflow_FinalCashFlow'{
        postings = marshal({list, postings}, Postings)
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
        details     = ff_eventsink_handler:marshal(string, Details)
    };
marshal(final_cash_flow_account, #{
        account_type   := AccountType,
        account_id     := AccountID
}) ->
    #'cashflow_FinalCashFlowAccount'{
        account_type   = marshal(account_type, AccountType),
        account_id     = ff_eventsink_handler:marshal(id, AccountID)
    };

marshal(account_type, CashflowAccount) ->
    % Mapped to thrift type WalletCashFlowAccount as is
    CashflowAccount;
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
        id      = ff_eventsink_handler:marshal(id, SessionID),
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
        id = ff_eventsink_handler:marshal(id, ProviderID)
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
        symbolic_code    = ff_eventsink_handler:marshal(string, SymbolicCode)
    };
marshal(amount, V) ->
    ff_eventsink_handler:marshal(integer, V);

% Catch this up in thrift validation
marshal(_, Other) ->
    Other.
