-module(ff_ct_provider_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("damsel/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function('ProcessWithdrawal', [Withdrawal, InternalState, Options], _Context, _Opts) ->
    DWithdrawal = decode_withdrawal(Withdrawal),
    DState = decode_state(InternalState),
    DOptions = decode_options(Options),
    {ok, Intent, NewState} = ff_ct_provider:process_withdrawal(DWithdrawal, DState, DOptions),
    {ok, #wthadpt_ProcessResult{
        intent = encode_intent(Intent),
        next_state = encode_state(NewState)
    }};
handle_function('GetQuote', [QuoteParams, Options], _Context, _Opts) ->
    Params = decode_quote_params(QuoteParams),
    DOptions = decode_options(Options),
    case ff_ct_provider:get_quote(Params, DOptions) of
        {ok, Quote} ->
            {ok, encode_quote(Quote)};
        {exception, not_enough_money} ->
            {exception, #wthadpt_GetQuoteFailure{failure = {
                limit_exceeded,
                {value_above_max_limit, #wthadpt_GetQuoteFailure{}}
            }}}
    end.

%%
%% Internals
%%

decode_withdrawal(#wthadpt_Withdrawal{
    id = Id,
    body = Body,
    destination = Destination,
    sender = Sender,
    receiver = Receiver,
    quote = Quote
}) ->
    #{
        id => Id,
        body => Body,
        destination => Destination,
        sender => Sender,
        receiver => Receiver,
        quote => Quote
    }.

decode_quote_params(#wthadpt_GetQuoteParams{
    idempotency_id = IdempotencyID,
    currency_from = CurrencyFrom,
    currency_to = CurrencyTo,
    exchange_cash = Cash
}) ->
    #{
        idempotency_id => IdempotencyID,
        currency_from => CurrencyFrom,
        currency_to => CurrencyTo,
        exchange_cash => Cash
    }.

decode_options(Options) ->
    Options.

decode_state(State) ->
    State.

%%

encode_state(State) ->
    State.

encode_intent({finish, {success, TrxInfo}}) ->
    {finish, #wthadpt_FinishIntent{status = {success, #wthadpt_Success{trx_info = encode_trx(TrxInfo)}}}};
encode_intent({finish, {failure, Failure}}) ->
    {finish, #wthadpt_FinishIntent{status = {failure, encode_failure(Failure)}}};
encode_intent({sleep, Timer}) ->
    {sleep, #wthadpt_SleepIntent{timer = encode_timer(Timer)}}.

encode_trx(#{id := Id} = TrxInfo) ->
    Timestamp = maps:get(timestamp, TrxInfo, undefined),
    Extra = maps:get(extra, TrxInfo, #{}),
    #domain_TransactionInfo{id = Id, timestamp = Timestamp, extra = Extra}.

encode_failure(Failure) ->
    Failure.

encode_timer(Timer) ->
    Timer.

encode_quote(#{
    cash_from := CashFrom,
    cash_to := CashTo,
    created_at := CreatedAt,
    expires_on := ExpiresOn,
    quote_data := QuoteData
}) ->
    #wthadpt_Quote{
        cash_from = CashFrom,
        cash_to = CashTo,
        created_at = CreatedAt,
        expires_on = ExpiresOn,
        quote_data = QuoteData
    }.
