-module(ff_withdrawal_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

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
handle_function_('Create', [Params], Context, Opts) ->
    ID = Params#wthd_WithdrawalParams.id,
    case ff_withdrawal:create(ID,
        decode(withdrawal_params, Params),
        decode(context, Params#wthd_WithdrawalParams.context))
    of
        ok ->
            handle_function_('Get', [ID], Context, Opts);
        {error, {source, notfound}} ->
            woody_error:raise(business, #fistful_SourceNotFound{});
        {error, {destination, notfound}} ->
            woody_error:raise(business, #fistful_DestinationNotFound{});
        {error, {destination, unauthorized}} ->
            woody_error:raise(business, #fistful_DestinationUnauthorized{});
        {error, {wallet, notfound}} ->
            woody_error:raise(business, #fistful_WalletNotFound{});
        {error, {provider, notfound}} ->
            woody_error:raise(business, #fistful_ProviderNotFound{});
        {error, {terms, {invalid_withdrawal_currency, _, {wallet_currency, _}}}} ->
            woody_error:raise(business, #fistful_CurrencyInvalid{});
        {error, {terms, {terms_violation, {cash_range, _}}}} ->
            woody_error:raise(business, #fistful_CashRangeError{});
        {error, exists} ->
            woody_error:raise(business, #fistful_IDExists{});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('Get', [ID], _Context, _Opts) ->
    case ff_withdrawal:get_machine(ID) of
        {ok, Machine} ->
            {ok, encode(withdrawal, {ID, Machine})};
        {error, notfound} ->
            woody_error:raise(business, #fistful_DestinationNotFound{})
    end;
handle_function_('GetEvents', [WithdrawalID, RangeParams], _Context, _Opts) ->
    Range = decode(range, RangeParams),
    case ff_withdrawal:events(WithdrawalID, Range) of
        {ok, Events} ->
            {ok, encode(events, Events)};
        {error, notfound} ->
            woody_error:raise(business, #fistful_WithdrawalNotFound{})
    end.

encode(withdrawal, {ID, Machine}) ->
    Withdrawal = ff_withdrawal:get(Machine),
    #wthd_Withdrawal {
        source      = ff_withdrawal:wallet_id(Withdrawal),
        destination = ff_withdrawal:destination_id(Withdrawal),
        body        = encode(transaction_body, ff_withdrawal:body(Withdrawal)),
        external_id = ff_withdrawal:external_id(Withdrawal),
        id          = ID,
        status      = encode(status, ff_withdrawal:status(Withdrawal)),
        context     = encode(context, ff_transfer_machine:ctx(Machine))
    };
encode(transaction_body, {Amount, Currency}) ->
    #'Cash'{
        amount = Amount,
        currency = encode(currency, Currency)
    };
encode(events, Events) ->
    GenWithdrawalEvent = fun({ID, {ev, Timestamp, Ev}}) ->
        #wthd_WithdrawalEvent{
            sequence   = ff_withdrawal_eventsink_publisher:marshal(event_id, ID),
            occured_at = ff_withdrawal_eventsink_publisher:marshal(timestamp, Timestamp),
            change     = ff_withdrawal_eventsink_publisher:marshal(event, Ev)
        }
    end,
    [ GenWithdrawalEvent(Event) || Event <- Events];
encode(status, pending) ->
    {pending, #wthd_WithdrawalPending{}};
encode(status, succeeded) ->
    {succeeded, #wthd_WithdrawalSucceeded{}};
encode(status, {failed, {terms_violation, {cash_range, _Details}}}) ->
    {failed, #wthd_WithdrawalFailed{
        failure = #wthd_Failure{}
    }};
encode(status, _Failure = #domain_Failure{}) ->
    {failed, #wthd_WithdrawalFailed{
        failure = #wthd_Failure{}
    }};
encode(currency, Currency) ->
    #'CurrencyRef'{ symbolic_code = Currency};
encode(context, Ctx) ->
    ff_context:wrap(Ctx).

decode(withdrawal_params, Params) ->
    #{
        wallet_id      => Params#wthd_WithdrawalParams.source,
        destination_id => Params#wthd_WithdrawalParams.destination,
        body           => decode(transaction_body,Params#wthd_WithdrawalParams.body),
        external_id    => Params#wthd_WithdrawalParams.external_id
    };
decode(transaction_body, Params) ->
    {
        Params#'Cash'.amount,
        decode(currency, Params#'Cash'.currency)
    };
decode(currency, #'CurrencyRef'{ symbolic_code = Currency}) ->
    Currency;
decode(context, undefined) ->
    undefined;
decode(context, Ctx) ->
    ff_context:unwrap(Ctx);
decode(range, Range) ->
    Cursor = Range#evsink_EventRange.'after',
    Limit  = Range#evsink_EventRange.limit,
    {Cursor, Limit, forward}.


