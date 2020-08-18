-module(ff_withdrawal_handler).
-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(withdrawal, #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%
handle_function_('GetQuote', [MarshaledParams], _Opts) ->
    Params = ff_withdrawal_codec:unmarshal_quote_params(MarshaledParams),
    ok = scoper:add_meta(maps:with([wallet_id, destination_id, external_id], Params)),
    case ff_withdrawal:get_quote(Params) of
        {ok, Quote} ->
            Response = ff_withdrawal_codec:marshal(quote, Quote),
            {ok, Response};
        {error, {wallet, notfound}} ->
            woody_error:raise(business, #fistful_WalletNotFound{});
        {error, {destination, notfound}} ->
            woody_error:raise(business, #fistful_DestinationNotFound{});
        {error, {destination, unauthorized}} ->
            woody_error:raise(business, #fistful_DestinationUnauthorized{});
        {error, {terms, {terms_violation, {not_allowed_currency, {DomainCurrency, DomainAllowed}}}}} ->
            Currency = ff_dmsl_codec:unmarshal(currency_ref, DomainCurrency),
            Allowed = [ff_dmsl_codec:unmarshal(currency_ref, C) || C <- DomainAllowed],
            woody_error:raise(business, #fistful_ForbiddenOperationCurrency{
                currency = ff_codec:marshal(currency_ref, Currency),
                allowed_currencies = ff_codec:marshal({set, currency_ref}, Allowed)
            });
        {error, {terms, {terms_violation, {cash_range, {Cash, Range}}}}} ->
            woody_error:raise(business, #fistful_ForbiddenOperationAmount{
                amount = ff_codec:marshal(cash, Cash),
                allowed_range = ff_codec:marshal(cash_range, Range)
            });
        {error, {inconsistent_currency, {Withdrawal, Wallet, Destination}}} ->
            woody_error:raise(business, #wthd_InconsistentWithdrawalCurrency{
                withdrawal_currency = ff_codec:marshal(currency_ref, Withdrawal),
                destination_currency = ff_codec:marshal(currency_ref, Destination),
                wallet_currency = ff_codec:marshal(currency_ref, Wallet)
            });
        {error, {destination_resource, {bin_data, _}}} ->
            woody_error:raise(business, #wthd_NoDestinationResourceInfo{})
    end;
handle_function_('Create', [MarshaledParams, MarshaledContext], Opts) ->
    Params = ff_withdrawal_codec:unmarshal_withdrawal_params(MarshaledParams),
    Context = ff_withdrawal_codec:unmarshal(ctx, MarshaledContext),
    ok = scoper:add_meta(maps:with([id, wallet_id, destination_id, external_id], Params)),
    case ff_withdrawal_machine:create(Params, Context) of
        ok ->
            handle_function_('Get', [maps:get(id, Params), #'EventRange'{}], Opts);
        {error, exists} ->
            handle_function_('Get', [maps:get(id, Params), #'EventRange'{}], Opts);
        {error, {wallet, notfound}} ->
            woody_error:raise(business, #fistful_WalletNotFound{});
        {error, {destination, notfound}} ->
            woody_error:raise(business, #fistful_DestinationNotFound{});
        {error, {destination, unauthorized}} ->
            woody_error:raise(business, #fistful_DestinationUnauthorized{});
        {error, {terms, {terms_violation, {not_allowed_currency, {DomainCurrency, DomainAllowed}}}}} ->
            Currency = ff_dmsl_codec:unmarshal(currency_ref, DomainCurrency),
            Allowed = [ff_dmsl_codec:unmarshal(currency_ref, C) || C <- DomainAllowed],
            woody_error:raise(business, #fistful_ForbiddenOperationCurrency{
                currency = ff_codec:marshal(currency_ref, Currency),
                allowed_currencies = ff_codec:marshal({set, currency_ref}, Allowed)
            });
        {error, {terms, {terms_violation, {cash_range, {Cash, Range}}}}} ->
            woody_error:raise(business, #fistful_ForbiddenOperationAmount{
                amount = ff_codec:marshal(cash, Cash),
                allowed_range = ff_codec:marshal(cash_range, Range)
            });
        {error, {inconsistent_currency, {Withdrawal, Wallet, Destination}}} ->
            woody_error:raise(business, #wthd_InconsistentWithdrawalCurrency{
                withdrawal_currency = ff_codec:marshal(currency_ref, Withdrawal),
                destination_currency = ff_codec:marshal(currency_ref, Destination),
                wallet_currency = ff_codec:marshal(currency_ref, Wallet)
            });
        {error, {destination_resource, {bin_data, not_found}}} ->
            woody_error:raise(business, #wthd_NoDestinationResourceInfo{})
    end;
handle_function_('Get', [ID, EventRange], _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    case ff_withdrawal_machine:get(ID, ff_codec:unmarshal(event_range, EventRange)) of
        {ok, Machine} ->
            Withdrawal = ff_withdrawal_machine:withdrawal(Machine),
            Context = ff_withdrawal_machine:ctx(Machine),
            Response = ff_withdrawal_codec:marshal_withdrawal_state(Withdrawal, Context),
            {ok, Response};
        {error, {unknown_withdrawal, ID}} ->
            woody_error:raise(business, #fistful_WithdrawalNotFound{})
    end;
handle_function_('GetContext', [ID], _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    case ff_withdrawal_machine:get(ID, {undefined, 0}) of
        {ok, Machine} ->
            Context = ff_withdrawal_machine:ctx(Machine),
            {ok, ff_codec:marshal(context, Context)};
        {error, {unknown_withdrawal, ID}} ->
            woody_error:raise(business, #fistful_WithdrawalNotFound{})
    end;
handle_function_('GetEvents', [ID, EventRange], _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    case ff_withdrawal_machine:events(ID, ff_codec:unmarshal(event_range, EventRange)) of
        {ok, Events} ->
            {ok, lists:map(fun ff_withdrawal_codec:marshal_event/1, Events)};
        {error, {unknown_withdrawal, ID}} ->
            woody_error:raise(business, #fistful_WithdrawalNotFound{})
    end;
handle_function_('CreateAdjustment', [ID, MarshaledParams], _Opts) ->
    Params = ff_withdrawal_adjustment_codec:unmarshal(adjustment_params, MarshaledParams),
    AdjustmentID = maps:get(id, Params),
    ok = scoper:add_meta(genlib_map:compact(#{
        id => ID,
        adjustment_id => AdjustmentID,
        external_id => maps:get(external_id, Params, undefined)
    })),
    case ff_withdrawal_machine:start_adjustment(ID, Params) of
        ok ->
            {ok, Machine} = ff_withdrawal_machine:get(ID),
            Withdrawal = ff_withdrawal_machine:withdrawal(Machine),
            {ok, Adjustment} = ff_withdrawal:find_adjustment(AdjustmentID, Withdrawal),
            {ok, ff_withdrawal_adjustment_codec:marshal(adjustment_state, Adjustment)};
        {error, {unknown_withdrawal, ID}} ->
            woody_error:raise(business, #fistful_WithdrawalNotFound{});
        {error, {invalid_withdrawal_status, Status}} ->
            woody_error:raise(business, #wthd_InvalidWithdrawalStatus{
                withdrawal_status = ff_withdrawal_codec:marshal(status, Status)
            });
        {error, {invalid_status_change, {unavailable_status, Status}}} ->
            woody_error:raise(business, #wthd_ForbiddenStatusChange{
                target_status = ff_withdrawal_codec:marshal(status, Status)
            });
        {error, {invalid_status_change, {already_has_status, Status}}} ->
            woody_error:raise(business, #wthd_AlreadyHasStatus{
                withdrawal_status = ff_withdrawal_codec:marshal(status, Status)
            });
        {error, {another_adjustment_in_progress, AnotherID}} ->
            woody_error:raise(business, #wthd_AnotherAdjustmentInProgress{
                another_adjustment_id = ff_codec:marshal(id, AnotherID)
            })
    end.
