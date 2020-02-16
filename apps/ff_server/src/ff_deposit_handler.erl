-module(ff_deposit_handler).
-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_deposit_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(deposit, #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%

handle_function_('Create', [MarshaledParams, MarshaledContext], Opts) ->
    Params = ff_deposit_codec:unmarshal(deposit_params, MarshaledParams),
    Context = ff_deposit_codec:unmarshal(context, MarshaledContext),
    ok = scoper:add_meta(maps:with([id, wallet_id, source_id, external_id], Params)),
    case ff_deposit_machine:create(Params, Context) of
        ok ->
            handle_function_('Get', [maps:get(id, Params), #'EventRange'{}], Opts);
        {error, exists} ->
            handle_function_('Get', [maps:get(id, Params), #'EventRange'{}], Opts);
        {error, {wallet, notfound}} ->
            woody_error:raise(business, #fistful_WalletNotFound{});
        {error, {source, notfound}} ->
            woody_error:raise(business, #fistful_SourceNotFound{});
        {error, {source, unauthorized}} ->
            woody_error:raise(business, #fistful_SourceUnauthorized{});
        {error, {terms_violation, {not_allowed_currency, {DomainCurrency, DomainAllowed}}}} ->
            Currency = ff_dmsl_codec:unmarshal(currency_ref, DomainCurrency),
            Allowed = [ff_dmsl_codec:unmarshal(currency_ref, C) || C <- DomainAllowed],
            woody_error:raise(business, #fistful_ForbiddenOperationCurrency{
                currency = ff_codec:marshal(currency_ref, Currency),
                allowed_currencies = ff_codec:marshal({set, currency_ref}, Allowed)
            });
        {error, {inconsistent_currency, {Deposit, Source, Wallet}}} ->
            woody_error:raise(business, #deposit_InconsistentDepositCurrency{
                deposit_currency = ff_codec:marshal(currency_ref, Deposit),
                source_currency = ff_codec:marshal(currency_ref, Source),
                wallet_currency = ff_codec:marshal(currency_ref, Wallet)
            });
        {error, {bad_deposit_amount, Amount}} ->
            woody_error:raise(business, #fistful_InvalidOperationAmount{
                amount = ff_codec:marshal(cash, Amount)
            })
    end;
handle_function_('Get', [ID, EventRange], _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    case ff_deposit_machine:get(ID, ff_codec:unmarshal(event_range, EventRange)) of
        {ok, Machine} ->
            Deposit = ff_deposit_machine:deposit(Machine),
            Context = ff_deposit_machine:ctx(Machine),
            {ok, ff_deposit_codec:marshal(deposit_state, #{
                deposit => Deposit,
                context => Context
            })};
        {error, {unknown_deposit, ID}} ->
            woody_error:raise(business, #fistful_DepositNotFound{})
    end;
handle_function_('GetContext', [ID], _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    case ff_deposit_machine:get(ID, {undefined, 0}) of
        {ok, Machine} ->
            Context = ff_deposit_machine:ctx(Machine),
            {ok, ff_codec:marshal(context, Context)};
        {error, {unknown_deposit, ID}} ->
            woody_error:raise(business, #fistful_DepositNotFound{})
    end;
handle_function_('GetEvents', [ID, EventRange], _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    case ff_deposit_machine:events(ID, ff_codec:unmarshal(event_range, EventRange)) of
        {ok, Events} ->
            {ok, [ff_deposit_codec:marshal(event, E) || E <- Events]};
        {error, {unknown_deposit, ID}} ->
            woody_error:raise(business, #fistful_DepositNotFound{})
    end;
handle_function_('CreateAdjustment', [ID, MarshaledParams], _Opts) ->
    Params = ff_deposit_adjustment_codec:unmarshal(adjustment_params, MarshaledParams),
    AdjustmentID = maps:get(id, Params),
    ok = scoper:add_meta(genlib_map:compact(#{
        id => ID,
        adjustment_id => AdjustmentID,
        external_id => maps:get(external_id, Params, undefined)
    })),
    case ff_deposit_machine:start_adjustment(ID, Params) of
        ok ->
            {ok, Machine} = ff_deposit_machine:get(ID),
            Deposit = ff_deposit_machine:deposit(Machine),
            {ok, Adjustment} = ff_deposit:find_adjustment(AdjustmentID, Deposit),
            {ok, ff_deposit_adjustment_codec:marshal(adjustment_state, Adjustment)};
        {error, {unknown_deposit, ID}} ->
            woody_error:raise(business, #fistful_DepositNotFound{});
        {error, {invalid_deposit_status, Status}} ->
            woody_error:raise(business, #deposit_InvalidDepositStatus{
                deposit_status = ff_deposit_codec:marshal(status, Status)
            });
        {error, {invalid_status_change, {unavailable_status, Status}}} ->
            woody_error:raise(business, #deposit_ForbiddenStatusChange{
                target_status = ff_deposit_codec:marshal(status, Status)
            });
        {error, {invalid_status_change, {already_has_status, Status}}} ->
            woody_error:raise(business, #deposit_AlreadyHasStatus{
                deposit_status = ff_deposit_codec:marshal(status, Status)
            });
        {error, {another_adjustment_in_progress, AnotherID}} ->
            woody_error:raise(business, #deposit_AnotherAdjustmentInProgress{
                another_adjustment_id = ff_codec:marshal(id, AnotherID)
            })
    end;
handle_function_('CreateRevert', [ID, MarshaledParams], _Opts) ->
    Params = ff_deposit_revert_codec:unmarshal(revert_params, MarshaledParams),
    RevertID = maps:get(id, Params),
    ok = scoper:add_meta(genlib_map:compact(#{
        id => ID,
        revert_id => RevertID,
        external_id => maps:get(external_id, Params, undefined)
    })),
    case ff_deposit_machine:start_revert(ID, Params) of
        ok ->
            {ok, Machine} = ff_deposit_machine:get(ID),
            Deposit = ff_deposit_machine:deposit(Machine),
            {ok, Revert} = ff_deposit:find_revert(RevertID, Deposit),
            {ok, ff_deposit_revert_codec:marshal(revert_state, Revert)};
        {error, {unknown_deposit, ID}} ->
            woody_error:raise(business, #fistful_DepositNotFound{});
        {error, {invalid_deposit_status, Status}} ->
            woody_error:raise(business, #deposit_InvalidDepositStatus{
                deposit_status = ff_deposit_codec:marshal(status, Status)
            });
        {error, {inconsistent_revert_currency, {Revert, Deposit}}} ->
            woody_error:raise(business, #deposit_InconsistentRevertCurrency{
                deposit_currency = ff_codec:marshal(currency_ref, Deposit),
                revert_currency = ff_codec:marshal(currency_ref, Revert)
            });
        {error, {insufficient_deposit_amount, {RevertBody, DepositAmount}}} ->
            woody_error:raise(business, #deposit_InsufficientDepositAmount{
                revert_body = ff_codec:marshal(cash, RevertBody),
                deposit_amount = ff_codec:marshal(cash, DepositAmount)
            });
        {error, {invalid_revert_amount, Amount}} ->
            woody_error:raise(business, #fistful_InvalidOperationAmount{
                amount = ff_codec:marshal(cash, Amount)
            })
    end;
handle_function_('CreateRevertAdjustment', [ID, RevertID, MarshaledParams], _Opts) ->
    Params = ff_deposit_revert_adjustment_codec:unmarshal(adjustment_params, MarshaledParams),
    AdjustmentID = maps:get(id, Params),
    ok = scoper:add_meta(genlib_map:compact(#{
        id => ID,
        revert_id => RevertID,
        adjustment_id => AdjustmentID,
        external_id => maps:get(external_id, Params, undefined)
    })),
    case ff_deposit_machine:start_revert_adjustment(ID, RevertID, Params) of
        ok ->
            {ok, Machine} = ff_deposit_machine:get(ID),
            Deposit = ff_deposit_machine:deposit(Machine),
            {ok, Revert} = ff_deposit:find_revert(RevertID, Deposit),
            {ok, Adjustment} = ff_deposit_revert:find_adjustment(AdjustmentID, Revert),
            {ok, ff_deposit_revert_adjustment_codec:marshal(adjustment_state, Adjustment)};
        {error, {unknown_deposit, ID}} ->
            woody_error:raise(business, #fistful_DepositNotFound{});
        {error, {unknown_revert, RevertID}} ->
            woody_error:raise(business, #deposit_RevertNotFound{
                id = ff_codec:marshal(id, RevertID)
            });
        {error, {invalid_revert_status, Status}} ->
            woody_error:raise(business, #deposit_InvalidRevertStatus{
                revert_status = ff_deposit_revert_codec:marshal(status, Status)
            });
        {error, {invalid_status_change, {unavailable_status, Status}}} ->
            woody_error:raise(business, #deposit_ForbiddenRevertStatusChange{
                target_status = ff_deposit_revert_codec:marshal(status, Status)
            });
        {error, {invalid_status_change, {already_has_status, Status}}} ->
            woody_error:raise(business, #deposit_RevertAlreadyHasStatus{
                revert_status = ff_deposit_revert_codec:marshal(status, Status)
            });
        {error, {another_adjustment_in_progress, AnotherID}} ->
            woody_error:raise(business, #deposit_AnotherAdjustmentInProgress{
                another_adjustment_id = ff_codec:marshal(id, AnotherID)
            })
    end.
