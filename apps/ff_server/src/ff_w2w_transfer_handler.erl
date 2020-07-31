-module(ff_w2w_transfer_handler).
-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_w2w_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function(Func, Args, Opts) ->
    scoper:scope(w2w_transfer, #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%
handle_function_('Create', [MarshaledParams, MarshaledContext], Opts) ->
    W2WTransferID = MarshaledParams#w2w_transfer_W2WTransferParams.id,
    Params = ff_w2w_transfer_codec:unmarshal_w2w_transfer_params(MarshaledParams),
    Context = ff_w2w_transfer_codec:unmarshal(ctx, MarshaledContext),
    ok = scoper:add_meta(maps:with([id, wallet_from_id, wallet_to_id, external_id], Params)),
    case w2w_transfer_machine:create(Params, Context) of
        ok ->
            handle_function_('Get', [W2WTransferID, #'EventRange'{}], Opts);
        {error, exists} ->
            woody_error:raise(business, #fistful_IDExists{});
        {error, {wallet_from, notfound}} ->
            woody_error:raise(business, #fistful_WalletNotFound{
                id = MarshaledParams#w2w_transfer_W2WTransferParams.wallet_from_id
            });
        {error, {wallet_to, notfound}} ->
            woody_error:raise(business, #fistful_WalletNotFound{
                id = MarshaledParams#w2w_transfer_W2WTransferParams.wallet_to_id
            });
        {error, {inconsistent_currency, {Transfer, From, To}}} ->
            woody_error:raise(business, #w2w_transfer_InconsistentW2WTransferCurrency{
                w2w_transfer_currency = ff_codec:marshal(currency_ref, Transfer),
                wallet_from_currency = ff_codec:marshal(currency_ref, From),
                wallet_to_currency = ff_codec:marshal(currency_ref, To)
            });
        {error, {terms, {terms_violation, {not_allowed_currency, {DomainCurrency, DomainAllowed}}}}} ->
            Currency = ff_dmsl_codec:unmarshal(currency_ref, DomainCurrency),
            Allowed = [ff_dmsl_codec:unmarshal(currency_ref, C) || C <- DomainAllowed],
            woody_error:raise(business, #fistful_ForbiddenOperationCurrency{
                currency = ff_codec:marshal(currency_ref, Currency),
                allowed_currencies = ff_codec:marshal({set, currency_ref}, Allowed)
            });
        {error, {terms, {bad_w2w_transfer_amount, Amount}}} ->
            woody_error:raise(business, #fistful_InvalidOperationAmount{
                amount = ff_codec:marshal(cash, Amount)
            });
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;

handle_function_('Get', [ID, EventRange], _Opts) ->
    {After, Limit} = ff_codec:unmarshal(event_range, EventRange),
    ok = scoper:add_meta(#{id => ID}),
    case w2w_transfer_machine:get(ID, {After, Limit}) of
        {ok, Machine} ->
            W2WTransfer = w2w_transfer_machine:w2w_transfer(Machine),
            Ctx = ff_machine:ctx(Machine),
            Response = ff_w2w_transfer_codec:marshal_w2w_transfer_state(W2WTransfer, Ctx),
            {ok, Response};
        {error, {unknown_w2w_transfer, _Ref}} ->
            woody_error:raise(business, #fistful_W2WNotFound{})
    end;

handle_function_('GetContext', [ID], _Opts) ->
    case w2w_transfer_machine:get(ID) of
        {ok, Machine} ->
            Ctx = ff_machine:ctx(Machine),
            Response = ff_w2w_transfer_codec:marshal(ctx, Ctx),
            {ok, Response};
        {error, {unknown_w2w_transfer, _Ref}} ->
            woody_error:raise(business, #fistful_W2WNotFound{})
    end;

handle_function_('CreateAdjustment', [ID, MarshaledParams], _Opts) ->
    Params = ff_w2w_transfer_adjustment_codec:unmarshal(adjustment_params, MarshaledParams),
    AdjustmentID = maps:get(id, Params),
    ok = scoper:add_meta(genlib_map:compact(#{
        id => ID,
        adjustment_id => AdjustmentID,
        external_id => maps:get(external_id, Params, undefined)
    })),
    case w2w_transfer_machine:start_adjustment(ID, Params) of
        ok ->
            {ok, Machine} = w2w_transfer_machine:get(ID),
            W2WTransfer = w2w_transfer_machine:w2w_transfer(Machine),
            {ok, Adjustment} = w2w_transfer:find_adjustment(AdjustmentID, W2WTransfer),
            {ok, ff_w2w_transfer_adjustment_codec:marshal(adjustment_state, Adjustment)};
        {error, {unknown_w2w_transfer, ID}} ->
            woody_error:raise(business, #fistful_W2WNotFound{});
        {error, {invalid_w2w_transfer_status, Status}} ->
            woody_error:raise(business, #w2w_transfer_InvalidW2WTransferStatus{
                w2w_transfer_status = ff_w2w_transfer_codec:marshal(status, Status)
            });
        {error, {invalid_status_change, {unavailable_status, Status}}} ->
            woody_error:raise(business, #w2w_transfer_ForbiddenStatusChange{
                target_status = ff_w2w_transfer_codec:marshal(status, Status)
            });
        {error, {invalid_status_change, {already_has_status, Status}}} ->
            woody_error:raise(business, #w2w_transfer_AlreadyHasStatus{
                w2w_transfer_status = ff_w2w_transfer_codec:marshal(status, Status)
            });
        {error, {another_adjustment_in_progress, AnotherID}} ->
            woody_error:raise(business, #w2w_transfer_AnotherAdjustmentInProgress{
                another_adjustment_id = ff_codec:marshal(id, AnotherID)
            })
    end.
