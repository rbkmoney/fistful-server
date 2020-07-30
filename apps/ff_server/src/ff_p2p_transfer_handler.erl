-module(ff_p2p_transfer_handler).
-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function(Func, Args, Opts) ->
    scoper:scope(p2p_transfer, #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%
handle_function_('Create', [MarshaledParams, Context], Opts) ->
    P2PTransferID = MarshaledParams#p2p_transfer_P2PTransferParams.id,
    Params = ff_p2p_transfer_codec:unmarshal_p2p_transfer_params(MarshaledParams),
    ok = scoper:add_meta(maps:with([id, identity_id, external_id], Params)),
    case p2p_transfer_machine:create(Params, Context) of
        ok ->
            handle_function_('Get', [P2PTransferID, #'EventRange'{}], Opts);
        {error, exists} ->
            woody_error:raise(business, #fistful_IDExists{});
        {error, {identity, notfound}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
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
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;

handle_function_('Get', [ID, EventRange], _Opts) ->
    {After, Limit} = ff_codec:unmarshal(event_range, EventRange),
    ok = scoper:add_meta(#{id => ID}),
    case p2p_transfer_machine:get(ID, {After, Limit}) of
        {ok, Machine} ->
            P2PTransfer = p2p_transfer_machine:p2p_transfer(Machine),
            Ctx = ff_machine:ctx(Machine),
            Response = ff_p2p_transfer_codec:marshal_p2p_transfer_state(P2PTransfer, Ctx),
            {ok, Response};
        {error, {unknown_p2p_transfer, _Ref}} ->
            woody_error:raise(business, #fistful_P2PNotFound{})
    end;

handle_function_('GetContext', [ID], _Opts) ->
    case p2p_transfer_machine:get(ID) of
        {ok, Machine} ->
            Ctx = ff_machine:ctx(Machine),
            Response = ff_p2p_transfer_codec:marshal(ctx, Ctx),
            {ok, Response};
        {error, {unknown_p2p_transfer, _Ref}} ->
            woody_error:raise(business, #fistful_P2PNotFound{})
    end;

handle_function_('GetEvents', [ID, EventRange], _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    case p2p_transfer_machine:events(ID, ff_codec:unmarshal(event_range, EventRange)) of
        {ok, Events} ->
            {ok, lists:map(fun ff_p2p_transfer_codec:marshal_event/1, Events)};
        {error, {unknown_p2p_transfer, ID}} ->
            woody_error:raise(business, #fistful_P2PNotFound{})
    end;

handle_function_('CreateAdjustment', [ID, MarshaledParams], _Opts) ->
    Params = ff_p2p_transfer_adjustment_codec:unmarshal(adjustment_params, MarshaledParams),
    AdjustmentID = maps:get(id, Params),
    ok = scoper:add_meta(genlib_map:compact(#{
        id => ID,
        adjustment_id => AdjustmentID,
        external_id => maps:get(external_id, Params, undefined)
    })),
    case p2p_transfer_machine:start_adjustment(ID, Params) of
        ok ->
            {ok, Machine} = p2p_transfer_machine:get(ID),
            P2PTransfer = p2p_transfer_machine:p2p_transfer(Machine),
            {ok, Adjustment} = p2p_transfer:find_adjustment(AdjustmentID, P2PTransfer),
            {ok, ff_p2p_transfer_adjustment_codec:marshal(adjustment_state, Adjustment)};
        {error, {unknown_p2p_transfer, ID}} ->
            woody_error:raise(business, #fistful_P2PNotFound{});
        {error, {invalid_p2p_transfer_status, Status}} ->
            woody_error:raise(business, #p2p_transfer_InvalidP2PTransferStatus{
                p2p_status = ff_p2p_transfer_codec:marshal(status, Status)
            });
        {error, {invalid_status_change, {unavailable_status, Status}}} ->
            woody_error:raise(business, #p2p_transfer_ForbiddenStatusChange{
                target_status = ff_p2p_transfer_codec:marshal(status, Status)
            });
        {error, {invalid_status_change, {already_has_status, Status}}} ->
            woody_error:raise(business, #p2p_transfer_AlreadyHasStatus{
                p2p_status = ff_p2p_transfer_codec:marshal(status, Status)
            });
        {error, {another_adjustment_in_progress, AnotherID}} ->
            woody_error:raise(business, #p2p_transfer_AnotherAdjustmentInProgress{
                another_adjustment_id = ff_codec:marshal(id, AnotherID)
            })
    end.
