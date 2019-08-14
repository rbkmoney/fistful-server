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
    scoper:scope(withdrawal, #{function => Func},
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
    Ctx = Params#wthd_WithdrawalParams.context,
    case ff_withdrawal_machine:create(
        ID,
        ff_withdrawal_codec:unmarshal_withdrawal_params(Params),
        ff_withdrawal_codec:unmarshal(ctx, Ctx)
    ) of
        ok ->
            handle_function_('Get', [ID], Context, Opts);
        {error, exists} ->
            woody_error:raise(business, #fistful_IDExists{});
        {error, {wallet, notfound}} ->
            woody_error:raise(business, #fistful_WalletNotFound{});
        {error, {destination, notfound}} ->
            woody_error:raise(business, #fistful_DestinationNotFound{});
        {error, {destination, unauthorized}} ->
            woody_error:raise(business, #fistful_DestinationUnauthorized{});
        {error, {terms, {invalid_withdrawal_currency, CurrencyID, {wallet_currency, CurrencyID2}}}} ->
            woody_error:raise(business, ff_withdrawal_codec:marshal_currency_invalid({CurrencyID, CurrencyID2}));
        {error, {terms, {terms_violation, {cash_range, {CashIn, CashRangeIn}}}}} ->
            %% TODO after ff_party changes, del dmsl_codec
            Cash  = ff_dmsl_codec:unmarshal(cash, CashIn),
            Range = ff_dmsl_codec:unmarshal(cash_range, CashRangeIn),
            woody_error:raise(business, ff_withdrawal_codec:marshal_cash_range_error({Cash, Range}));
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('Get', [ID], _Context, _Opts) ->
    case ff_withdrawal_machine:get(ID) of
        {ok, Machine} ->
            Ctx = ff_withdrawal_machine:ctx(Machine),
            Context = ff_withdrawal_codec:marshal(ctx, Ctx),
            Withdrawal = ff_withdrawal_codec:marshal_withdrawal(ff_withdrawal_machine:withdrawal(Machine)),
            {ok, Withdrawal#wthd_Withdrawal{context = Context}};
        {error, notfound} ->
            woody_error:raise(business, #fistful_DestinationNotFound{})
    end;
handle_function_('GetEvents', [WithdrawalID, RangeParams], _Context, _Opts) ->
    Range = ff_codec:unmarshal(range, RangeParams),
    case ff_withdrawal_machine:events(WithdrawalID, Range) of
        {ok, Events} ->
            {ok, [ff_withdrawal_codec:marshal_event(Ev) || Ev <- Events]};
        {error, notfound} ->
            woody_error:raise(business, #fistful_WithdrawalNotFound{})
    end.

