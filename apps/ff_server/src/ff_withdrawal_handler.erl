-module(ff_withdrawal_handler).
-behaviour(ff_woody_wrapper).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
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
handle_function_('Create', [Params], Opts) ->
    ID = Params#wthd_WithdrawalParams.id,
    Ctx = Params#wthd_WithdrawalParams.context,
    case ff_withdrawal_machine:create(
        ff_withdrawal_codec:unmarshal_withdrawal_params(Params),
        ff_withdrawal_codec:unmarshal(ctx, Ctx)
    ) of
        ok ->
            handle_function_('Get', [ID], Opts);
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
        {error, {terms, {terms_violation, {cash_range, {Cash, CashRange}}}}} ->
            woody_error:raise(business, ff_withdrawal_codec:marshal_cash_range_error({Cash, CashRange}));
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('Get', [ID], _Opts) ->
    case ff_withdrawal_machine:get(ID) of
        {ok, Machine} ->
            Ctx = ff_withdrawal_machine:ctx(Machine),
            Context = ff_withdrawal_codec:marshal(ctx, Ctx),
            Withdrawal = ff_withdrawal_codec:marshal_withdrawal(ff_withdrawal_machine:withdrawal(Machine)),
            {ok, Withdrawal#wthd_Withdrawal{context = Context}};
        {error, {unknown_withdrawal, _ID}} ->
            woody_error:raise(business, #fistful_WithdrawalNotFound{})
    end;
handle_function_('GetEvents', [WithdrawalID, RangeParams], _Opts) ->
    Range = ff_codec:unmarshal(range, RangeParams),
    case ff_withdrawal_machine:events(WithdrawalID, Range) of
        {ok, Events} ->
            {ok, [ff_withdrawal_codec:marshal_event(Ev) || Ev <- Events]};
        {error, {unknown_withdrawal, _ID}} ->
            woody_error:raise(business, #fistful_WithdrawalNotFound{})
    end.

