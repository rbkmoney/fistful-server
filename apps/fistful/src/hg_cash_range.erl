%% TODO merge with ff_range

-module(hg_cash_range).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export([is_inside/2]).

-type cash_range() :: dmsl_domain_thrift:'CashRange'().
-type cash()       :: dmsl_domain_thrift:'Cash'().

-spec is_inside(cash(), cash_range()) ->
    within | {exceeds, lower | upper}.

is_inside(Cash, CashRange = #domain_CashRange{lower = Lower, upper = Upper}) ->
    case {
        compare_cash(fun erlang:'>'/2, Cash, Lower),
        compare_cash(fun erlang:'<'/2, Cash, Upper)
    } of
        {true, true} ->
            within;
        {false, true} ->
            {exceeds, lower};
        {true, false} ->
            {exceeds, upper};
        _ ->
            error({misconfiguration, {'Invalid cash range specified', CashRange, Cash}})
    end.

-define(cash(Amount, SymCode),
    #domain_Cash{
        amount = Amount,
        currency = #domain_CurrencyRef{symbolic_code = SymCode}
    }).

compare_cash(_, V, {inclusive, V}) ->
    true;
compare_cash(F, ?cash(A, C), {_, ?cash(Am, C)}) ->
    F(A, Am);
compare_cash(_, _, _) ->
    error.
