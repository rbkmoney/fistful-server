-module(ff_payouts_terminal).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type withdrawal_terminal() :: #{
    id := id(),
    provider_ref := ff_payouts_provider:withdrawal_provider_ref(),
    withdrawal_terms := dmsl_domain_thrift:'WithdrawalProvisionTerms'(),
    adapter_opts := map()
}.

-type id() :: dmsl_domain_thrift:'ObjectID'().

-type withdrawal_terminal_ref() :: dmsl_domain_thrift:'WithdrawalTerminalRef'().

-export_type([id/0]).
-export_type([withdrawal_terminal/0]).
-export_type([withdrawal_terminal_ref/0]).

-export([adapter_opts/1]).
-export([ref/1]).
-export([get/1]).
-export([validate_terms/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec adapter_opts(withdrawal_terminal()) -> map().

adapter_opts(#{adapter_opts := undefined}) ->
    #{}; %% Opts are optional for terminals
adapter_opts(#{adapter_opts := AdapterOpts}) ->
    AdapterOpts.

%%

-spec ref(id()) -> withdrawal_terminal_ref().

ref(ID) ->
    #domain_WithdrawalTerminalRef{id = ID}.

-spec get(id()) ->
    {ok, withdrawal_terminal()} |
    {error, notfound}.

get(ID) ->
    do(fun () ->
        WithdrawalTerminal = unwrap(ff_domain_config:object({withdrawal_terminal, ref(ID)})),
        decode(ID, WithdrawalTerminal)
    end).

-spec validate_terms(withdrawal_terminal(), hg_selector:varset()) ->
    {ok, valid} |
    {error, Error :: term()}.

validate_terms(#{withdrawal_terms := undefined}, _VS) ->
    {ok, valid}; %% Terms are optional for terminals
validate_terms(#{withdrawal_terms := WithdrawalTerms}, VS) ->
    #domain_WithdrawalProvisionTerms{
        currencies = CurrenciesSelector,
        %% PayoutMethodsSelector is useless for withdrawals
        %% so we can just ignore it
        %% payout_methods = PayoutMethodsSelector,
        cash_limit = CashLimitSelector
    } = WithdrawalTerms,
    do(fun () ->
        valid = unwrap(validate_currencies(CurrenciesSelector, VS)),
        valid = unwrap(validate_cash_limit(CashLimitSelector, VS))
    end).

%%

validate_currencies(CurrenciesSelector, #{currency := CurrencyRef} = VS) ->
    Currencies = unwrap(hg_selector:reduce_to_value(CurrenciesSelector, VS)),
    case ordsets:is_element(CurrencyRef, Currencies) of
        true ->
            {ok, valid};
        false ->
            {error, {terms_violation, {not_allowed_currency, {CurrencyRef, Currencies}}}}
    end.

validate_cash_limit(CashLimitSelector, #{cost := Cash} = VS) ->
    CashRange = unwrap(hg_selector:reduce_to_value(CashLimitSelector, VS)),
    case hg_cash_range:is_inside(Cash, CashRange) of
        within ->
            {ok, valid};
        _NotInRange  ->
            {error, {terms_violation, {cash_range, {Cash, CashRange}}}}
    end.

%%

decode(ID, #domain_WithdrawalTerminal{
    options = ProxyOptions,
    terms = WithdrawalTerms,
    provider_ref = ProviderRef
}) ->
    #{
        id => ID,
        provider_ref => ProviderRef,
        withdrawal_terms => WithdrawalTerms,
        adapter_opts => ProxyOptions
    }.