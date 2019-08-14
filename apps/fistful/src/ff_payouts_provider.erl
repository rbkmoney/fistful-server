-module(ff_payouts_provider).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-type withdrawal_provider() :: #{
    id := id(),
    identity := ff_identity:id(),
    withdrawal_terms := dmsl_domain_thrift:'WithdrawalProvisionTerms'(),
    accounts := accounts(),
    adapter := ff_adapter:adapter(),
    adapter_opts := map()
}.

-type id()       :: dmsl_domain_thrift:'ObjectID'().
-type accounts() :: #{ff_currency:id() => ff_account:account()}.

-type withdrawal_provider_ref() :: dmsl_domain_thrift:'WithdrawalProviderRef'().

-export_type([id/0]).
-export_type([withdrawal_provider/0]).

-export([id/1]).
-export([accounts/1]).
-export([adapter/1]).
-export([adapter_opts/1]).

-export([ref/1]).
-export([get/1]).
-export([compute_fees/2]).
-export([validate_terms/2]).

%% Pipeline

-compile({parse_transform, ff_pipeline}).
-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec id(withdrawal_provider()) -> id().
-spec accounts(withdrawal_provider()) -> accounts().
-spec adapter(withdrawal_provider()) -> ff_adapter:adapter().
-spec adapter_opts(withdrawal_provider()) -> map().

id(#{id := ID}) ->
    ID.

accounts(#{accounts := Accounts}) ->
    Accounts.

adapter(#{adapter := Adapter}) ->
    Adapter.

adapter_opts(#{adapter_opts := AdapterOpts}) ->
    AdapterOpts.

%%

-spec ref(id()) -> withdrawal_provider_ref().

ref(ID) ->
    #domain_WithdrawalProviderRef{id = ID}.

-spec get(id()) ->
    {ok, withdrawal_provider()} |
    {error, notfound}.

get(ID) ->
    do(fun () ->
        WithdrawalProvider = unwrap(ff_domain_config:object({withdrawal_provider, ref(ID)})),
        decode(ID, WithdrawalProvider)
    end).

-spec compute_fees(withdrawal_provider(), hg_selector:varset()) -> ff_cash_flow:cash_flow_fee().

compute_fees(#{withdrawal_terms := WithdrawalTerms}, VS) ->
    #domain_WithdrawalProvisionTerms{cash_flow = CashFlowSelector} = WithdrawalTerms,
    CashFlow = unwrap(hg_selector:reduce_to_value(CashFlowSelector, VS)),
    #{
        postings => ff_cash_flow:decode_domain_postings(CashFlow)
    }.

-spec validate_terms(withdrawal_provider(), hg_selector:varset()) ->
    {ok, valid} |
    {error, Error :: term()}.

validate_terms(#{withdrawal_terms := WithdrawalTerms}, VS) ->
    #domain_WithdrawalProvisionTerms{
        currencies = CurrenciesSelector,
        payout_methods = PayoutMethodsSelector,
        cash_limit = CashLimitSelector
    } = WithdrawalTerms,
    do(fun () ->
        valid = unwrap(validate_currencies(CurrenciesSelector, VS)),
        valid = unwrap(validate_payout_methods(PayoutMethodsSelector, VS)),
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

validate_payout_methods(_, _) ->
    %% PayoutMethodsSelector is useless for withdrawals
    %% so we can just ignore it
    {ok, valid}.

validate_cash_limit(CashLimitSelector, #{cost := Cash} = VS) ->
    CashRange = unwrap(hg_selector:reduce_to_value(CashLimitSelector, VS)),
    case hg_cash_range:is_inside(Cash, CashRange) of
        within ->
            {ok, valid};
        _NotInRange  ->
            {error, {terms_violation, {cash_range, {Cash, CashRange}}}}
    end.

decode(ID, #domain_WithdrawalProvider{
    proxy = Proxy,
    identity = Identity,
    withdrawal_terms = WithdrawalTerms,
    accounts = Accounts
}) ->
    maps:merge(
        #{
            id               => ID,
            identity         => Identity,
            withdrawal_terms => WithdrawalTerms,
            accounts         => decode_accounts(Identity, Accounts)
        },
        decode_adapter(Proxy)
    ).

decode_accounts(Identity, Accounts) ->
    maps:fold(
        fun(CurrencyRef, ProviderAccount, Acc) ->
            #domain_CurrencyRef{symbolic_code = CurrencyID} = CurrencyRef,
            #domain_ProviderAccount{settlement = AccountID} = ProviderAccount,
            maps:put(
                CurrencyID,
                #{
                    % FIXME
                    id => Identity,
                    identity => Identity,
                    currency => CurrencyID,
                    accounter_account_id => AccountID
                },
                Acc
            )
        end,
        #{},
        Accounts
    ).

decode_adapter(#domain_Proxy{ref = ProxyRef, additional = ProviderOpts}) ->
    Proxy = unwrap(ff_domain_config:object({proxy, ProxyRef})),
    #domain_ProxyDefinition{
        url = URL,
        options = ProxyOpts
    } = Proxy,
    #{
        adapter => ff_woody_client:new(URL),
        adapter_opts => maps:merge(ProviderOpts, ProxyOpts)
    }.

