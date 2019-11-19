-module(ff_p2p_provider).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type p2p_provider() :: #{
    id := id(),
    identity := ff_identity:id(),
    p2p_terms := dmsl_domain_thrift:'P2PProvisionTerms'(),
    accounts := accounts(),
    adapter := ff_adapter:adapter(),
    adapter_opts := map()
}.

-type id()       :: dmsl_domain_thrift:'ObjectID'().
-type accounts() :: #{ff_currency:id() => ff_account:account()}.

-type p2p_provider_ref() :: dmsl_domain_thrift:'P2PProviderRef'().
-type currency_ref() :: dmsl_domain_thrift:'CurrencyRef'().
-type cash() :: dmsl_domain_thrift:'Cash'().
-type cash_range() :: dmsl_domain_thrift:'CashRange'().
-type validate_terms_error() :: {terms_violation,
                                    {not_allowed_currency, {currency_ref(), [currency_ref()]}} |
                                    {cash_range, {cash(), cash_range()}}
                                }.

-export_type([id/0]).
-export_type([p2p_provider/0]).
-export_type([validate_terms_error/0]).

-export([id/1]).
-export([accounts/1]).
-export([adapter/1]).
-export([adapter_opts/1]).

-export([ref/1]).
-export([get/1]).
-export([compute_fees/2]).
-export([validate_terms/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec id(p2p_provider()) -> id().
-spec accounts(p2p_provider()) -> accounts().
-spec adapter(p2p_provider()) -> ff_adapter:adapter().
-spec adapter_opts(p2p_provider()) -> map().

id(#{id := ID}) ->
    ID.

accounts(#{accounts := Accounts}) ->
    Accounts.

adapter(#{adapter := Adapter}) ->
    Adapter.

adapter_opts(#{adapter_opts := AdapterOpts}) ->
    AdapterOpts.

%%

-spec ref(id()) -> p2p_provider_ref().

ref(ID) ->
    #domain_P2PProviderRef{id = ID}.

-spec get(id()) ->
    {ok, p2p_provider()} |
    {error, notfound}.

get(ID) ->
    do(fun () ->
        P2PProvider = unwrap(ff_domain_config:object({p2p_provider, ref(ID)})),
        decode(ID, P2PProvider)
    end).

-spec compute_fees(p2p_provider(), hg_selector:varset()) -> ff_cash_flow:cash_flow_fee().

compute_fees(#{p2p_terms := P2PTerms}, VS) ->
    #domain_P2PProvisionTerms{cash_flow = CashFlowSelector} = P2PTerms,
    {ok, CashFlow} = hg_selector:reduce_to_value(CashFlowSelector, VS),
    #{
        postings => ff_cash_flow:decode_domain_postings(CashFlow)
    }.

-spec validate_terms(p2p_provider(), hg_selector:varset()) ->
    {ok, valid} |
    {error, validate_terms_error()}.

validate_terms(#{p2p_terms := P2PTerms}, VS) ->
    #domain_P2PProvisionTerms{
        currencies = CurrenciesSelector,
        fees = FeeSelector,
        cash_limit = CashLimitSelector
    } = P2PTerms,
    do(fun () ->
        valid = unwrap(validate_currencies(CurrenciesSelector, VS)),
        valid = unwrap(validate_fee_term_is_reduced(FeeSelector, VS)),
        valid = unwrap(validate_cash_limit(CashLimitSelector, VS))
    end).

%%

validate_currencies(CurrenciesSelector, #{currency := CurrencyRef} = VS) ->
    {ok, Currencies} = hg_selector:reduce_to_value(CurrenciesSelector, VS),
    case ordsets:is_element(CurrencyRef, Currencies) of
        true ->
            {ok, valid};
        false ->
            {error, {terms_violation, {not_allowed_currency, {CurrencyRef, Currencies}}}}
    end.

validate_fee_term_is_reduced(FeeSelector, VS) ->
    {ok, _Fees} = hg_selector:reduce_to_value(FeeSelector, VS),
    {ok, valid}.

validate_cash_limit(CashLimitSelector, #{cost := Cash} = VS) ->
    {ok, CashRange} = hg_selector:reduce_to_value(CashLimitSelector, VS),
    case hg_cash_range:is_inside(Cash, CashRange) of
        within ->
            {ok, valid};
        _NotInRange  ->
            {error, {terms_violation, {cash_range, {Cash, CashRange}}}}
    end.

decode(ID, #domain_P2PProvider{
    proxy = Proxy,
    identity = Identity,
    p2p_terms = P2PTerms,
    accounts = Accounts
}) ->
    maps:merge(
        #{
            id               => ID,
            identity         => Identity,
            p2p_terms        => P2PTerms,
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
    {ok, Proxy} = ff_domain_config:object({proxy, ProxyRef}),
    #domain_ProxyDefinition{
        url = URL,
        options = ProxyOpts
    } = Proxy,
    #{
        adapter => ff_woody_client:new(URL),
        adapter_opts => maps:merge(ProviderOpts, ProxyOpts)
    }.

