-module(ff_payouts_provider).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type withdrawal_provider() :: #{
    id := id(),
    identity => ff_identity:id(),
    withdrawal_terms => dmsl_domain_thrift:'WithdrawalProvisionTerms'(),
    accounts := accounts(),
    adapter := ff_adapter:adapter(),
    adapter_opts := map(),
    terminal_selector => dmsl_domain_thrift:'WithdrawalTerminalSelector'()
}.

-type id()       :: dmsl_domain_thrift:'ObjectID'().
-type accounts() :: #{ff_currency:id() => ff_account:account()}.

-type withdrawal_provider_ref() :: dmsl_domain_thrift:'WithdrawalProviderRef'().
-type withdrawal_provision_terms() :: dmsl_domain_thrift:'WithdrawalProvisionTerms'().

-export_type([id/0]).
-export_type([withdrawal_provider/0]).
-export_type([withdrawal_provider_ref/0]).

-export([id/1]).
-export([accounts/1]).
-export([adapter/1]).
-export([adapter_opts/1]).
-export([terms/1]).

-export([ref/1]).
-export([get/1]).
-export([compute_fees/2]).
-export([compute_withdrawal_terminals_with_priority/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec id(withdrawal_provider()) -> id().
-spec accounts(withdrawal_provider()) -> accounts().
-spec adapter(withdrawal_provider()) -> ff_adapter:adapter().
-spec adapter_opts(withdrawal_provider()) -> map().
-spec terms(withdrawal_provider()) -> withdrawal_provision_terms() | undefined.

id(#{id := ID}) ->
    ID.

accounts(#{accounts := Accounts}) ->
    Accounts.

adapter(#{adapter := Adapter}) ->
    Adapter.

adapter_opts(#{adapter_opts := AdapterOpts}) ->
    AdapterOpts.

terms(WithdrawalProvider) ->
    maps:get(withdrawal_terms, WithdrawalProvider, undefined).

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

-spec compute_fees(withdrawal_provider(), hg_selector:varset()) ->
    {ok, ff_cash_flow:cash_flow_fee()} | {error, term()}.

compute_fees(WithdrawalProvider, VS) ->
    case terms(WithdrawalProvider) of
        Terms when Terms =/= undefined ->
            {ok, compute_fees_(Terms, VS)};
        _ ->
            {error, {misconfiguration, {missing, withdrawal_terms}}}
    end.

compute_fees_(WithdrawalTerms, VS) ->
    #domain_WithdrawalProvisionTerms{cash_flow = CashFlowSelector} = WithdrawalTerms,
    CashFlow = unwrap(hg_selector:reduce_to_value(CashFlowSelector, VS)),
    #{
        postings => ff_cash_flow:decode_domain_postings(CashFlow)
    }.

-spec compute_withdrawal_terminals_with_priority(withdrawal_provider(), hg_selector:varset()) ->
    {ok, [{ff_payouts_terminal:id(), ff_payouts_terminal:withdrawal_terminal_priority()}]} | {error, term()}.

compute_withdrawal_terminals_with_priority(WithdrawalProvider, VS) ->
    case maps:get(terminal_selector, WithdrawalProvider, undefined) of
        Selector when Selector =/= undefined ->
            compute_withdrawal_terminals_(Selector, VS);
        _ ->
            {error, {misconfiguration, {missing, terminal_selector}}}
    end.

compute_withdrawal_terminals_(TerminalSelector, VS) ->
    case hg_selector:reduce_to_value(TerminalSelector, VS) of
        {ok, Terminals} ->
            {ok, [{TerminalID, Priority}
                || #domain_WithdrawalTerminalRef{id = TerminalID, priority = Priority} <- Terminals]};
        Error ->
            Error
    end.

%%

decode(ID, #domain_WithdrawalProvider{
    proxy = Proxy,
    identity = Identity,
    withdrawal_terms = WithdrawalTerms,
    accounts = Accounts,
    terminal = TerminalSelector
}) ->
    genlib_map:compact(maps:merge(
        #{
            id                => ID,
            identity          => Identity,
            withdrawal_terms  => WithdrawalTerms,
            accounts          => decode_accounts(Identity, Accounts),
            terminal_selector => TerminalSelector
        },
        decode_adapter(Proxy)
    )).

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

