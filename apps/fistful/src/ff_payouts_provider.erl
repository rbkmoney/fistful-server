-module(ff_payouts_provider).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-type payouts_provider() :: #{
    id := id(),
    identity := ff_identity:id(),
    payout_terms := dmsl_domain_thrift:'PayoutsProvisionTerms'(),
    accounts := accounts(),
    adapter := ff_adapter:adapter(),
    adapter_opts := map()
}.

-type id()       :: dmsl_domain_thrift:'ObjectID'().
-type accounts() :: #{ff_currency:id() => ff_account:account()}.

-type payouts_provider_ref() :: dmsl_domain_thrift:'PayoutsProviderRef'().

-export_type([id/0]).
-export_type([payouts_provider/0]).

-export([id/1]).
-export([accounts/1]).
-export([adapter/1]).
-export([adapter_opts/1]).

-export([ref/1]).
-export([get/1]).
-export([compute_fees/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec id(payouts_provider()) -> id().
-spec accounts(payouts_provider()) -> accounts().
-spec adapter(payouts_provider()) -> ff_adapter:adapter().
-spec adapter_opts(payouts_provider()) -> map().

id(#{id := ID}) ->
    ID.

accounts(#{accounts := Accounts}) ->
    Accounts.

adapter(#{adapter := Adapter}) ->
    Adapter.

adapter_opts(#{adapter_opts := AdapterOpts}) ->
    AdapterOpts.

%%

-spec ref(id()) -> payouts_provider_ref().

ref(ID) ->
    #domain_PayoutsProviderRef{id = ID}.

-spec get(id()) ->
    {ok, payouts_provider()} |
    {error, notfound}.

get(ID) ->
    do(fun () ->
        PayoutsProvider = unwrap(ff_domain_config:object({payouts_provider, ref(ID)})),
        decode(ID, PayoutsProvider)
    end).

-spec compute_fees(payouts_provider(), hg_selector:varset()) -> ff_cash_flow:cash_flow_fee().

compute_fees(#{payout_terms := PayoutTerms}, VS) ->
    #domain_PayoutsProvisionTerms{cash_flow = CashFlowSelector} = PayoutTerms,
    CashFlow = unwrap(hg_selector:reduce_to_value(CashFlowSelector, VS)),
    #{
        postings => ff_cash_flow:decode_domain_postings(CashFlow)
    }.

%%

decode(ID, #domain_PayoutsProvider{
    proxy = Proxy,
    identity = Identity,
    payout_terms = PayoutTerms,
    accounts = Accounts
}) ->
    maps:merge(
        #{
            id              => ID,
            identity        => Identity,
            payout_terms    => PayoutTerms,
            accounts        => decode_accounts(Identity, Accounts)
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

