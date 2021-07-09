-module(ff_p2p_provider).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type provider() :: #{
    id := id(),
    identity := ff_identity:id(),
    terms := dmsl_domain_thrift:'ProvisionTermSet'(),
    accounts := accounts(),
    adapter := ff_adapter:adapter(),
    adapter_opts := map()
}.

-type id() :: dmsl_domain_thrift:'ObjectID'().
-type accounts() :: #{ff_currency:id() => ff_account:account()}.
-type adapter() :: ff_adapter:adapter().
-type adapter_opts() :: map().

-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type term_set() :: dmsl_domain_thrift:'ProvisionTermSet'().
-type provision_terms() :: dmsl_domain_thrift:'P2PProvisionTerms'().

-export_type([id/0]).
-export_type([provider_ref/0]).
-export_type([provider/0]).
-export_type([adapter/0]).
-export_type([adapter_opts/0]).
-export_type([provision_terms/0]).

-export([id/1]).
-export([accounts/1]).
-export([adapter/1]).
-export([adapter_opts/1]).
-export([terms/1]).
-export([provision_terms/1]).

-export([ref/1]).
-export([get/1]).
-export([get/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec id(provider()) -> id().
-spec accounts(provider()) -> accounts().
-spec adapter(provider()) -> ff_adapter:adapter().
-spec adapter_opts(provider()) -> map().

id(#{id := ID}) ->
    ID.

accounts(#{accounts := Accounts}) ->
    Accounts.

adapter(#{adapter := Adapter}) ->
    Adapter.

adapter_opts(#{adapter_opts := AdapterOpts}) ->
    AdapterOpts.

-spec terms(provider()) -> term_set() | undefined.
terms(Provider) ->
    maps:get(terms, Provider, undefined).

-spec provision_terms(provider()) -> provision_terms() | undefined.
provision_terms(Provider) ->
    case terms(Provider) of
        Terms when Terms =/= undefined ->
            case Terms#domain_ProvisionTermSet.wallet of
                WalletTerms when WalletTerms =/= undefined ->
                    WalletTerms#domain_WalletProvisionTerms.p2p;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

%%

-spec ref(id()) -> provider_ref().
ref(ID) ->
    #domain_ProviderRef{id = ID}.

-spec get(id()) ->
    {ok, provider()}
    | {error, notfound}.
get(ID) ->
    get(latest, ID).

-spec get(ff_domain_config:revision(), id()) ->
    {ok, provider()}
    | {error, notfound}.
get(DomainRevision, ID) ->
    do(fun() ->
        P2PProvider = unwrap(ff_domain_config:object(DomainRevision, {provider, ref(ID)})),
        decode(ID, P2PProvider)
    end).

decode(ID, #domain_Provider{
    proxy = Proxy,
    identity = Identity,
    terms = Terms,
    accounts = Accounts
}) ->
    maps:merge(
        #{
            id => ID,
            identity => Identity,
            terms => Terms,
            accounts => decode_accounts(Identity, Accounts)
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
