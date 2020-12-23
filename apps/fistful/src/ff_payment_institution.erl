-module(ff_payment_institution).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type id()       :: dmsl_domain_thrift:'ObjectID'().
-type payment_institution() :: #{
    id                          := id(),
    system_accounts             := dmsl_domain_thrift:'SystemAccountSetSelector'(),
    identity                    := binary(),
    withdrawal_providers        := dmsl_domain_thrift:'ProviderSelector'(),
    p2p_providers               := dmsl_domain_thrift:'ProviderSelector'(),
    p2p_inspector               := dmsl_domain_thrift:'P2PInspectorSelector'(),
    withdrawal_routing_rules    => dmsl_domain_thrift:'RoutingRules'(),
    p2p_transfer_routing_rules  => dmsl_domain_thrift:'RoutingRules'()
}.

-type payinst_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().

-type system_accounts() :: #{
    ff_currency:id() => system_account()
}.

-type system_account() :: #{
    settlement  => ff_account:account(),
    subagent    => ff_account:account()
}.

-export_type([id/0]).
-export_type([payment_institution/0]).

-export([id/1]).

-export([ref/1]).
-export([get/2]).
-export([compute_withdrawal_providers/2]).
-export([compute_p2p_transfer_providers/2]).
-export([compute_p2p_inspector/2]).
-export([compute_system_accounts/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec id(payment_institution()) -> id().

id(#{id := ID}) ->
    ID.

%%

-spec ref(id()) -> payinst_ref().

ref(ID) ->
    #domain_PaymentInstitutionRef{id = ID}.

-spec get(id(), ff_domain_config:revision()) ->
    {ok, payment_institution()} |
    {error, notfound}.

get(ID, DomainRevision) ->
    do(fun () ->
        PaymentInstitution = unwrap(ff_domain_config:object(DomainRevision, {payment_institution, ref(ID)})),
        decode(ID, PaymentInstitution)
    end).

-spec compute_withdrawal_providers(payment_institution(), hg_selector:varset()) ->
    {ok, [ff_payouts_provider:id()]} | {error, term()}.

compute_withdrawal_providers(#{withdrawal_providers := ProviderSelector}, VS) ->
    case hg_selector:reduce_to_value(ProviderSelector, VS) of
        {ok, Providers} ->
            {ok, [ProviderID || #domain_ProviderRef{id = ProviderID} <- Providers]};
        Error ->
            Error
    end.

-spec compute_p2p_transfer_providers(payment_institution(), hg_selector:varset()) ->
    {ok, [ff_payouts_provider:id()]} | {error, term()}.

compute_p2p_transfer_providers(#{p2p_providers := ProviderSelector}, VS) ->
    case hg_selector:reduce_to_value(ProviderSelector, VS) of
        {ok, Providers} ->
            {ok, [ProviderID || #domain_ProviderRef{id = ProviderID} <- Providers]};
        Error ->
            Error
    end.

-spec compute_p2p_inspector(payment_institution(), hg_selector:varset()) ->
    {ok, p2p_inspector:id()} | {error, term()}.
compute_p2p_inspector(#{p2p_inspector := InspectorSelector} = PS, _VS) when InspectorSelector =:= undefined ->
    {error, {misconfiguration, {'No p2p inspector in a given payment_institution', PS}}};
compute_p2p_inspector(#{p2p_inspector := InspectorSelector}, VS) ->
    case hg_selector:reduce_to_value(InspectorSelector, VS) of
        {ok, #domain_P2PInspectorRef{id = InspectorID}} ->
            {ok, InspectorID};
        Error ->
            Error
    end.

-spec compute_system_accounts(payment_institution(), hg_selector:varset()) ->
    {ok, system_accounts()} | {error, term()}.

compute_system_accounts(PaymentInstitution, VS) ->
    #{
        identity := Identity,
        system_accounts := SystemAccountsSelector
    } = PaymentInstitution,
    do(fun() ->
        SystemAccountSetRef = unwrap(hg_selector:reduce_to_value(SystemAccountsSelector, VS)),
        SystemAccountSet = unwrap(ff_domain_config:object({system_account_set, SystemAccountSetRef})),
        decode_system_account_set(Identity, SystemAccountSet)
    end).
%%

decode(ID, #domain_PaymentInstitution{
    wallet_system_account_set = SystemAccounts,
    identity = Identity,
    withdrawal_providers_legacy = WithdrawalProvidersLegacy,
    p2p_providers_legacy = P2PProviders,
    p2p_inspector = P2PInspector,
    withdrawal_routing_rules = WithdrawalRoutingRules,
    p2p_transfer_routing_rules = P2PTransferRoutingRules
}) ->
    #{
        id                          => ID,
        system_accounts             => SystemAccounts,
        identity                    => Identity,
        withdrawal_providers_legacy => WithdrawalProvidersLegacy,
        p2p_providers_legacy        => P2PProviders,
        p2p_inspector               => P2PInspector,
        withdrawal_routing_rules    => WithdrawalRoutingRules,
        p2p_transfer_routing_rules  => P2PTransferRoutingRules
    }.

decode_system_account_set(Identity, #domain_SystemAccountSet{accounts = Accounts}) ->
    maps:fold(
        fun(CurrencyRef, SystemAccount, Acc) ->
            #domain_CurrencyRef{symbolic_code = CurrencyID} = CurrencyRef,
            maps:put(
                CurrencyID,
                decode_system_account(SystemAccount, CurrencyID, Identity),
                Acc
            )
        end,
        #{},
        Accounts
    ).

decode_system_account(SystemAccount, CurrencyID, Identity) ->
    #domain_SystemAccount{
        settlement  = SettlementAccountID,
        subagent    = SubagentAccountID
    } = SystemAccount,
    #{
        settlement  => decode_account(SettlementAccountID, CurrencyID, Identity),
        subagent    => decode_account(SubagentAccountID, CurrencyID, Identity)
    }.

decode_account(AccountID, CurrencyID, Identity) when AccountID =/= undefined ->
    #{
        % FIXME
        id => Identity,
        identity => Identity,
        currency => CurrencyID,
        accounter_account_id => AccountID
    };
decode_account(undefined, _, _) ->
    undefined.
