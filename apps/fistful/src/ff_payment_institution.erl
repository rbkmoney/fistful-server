-module(ff_payment_institution).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type id() :: dmsl_domain_thrift:'ObjectID'().

-type domain_revision() :: ff_domain_config:revision().
-type party_varset() :: ff_varset:varset().

-type payment_institution() :: #{
    id := id(),
    system_accounts := dmsl_domain_thrift:'SystemAccountSetSelector'(),
    identity := binary(),
    withdrawal_providers := dmsl_domain_thrift:'ProviderSelector'(),
    withdrawal_routing_rules := dmsl_domain_thrift:'RoutingRules'(),
    payment_system => dmsl_domain_thrift:'PaymentSystemSelector'()
}.

-type payinst_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().

-type system_accounts() :: #{
    ff_currency:id() => system_account()
}.

-type system_account() :: #{
    settlement => ff_account:account(),
    subagent => ff_account:account()
}.

-export_type([id/0]).
-export_type([payinst_ref/0]).
-export_type([payment_institution/0]).

-export([id/1]).

-export([ref/1]).
-export([get/3]).
-export([withdrawal_providers/1]).
-export([system_accounts/2]).
-export([payment_system/1]).

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

-spec get(id(), party_varset(), domain_revision()) ->
    {ok, payment_institution()}
    | {error, payinst_not_found}.
get(PaymentInstitutionID, VS, DomainRevision) ->
    do(fun() ->
        PaymentInstitutionRef = ref(PaymentInstitutionID),
        PaymentInstitution = unwrap(ff_party:compute_payment_institution(PaymentInstitutionRef, VS, DomainRevision)),
        decode(PaymentInstitutionID, PaymentInstitution)
    end).

get_selector_value(Name, Selector) ->
    case Selector of
        {value, V} ->
            {ok, V};
        Ambiguous ->
            {error, {misconfiguration, {'Could not reduce selector to a value', {Name, Ambiguous}}}}
    end.

-spec payment_system(payment_institution()) ->
    {ok, ff_resource:payment_system() | undefined}
    | {error, term()}.
payment_system(#{payment_system := PaymentSystem}) ->
    case get_selector_value(payment_system, PaymentSystem) of
        {ok, #'domain_PaymentSystemRef'{id = ID}} ->
            {ok, #{id => ID}};
        {error, Error} ->
            {error, Error}
    end;
payment_system(_PaymentInstitution) ->
    {ok, undefined}.

-spec withdrawal_providers(payment_institution()) ->
    {ok, [ff_payouts_provider:id()]}
    | {error, term()}.
withdrawal_providers(#{withdrawal_providers := ProvidersSelector}) ->
    ct:print("Selector ~p~n", [ProvidersSelector]),
    case get_selector_value(withdrawal_providers, ProvidersSelector) of
        {ok, Providers} ->
            {ok, [ProviderID || #domain_ProviderRef{id = ProviderID} <- Providers]};
        {error, Error} ->
            {error, Error}
    end.

-spec system_accounts(payment_institution(), domain_revision()) ->
    {ok, system_accounts()}
    | {error, term()}.
system_accounts(PaymentInstitution, DomainRevision) ->
    #{
        identity := Identity,
        system_accounts := SystemAccountSetSelector
    } = PaymentInstitution,
    do(fun() ->
        SystemAccountSetRef = unwrap(get_selector_value(system_accounts, SystemAccountSetSelector)),
        SystemAccountSet = unwrap(ff_domain_config:object(DomainRevision, {system_account_set, SystemAccountSetRef})),
        decode_system_account_set(Identity, SystemAccountSet)
    end).

%%

decode(ID, #domain_PaymentInstitution{
    wallet_system_account_set = SystemAccounts,
    identity = Identity,
    withdrawal_providers = WithdrawalProviders,
    withdrawal_routing_rules = WithdrawalRoutingRules,
    payment_system = PaymentSystem
}) ->
    genlib_map:compact(#{
        id => ID,
        system_accounts => SystemAccounts,
        identity => Identity,
        withdrawal_providers => WithdrawalProviders,
        withdrawal_routing_rules => WithdrawalRoutingRules,
        payment_system => PaymentSystem
    }).

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
        settlement = SettlementAccountID,
        subagent = SubagentAccountID
    } = SystemAccount,
    #{
        settlement => decode_account(SettlementAccountID, CurrencyID, Identity),
        subagent => decode_account(SubagentAccountID, CurrencyID, Identity)
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
