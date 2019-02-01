-module(ff_payment_institution).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-type id()       :: dmsl_domain_thrift:'ObjectID'().
-type payment_institution() :: #{
    id              := id(),
    system_accounts := dmsl_domain_thrift:'SystemAccountSetSelector'(),
    identity        := binary(),
    providers       := dmsl_domain_thrift:'WithdrawalProviderSelector'()
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
-export([get/1]).
-export([compute_withdrawal_provider/2]).
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

-spec get(id()) ->
    {ok, payment_institution()} |
    {error, notfound}.

get(ID) ->
    do(fun () ->
        PaymentInstitution = unwrap(ff_domain_config:object({payment_institution, ref(ID)})),
        decode(ID, PaymentInstitution)
    end).

-spec compute_withdrawal_provider(payment_institution(), hg_selector:varset()) ->
    {ok, ff_payouts_provider:id()} | {error, term()}.

compute_withdrawal_provider(#{providers := ProviderSelector}, VS) ->
    do(fun() ->
        Providers = unwrap(hg_selector:reduce_to_value(ProviderSelector, VS)),
        %% TODO choose wizely one of them
        [#domain_WithdrawalProviderRef{id = ProviderID} | _] = Providers,
        ProviderID
    end).

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
    withdrawal_providers = Providers
}) ->
    #{
        id              => ID,
        system_accounts => SystemAccounts,
        identity        => Identity,
        providers       => Providers
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
