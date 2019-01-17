-module(ff_payment_institution).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-type id()       :: dmsl_domain_thrift:'ObjectID'().
-type payment_institution() :: #{
    id              := id(),
    system_accounts := dmsl_domain_thrift:'SystemAccountSetSelector'(),
    identity        := binary(),
    providers       := dmsl_domain_thrift:'PayoutsProviderSelector'()
}.

-type payinst_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().

-export_type([id/0]).
-export_type([payment_institution/0]).

-export([id/1]).

-export([ref/1]).
-export([get/1]).
-export([compute_payouts_provider/2]).
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

-spec compute_payouts_provider(payment_institution(), hg_selector:varset()) ->
    {ok, ff_payouts_provider:id()} | {error, term()}.

compute_payouts_provider(#{providers := Providers}, VS) ->
    do(fun() ->
        PayoutsProviders = unwrap(hg_selector:reduce_to_value(Providers, VS)),
        %% TODO choose wizely one of them
        [#domain_PayoutsProviderRef{id = ProviderID} | _] = PayoutsProviders,
        ProviderID
    end).

-spec compute_system_accounts(payment_institution(), hg_selector:varset()) ->
    {ok, ff_withdrawal_provider:accounts()} | {error, term()}.

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
    payout_providers = Providers
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
            #domain_SystemAccount{settlement = AccountID} = SystemAccount,
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
