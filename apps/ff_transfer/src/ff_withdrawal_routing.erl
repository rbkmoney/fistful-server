-module(ff_withdrawal_routing).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([prepare_route/3]).
-export([get_provider/1]).
-export([get_terminal/1]).

-import(ff_pipeline, [do/1, unwrap/1]).

-type route() :: #{
    provider_id := provider_id(),
    terminal_id => terminal_id()
}.

-export_type([route/0]).

-type id()              :: binary().
-type identity()        :: ff_identity:identity_state().
-type domain_revision() :: ff_domain_config:revision().
-type party_varset()    :: hg_selector:varset().

-type provider_id()  :: pos_integer() | id().
-type provider()     :: ff_payouts_provider:withdrawal_provider().

-type terminal_id()  :: ff_payouts_terminal:id().
-type terminal()     :: ff_payouts_terminal:withdrawal_terminal().

-type withdrawal_provision_terms() :: dmsl_domain_thrift:'WithdrawalProvisionTerms'().
-type currency_selector() :: dmsl_domain_thrift:'CurrencySelector'().
-type cash_limit_selector() :: dmsl_domain_thrift:'CashLimitSelector'().

%%

-spec prepare_route(party_varset(), identity(), domain_revision()) ->
    {ok, route()} | {error, route_not_found}.

prepare_route(PartyVarset, Identity, DomainRevision) ->
    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, DomainRevision),
    case ff_payment_institution:compute_withdrawal_providers(PaymentInstitution, PartyVarset) of
        {ok, Providers}  ->
            choose_route(Providers, PartyVarset);
        {error, {misconfiguration, _Details} = Error} ->
            %% TODO: Do not interpret such error as an empty route list.
            %% The current implementation is made for compatibility reasons.
            %% Try to remove and follow the tests.
            _ = logger:warning("Route search failed: ~p", [Error]),
            {error, route_not_found}
    end.

-spec get_provider(route()) ->
    provider_id().

get_provider(#{provider_id := ProviderID}) ->
    ProviderID.

-spec get_terminal(route()) ->
    ff_maybe:maybe(terminal_id()).

get_terminal(Route) ->
    maps:get(terminal_id, Route, undefined).

%%

-spec choose_route([provider_id()], party_varset()) ->
    {ok, route()} | {error, route_not_found}.

choose_route(Providers, PartyVarset) ->
    do(fun() ->
        unwrap(choose_route_(Providers, PartyVarset))
    end).

choose_route_([], _PartyVarset) ->
    {error, route_not_found};
choose_route_([ProviderID | Rest], PartyVarset) ->
    Provider = unwrap(ff_payouts_provider:get(ProviderID)),
    {ok, Terminals} = get_provider_terminals(Provider, PartyVarset),
    case get_valid_terminals(Terminals, Provider, PartyVarset, []) of
        [TerminalID | _] ->
            {ok, make_route(ProviderID, TerminalID)};
        [] ->
            choose_route_(Rest, PartyVarset)
    end.

-spec get_provider_terminals(provider(), party_varset()) ->
    {ok, [terminal_id()]}.

get_provider_terminals(Provider, VS) ->
    case ff_payouts_provider:compute_withdrawal_terminals(Provider, VS) of
        {ok, Terminals}  ->
            {ok, Terminals};
        {error, {misconfiguration, _Details} = Error} ->
            _ = logger:warning("Provider terminal search failed: ~p", [Error]),
            {ok, []}
    end.

get_valid_terminals([], _Provider, _PartyVarset, Acc) ->
    Acc;
get_valid_terminals([TerminalID | Rest], Provider, PartyVarset, Acc0) ->
    Terminal = unwrap(ff_payouts_terminal:get(TerminalID)),
    Acc = case validate_terms(Provider, Terminal, PartyVarset) of
        {ok, valid} ->
            [TerminalID | Acc0];
        {error, _Error} ->
            Acc0
    end,
    get_valid_terminals(Rest, Provider, PartyVarset, Acc).

-spec validate_terms(provider(), terminal(), hg_selector:varset()) ->
    {ok, valid} |
    {error, Error :: term()}.

validate_terms(Provider, Terminal, PartyVarset) ->
    do(fun () ->
        ProviderTerms = ff_payouts_provider:terms(Provider),
        TerminalTerms = ff_payouts_terminal:terms(Terminal),
        _ = unwrap(assert_terms_defined(ProviderTerms, TerminalTerms)),
        CombinedTerms = get_combined_terms(ProviderTerms, TerminalTerms),
        unwrap(validate_combined_terms(CombinedTerms, PartyVarset))
    end).

assert_terms_defined(undefined, undefined) ->
    %% Missing withdrawal_terms mean that operations for this provider/terminal are forbidden
    {error, terms_undefined};
assert_terms_defined(_, _) ->
    ok.

-spec validate_combined_terms(withdrawal_provision_terms(), hg_selector:varset()) ->
    {ok, valid} |
    {error, Error :: term()}.

validate_combined_terms(CombinedTerms, PartyVarset) ->
    do(fun () ->
        #domain_WithdrawalProvisionTerms{
            currencies = CurrenciesSelector,
            %% PayoutMethodsSelector is useless for withdrawals
            %% so we can just ignore it
            %% payout_methods = PayoutMethodsSelector,
            cash_limit = CashLimitSelector
        } = CombinedTerms,
        valid = unwrap(validate_selectors_defined(CombinedTerms)),
        valid = unwrap(validate_currencies(CurrenciesSelector, PartyVarset)),
        valid = unwrap(validate_cash_limit(CashLimitSelector, PartyVarset))
    end).

-spec get_combined_terms(Terms, Terms) -> withdrawal_provision_terms() when
    Terms :: ff_maybe:maybe(withdrawal_provision_terms()).

get_combined_terms(ProviderTerms, TerminalTerms) ->
    merge_withdrawal_terms(ProviderTerms, TerminalTerms).

-spec validate_selectors_defined(withdrawal_provision_terms()) ->
    {ok, valid} |
    {error, Error :: term()}.

validate_selectors_defined(#domain_WithdrawalProvisionTerms{
    currencies = CurrenciesSelector,
    payout_methods = PayoutMethodsSelector,
    cash_limit = CashLimitSelector,
    cash_flow = CashFlowSelector
}) ->
    Selectors = [CurrenciesSelector, PayoutMethodsSelector, CashLimitSelector, CashFlowSelector],
    case lists:any(fun(Selector) -> Selector =:= undefined end, Selectors) of
        false ->
            {ok, valid};
        true ->
            {error, terms_undefined}
    end.

-spec validate_currencies(currency_selector(), hg_selector:varset()) ->
    {ok, valid} |
    {error, Error :: term()}.

validate_currencies(CurrenciesSelector, #{currency := CurrencyRef} = VS) ->
    Currencies = unwrap(hg_selector:reduce_to_value(CurrenciesSelector, VS)),
    case ordsets:is_element(CurrencyRef, Currencies) of
        true ->
            {ok, valid};
        false ->
            {error, {terms_violation, {not_allowed_currency, {CurrencyRef, Currencies}}}}
    end.

-spec validate_cash_limit(cash_limit_selector(), hg_selector:varset()) ->
    {ok, valid} |
    {error, Error :: term()}.

validate_cash_limit(CashLimitSelector, #{cost := Cash} = VS) ->
    CashRange = unwrap(hg_selector:reduce_to_value(CashLimitSelector, VS)),
    case hg_cash_range:is_inside(Cash, CashRange) of
        within ->
            {ok, valid};
        _NotInRange  ->
            {error, {terms_violation, {cash_range, {Cash, CashRange}}}}
    end.

-spec make_route(provider_id(), terminal_id()) ->
    route().

make_route(ProviderID, TerminalID) ->
    #{
        provider_id => ProviderID,
        terminal_id => TerminalID
    }.

merge_withdrawal_terms(
    #domain_WithdrawalProvisionTerms{
        currencies     = PCurrencies,
        payout_methods = PPayoutMethods,
        cash_limit     = PCashLimit,
        cash_flow      = PCashflow
    },
    #domain_WithdrawalProvisionTerms{
        currencies     = TCurrencies,
        payout_methods = TPayoutMethods,
        cash_limit     = TCashLimit,
        cash_flow      = TCashflow
    }
) ->
    #domain_WithdrawalProvisionTerms{
        currencies      = ff_maybe:get_defined(TCurrencies,    PCurrencies),
        payout_methods  = ff_maybe:get_defined(TPayoutMethods, PPayoutMethods),
        cash_limit      = ff_maybe:get_defined(TCashLimit,     PCashLimit),
        cash_flow       = ff_maybe:get_defined(TCashflow,      PCashflow)
    };
merge_withdrawal_terms(ProviderTerms, TerminalTerms) ->
    ff_maybe:get_defined(TerminalTerms, ProviderTerms).
