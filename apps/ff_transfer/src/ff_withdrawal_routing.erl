-module(ff_withdrawal_routing).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([prepare_routes/3]).
-export([make_route/2]).
-export([get_provider/1]).
-export([get_terminal/1]).

-import(ff_pipeline, [do/1, unwrap/1]).

-type route() :: #{
    version := 1,
    provider_id := provider_id(),
    terminal_id => terminal_id(),
    provider_id_legacy => provider_id()
}.

-export_type([route/0]).

-type identity()        :: ff_identity:identity_state().
-type domain_revision() :: ff_domain_config:revision().
-type party_varset()    :: hg_selector:varset().

-type provider_id()  :: ff_payouts_provider:id().
-type provider()     :: ff_payouts_provider:provider().

-type terminal_id()  :: ff_payouts_terminal:id().
-type terminal()     :: ff_payouts_terminal:terminal().
-type terminal_priority() :: ff_payouts_terminal:terminal_priority().

-type withdrawal_provision_terms() :: dmsl_domain_thrift:'WithdrawalProvisionTerms'().
-type currency_selector() :: dmsl_domain_thrift:'CurrencySelector'().
-type cash_limit_selector() :: dmsl_domain_thrift:'CashLimitSelector'().

%%

-spec prepare_routes(party_varset(), identity(), domain_revision()) ->
    {ok, [route()]} | {error, route_not_found}.

prepare_routes(PartyVarset, Identity, DomainRevision) ->
    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, DomainRevision),
    case ff_payment_institution:compute_withdrawal_providers(PaymentInstitution, PartyVarset) of
        {ok, Providers}  ->
            filter_routes(Providers, PartyVarset);
        {error, {misconfiguration, _Details} = Error} ->
            %% TODO: Do not interpret such error as an empty route list.
            %% The current implementation is made for compatibility reasons.
            %% Try to remove and follow the tests.
            _ = logger:warning("Route search failed: ~p", [Error]),
            {error, route_not_found}
    end.

-spec make_route(provider_id(), terminal_id() | undefined) ->
    route().

make_route(ProviderID, TerminalID) ->
    genlib_map:compact(#{
        version => 1,
        provider_id => ProviderID,
        terminal_id => TerminalID
    }).

-spec get_provider(route()) ->
    provider_id().

get_provider(#{provider_id := ProviderID}) ->
    ProviderID.

-spec get_terminal(route()) ->
    ff_maybe:maybe(terminal_id()).

get_terminal(Route) ->
    maps:get(terminal_id, Route, undefined).

%%

-spec filter_routes([provider_id()], party_varset()) ->
    {ok, [route()]} | {error, route_not_found}.

filter_routes(Providers, PartyVarset) ->
    do(fun() ->
        unwrap(filter_routes_(Providers, PartyVarset, #{}))
    end).

filter_routes_([], _PartyVarset, Acc) when map_size(Acc) == 0->
    {error, route_not_found};
filter_routes_([], _PartyVarset, Acc) ->
    {ok, convert_to_route(Acc)};
filter_routes_([ProviderID | Rest], PartyVarset, Acc0) ->
    Provider = unwrap(ff_payouts_provider:get(ProviderID)),
    {ok, TerminalsWithPriority} = get_provider_terminals_with_priority(Provider, PartyVarset),
    Acc = case get_valid_terminals_with_priority(TerminalsWithPriority, Provider, PartyVarset, []) of
        [] ->
            Acc0;
        TPL ->
            lists:foldl(
                fun({TerminalID, Priority}, Acc1) ->
                    Terms = maps:get(Priority, Acc1, []),
                    maps:put(Priority, [{ProviderID, TerminalID} | Terms], Acc1)
                end,
                Acc0,
                TPL
            )
    end,
    filter_routes_(Rest, PartyVarset, Acc).

-spec get_provider_terminals_with_priority(provider(), party_varset()) ->
    {ok, [{terminal_id(), terminal_priority()}]}.

get_provider_terminals_with_priority(Provider, VS) ->
    case ff_payouts_provider:compute_withdrawal_terminals_with_priority(Provider, VS) of
        {ok, TerminalsWithPriority}  ->
            {ok, TerminalsWithPriority};
        {error, {misconfiguration, _Details} = Error} ->
            _ = logger:warning("Provider terminal search failed: ~p", [Error]),
            {ok, []}
    end.

get_valid_terminals_with_priority([], _Provider, _PartyVarset, Acc) ->
    Acc;
get_valid_terminals_with_priority([{TerminalID, Priority} | Rest], Provider, PartyVarset, Acc0) ->
    Terminal = unwrap(ff_payouts_terminal:get(TerminalID)),
    Acc = case validate_terms(Provider, Terminal, PartyVarset) of
        {ok, valid} ->
            [{TerminalID, Priority} | Acc0];
        {error, _Error} ->
            Acc0
    end,
    get_valid_terminals_with_priority(Rest, Provider, PartyVarset, Acc).

-spec validate_terms(provider(), terminal(), hg_selector:varset()) ->
    {ok, valid} |
    {error, Error :: term()}.

validate_terms(Provider, Terminal, PartyVarset) ->
    do(fun () ->
        ProviderTerms = ff_payouts_provider:provision_terms(Provider),
        TerminalTerms = ff_payouts_terminal:provision_terms(Terminal),
        _ = unwrap(assert_terms_defined(TerminalTerms, ProviderTerms)),
        CombinedTerms = merge_withdrawal_terms(ProviderTerms, TerminalTerms),
        unwrap(validate_combined_terms(CombinedTerms, PartyVarset))
    end).

assert_terms_defined(undefined, undefined) ->
    {error, terms_undefined};
assert_terms_defined(_, _) ->
    {ok, valid}.

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

-spec validate_selectors_defined(withdrawal_provision_terms()) ->
    {ok, valid} |
    {error, Error :: term()}.

validate_selectors_defined(Terms) ->
    Selectors = [
        Terms#domain_WithdrawalProvisionTerms.currencies,
        Terms#domain_WithdrawalProvisionTerms.payout_methods,
        Terms#domain_WithdrawalProvisionTerms.cash_limit,
        Terms#domain_WithdrawalProvisionTerms.cash_flow
    ],
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

convert_to_route(ProviderTerminalMap) ->
    lists:foldl(
        fun({_, Data}, Acc) ->
            SortedRoutes = [make_route(P, T) || {P, T} <- lists:sort(Data)],
            SortedRoutes ++ Acc
        end,
        [],
        lists:keysort(1, maps:to_list(ProviderTerminalMap))
    ).
