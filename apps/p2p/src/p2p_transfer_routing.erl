-module(p2p_transfer_routing).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([prepare_routes/3]).
-export([get_provider/1]).
-export([get_terminal/1]).
-export([make_route/2]).

-import(ff_pipeline, [do/1, unwrap/1]).

-type route() :: #{
    version := 1,
    provider_id := provider_id(),
    terminal_id => terminal_id()
}.

-export_type([route/0]).

-type identity() :: ff_identity:identity_state().
-type domain_revision() :: ff_domain_config:revision().
-type party_varset() :: hg_selector:varset().

-type provider_id() :: ff_p2p_provider:id().
-type provider() :: ff_p2p_provider:provider().

-type terminal_id() :: ff_p2p_terminal:id().
-type terminal() :: ff_p2p_terminal:terminal().

-type routing_rule_route() :: ff_routing_rule:route().
-type reject_context() :: ff_routing_rule:reject_context().

-type p2p_provision_terms() :: dmsl_domain_thrift:'P2PProvisionTerms'().

-spec prepare_routes(party_varset(), identity(), domain_revision()) -> {ok, [route()]} | {error, route_not_found}.
prepare_routes(PartyVarset, Identity, DomainRevision) ->
    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, DomainRevision),
    {Routes, RejectContext0} = ff_routing_rule:gather_routes(
        PaymentInstitution,
        p2p_transfer_routing_rules,
        PartyVarset,
        DomainRevision
    ),
    {ValidatedRoutes, RejectContext1} = filter_valid_routes(Routes, RejectContext0, PartyVarset),
    case ValidatedRoutes of
        [] ->
            ff_routing_rule:log_reject_context(RejectContext1),
            logger:log(info, "Fallback to legacy method of routes gathering"),
            {ok, Providers} = ff_payment_institution:compute_p2p_transfer_providers(PaymentInstitution, PartyVarset),
            FilteredRoutes = filter_routes_legacy(Providers, PartyVarset),
            case FilteredRoutes of
                [] ->
                    {error, route_not_found};
                [_Route | _] ->
                    {ok, FilteredRoutes}
            end;
        [_Route | _] ->
            {ok, ValidatedRoutes}
    end.

-spec make_route(provider_id(), terminal_id() | undefined) -> route().
make_route(ProviderID, TerminalID) ->
    genlib_map:compact(#{
        version => 1,
        provider_id => ProviderID,
        terminal_id => TerminalID
    }).

-spec get_provider(route()) -> provider_id().
get_provider(#{provider_id := ProviderID}) ->
    ProviderID.

-spec get_terminal(route()) -> ff_maybe:maybe(terminal_id()).
get_terminal(Route) ->
    maps:get(terminal_id, Route, undefined).

-spec filter_valid_routes([routing_rule_route()], reject_context(), party_varset()) -> {[route()], reject_context()}.
filter_valid_routes(Routes, RejectContext, PartyVarset) ->
    filter_valid_routes_(Routes, PartyVarset, {#{}, RejectContext}).

filter_valid_routes_([], _, {Acc, RejectContext}) when map_size(Acc) == 0 ->
    {[], RejectContext};
filter_valid_routes_([], _, {Acc, RejectContext}) ->
    {convert_to_route(Acc), RejectContext};
filter_valid_routes_([Route | Rest], PartyVarset, {Acc0, RejectContext0}) ->
    Terminal = maps:get(terminal, Route),
    TerminalRef = maps:get(terminal_ref, Route),
    TerminalID = TerminalRef#domain_TerminalRef.id,
    ProviderRef = Terminal#domain_Terminal.provider_ref,
    ProviderID = ProviderRef#domain_ProviderRef.id,
    Priority = maps:get(priority, Route, undefined),
    {ok, P2PTerminal} = ff_p2p_terminal:get(TerminalID),
    {ok, P2PProvider} = ff_p2p_provider:get(ProviderID),
    {Acc, RejectConext} =
        case validate_terms(P2PProvider, P2PTerminal, PartyVarset) of
            {ok, valid} ->
                Terms = maps:get(Priority, Acc0, []),
                Acc1 = maps:put(Priority, [{ProviderID, TerminalID} | Terms], Acc0),
                {Acc1, RejectContext0};
            {error, RejectReason} ->
                RejectedRoutes0 = maps:get(rejected_routes, RejectContext0),
                RejectedRoutes1 = [{ProviderRef, TerminalRef, RejectReason} | RejectedRoutes0],
                RejectContext1 = maps:put(rejected_routes, RejectedRoutes1, RejectContext0),
                {Acc0, RejectContext1}
        end,
    filter_valid_routes_(Rest, PartyVarset, {RejectConext, Acc}).

-spec filter_routes_legacy([provider_id()], party_varset()) -> [route()].
filter_routes_legacy(Providers, VS) ->
    lists:foldr(
        fun(ProviderID, Acc) ->
            {ok, Provider} = ff_p2p_provider:get(ProviderID),
            case validate_terms_legacy(Provider, VS) of
                {ok, valid} ->
                    [make_route(ProviderID, undefined) | Acc];
                {error, _Error} ->
                    Acc
            end
        end,
        [],
        Providers
    ).

-spec validate_terms_legacy(provider(), party_varset()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_terms_legacy(Provider, VS) ->
    do(fun() ->
        ProviderTerms = ff_p2p_provider:provision_terms(Provider),
        unwrap(validate_combined_terms(ProviderTerms, VS))
    end).

-spec validate_terms(provider(), terminal(), party_varset()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_terms(Provider, Terminal, VS) ->
    do(fun() ->
        ProviderTerms = ff_p2p_provider:provision_terms(Provider),
        TerminalTerms = ff_p2p_terminal:provision_terms(Terminal),
        _ = unwrap(assert_terms_defined(TerminalTerms, ProviderTerms)),
        CombinedTerms = merge_p2p_terms(ProviderTerms, TerminalTerms),
        unwrap(validate_combined_terms(CombinedTerms, VS))
    end).

assert_terms_defined(undefined, undefined) ->
    {error, terms_undefined};
assert_terms_defined(_, _) ->
    {ok, valid}.

-spec validate_combined_terms(p2p_provision_terms(), party_varset()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_combined_terms(CombinedTerms, VS) ->
    do(fun() ->
        #domain_P2PProvisionTerms{
            currencies = CurrenciesSelector,
            fees = FeeSelector,
            cash_limit = CashLimitSelector
        } = CombinedTerms,
        valid = unwrap(validate_currencies(CurrenciesSelector, VS)),
        valid = unwrap(validate_fee_term_is_reduced(FeeSelector, VS)),
        valid = unwrap(validate_cash_limit(CashLimitSelector, VS))
    end).

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
        _NotInRange ->
            {error, {terms_violation, {cash_range, {Cash, CashRange}}}}
    end.

-spec merge_p2p_terms(
    ff_p2p_provider:provision_terms() | undefined,
    ff_p2p_terminal:provision_terms() | undefined
) -> ff_maybe:maybe(p2p_provision_terms()).
merge_p2p_terms(
    #domain_P2PProvisionTerms{
        currencies = PCurrencies,
        fees = PFees,
        cash_limit = PCashLimit,
        cash_flow = PCashflow
    },
    #domain_P2PProvisionTerms{
        currencies = TCurrencies,
        fees = TFees,
        cash_limit = TCashLimit,
        cash_flow = TCashflow
    }
) ->
    #domain_P2PProvisionTerms{
        currencies = ff_maybe:get_defined(TCurrencies, PCurrencies),
        fees = ff_maybe:get_defined(PFees, TFees),
        cash_limit = ff_maybe:get_defined(TCashLimit, PCashLimit),
        cash_flow = ff_maybe:get_defined(TCashflow, PCashflow)
    };
merge_p2p_terms(ProviderTerms, TerminalTerms) ->
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
