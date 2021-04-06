-module(p2p_transfer_routing).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([prepare_routes/3]).
-export([get_provider/1]).
-export([get_terminal/1]).

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

-type provider_ref() :: ff_p2p_provider:provider_ref().
-type provider_id() :: ff_p2p_provider:id().

-type terminal_ref() :: ff_p2p_terminal:terminal_ref().
-type terminal_id() :: ff_p2p_terminal:id().

-type routing_rule_route() :: ff_routing_rule:route().
-type reject_context() :: ff_routing_rule:reject_context().

-type p2p_provision_terms() :: dmsl_domain_thrift:'P2PProvisionTerms'().
-type currency_selector() :: dmsl_domain_thrift:'CurrencySelector'().
-type cash_limit_selector() :: dmsl_domain_thrift:'CashLimitSelector'().
-type fee_selector() :: dmsl_domain_thrift:'FeeSelector'().

-spec prepare_routes(party_varset(), identity(), domain_revision()) -> {ok, [route()]} | {error, route_not_found}.
prepare_routes(VS, Identity, DomainRevision) ->
    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, DomainRevision),
    {Routes, RejectContext0} = ff_routing_rule:gather_routes(
        PaymentInstitution,
        p2p_transfer_routing_rules,
        VS,
        DomainRevision
    ),
    {ValidatedRoutes, RejectContext1} = filter_valid_routes(Routes, RejectContext0, VS, DomainRevision),
    case ValidatedRoutes of
        [] ->
            ff_routing_rule:log_reject_context(RejectContext1),
            logger:log(info, "Fallback to legacy method of routes gathering"),
            {ok, Providers} = ff_payment_institution:compute_p2p_transfer_providers(PaymentInstitution, VS),
            FilteredRoutes = filter_routes_legacy(Providers, VS, DomainRevision),
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

-spec filter_valid_routes([routing_rule_route()], reject_context(), party_varset(), domain_revision()) ->
    {[route()], reject_context()}.
filter_valid_routes(Routes, RejectContext, VS, DomainRevision) ->
    filter_valid_routes_(Routes, VS, {#{}, RejectContext}, DomainRevision).

filter_valid_routes_([], _, {Acc, RejectContext}, _DomainRevision) when map_size(Acc) == 0 ->
    {[], RejectContext};
filter_valid_routes_([], _, {Acc, RejectContext}, _DomainRevision) ->
    {convert_to_route(Acc), RejectContext};
filter_valid_routes_([Route | Rest], VS, {Acc0, RejectContext0}, DomainRevision) ->
    Terminal = maps:get(terminal, Route),
    TerminalRef = maps:get(terminal_ref, Route),
    TerminalID = TerminalRef#domain_TerminalRef.id,
    ProviderRef = Terminal#domain_Terminal.provider_ref,
    ProviderID = ProviderRef#domain_ProviderRef.id,
    Priority = maps:get(priority, Route, undefined),
    {Acc, RejectConext} =
        case validate_terms(ProviderRef, TerminalRef, VS, DomainRevision) of
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
    filter_valid_routes_(Rest, VS, {RejectConext, Acc}, DomainRevision).

-spec filter_routes_legacy([provider_id()], party_varset(), domain_revision()) -> [route()].
filter_routes_legacy(Providers, VS, DomainRevision) ->
    lists:foldr(
        fun(ProviderID, Acc) ->
            ProviderRef = ff_p2p_provider:ref(ProviderID),
            case validate_terms_legacy(ProviderRef, VS, DomainRevision) of
                {ok, valid} ->
                    [make_route(ProviderID, undefined) | Acc];
                {error, _Error} ->
                    Acc
            end
        end,
        [],
        Providers
    ).

-spec validate_terms_legacy(provider_ref(), party_varset(), domain_revision()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_terms_legacy(ProviderRef, VS, DomainRevision) ->
    case ff_party:compute_provider(ProviderRef, VS, DomainRevision) of
        {ok, Provider} ->
            case Provider of
                #domain_Provider{
                    terms = #domain_ProvisionTermSet{
                        wallet = #domain_WalletProvisionTerms{
                            p2p = ProviderTerms
                        }
                    }
                } ->
                    do_validate_terms(ProviderTerms, VS);
                _ ->
                    {error, {misconfiguration, {missing, p2p_terms}}}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec validate_terms(provider_ref(), terminal_ref(), party_varset(), domain_revision()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_terms(ProviderRef, TerminalRef, VS, DomainRevision) ->
    case ff_party:compute_provider_terminal_terms(ProviderRef, TerminalRef, VS, DomainRevision) of
        {ok, ProviderTerminalTermset} ->
            case ProviderTerminalTermset of
                #domain_ProvisionTermSet{
                    wallet = #domain_WalletProvisionTerms{
                        p2p = P2PProvisionTerms
                    }
                } ->
                    do_validate_terms(P2PProvisionTerms, VS);
                _ ->
                    {error, {misconfiguration, {missing, p2p_terms}}}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec do_validate_terms(p2p_provision_terms(), party_varset()) ->
    {ok, valid}
    | {error, Error :: term()}.
do_validate_terms(CombinedTerms, VS) ->
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

-spec validate_currencies(currency_selector(), party_varset()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_currencies({value, Currencies}, #{currency := CurrencyRef}) ->
    case ordsets:is_element(CurrencyRef, Currencies) of
        true ->
            {ok, valid};
        false ->
            {error, {terms_violation, {not_allowed_currency, {CurrencyRef, Currencies}}}}
    end;
validate_currencies(_NotReducedSelector, _VS) ->
    {error, {misconfiguration, {not_reduced_termset, currencies}}}.

-spec validate_fee_term_is_reduced(fee_selector(), party_varset()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_fee_term_is_reduced({value, _Fees}, _VS) ->
    {ok, valid};
validate_fee_term_is_reduced(_NotReducedSelector, _VS) ->
    {error, {misconfiguration, {not_reduced_termset, currencies}}}.

-spec validate_cash_limit(cash_limit_selector(), party_varset()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_cash_limit({value, CashRange}, #{cost := Cash}) ->
    case hg_cash_range:is_inside(Cash, CashRange) of
        within ->
            {ok, valid};
        _NotInRange ->
            {error, {terms_violation, {cash_range, {Cash, CashRange}}}}
    end;
validate_cash_limit(_NotReducedSelector, _VS) ->
    {error, {misconfiguration, {not_reduced_termset, cash_range}}}.

convert_to_route(ProviderTerminalMap) ->
    lists:foldl(
        fun({_, Data}, Acc) ->
            SortedRoutes = [make_route(P, T) || {P, T} <- lists:sort(Data)],
            SortedRoutes ++ Acc
        end,
        [],
        lists:keysort(1, maps:to_list(ProviderTerminalMap))
    ).
