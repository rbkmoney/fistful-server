-module(ff_withdrawal_routing).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([prepare_routes/3]).
-export([make_route/2]).
-export([get_provider/1]).
-export([get_terminal/1]).
-export([provision_terms/2]).
-export([merge_withdrawal_terms/2]).

-import(ff_pipeline, [do/1, unwrap/1]).

-type route() :: #{
    version := 1,
    provider_id := provider_id(),
    terminal_id => terminal_id(),
    provider_id_legacy => provider_id()
}.

-export_type([route/0]).

-type identity() :: ff_identity:identity_state().
-type domain_revision() :: ff_domain_config:revision().
-type party_varset() :: ff_varset:varset().

-type provider_ref() :: ff_payouts_provider:provider_ref().
-type provider_id() :: ff_payouts_provider:id().

-type terminal_ref() :: ff_payouts_terminal:terminal_ref().
-type terminal_id() :: ff_payouts_terminal:id().
-type terminal_priority() :: ff_payouts_terminal:terminal_priority().

-type routing_rule_route() :: ff_routing_rule:route().
-type reject_context() :: ff_routing_rule:reject_context().

-type withdrawal_provision_terms() :: dmsl_domain_thrift:'WithdrawalProvisionTerms'().
-type currency_selector() :: dmsl_domain_thrift:'CurrencySelector'().
-type cash_limit_selector() :: dmsl_domain_thrift:'CashLimitSelector'().

%%

-spec prepare_routes(party_varset(), identity(), domain_revision()) -> {ok, [route()]} | {error, route_not_found}.
prepare_routes(PartyVarset, Identity, DomainRevision) ->
    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, PartyVarset, DomainRevision),
    ct:pal(error, "WOLOLO> prepare_routes -> PaymentInstitution=~p~n", [PaymentInstitution]),
    {Routes, RejectContext0} = ff_routing_rule:gather_routes(
        PaymentInstitution,
        withdrawal_routing_rules,
        PartyVarset,
        DomainRevision
    ),
    {ValidatedRoutes, RejectContext1} = filter_valid_routes(Routes, RejectContext0, PartyVarset, DomainRevision),
    case ValidatedRoutes of
        [] ->
            ff_routing_rule:log_reject_context(RejectContext1),
            ct:pal(error, "WOLOLO> prepare_routes -> Fallback to legacy method of routes gathering"),
            logger:log(info, "Fallback to legacy method of routes gathering"),
            case ff_payment_institution:withdrawal_providers(PaymentInstitution) of
                {ok, Providers} ->
                    filter_routes_legacy(Providers, PartyVarset, DomainRevision);
                {error, {misconfiguration, _Details} = Error} ->
                    %% TODO: Do not interpret such error as an empty route list.
                    %% The current implementation is made for compatibility reasons.
                    %% Try to remove and follow the tests.
                    _ = logger:warning("Route search failed: ~p", [Error]),
                    {error, route_not_found}
            end;
        _ ->
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

-spec provision_terms(route(), domain_revision()) -> ff_maybe:maybe(withdrawal_provision_terms()).
provision_terms(Route, DomainRevision) ->
    ProviderID = get_provider(Route),
    {ok, Provider} = ff_payouts_provider:get(ProviderID, DomainRevision),
    ProviderTerms = ff_payouts_provider:provision_terms(Provider),
    TerminalTerms =
        case get_terminal(Route) of
            undefined ->
                undefined;
            TerminalID ->
                {ok, Terminal} = ff_payouts_terminal:get(TerminalID, DomainRevision),
                ff_payouts_terminal:provision_terms(Terminal)
        end,
    merge_withdrawal_terms(ProviderTerms, TerminalTerms).

-spec merge_withdrawal_terms(
    ff_payouts_provider:provision_terms() | undefined,
    ff_payouts_terminal:provision_terms() | undefined
) -> ff_maybe:maybe(withdrawal_provision_terms()).
merge_withdrawal_terms(
    #domain_WithdrawalProvisionTerms{
        currencies = PCurrencies,
        payout_methods = PPayoutMethods,
        cash_limit = PCashLimit,
        cash_flow = PCashflow
    },
    #domain_WithdrawalProvisionTerms{
        currencies = TCurrencies,
        payout_methods = TPayoutMethods,
        cash_limit = TCashLimit,
        cash_flow = TCashflow
    }
) ->
    #domain_WithdrawalProvisionTerms{
        currencies = ff_maybe:get_defined(TCurrencies, PCurrencies),
        payout_methods = ff_maybe:get_defined(TPayoutMethods, PPayoutMethods),
        cash_limit = ff_maybe:get_defined(TCashLimit, PCashLimit),
        cash_flow = ff_maybe:get_defined(TCashflow, PCashflow)
    };
merge_withdrawal_terms(ProviderTerms, TerminalTerms) ->
    ff_maybe:get_defined(TerminalTerms, ProviderTerms).

%%

-spec filter_valid_routes([routing_rule_route()], reject_context(), party_varset(), domain_revision()) ->
    {[route()], reject_context()}.
filter_valid_routes(Routes, RejectContext, PartyVarset, DomainRevision) ->
    filter_valid_routes_(Routes, PartyVarset, {#{}, RejectContext}, DomainRevision).

filter_valid_routes_([], _, {Acc, RejectContext}, _DomainRevision) when map_size(Acc) == 0 ->
    {[], RejectContext};
filter_valid_routes_([], _, {Acc, RejectContext}, _DomainRevision) ->
    {convert_to_route(Acc), RejectContext};
filter_valid_routes_([Route | Rest], PartyVarset, {Acc0, RejectContext0}, DomainRevision) ->
    Terminal = maps:get(terminal, Route),
    TerminalRef = maps:get(terminal_ref, Route),
    TerminalID = TerminalRef#domain_TerminalRef.id,
    ProviderRef = Terminal#domain_Terminal.provider_ref,
    ProviderID = ProviderRef#domain_ProviderRef.id,
    Priority = maps:get(priority, Route, undefined),
    {Acc, RejectContext} =
        case validate_terms(ProviderRef, TerminalRef, PartyVarset, DomainRevision) of
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
    filter_valid_routes_(Rest, PartyVarset, {Acc, RejectContext}, DomainRevision).

-spec filter_routes_legacy([provider_id()], party_varset(), domain_revision()) ->
    {ok, [route()]} | {error, route_not_found}.
filter_routes_legacy(Providers, PartyVarset, DomainRevision) ->
    do(fun() ->
        unwrap(filter_routes_legacy_(Providers, PartyVarset, DomainRevision, #{}))
    end).

filter_routes_legacy_([], _PartyVarset, _DomainRevision, Acc) when map_size(Acc) == 0 ->
    {error, route_not_found};
filter_routes_legacy_([], _PartyVarset, _DomainRevision, Acc) ->
    {ok, convert_to_route(Acc)};
filter_routes_legacy_([ProviderID | Rest], PartyVarset, DomainRevision, Acc0) ->
    ProviderRef = ff_payouts_provider:ref(ProviderID),
    {ok, TerminalsWithPriority} = compute_withdrawal_terminals_with_priority(ProviderRef, PartyVarset, DomainRevision),
    Acc =
        case get_valid_terminals_with_priority(TerminalsWithPriority, ProviderRef, PartyVarset, DomainRevision, []) of
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
    filter_routes_legacy_(Rest, PartyVarset, DomainRevision, Acc).

-spec compute_withdrawal_terminals_with_priority(provider_ref(), party_varset(), domain_revision()) ->
    {ok, [{terminal_id(), terminal_priority()}]} | {error, term()}.
compute_withdrawal_terminals_with_priority(ProviderRef, VS, DomainRevision) ->
    case ff_party:compute_provider(ProviderRef, VS, DomainRevision) of
        {ok, Provider} ->
            case Provider of
                #domain_Provider{
                    terminal = {value, Terminals}
                } ->
                    {ok, [
                        {TerminalID, Priority}
                     || #domain_ProviderTerminalRef{id = TerminalID, priority = Priority} <- Terminals
                    ]};
                _ ->
                    Error = {misconfiguration, {missing, terminal_selector}},
                    _ = logger:warning("Provider terminal search failed: ~p", [Error]),
                    {ok, []}
            end;
        {error, Error} ->
            {error, Error}
    end.

get_valid_terminals_with_priority([], _ProviderRef, _PartyVarset, _DomainRevision, Acc) ->
    Acc;
get_valid_terminals_with_priority([{TerminalID, Priority} | Rest], ProviderRef, PartyVarset, DomainRevision, Acc0) ->
    TerminalRef = ff_payouts_terminal:ref(TerminalID),
    Acc =
        case validate_terms(ProviderRef, TerminalRef, PartyVarset, DomainRevision) of
            {ok, valid} ->
                [{TerminalID, Priority} | Acc0];
            {error, _Error} ->
                Acc0
        end,
    get_valid_terminals_with_priority(Rest, ProviderRef, PartyVarset, DomainRevision, Acc).

-spec validate_terms(provider_ref(), terminal_ref(), party_varset(), domain_revision()) ->
    {ok, valid}
    | {error, Error :: term()}.
validate_terms(ProviderRef, TerminalRef, PartyVarset, DomainRevision) ->
    case ff_party:compute_provider_terminal_terms(ProviderRef, TerminalRef, PartyVarset, DomainRevision) of
        {ok, #domain_ProvisionTermSet{
            wallet = #domain_WalletProvisionTerms{
                withdrawals = WithdrawalProvisionTerms
            }
        }} ->
            do_validate_terms(WithdrawalProvisionTerms, PartyVarset);
        {error, Error} ->
            %% TODO: test for provision_termset_undefined error after routing migration
            {error, Error}
    end.

-spec do_validate_terms(withdrawal_provision_terms(), party_varset()) ->
    {ok, valid}
    | {error, Error :: term()}.
do_validate_terms(CombinedTerms, PartyVarset) ->
    do(fun() ->
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
    {ok, valid}
    | {error, Error :: term()}.
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

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec convert_to_route_test() -> _.
convert_to_route_test() ->
    ?assertEqual(
        [],
        convert_to_route(#{})
    ),
    ?assertEqual(
        [
            #{provider_id => 100, terminal_id => 2000, version => 1},
            #{provider_id => 100, terminal_id => 2001, version => 1},
            #{provider_id => 200, terminal_id => 2100, version => 1},
            #{provider_id => 200, terminal_id => 2101, version => 1},
            #{provider_id => 300, terminal_id => 2200, version => 1}
        ],
        convert_to_route(#{
            1000 => [{100, 2000}, {100, 2001}],
            900 => [{200, 2100}, {200, 2101}],
            100 => [{300, 2200}]
        })
    ).

-endif.
