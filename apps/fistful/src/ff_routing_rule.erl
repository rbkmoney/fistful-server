-module(ff_routing_rule).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%%
-define(const(Bool), {constant, Bool}).

%%
-type id() :: dmsl_domain_thrift:'ObjectID'().
% -type ruleset_ref() :: dmsl_domain_thrift:'RoutingRulesetRef'().
-type provider() :: dmsl_domain_thrift:'Provider'().
-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type terminal() :: dmsl_domain_thrift:'Terminal'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().

-type payment_institution() :: ff_payment_institution:payment_institution().

-type varset() :: hg_selector:varset().
-type revision() :: ff_domain_config:revision().
-type routing_rule_tag() :: p2p_transfer_routing_rules | withdrawal_routing_rules.

-type routing_ruleset() :: #{
    name        := binary(),
    description := binary(),
    decisions   := dmsl_domain_thrift:'RoutingDecisions'()
}.

-type route() :: {provider_with_ref(), weighted_terminal()}.
-type provider_with_ref() :: {id(), provider_ref(), provider()}.

-type weighted_terminal() :: {terminal_ref(), terminal(), terminal_priority()}.
-type terminal_priority() :: {terminal_priority_rating(), terminal_priority_weight()}.

-type terminal_priority_rating() :: integer().
-type terminal_priority_weight() :: integer().

-type reject_context() :: #{
    varset := varset(),
    rejected_providers := list(rejected_provider()),
    rejected_routes := list(rejected_route())
}.

-type rejected_provider() :: {provider_ref(), Reason :: term()}.
-type rejected_route() :: {provider_ref(), terminal_ref(), Reason :: term()}.

%%
-export_type([route/0]).

-export([gather_routes/4]).
-export([choose_route/1]).

%% Pipeline

-import(ff_pipeline, [unwrap/1]).

%%

-spec gather_routes(payment_institution(), varset(), revision(), routing_rule_tag()) ->
    {[route()], reject_context()}.
gather_routes(#{p2p_transfer_routing_rules := undefined}, VS, _Revision, _RoutingRuleTag) ->
    {[], #{varset => VS, rejected_providers => [], rejected_routes => []}};
gather_routes(PaymentInstitution, VS, Revision, RoutingRuleTag) ->
    RejectedContext = #{
        varset => VS,
        rejected_providers => [],
        rejected_routes => []
    },
    #{p2p_transfer_routing_rules := RoutingRules} = PaymentInstitution,
    #{policies := Policies} = RoutingRules,
    #{prohibitions := Prohibitions} = RoutingRules,
    PermitRuleSet = get_rule_set(Policies, Revision, RoutingRuleTag),
    DenyRuleSet = get_rule_set(Prohibitions, Revision, RoutingRuleTag),
    Candidates = reduce(PermitRuleSet, VS, Revision, RoutingRuleTag),
    Routes = collect_routes(Candidates, VS, Revision),
    ProhibitionsTable = get_table_prohibitions(DenyRuleSet, VS, Revision, RoutingRuleTag),
    {AcceptedRoutes, RejectedRoutes} = prohibited_routes_filter(Routes, ProhibitionsTable),
    {AcceptedRoutes, RejectedContext#{rejected_routes => RejectedRoutes}}.

-spec choose_route([route()]) ->
    {ok, route()} | {error, route_not_found}.
choose_route([]) ->
    {error, route_not_found};
choose_route([Route]) ->
    {ok, Route};
choose_route([First | Rest]) ->
    Route = lists:foldl(
        fun(RouteIn, CurrentRouteChosen) ->
            max(RouteIn, CurrentRouteChosen)
        end,
        First,
        Rest
    ),
    {ok, Route}.

%%
get_rule_set(Ref, DomainRevision, RoutingRuleTag) ->
    RuleSet = unwrap(ff_domain_config:object(DomainRevision, {RoutingRuleTag, Ref})),
    decode_rule_set(RuleSet).

get_terminal_ref(Candidate) ->
    Candidate#domain_RoutingCandidate.terminal.

get_description(Candidate) ->
    Candidate#domain_RoutingCandidate.description.

get_table_prohibitions(DenyRuleSet, VS, Revision, RoutingRuleTag) ->
    Candidates = reduce(DenyRuleSet, VS, Revision, RoutingRuleTag),
    lists:foldl(
        fun(C, Acc) ->
            Acc#{get_terminal_ref(C) => get_description(C)}
        end,
        #{},
        Candidates
    ).

decode_rule_set(RuleSet) ->
    #domain_RoutingRuleset{
        name = Name,
        description = Description,
        decisions = Decisions
    } = RuleSet,
    #{
        name        => Name,
        description => Description,
        decisions   => Decisions
    }.

-spec reduce(routing_ruleset(), varset(), revision(), routing_rule_tag()) ->
    [route()].
reduce(RuleSet, VS, Revision, RoutingRuleTag) ->
    #{decisions := Decisions} = RuleSet,
    reduce_decisions(Decisions, VS, Revision, RoutingRuleTag).

reduce_decisions({_, []}, _VS, _Revision, _RoutingRuleTag) ->
    [];
reduce_decisions({delegates, Delegates}, VS, Rev, RoutingRuleTag) ->
    reduce_delegates_decision(Delegates, VS, Rev, RoutingRuleTag);
reduce_decisions({candidates, Candidates}, VS, Rev, RoutingRuleTag) ->
    reduce_candidates_decision(Candidates, VS, Rev, RoutingRuleTag).

reduce_delegates_decision([], _VS, _Rev, _RoutingRuleTag) ->
    [];
reduce_delegates_decision([D | Delegates], VS, Rev, RoutingRuleTag) ->
    Predicate = D#domain_RoutingDelegate.allowed,
    RulesetRef = D#domain_RoutingDelegate.ruleset,
    case hg_selector:reduce_predicate(Predicate, VS) of
        ?const(false) ->
            reduce_delegates_decision(Delegates, VS, Rev, RoutingRuleTag);
        ?const(true) ->
            RuleSet = get_rule_set(RulesetRef, Rev, RoutingRuleTag),
            reduce(RuleSet, VS, Rev, RoutingRuleTag);
        _ ->
            logger:warning(
                "Routing rule misconfiguration, can't reduce decision. Predicate: ~p~n Varset:~n~p",
                [Predicate, VS]
            ),
            []
    end.

reduce_candidates_decision(Candidates, VS, _Rev, _RoutingRuleTag) ->
    lists:foldl(
        fun(C, AccIn) ->
            Predicate = C#domain_RoutingCandidate.allowed,
            case hg_selector:reduce_predicate(Predicate, VS) of
                ?const(false) ->
                    AccIn;
                ?const(true) ->
                    [C | AccIn];
                _ ->
                    logger:warning(
                        "Routing rule misconfiguration, can't reduce decision. Predicate: ~p~nVarset:~n~p",
                        [Predicate, VS]
                    ),
                    AccIn
            end
        end,
        [],
        Candidates
    ).

collect_routes(Candidates, _VS, Revision) ->
    lists:foldl(
        fun(Candidate, Collected) ->
            #domain_RoutingCandidate{
                terminal = TerminalRef,
                priority = Priority,
                weight = Weight
            } = Candidate,
            Terminal = unwrap(ff_domain_config:object(Revision, {terminal, TerminalRef})),
            #domain_Terminal{
                provider_ref = ProviderRef
            } = Terminal,
            #domain_ProviderRef{id = ProviderID} = ProviderRef,
            Provider = unwrap(ff_domain_config:object(Revision, {provider, ProviderRef})),
            [{{ProviderID, ProviderRef, Provider}, {TerminalRef, Terminal, {Priority, Weight}}} | Collected]
        end,
        [],
        Candidates
    ).

prohibited_routes_filter(Routes, Prohibitions) ->
    lists:foldl(
        fun(Route, {Accepted, RejectedIn}) ->
            {{_, ProviderRef, _}, {TerminalRef, _, _}} = Route,
            case maps:find(TerminalRef, Prohibitions) of
                error ->
                    {[Route | Accepted], RejectedIn};
                {ok, Description} ->
                    RejectedOut = [{ProviderRef, TerminalRef, {'RoutingRule', Description}} | RejectedIn],
                    {Accepted, RejectedOut}
            end
        end,
        {[], []},
        Routes
    ).

%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-type testcase() :: {_, fun()}.

-spec record_comparsion_test() -> [testcase()].
record_comparsion_test() ->
    Bigger =
        {#route_scores{
                availability_condition = 1,
                availability = 0.5,
                conversion_condition = 1,
                conversion = 0.5,
                priority_rating = 1,
                random_condition = 1
            },
            {42, 42}},
    Smaller =
        {#route_scores{
                availability_condition = 0,
                availability = 0.1,
                conversion_condition = 1,
                conversion = 0.5,
                priority_rating = 1,
                random_condition = 1
            },
            {99, 99}},
    Bigger = select_better_route(Bigger, Smaller).

-endif.
