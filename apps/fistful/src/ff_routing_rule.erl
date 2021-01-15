-module(ff_routing_rule).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([gather_routes/4]).
-export([get_providers/1]).

-type id() :: dmsl_domain_thrift:'ObjectID'().
-type payment_institution() :: ff_payment_institution:payment_institution().
-type routing_rules() :: dmsl_domain_thrift:'RoutingRules'().
-type routing_ruleset_ref() :: dmsl_domain_thrift:'RoutingRulesetRef'().
-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().
-type varset() :: hg_selector:varset().
-type revision() :: ff_domain_config:revision().
-type routing_rule_tag() :: p2p_transfer_routing_rules | withdrawal_routing_rules.
-type candidate() :: dmsl_domain_thrift:'RoutingCandidate'().
-type candidate_description() :: binary() | undefined.

-type route() :: #{
    provider => dmsl_domain_thrift:'Provider'(),
    provider_ref => provider_ref(),
    provider_id => id(),
    terminal := dmsl_domain_thrift:'Terminal'(),
    terminal_ref := terminal_ref(),
    terminal_id := id()
}.

-type reject_context() :: #{
    varset := varset(),
    rejected_providers := [rejected_provider()],
    rejected_routes := [rejected_route()]
}.

-type rejected_provider() :: {provider_ref(), Reason :: term()}.
-type rejected_route() :: {provider_ref(), terminal_ref(), Reason :: term()}.

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec gather_routes(payment_institution(), routing_rule_tag(), varset(), revision()) -> {[route()], reject_context()}.
gather_routes(PaymentInstitution, RoutingRuleTag, VS, Revision) ->
    RejectedContext = #{
        varset => VS,
        rejected_providers => [],
        rejected_routes => []
    },
    case do_gather_routes(PaymentInstitution, RoutingRuleTag, VS, Revision) of
        {ok, {AcceptedRoutes, RejectedRoutes}} ->
            {AcceptedRoutes, RejectedContext#{rejected_routes => RejectedRoutes}};
        {error, not_found} ->
            {[], RejectedContext};
        {error, unreduced} ->
            {[], RejectedContext}
    end.

-spec do_gather_routes(payment_institution(), routing_rule_tag(), varset(), revision()) ->
    {ok, {[route()], [route()]}}
    | {error, not_found}
    | {error, unreduced}.
do_gather_routes(PaymentInstitution, RoutingRuleTag, VS, Revision) ->
    do(fun() ->
        RoutingRules = unwrap(get_routing_rules(PaymentInstitution, RoutingRuleTag)),
        Policies = RoutingRules#domain_RoutingRules.policies,
        Prohibitions = RoutingRules#domain_RoutingRules.prohibitions,
        PermitCandidates = unwrap(compute_routing_ruleset(Policies, VS, Revision)),
        DenyCandidates = unwrap(compute_routing_ruleset(Prohibitions, VS, Revision)),
        {AcceptedRoutes, RejectedRoutes} = prohibited_candidates_filter(
            PermitCandidates,
            DenyCandidates,
            Revision
        ),
        {AcceptedRoutes, RejectedRoutes}
    end).

-spec get_routing_rules(payment_institution(), routing_rule_tag()) -> {ok, routing_rules()} | {error, not_found}.
get_routing_rules(PaymentInstitution, RoutingRuleTag) ->
    RoutingRules = maps:get(RoutingRuleTag, PaymentInstitution, undefined),
    case RoutingRules of
        undefined ->
            logger:warning("Routing rules not found. Routing rule tag: ~p~n, institution: ~p~n", [
                RoutingRuleTag,
                PaymentInstitution
            ]),
            {error, not_found};
        RoutingRules ->
            {ok, RoutingRules}
    end.

-spec compute_routing_ruleset(routing_ruleset_ref(), varset(), revision()) -> {ok, [candidate()]} | {error, unreduced}.
compute_routing_ruleset(RulesetRef, VS, Revision) ->
    {ok, RuleSet} = ff_party:compute_routing_ruleset(RulesetRef, VS, Revision),
    case RuleSet#domain_RoutingRuleset.decisions of
        {candidates, Candidates} ->
            {ok, Candidates};
        {delegates, _} ->
            {error, unreduced}
    end.

-spec prohibited_candidates_filter([candidate()], [candidate()], revision()) -> {[route()], [rejected_route()]}.
prohibited_candidates_filter(Candidates, ProhibitedCandidates, Revision) ->
    ProhibitionTable = lists:foldl(
        fun(C, Acc) ->
            Acc#{get_terminal_ref(C) => get_description(C)}
        end,
        #{},
        ProhibitedCandidates
    ),
    lists:foldr(
        fun(C, {Accepted, Rejected}) ->
            Route = decode_candidate(C, Revision),
            TerminalRef = maps:get(terminal_ref, Route),
            case maps:find(TerminalRef, ProhibitionTable) of
                error ->
                    {[Route | Accepted], Rejected};
                {ok, Description} ->
                    ProviderRef = maps:get(provider_ref, Route, undefined),
                    {Accepted, [{ProviderRef, TerminalRef, {'RoutingRule', Description}} | Rejected]}
            end
        end,
        {[], []},
        Candidates
    ).

-spec get_terminal_ref(candidate()) -> terminal_ref().
get_terminal_ref(Candidate) ->
    Candidate#domain_RoutingCandidate.terminal.

-spec get_description(candidate()) -> candidate_description().
get_description(Candidate) ->
    Candidate#domain_RoutingCandidate.description.

-spec get_providers([route()]) -> [id()].
get_providers(Routes) ->
    lists:foldr(
        fun(R, Acc) ->
            case maps:get(provider_id, R, undefined) of
                undefined ->
                    Acc;
                ProviderID ->
                    [ProviderID | Acc]
            end
        end,
        [],
        Routes
    ).

-spec decode_candidate(candidate(), revision()) -> route().
decode_candidate(Candidate, Revision) ->
    TerminalRef = Candidate#domain_RoutingCandidate.terminal,
    TerminalID = TerminalRef#domain_TerminalRef.id,
    Terminal = unwrap(ff_domain_config:object(Revision, {terminal, TerminalRef})),
    Route = #{
        terminal => Terminal,
        terminal_ref => TerminalRef,
        terminal_id => TerminalID
    },
    case Terminal#domain_Terminal.provider_ref of
        undefined ->
            Route;
        ProviderRef ->
            ProviderID = ProviderRef#domain_ProviderRef.id,
            Provider = unwrap(ff_domain_config:object(Revision, {provider, ProviderRef})),
            Route#{
                provider => Provider,
                provider_ref => ProviderRef,
                provider_id => ProviderID
            }
    end.
