-module(ff_routing_rule).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([gather_routes/4]).
-export([get_providers/1]).

-type id() :: dmsl_domain_thrift:'ObjectID'().
-type payment_institution() :: ff_payment_institution:payment_institution().
-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().
-type varset() :: hg_selector:varset().
-type revision() :: ff_domain_config:revision().
-type routing_rule_tag() :: p2p_transfer_routing_rules | withdrawal_routing_rules.
-type candidate() :: dmsl_domain_thrift:'RoutingCandidate'().
-type candidate_description() :: binary() | undefined.

-type route() :: #{
    provider        => dmsl_domain_thrift:'Provider'(),
    provider_ref    => provider_ref(),
    provider_id     => id(),
    terminal        => dmsl_domain_thrift:'Terminal'(),
    terminal_ref    => terminal_ref(),
    terminal_id     => id()
}.

-type reject_context() :: #{
    varset := varset(),
    rejected_providers := [rejected_provider()],
    rejected_routes := [rejected_route()]
}.

-type rejected_provider() :: {provider_ref(), Reason :: term()}.
-type rejected_route() :: {provider_ref(), terminal_ref(), Reason :: term()}.

%% Pipeline

-import(ff_pipeline, [unwrap/1]).

%%

-spec gather_routes(payment_institution(), routing_rule_tag(), varset(), revision()) ->
    {[route()], reject_context()}.
gather_routes(PaymentInstitution, RoutingRuleTag, VS, Revision) ->
    RejectedContext = #{
        varset => VS,
        rejected_providers => [],
        rejected_routes => []
    },
    RoutingRules = maps:get(RoutingRuleTag, PaymentInstitution, undefined),
    case RoutingRules of
        undefined ->
            {[], RejectedContext};
        _ ->
            Policies = RoutingRules#domain_RoutingRules.policies,
            Prohibitions = RoutingRules#domain_RoutingRules.prohibitions,
            {ok, PermitRuleSet} = ff_party:compute_routing_ruleset(Policies, VS, Revision),
            {ok, DenyRuleSet} = ff_party:compute_routing_ruleset(Prohibitions, VS, Revision),
            {candidates, PermittedCandidates} = PermitRuleSet#domain_RoutingRuleset.decisions,
            {candidates, ProhibitedCandidates} = DenyRuleSet#domain_RoutingRuleset.decisions,
            {AcceptedRoutes, RejectedRoutes}  = prohibited_candidates_filter(
                PermittedCandidates,
                ProhibitedCandidates,
                VS,
                Revision
            ),
            {AcceptedRoutes, RejectedContext#{rejected_routes => RejectedRoutes}}
    end.

-spec prohibited_candidates_filter([candidate()], [candidate()], varset(), revision()) ->
    {[route()], [rejected_route()]}.
prohibited_candidates_filter(Candidates, ProhibitedCandidates, VS, Revision) ->
    ProhibitionTable = lists:foldl(
        fun(C, Acc) ->
            Acc#{get_terminal_ref(C) => get_description(C)}
        end,
        #{},
        ProhibitedCandidates
    ),
    lists:foldl(
        fun(C, {Accepted, Rejected}) ->
            Route = decode_candidate(C, VS, Revision),
            #{
                terminal_ref := TerminalRef,
                provider_ref := ProviderRef
            } = Route,
            case maps:find(TerminalRef, ProhibitionTable) of
                error ->
                    {[Route | Accepted], Rejected};
                {ok, Description} ->
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

-spec get_providers([route()]) ->
    [id()].
get_providers(Routes) ->
    lists:foldl(
        fun(R, Acc) ->
            #{provider_id := ProviderID} = R,
            [ProviderID | Acc]
        end,
        [],
        Routes
    ).

-spec decode_candidate(candidate(), varset(), revision()) ->
    route().
decode_candidate(Candidate, _VS, Revision) ->
    TerminalRef = Candidate#domain_RoutingCandidate.terminal,
    TerminalID = TerminalRef#domain_TerminalRef.id,
    Terminal = unwrap(ff_domain_config:object(Revision, {terminal, TerminalRef})),
    ProviderRef = Terminal#domain_Terminal.provider_ref,
    ProviderID = ProviderRef#domain_ProviderRef.id,
    Provider = unwrap(ff_domain_config:object(Revision, {provider, ProviderRef})),
    #{
        provider        => Provider,
        provider_ref    => ProviderRef,
        provider_id     => ProviderID,
        terminal        => Terminal,
        terminal_ref    => TerminalRef,
        terminal_id     => TerminalID
    }.
