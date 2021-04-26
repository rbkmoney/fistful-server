-module(ff_routing_rule).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([gather_routes/4]).
-export([log_reject_context/1]).

-type payment_institution() :: ff_payment_institution:payment_institution().
-type routing_ruleset_ref() :: dmsl_domain_thrift:'RoutingRulesetRef'().
-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type provider() :: dmsl_domain_thrift:'Provider'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().
-type terminal() :: dmsl_domain_thrift:'Terminal'().
-type priority() :: integer().
-type weight() :: integer().
-type varset() :: hg_selector:varset().
-type revision() :: ff_domain_config:revision().
-type routing_rule_tag() :: p2p_transfer_routing_rules | withdrawal_routing_rules.
-type candidate() :: dmsl_domain_thrift:'RoutingCandidate'().
-type candidate_description() :: binary() | undefined.

-type route() :: #{
    terminal_ref := terminal_ref(),
    terminal := terminal(),
    priority => priority(),
    weight => weight(),
    provider => provider()
}.

-export_type([route/0]).
-export_type([provider/0]).
-export_type([terminal/0]).
-export_type([reject_context/0]).
-export_type([rejected_route/0]).

-type reject_context() :: #{
    varset := varset(),
    rejected_routes := [rejected_route()]
}.

-type rejected_route() :: {provider_ref(), terminal_ref(), Reason :: term()}.

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec gather_routes(payment_institution(), routing_rule_tag(), varset(), revision()) -> {[route()], reject_context()}.
gather_routes(PaymentInstitution, RoutingRuleTag, VS, Revision) ->
    RejectContext = #{
        varset => VS,
        rejected_routes => []
    },
    case do_gather_routes(PaymentInstitution, RoutingRuleTag, VS, Revision) of
        {ok, {AcceptedRoutes, RejectedRoutes}} ->
            {AcceptedRoutes, RejectContext#{rejected_routes => RejectedRoutes}};
        {error, misconfiguration} ->
            logger:warning("Routing rule misconfiguration. Varset:~n~p", [VS]),
            {[], RejectContext}
    end.

-spec do_gather_routes(payment_institution(), routing_rule_tag(), varset(), revision()) ->
    {ok, {[route()], [rejected_route()]}}
    | {error, misconfiguration}.
do_gather_routes(PaymentInstitution, RoutingRuleTag, VS, Revision) ->
    do(fun() ->
        case maps:get(RoutingRuleTag, PaymentInstitution) of
            undefined ->
                logger:log(
                    warning,
                    "Payment routing rules is undefined, PaymentInstitution: ~p",
                    [PaymentInstitution]
                ),
                {[], []};
            RoutingRules ->
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
        end
    end).

-spec compute_routing_ruleset(routing_ruleset_ref(), varset(), revision()) ->
    {ok, [candidate()]}
    | {error, misconfiguration}.
compute_routing_ruleset(RulesetRef, VS, Revision) ->
    {ok, Ruleset} = ff_party:compute_routing_ruleset(RulesetRef, VS, Revision),
    check_ruleset_computing(Ruleset#domain_RoutingRuleset.decisions).

check_ruleset_computing({delegates, _}) ->
    {error, misconfiguration};
check_ruleset_computing({candidates, Candidates}) ->
    AllReduced = lists:all(
        fun(C) ->
            case C#domain_RoutingCandidate.allowed of
                {constant, _} ->
                    true;
                _ ->
                    false
            end
        end,
        Candidates
    ),
    case AllReduced of
        true ->
            {ok, Candidates};
        false ->
            {error, misconfiguration}
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
            Route = make_route(C, Revision),
            #{terminal_ref := TerminalRef} = Route,
            case maps:find(TerminalRef, ProhibitionTable) of
                error ->
                    {[Route | Accepted], Rejected};
                {ok, Description} ->
                    #{terminal := Terminal} = Route,
                    ProviderRef = Terminal#domain_Terminal.provider_ref,
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

-spec make_route(candidate(), revision()) -> route().
make_route(Candidate, Revision) ->
    TerminalRef = Candidate#domain_RoutingCandidate.terminal,
    {ok, Terminal} = ff_domain_config:object(Revision, {terminal, TerminalRef}),
    Priority = Candidate#domain_RoutingCandidate.priority,
    Weight = Candidate#domain_RoutingCandidate.weight,
    ProviderRef = Terminal#domain_Terminal.provider_ref,
    {ok, Provider} = ff_domain_config:object(Revision, {provider, ProviderRef}),
    genlib_map:compact(#{
        terminal_ref => TerminalRef,
        terminal => Terminal,
        priority => Priority,
        weight => Weight,
        provider => Provider
    }).

-spec log_reject_context(reject_context()) -> ok.
log_reject_context(RejectContext) ->
    Level = warning,
    RejectReason = unknown,
    _ = logger:log(
        Level,
        "No route found, reason = ~p, varset: ~p",
        [RejectReason, maps:get(varset, RejectContext)],
        logger:get_process_metadata()
    ),
    _ = logger:log(
        Level,
        "No route found, reason = ~p, rejected routes: ~p",
        [RejectReason, maps:get(rejected_routes, RejectContext)],
        logger:get_process_metadata()
    ),
    ok.
