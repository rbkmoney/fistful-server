-module(ff_routing_rule).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([gather_routes/4]).
-export([log_reject_context/1]).

-type id() :: dmsl_domain_thrift:'ObjectID'().
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
    provider => provider(),
    provider_id => id(),
    terminal := terminal(),
    terminal_id := id(),
    priority => priority(),
    weight => weight()
}.

-export_type([route/0]).
-export_type([provider/0]).
-export_type([terminal/0]).

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

-spec gather_routes(payment_institution(), routing_rule_tag(), varset(), revision()) -> [route()].
gather_routes(PaymentInstitution, RoutingRuleTag, VS, Revision) ->
    case do_gather_routes(PaymentInstitution, RoutingRuleTag, VS, Revision) of
        {ok, {AcceptedRoutes, RejectedRoutes}} ->
            case AcceptedRoutes of
                [] ->
                    RejectedContext = #{
                        varset => VS,
                        rejected_providers => [],
                        rejected_routes => RejectedRoutes
                    },
                    log_reject_context(RejectedContext),
                    [];
                [_Route | _] ->
                    AcceptedRoutes
            end;
        {error, Error} ->
            case Error of
                misconfiguration ->
                    logger:warning("Routing rule misconfiguration. Varset:~n~p", [VS]);
                _ ->
                    %% TODO: full logging, when new routing will be implemented
                    Error = Error
            end,
            []
    end.

-spec do_gather_routes(payment_institution(), routing_rule_tag(), varset(), revision()) ->
    {ok, {[route()], [route()]}}
    | {error, misconfiguration}
    | {error, ruleset_not_found}.
do_gather_routes(PaymentInstitution, RoutingRuleTag, VS, Revision) ->
    do(fun() ->
        case maps:find(RoutingRuleTag, PaymentInstitution) of
            {ok, RoutingRules} ->
                Policies = RoutingRules#domain_RoutingRules.policies,
                Prohibitions = RoutingRules#domain_RoutingRules.prohibitions,
                PermitCandidates = unwrap(compute_routing_ruleset(Policies, VS, Revision)),
                DenyCandidates = unwrap(compute_routing_ruleset(Prohibitions, VS, Revision)),
                {AcceptedRoutes, RejectedRoutes} = prohibited_candidates_filter(
                    PermitCandidates,
                    DenyCandidates,
                    Revision
                ),
                {AcceptedRoutes, RejectedRoutes};
            error ->
                {[], []}
        end
    end).

-spec compute_routing_ruleset(routing_ruleset_ref(), varset(), revision()) ->
    {ok, [candidate()]}
    | {error, misconfiguration}
    | {error, ruleset_not_found}.
compute_routing_ruleset(RulesetRef, VS, Revision) ->
    case ff_party:compute_routing_ruleset(RulesetRef, VS, Revision) of
        {ok, Ruleset} ->
            check_ruleset_computing(Ruleset#domain_RoutingRuleset.decisions);
        {error, Error} ->
            {error, Error}
    end.

check_ruleset_computing({delegates, _}) ->
    {error, misconfiguration};
check_ruleset_computing({candidates, Candidates}) ->
    CheckedCandidates = lists:takewhile(
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
    case CheckedCandidates =:= Candidates of
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
            TerminalRef = get_terminal_ref(C),
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

-spec make_route(candidate(), revision()) -> route().
make_route(Candidate, Revision) ->
    Priority = Candidate#domain_RoutingCandidate.priority,
    Weight = Candidate#domain_RoutingCandidate.weight,
    TerminalRef = Candidate#domain_RoutingCandidate.terminal,
    TerminalID = TerminalRef#domain_TerminalRef.id,
    {ok, Terminal} = ff_domain_config:object(Revision, {terminal, TerminalRef}),
    ProviderRef = Terminal#domain_Terminal.provider_ref,
    ProviderID = ProviderRef#domain_ProviderRef.id,
    {ok, Provider} = ff_domain_config:object(Revision, {provider, ProviderRef}),
    genlib_map:compact(#{
        terminal => Terminal,
        terminal_id => TerminalID,
        provider => Provider,
        provider_id => ProviderID,
        priority => Priority,
        weight => Weight
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
        "No route found, reason = ~p, rejected providers: ~p",
        [RejectReason, maps:get(rejected_providers, RejectContext)],
        logger:get_process_metadata()
    ),
    _ = logger:log(
        Level,
        "No route found, reason = ~p, rejected routes: ~p",
        [RejectReason, maps:get(rejected_routes, RejectContext)],
        logger:get_process_metadata()
    ),
    ok.
