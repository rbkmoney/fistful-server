-module(ff_routing_rule).

-type id() :: dmsl_domain_thrift:'ObjectID'().
-type payment_institution() :: ff_payment_institution:payment_institution().

-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().

-type varset() :: hg_selector:varset().
-type revision() :: ff_domain_config:revision().

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

-export([gather_routes/3]).
-export([get_providers/1]).

-spec gather_routes(payment_institution(), varset(), revision()) ->
    {[route()], reject_context()}.
gather_routes(_PaymentInstitution, VS, _Revision) ->
    RejectedContext = #{
        varset => VS,
        rejected_providers => [],
        rejected_routes => []
    },
    {[], RejectedContext}.

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
