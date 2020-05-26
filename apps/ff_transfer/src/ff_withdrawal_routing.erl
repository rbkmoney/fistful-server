-module(ff_withdrawal_routing).

-export([prepare_route/3]).

-import(ff_pipeline, [do/1, unwrap/1]).

-type route() :: #{
    provider_id := provider_id(),
    terminal_id := terminal_id() | undefined
}.

-export_type([route/0]).

-type id()              :: binary().
-type identity()        :: ff_identity:identity().
-type domain_revision() :: ff_domain_config:revision().
-type party_varset()    :: hg_selector:varset().

-type provider_id()  :: pos_integer() | id().
-type provider()     :: ff_payouts_provider:withdrawal_provider().
-type provider_def() :: {provider_id(), provider()}.

-type terminal_id()  :: ff_payouts_terminal:id().
-type terminal()     :: ff_payouts_terminal:withdrawal_terminal().
-type terminal_def() :: {terminal_id(), terminal()}.

%%

-spec prepare_route(party_varset(), identity(), domain_revision()) ->
    {ok, route()} | {error, route_not_found}.

prepare_route(PartyVarset, Identity, DomainRevision) ->
    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, DomainRevision),
    case ff_payment_institution:compute_withdrawal_providers(PaymentInstitution, PartyVarset) of
        {ok, Providers}  ->
            choose_route(Providers, PartyVarset);
        {error, {misconfiguration, _Details} = Error} ->
            %% TODO: Do not interpret such error as an empty route list.
            %% The current implementation is made for compatibility reasons.
            %% Try to remove and follow the tests.
            _ = logger:warning("Route search failed: ~p", [Error]),
            {error, route_not_found}
    end.

%%

-spec choose_route([provider_id()], party_varset()) ->
    {ok, route()} | {error, route_not_found}.

choose_route(Providers, PartyVarset) ->
    do(fun() ->
        ProviderDef = unwrap(choose_provider(Providers, PartyVarset)),
        Terminals = unwrap(get_provider_terminals(ProviderDef, PartyVarset)),
        TerminalDef = unwrap(choose_terminal(Terminals, PartyVarset)),
        make_route(ProviderDef, TerminalDef)
    end).

-spec choose_provider([provider_id()], party_varset()) ->
    {ok, provider_def()} | {error, route_not_found}.

choose_provider(Providers, VS) ->
    case gather_valid_providers(Providers, VS) of
        [Provider | _] ->
            {ok, Provider};
        [] ->
            {error, route_not_found}
    end.

-spec gather_valid_providers([provider_id()], party_varset()) ->
    [provider_def()].

gather_valid_providers(Providers, VS) ->
    lists:foldr(
        fun(ID, Acc) ->
            Provider = unwrap(ff_payouts_provider:get(ID)),
            case validate_provider_terms(Provider, VS) of
                true -> [{ID, Provider} | Acc];
                false -> Acc
            end
        end,
        [], Providers
    ).

-spec validate_provider_terms(provider(), party_varset()) ->
    boolean().

validate_provider_terms(Provider, VS) ->
    case ff_payouts_provider:validate_terms(Provider, VS) of
        {ok, valid} ->
            true;
        {error, _Error} ->
            false
    end.

-spec get_provider_terminals(provider_def(), party_varset()) ->
    {ok, [terminal_id()]} | {error, route_not_found}.

get_provider_terminals({_, Provider}, VS) ->
    case ff_payouts_provider:compute_withdrawal_terminals(Provider, VS) of
        {ok, Terminals}  ->
            {ok, Terminals};
        {error, {misconfiguration, _Details} = Error} ->
            %% TODO: Do not interpret such error as an empty route list.
            %% The current implementation is made for compatibility reasons.
            %% Try to remove and follow the tests.
            _ = logger:warning("Route search failed: ~p", [Error]),
            {error, route_not_found}
    end.

-spec choose_terminal([terminal_id()], party_varset()) ->
    {ok, terminal_def()} | {error, route_not_found}.

choose_terminal(Terminals, VS) ->
    case gather_valid_terminals(Terminals, VS) of
        [Terminal | _] ->
            {ok, Terminal};
        [] ->
            {error, route_not_found}
    end.

-spec gather_valid_terminals([terminal_id()], party_varset()) ->
    [terminal_def()].

gather_valid_terminals(Terminals, VS) ->
    lists:foldr(
        fun(ID, Acc) ->
            Terminal = unwrap(ff_payouts_terminal:get(ID)),
            case validate_terminal_terms(Terminal, VS) of
                true -> [{ID, Terminals} | Acc];
                false -> Acc
            end
        end,
        [], Terminals
    ).

-spec validate_terminal_terms(terminal(), party_varset()) ->
    boolean().

validate_terminal_terms(Provider, VS) ->
    case ff_payouts_terminal:validate_terms(Provider, VS) of
        {ok, valid} ->
            true;
        {error, _Error} ->
            false
    end.

-spec make_route(provider_def(), terminal_def()) ->
    route().

make_route({ProviderID, _Provider}, {TerminalID, _Terminal}) ->
    #{
        provider_id => ProviderID,
        terminal_id => TerminalID
    }.
