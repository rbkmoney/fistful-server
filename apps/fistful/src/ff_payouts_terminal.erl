-module(ff_payouts_terminal).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type terminal() :: #{
    id := id(),
    name := binary(),
    description := binary(),
    options => dmsl_domain_thrift:'ProxyOptions'(),
    risk_coverage => atom(),
    provider_ref => dmsl_domain_thrift:'ProviderRef'(),
    terms => dmsl_domain_thrift:'ProvisionTermSet'()
}.

-type id() :: dmsl_domain_thrift:'ObjectID'().
-type terminal_priority() :: integer() | undefined.

-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().
-type term_set() :: dmsl_domain_thrift:'ProvisionTermSet'().
-type provision_terms() :: dmsl_domain_thrift:'WithdrawalProvisionTerms'().
-type domain_revision() :: ff_domain_config:revision().

-export_type([id/0]).
-export_type([terminal/0]).
-export_type([terminal_ref/0]).
-export_type([terminal_priority/0]).
-export_type([provision_terms/0]).
-export_type([domain_revision/0]).

-export([adapter_opts/1]).
-export([terms/1]).
-export([provision_terms/1]).

-export([ref/1]).
-export([get/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec adapter_opts(terminal()) -> map().
adapter_opts(Terminal) ->
    maps:get(options, Terminal, #{}).

-spec terms(terminal()) -> term_set() | undefined.
terms(Terminal) ->
    maps:get(terms, Terminal, undefined).

-spec provision_terms(terminal()) -> provision_terms() | undefined.
provision_terms(Terminal) ->
    case terms(Terminal) of
        Terms when Terms =/= undefined ->
            case Terms#domain_ProvisionTermSet.wallet of
                WalletTerms when WalletTerms =/= undefined ->
                    WalletTerms#domain_WalletProvisionTerms.withdrawals;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

%%

-spec ref(id()) -> terminal_ref().
ref(ID) ->
    #domain_TerminalRef{id = ID}.

-spec get(id(), domain_revision()) ->
    {ok, terminal()}
    | {error, notfound}.
get(ID, DomainRevision) ->
    do(fun() ->
        WithdrawalTerminal = unwrap(ff_domain_config:object(DomainRevision, {terminal, ref(ID)})),
        decode(ID, WithdrawalTerminal)
    end).

%%

decode(ID, #domain_Terminal{
    name = Name,
    description = Description,
    options = ProxyOptions,
    risk_coverage = RiskCoverage,
    provider_ref = ProviderRef,
    terms = ProvisionTermSet
}) ->
    genlib_map:compact(#{
        id => ID,
        name => Name,
        description => Description,
        options => ProxyOptions,
        risk_coverage => RiskCoverage,
        provider_ref => ProviderRef,
        terms => ProvisionTermSet
    }).
