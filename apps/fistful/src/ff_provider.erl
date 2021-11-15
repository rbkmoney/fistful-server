%%%
%%% Wallet provider
%%%
%%% TODOs
%%%
%%%  - If an identity class is essentially a contract template then there's no
%%%    way we could tell which currencies provider does provide without knowing
%%%    what identity class we're talking about.
%%%  - It's generally considerably easier to reference any referencable object
%%%    with a single ID, like we do in the domain config. So instead of a
%%%    hierarchy it would be easier to deal with a plain collection of objects.

-module(ff_provider).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type contract_template_ref() :: dmsl_domain_thrift:'ContractTemplateRef'().
-type contractor_level() :: dmsl_domain_thrift:'ContractorIdentificationLevel'().

-type id() :: binary().
-type provider() :: #{
    id := id(),
    payinst_ref := payinst_ref(),
    payinst := payinst(),
    contract_template_ref := contract_template_ref(),
    contractor_level := contractor_level()
}.

%  TODO: specify exacte type
-type configuration() :: #{
    payment_institution_id := integer(),
    contract_template_id := integer(),
    contractor_level := contractor_level()
}.

-type payinst() :: dmsl_domain_thrift:'PaymentInstitution'().
-type payinst_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().

-export_type([id/0]).
-export_type([provider/0]).

-export([id/1]).
-export([name/1]).
-export([residences/1]).
-export([payinst/1]).
-export([contract_template/1]).
-export([contractor_level/1]).

-export([list/0]).
-export([get/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec id(provider()) -> id().
-spec name(provider()) -> binary().
-spec residences(provider()) -> [ff_residence:id()].
-spec payinst(provider()) -> payinst_ref().
-spec contract_template(provider()) -> contract_template_ref().
-spec contractor_level(provider()) -> contractor_level().

id(#{id := ID}) ->
    ID.

name(#{payinst := PI}) ->
    PI#domain_PaymentInstitution.name.

residences(#{payinst := PI}) ->
    PI#domain_PaymentInstitution.residences.

payinst(#{payinst_ref := V}) ->
    V.

contract_template(#{contract_template_ref := Ref}) ->
    Ref.

contractor_level(#{contractor_level := Level}) ->
    Level.

%%

-spec list() -> [provider()].
list() ->
    [
        Provider
     || ID <- list_providers(),
        {ok, Provider} <- [ff_provider:get(ID)]
    ].

-spec get(id()) ->
    {ok, provider()}
    | {error, notfound}.
get(ID) ->
    do(fun() ->
        % TODO
        %  - We need to somehow expose these things in the domain config
        %  - Possibly inconsistent view of domain config
        Config = unwrap(get_config(ID)),
        PaymentInstitutionRef = #domain_PaymentInstitutionRef{id = cfg(payment_institution, Config)},
        {ok, PaymentInstitution} = ff_domain_config:object({payment_institution, PaymentInstitutionRef}),
        ContractTemplateRef = #domain_ContractTemplateRef{id = cfg(contract_template_id, Config)},
        % TODO FF-235: we should check configuration when start fistful
        %  after move on domain_config we mustn't do that there
        ok = validate_contract_template_ref(ContractTemplateRef),
        #{
            id => ID,
            payinst_ref => PaymentInstitutionRef,
            payinst => PaymentInstitution,
            contract_template_ref => ContractTemplateRef,
            contractor_level => cfg(contractor_level, Config)
        }
    end).

%% Provider Configuration

-spec get_config(id()) ->
    {ok, configuration()}
    | {error, notfound}.
get_config(ID) ->
    case genlib_app:env(fistful, providers, #{}) of
        #{ID := ProviderConfig} ->
            {ok, ProviderConfig};
        #{} ->
            {error, notfound}
    end.

cfg(payment_institution, C) ->
    maps:get(payment_institution_id, C);
cfg(contract_template_id, C) ->
    maps:get(contract_template_id, C);
cfg(contractor_level, C) ->
    maps:get(contractor_level, C).

validate_contract_template_ref(ContractTemplateRef) ->
    {ok, _} = ff_domain_config:object({contract_template, ContractTemplateRef}),
    ok.

-spec list_providers() -> [id()].
list_providers() ->
    maps:keys(genlib_app:env(fistful, providers, #{})).
