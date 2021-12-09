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
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-type contract_template_ref() :: dmsl_domain_thrift:'ContractTemplateRef'().
-type contractor_level() :: dmsl_domain_thrift:'ContractorIdentificationLevel'().

-type id() :: binary().
-type provider_ref() :: dmsl_domain_thrift:'IdentityProviderRef'().
-type provider() :: #{
    id := provider_ref(),
    payinst_ref := payinst_ref(),
    payinst := payinst(),
    contract_template_ref := contract_template_ref(),
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

id(#{id := Ref}) ->
    Ref#domain_IdentityProviderRef.id.

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
    ProviderRefs = list_providers(),
    [
        Provider
     || Ref <- ProviderRefs,
        {ok, Provider} <- [ff_provider:get(Ref)]
    ].

-spec get(id() | provider_ref()) ->
    {ok, provider()}
    | {error, notfound}.
get(ID) when is_binary(ID) ->
    ff_provider:get(#domain_IdentityProviderRef{id = ID});
get(IdentityProviderRef) ->
    do(fun() ->
        % TODO
        %  - Possibly inconsistent view of domain config
        IdentityProvider = unwrap(ff_domain_config:object({identity_provider, IdentityProviderRef})),
        PaymentInstitutionRef = IdentityProvider#domain_IdentityProvider.payment_institution,
        {ok, PaymentInstitution} = ff_domain_config:object({payment_institution, PaymentInstitutionRef}),
        #{
            id => IdentityProviderRef,
            payinst_ref => PaymentInstitutionRef,
            payinst => PaymentInstitution,
            contract_template_ref => IdentityProvider#domain_IdentityProvider.contract_template,
            contractor_level => IdentityProvider#domain_IdentityProvider.contractor_level
        }
    end).

%% Provider Configuration

-spec list_providers() -> [provider_ref()].
list_providers() ->
    #'VersionedObject'{
        % version = Version,
        object = {globals, #domain_GlobalsObject{data = Globals}}
    } = dmt_client:checkout_versioned_object(latest, globals()),

    case Globals#domain_Globals.identity_providers of
        undefined -> [];
        List -> List
    end.

globals() ->
    {globals, #domain_GlobalsRef{}}.
