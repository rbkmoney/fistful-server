%%%
%%% Wallet provider
%%%
%%% TODOs
%%%
%%%  - If an identity class is essentially a contract template then there's no
%%%    way we could tell which currencies provider does provide without knowing
%%%    what identity class we're talking about.
%%%

-module(ff_provider).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-type id()       :: binary().
-type provider() :: #{
    payinst_ref := payinst_ref(),
    payinst     := payinst()
}.

-type payinst()     :: dmsl_domain_thrift:'PaymentInstitution'().
-type payinst_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().

-export_type([id/0]).
-export_type([provider/0]).

-export([name/1]).
-export([residences/1]).
-export([payinst/1]).

-export([get/1]).
-export([list_identity_classes/1]).
-export([get_identity_class/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec name(provider()) -> binary().
name(#{payinst := PI}) -> PI#domain_PaymentInstitution.name.

-spec residences(provider()) -> [ff_residence:id()].
residences(#{payinst := PI}) -> PI#domain_PaymentInstitution.residences.

-spec payinst(provider()) -> payinst_ref().
payinst(#{payinst_ref := V}) -> V.

%%

-spec get(id()) ->
    {ok, provider()} |
    {error, notfound}.

get(ID) ->
    do(fun () ->
        % TODO
        %  - We need to somehow expose these things in the domain config.
        %  - Possibly inconsistent view of domain config.
        Config = unwrap(get_provider_config(ID)),
        PaymentInstitutionRef = #domain_PaymentInstitutionRef{id = maps:get(payment_institution_id, Config)},
        PaymentInstitution    = unwrap(ff_domain_config:object({payment_institution, PaymentInstitutionRef})),
        IdentityClassesConfig = maps:get(identity_classes, Config),
        IdentityClasses       = maps:map(
            fun (IdentityClassID, ICC) ->
                Name                 = maps:get(name, ICC, IdentityClassID),
                ContractTemplateRef  = #domain_ContractTemplateRef{id = maps:get(contact_template_id, ICC)},
                IdentityLevelsConfig = maps:get(levels, ICC, #{}),
                IdentityLevels       = maps:map(
                    fun (IdentityLevelID, ILC) ->
                        error(noimpl)
                    end,
                    IdentityLevelsConfig
                ),
                #{
                    name                  => Name,
                    contract_template_ref => ContractTemplateRef,
                    levels                => IdentityLevels
                }
            end,
            IdentityClassesConfig
        ),
        #{
            payinst_ref      => PaymentInstitutionRef,
            payinst          => PaymentInstitution,
            identity_classes => IdentityClasses
        }
    end).

-spec list_identity_classes(provider()) ->
    [ff_identity:class_id()].

list_identity_classes(#{identity_classes := ICs}) ->
    maps:keys(ICs).

-spec get_identity_class(ff_identity:class_id(), provider()) ->
    {ok, ff_identity:class()} |
    {error, notfound}.

get_identity_class(IdentityClassID, #{identity_classes := ICs}) ->
    ff_map:find(IdentityClassID, ICs).

%%

-spec get_provider_config(id()) ->
    {ok, #{}} |
    {error, notfound}.

get_provider_config(ID) ->
    case genlib_app:env(fistful, providers, #{}) of
        #{ID := Provider} ->
            {ok, Provider};
        #{} ->
            {error, notfound}
    end.
