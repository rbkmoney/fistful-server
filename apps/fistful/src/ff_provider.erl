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

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("ff_cth/include/ct_domain.hrl").

-type id()       :: binary().
-type provider() :: #{
    id               := id(),
    payinst_ref      := payinst_ref(),
    payinst          := payinst(),
    identity_classes := #{
        ff_identity:class_id() => ff_identity:class()
    }
}.

-type payinst()     :: dmsl_domain_thrift:'PaymentInstitution'().
-type payinst_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().

-export_type([id/0]).
-export_type([provider/0]).

-export([id/1]).
-export([name/1]).
-export([residences/1]).
-export([payinst/1]).

-export([list/0]).
-export([get/1]).
-export([list_identity_classes/1]).
-export([get_identity_class/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec id(provider()) ->
    id().
-spec name(provider()) ->
    binary().
-spec residences(provider()) ->
    [ff_residence:id()].
-spec payinst(provider()) ->
    payinst_ref().

id(#{id := ID}) ->
    ID.
name(#{payinst := PI}) ->
    PI#domain_PaymentInstitution.name.
residences(#{payinst := PI}) ->
    PI#domain_PaymentInstitution.residences.
payinst(#{payinst_ref := V}) ->
    V.

%%

-spec list() ->
    [provider()].

list() ->
    [Provider ||
        ID             <- list_providers(),
        {ok, Provider} <- [ff_provider:get(ID)]
    ].

-spec get(id()) ->
    {ok, provider()} |
    {error, notfound}.

get(ID) ->
    do(fun () ->
        % TODO
        %  - We need to somehow expose these things in the domain config
        %  - Possibly inconsistent view of domain config
        C = unwrap(get_provider_config(ID)),
        PaymentInstitutionRef = ?payinst(maps:get(payment_institution_id, C)),
        {ok, PaymentInstitution} = ff_domain_config:object({payment_institution, PaymentInstitutionRef}),
        IdentityClasses = maps:map(
            fun (ICID, ICC) ->
                Name = maps:get(name, ICC, ICID),
                ContractTemplateRef = ?tmpl(maps:get(contract_template_id, ICC)),
                {ok, _} = ff_domain_config:object({contract_template, ContractTemplateRef}),
                Levels = maps:map(
                    fun (LID, LC) ->
                        LName = maps:get(name, LC, LID),
                        ContractorLevel = maps:get(contractor_level, LC),
                        % TODO
                        %  - `ok = assert_contractor_level(ContractorLevel)`
                        #{
                            id               => LID,
                            name             => LName,
                            contractor_level => ContractorLevel
                        }
                    end,
                    maps:get(levels, ICC)
                ),
                ChallengeClasses = maps:map(
                    fun (CCID, CCC) ->
                        CCName        = maps:get(name, CCC, CCID),
                        BaseLevelID   = maps:get(base, CCC),
                        TargetLevelID = maps:get(target, CCC),
                        {ok, _} = maps:find(BaseLevelID, Levels),
                        {ok, _} = maps:find(TargetLevelID, Levels),
                        #{
                            id           => CCID,
                            name         => CCName,
                            base_level   => BaseLevelID,
                            target_level => TargetLevelID
                        }
                    end,
                    maps:get(challenges, ICC, #{})
                ),
                InitialLevelID = maps:get(initial_level, ICC),
                {ok, _} = maps:find(InitialLevelID, Levels),
                #{
                    id                    => ICID,
                    name                  => Name,
                    contract_template_ref => ContractTemplateRef,
                    initial_level         => InitialLevelID,
                    levels                => Levels,
                    challenge_classes     => ChallengeClasses
                }
            end,
            maps:get(identity_classes, C)
        ),
        #{
            id               => ID,
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

-spec list_providers() ->
    [id()].

list_providers() ->
    maps:keys(genlib_app:env(fistful, providers, #{})).
