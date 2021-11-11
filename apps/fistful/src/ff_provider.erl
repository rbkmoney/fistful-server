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
    % TODO[FF-235]
    % identity_classes := #{
    %     ff_identity_class:id() => ff_identity_class:class()
    % }
}.

%  TODO: specify exacte type
-type configuration() :: #{
    payment_institution_id := integer(),
    contract_template_id := integer(),
    contractor_level := contractor_level()
}.

-type payinst() :: dmsl_domain_thrift:'PaymentInstitution'().
-type payinst_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().
-type identity_classes() :: #{ff_identity_class:id() => ff_identity_class:class()}.

-export_type([id/0]).
-export_type([provider/0]).
-export_type([identity_classes/0]).

-export([id/1]).
-export([name/1]).
-export([residences/1]).
-export([payinst/1]).
-export([contract_template/1]).
-export([contractor_level/1]).

% -export([identity_classes/1]).

-export([list/0]).
-export([get/1]).
-export([list_identity_classes/1]).
-export([get_identity_class/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec id(provider()) -> id().
-spec name(provider()) -> binary().
-spec residences(provider()) -> [ff_residence:id()].
-spec payinst(provider()) -> payinst_ref().
-spec contract_template(provider()) -> contract_template_ref().
-spec contractor_level(provider()) -> contractor_level().
% -spec identity_classes(provider()) -> identity_classes().

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
        % TODO[FF-235] Delete
        % IdentityClasses = maps:map(
        %     fun decode_identity_class/2,
        %     maps:get(identity_classes, Config)
        % ),
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

-spec list_identity_classes(provider()) -> [ff_identity_class:id()].
list_identity_classes(#{identity_classes := ICs}) ->
    maps:keys(ICs).

-spec get_identity_class(ff_identity_class:id(), provider()) ->
    {ok, ff_identity_class:class()}
    | {error, notfound}.
get_identity_class(IdentityClassID, #{identity_classes := ICs}) ->
    ff_map:find(IdentityClassID, ICs).

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

% decode_identity_class(ICID, ICC) ->
%     Name = maps:get(name, ICC, ICID),
%     ContractTemplateRef = #domain_ContractTemplateRef{id = maps:get(contract_template_id, ICC)},
%     {ok, _} = ff_domain_config:object({contract_template, ContractTemplateRef}),
%     Levels = maps:map(
%         fun(LID, LC) ->
%             LName = maps:get(name, LC, LID),
%             ContractorLevel = maps:get(contractor_level, LC),
%             % TODO
%             %  - `ok = assert_contractor_level(ContractorLevel)`
%             #{
%                 id => LID,
%                 name => LName,
%                 contractor_level => ContractorLevel
%             }
%         end,
%         maps:get(levels, ICC)
%     ),
%     ChallengeClasses = maps:map(
%         fun(CCID, CCC) ->
%             CCName = maps:get(name, CCC, CCID),
%             BaseLevelID = maps:get(base, CCC),
%             TargetLevelID = maps:get(target, CCC),
%             {ok, _} = maps:find(BaseLevelID, Levels),
%             {ok, _} = maps:find(TargetLevelID, Levels),
%             #{
%                 id => CCID,
%                 name => CCName,
%                 base_level => BaseLevelID,
%                 target_level => TargetLevelID
%             }
%         end,
%         maps:get(challenges, ICC, #{})
%     ),
%     InitialLevelID = maps:get(initial_level, ICC),
%     {ok, _} = maps:find(InitialLevelID, Levels),
%     #{
%         id => ICID,
%         name => Name,
%         contract_template_ref => ContractTemplateRef,
%         initial_level => InitialLevelID,
%         levels => Levels,
%         challenge_classes => ChallengeClasses
%     }.
