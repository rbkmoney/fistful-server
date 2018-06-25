%%%
%%% Identity class
%%%

-module(ff_identity_class).

%%

-type id() :: binary().

-type class() :: #{
    contract_template_ref := contract_template_ref(),
    initial_level_id      := level_id(),
    levels                := #{level_id() => level()},
    challenge_classes     := #{challenge_class_id() => challenge_class()}
}.

-type contract_template_ref() ::
    dmsl_domain_thrift:'ContractTemplateRef'().

%%

-type level_id() :: binary().
-type level() :: #{
    name             := binary(),
    contractor_level := contractor_level()
}.

-type contractor_level() ::
    dmsl_domain_thrift:'ContractorIdentificationLevel'().

%%

-type challenge_class_id() :: binary().

-type challenge_class() :: #{
    name             := binary(),
    base_level_id    := level_id(),
    target_level_id  := level_id()
}.

-export([name/1]).
-export([contract_template/1]).
-export([initial_level/1]).
-export([level/2]).
-export([level_name/1]).
-export([contractor_level/1]).
-export([challenge_class/2]).
-export([base_level/2]).
-export([target_level/2]).
-export([challenge_class_name/1]).

-export_type([id/0]).
-export_type([class/0]).
-export_type([level_id/0]).
-export_type([level/0]).
-export_type([challenge_class_id/0]).
-export_type([challenge_class/0]).

%% Class

-spec name(class()) ->
    binary().

name(#{name := V}) ->
    V.

-spec contract_template(class()) ->
    contract_template_ref().

contract_template(#{contract_template_ref := V}) ->
    V.

-spec initial_level(class()) ->
    level().

initial_level(#{initial_level_id := V} = Class) ->
    {ok, Level} = level(V, Class), Level.

-spec level(level_id(), class()) ->
    {ok, level()} |
    {error, notfound}.

level(ID, #{levels := Levels}) ->
    ff_map:find(ID, Levels).

-spec challenge_class(challenge_class_id(), class()) ->
    {ok, challenge_class()} |
    {error, notfound}.

challenge_class(ID, #{challenge_classs := ChallengeClasses}) ->
    ff_map:find(ID, ChallengeClasses).

%% Level

-spec level_name(level()) ->
    binary().

level_name(#{name := V}) ->
    V.

-spec contractor_level(level()) ->
    contractor_level().

contractor_level(#{contractor_level := V}) ->
    V.

%% Challenge

-spec challenge_class_name(challenge_class()) ->
    binary().

challenge_class_name(#{name := V}) ->
    V.

-spec base_level(challenge_class(), class()) ->
    level().

base_level(#{base_level_id := ID}, Class) ->
    {ok, Level} = level(ID, Class), Level.

-spec target_level(challenge_class(), class()) ->
    level().

target_level(#{target_level_id := ID}, Class) ->
    {ok, Level} = level(ID, Class),
    Level.
