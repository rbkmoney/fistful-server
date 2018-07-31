%%%
%%% Identity class
%%%

-module(ff_identity_class).

%%

-type id() :: binary().

-type class() :: #{
    id                    := id(),
    name                  := binary(),
    contract_template_ref := contract_template_ref(),
    initial_level         := level(),
    levels                := #{level_id() => level()},
    challenge_classes     := #{challenge_class_id() => challenge_class()}
}.

-type contract_template_ref() ::
    dmsl_domain_thrift:'ContractTemplateRef'().

%%

-type level_id() :: binary().
-type level() :: #{
    id               := level_id(),
    name             := binary(),
    contractor_level := contractor_level()
}.

-type contractor_level() ::
    dmsl_domain_thrift:'ContractorIdentificationLevel'().

%%

-type challenge_class_id() :: binary().

-type challenge_class() :: #{
    id           := challenge_class_id(),
    name         := binary(),
    base_level   := level(),
    target_level := level()
}.

-export([id/1]).
-export([name/1]).
-export([contract_template/1]).
-export([initial_level/1]).
-export([level/2]).

-export([level_name/1]).
-export([contractor_level/1]).
-export([challenge_class/2]).

-export([base_level/1]).
-export([target_level/1]).
-export([challenge_class_name/1]).

-export_type([id/0]).
-export_type([class/0]).
-export_type([level_id/0]).
-export_type([level/0]).
-export_type([challenge_class_id/0]).
-export_type([challenge_class/0]).

%% Class

-spec id(class()) ->
    id().

id(#{id := V}) ->
    V.

-spec name(class()) ->
    binary().

name(#{name := V}) ->
    V.

-spec contract_template(class()) ->
    contract_template_ref().

contract_template(#{contract_template_ref := V}) ->
    V.

-spec initial_level(class()) ->
    level_id().

initial_level(#{initial_level := V}) ->
    V.

-spec level(level_id(), class()) ->
    {ok, level()} |
    {error, notfound}.

level(ID, #{levels := Levels}) ->
    ff_map:find(ID, Levels).

-spec challenge_class(challenge_class_id(), class()) ->
    {ok, challenge_class()} |
    {error, notfound}.

challenge_class(ID, #{challenge_classes := ChallengeClasses}) ->
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

-spec base_level(challenge_class()) ->
    level_id().

base_level(#{base_level := V}) ->
    V.

-spec target_level(challenge_class()) ->
    level_id().

target_level(#{target_level := V}) ->
    V.
