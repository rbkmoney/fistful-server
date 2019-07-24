%%%
%%% Identity class
%%%

-module(ff_identity_class).

%%

-type id() :: binary().

%%

-type challenge_class_id() :: binary().

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
-export_type([challenge_class_id/0]).

%% Class

-spec id(ff_identity:class()) ->
    id().

id(#{id := V}) ->
    V.

-spec name(ff_identity:class()) ->
    binary().

name(#{name := V}) ->
    V.

-spec contract_template(ff_identity:class()) ->
    ff_identity:contract_template_ref().

contract_template(#{contract_template_ref := V}) ->
    V.

-spec initial_level(ff_identity:class()) ->
    ff_identity:level_id().

initial_level(#{initial_level := V}) ->
    V.

-spec level(ff_identity:level_id(), ff_identity:class()) ->
    {ok, ff_identity:level()} |
    {error, notfound}.

level(ID, #{levels := Levels}) ->
    ff_map:find(ID, Levels).

-spec challenge_class(challenge_class_id(), ff_identity:class()) ->
    {ok, ff_identity_challenge:challenge_class()} |
    {error, notfound}.

challenge_class(ID, #{challenge_classes := ChallengeClasses}) ->
    ff_map:find(ID, ChallengeClasses).

%% Level

-spec level_name(ff_identity:level()) ->
    binary().

level_name(#{name := V}) ->
    V.

-spec contractor_level(ff_identity:level()) ->
    ff_identity:contractor_level().

contractor_level(#{contractor_level := V}) ->
    V.

%% Challenge

-spec challenge_class_name(ff_identity_challenge:challenge_class()) ->
    binary().

challenge_class_name(#{name := V}) ->
    V.

-spec base_level(ff_identity_challenge:challenge_class()) ->
    ff_identity:level_id().

base_level(#{base_level := V}) ->
    V.

-spec target_level(ff_identity_challenge:challenge_class()) ->
    ff_identity_challenge:level_id().

target_level(#{target_level := V}) ->
    V.
