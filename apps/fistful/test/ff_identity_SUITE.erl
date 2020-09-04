-module(ff_identity_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_missing_fails/1]).
-export([create_missing_fails/1]).
-export([create_ok/1]).
-export([identify_ok/1]).

%%

-import(ff_pipeline, [unwrap/1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        get_missing_fails,
        create_missing_fails,
        create_ok,
        identify_ok
    ].

-spec get_missing_fails(config()) -> test_return().
-spec create_missing_fails(config()) -> test_return().
-spec create_ok(config()) -> test_return().
-spec identify_ok(config()) -> test_return().

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    ct_helper:makeup_cfg([
        ct_helper:test_case_name(init),
        ct_payment_system:setup()
    ], C).

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C),
    ok.

%%

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

%%

get_missing_fails(_C) ->
    ID = genlib:unique(),
    {error, notfound} = ff_identity_machine:get(ID).

create_missing_fails(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    Name = <<"Identity Name">>,
    {error, {provider, notfound}} = ff_identity_machine:create(
        #{
            id       => ID,
            name     => Name,
            party    => Party,
            provider => <<"who">>,
            class    => <<"person">>
        },
        ff_entity_context:new()
    ),
    {error, {identity_class, notfound}} = ff_identity_machine:create(
        #{
            id       => ID,
            name     => Name,
            party    => Party,
            provider => <<"good-one">>,
            class    => <<"nosrep">>
        },
        ff_entity_context:new()
    ).

create_ok(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    Name = <<"Identity Name">>,
    ok = ff_identity_machine:create(
        #{
            id       => ID,
            name     => Name,
            party    => Party,
            provider => <<"good-one">>,
            class    => <<"person">>
        },
        ff_entity_context:new()
    ),
    I1 = ff_identity_machine:identity(unwrap(ff_identity_machine:get(ID))),
    {ok, accessible} = ff_identity:is_accessible(I1),
    Party = ff_identity:party(I1),
    Party = ff_identity:party(I1).

identify_ok(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    Name = <<"Identity Name">>,
    ok = ff_identity_machine:create(
        #{
            id       => ID,
            name     => Name,
            party    => Party,
            provider => <<"good-one">>,
            class    => <<"person">>
        },
        ff_entity_context:new()
    ),
    ICID = genlib:unique(),
    {ok, S1} = ff_identity_machine:get(ID),
    I1 = ff_identity_machine:identity(S1),
    {error, notfound} = ff_identity:challenge(ICID, I1),
    D1 = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    D2 = ct_identdocstore:rus_domestic_passport(C),
    ChallengeParams = #{
        id     => ICID,
        class  => <<"sword-initiation">>
    },
    {error, {challenge, {proof, insufficient}}} = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => []}
    ),
    {error, {challenge, {proof, insufficient}}} = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => [D1]}
    ),
    ok = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => [D1, D2]}
    ),
    {error, {challenge, {pending, ICID}}} = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => [D1, D2]}
    ),
    {completed, _} = ct_helper:await(
        {completed, #{resolution => approved}},
        fun () ->
            {ok, S}  = ff_identity_machine:get(ID),
            {ok, IC} = ff_identity:challenge(ICID, ff_identity_machine:identity(S)),
            ff_identity_challenge:status(IC)
        end
    ),
    {ok, S3}  = ff_identity_machine:get(ID),
    I3 = ff_identity_machine:identity(S3),
    {ok, ICID} = ff_identity:effective_challenge(I3).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.
