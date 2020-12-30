-module(ff_identity_handler_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_identity_ok/1]).
-export([run_challenge_ok/1]).
-export([get_challenge_event_ok/1]).
-export([get_event_unknown_identity_ok/1]).
-export([start_challenge_token_fail/1]).
-export([get_challenges_ok/1]).

-spec create_identity_ok(config()) -> test_return().
-spec run_challenge_ok(config()) -> test_return().
-spec get_challenge_event_ok(config()) -> test_return().
-spec get_event_unknown_identity_ok(config()) -> test_return().
-spec start_challenge_token_fail(config()) -> test_return().
-spec get_challenges_ok(config()) -> test_return().

%%

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        get_challenges_ok,
        create_identity_ok,
        run_challenge_ok,
        get_challenge_event_ok,
        get_event_unknown_identity_ok,
        start_challenge_token_fail
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    ct_helper:makeup_cfg(
        [
            ct_helper:test_case_name(init),
            ct_payment_system:setup()
        ],
        C
    ).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C).

%%

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

%%-------
%% TESTS
%%-------

create_identity_ok(_C) ->
    PartyID = create_party(),
    EID = genlib:unique(),
    Name = <<"Identity Name">>,
    ProvID = <<"good-one">>,
    ClassID = <<"person">>,
    Ctx = #{<<"NS">> => #{<<"owner">> => PartyID}},
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Identity = create_identity(EID, Name, PartyID, ProvID, ClassID, Ctx, Metadata),
    IID = Identity#idnt_IdentityState.id,
    {ok, Identity_} = call_api('Get', {IID, #'EventRange'{}}),

    ProvID = Identity_#idnt_IdentityState.provider_id,
    IID = Identity_#idnt_IdentityState.id,
    Name = Identity_#idnt_IdentityState.name,
    PartyID = Identity_#idnt_IdentityState.party_id,
    ClassID = Identity_#idnt_IdentityState.class_id,
    unblocked = Identity_#idnt_IdentityState.blocking,
    Metadata = Identity_#idnt_IdentityState.metadata,
    Ctx0 = Ctx#{
        <<"com.rbkmoney.wapi">> => #{<<"name">> => Name}
    },
    Ctx0 = ff_entity_context_codec:unmarshal(Identity_#idnt_IdentityState.context),
    ok.

run_challenge_ok(C) ->
    Context = #{<<"NS">> => nil},
    EID = genlib:unique(),
    PartyID = create_party(),
    ChallengeID = genlib:unique(),
    Name = <<"Identity Name">>,
    ProvID = <<"good-one">>,
    ClassID = <<"person">>,
    ChlClassID = <<"sword-initiation">>,
    IdentityState = create_identity(EID, Name, PartyID, ProvID, ClassID, Context),

    IID = IdentityState#idnt_IdentityState.id,
    Params2 = gen_challenge_param(ChlClassID, ChallengeID, C),
    {ok, Challenge} = call_api('StartChallenge', {IID, Params2}),

    ChallengeID = Challenge#idnt_ChallengeState.id,
    ChlClassID = Challenge#idnt_ChallengeState.cls,
    Proofs = Params2#idnt_ChallengeParams.proofs,
    Proofs = Challenge#idnt_ChallengeState.proofs,
    true = {failed, #idnt_ChallengeFailed{}} =/= Challenge#idnt_ChallengeState.status.

get_challenge_event_ok(C) ->
    Context = #{<<"NS">> => #{}},
    ProvID = <<"good-one">>,
    ClassID = <<"person">>,
    EID = genlib:unique(),
    PartyID = create_party(),
    Name = <<"Identity Name">>,
    ChlClassID = <<"sword-initiation">>,
    Identity = create_identity(EID, Name, PartyID, ProvID, ClassID, Context),

    IID = Identity#idnt_IdentityState.id,
    Params2 = gen_challenge_param(ChlClassID, IID, C),
    {ok, _} = call_api('StartChallenge', {IID, Params2}),
    Range = #'EventRange'{
        limit = 1000,
        'after' = undefined
    },

    FindStatusChanged = fun
        (#idnt_Event{change = {identity_challenge, ChallengeChange}}, AccIn) ->
            case ChallengeChange#idnt_ChallengeChange.payload of
                {status_changed, Status} -> Status;
                _Other -> AccIn
            end;
        (_Ev, AccIn) ->
            AccIn
    end,

    {completed, #idnt_ChallengeCompleted{resolution = approved}} = ct_helper:await(
        {completed, #idnt_ChallengeCompleted{resolution = approved}},
        fun() ->
            {ok, Events} = call_api('GetEvents', {IID, Range}),
            lists:foldl(FindStatusChanged, undefined, Events)
        end,
        genlib_retry:linear(10, 1000)
    ),
    {ok, Identity2} = call_api('Get', {IID, #'EventRange'{}}),
    ?assertNotEqual(undefined, Identity2#idnt_IdentityState.effective_challenge_id),
    ?assertNotEqual(undefined, Identity2#idnt_IdentityState.level_id).

get_event_unknown_identity_ok(_C) ->
    Ctx = #{<<"NS">> => #{}},
    EID = genlib:unique(),
    PID = create_party(),
    Name = <<"Identity Name">>,
    ProvID = <<"good-one">>,
    ClassID = <<"person">>,
    create_identity(EID, Name, PID, ProvID, ClassID, Ctx),
    Range = #'EventRange'{
        limit = 1,
        'after' = undefined
    },
    {exception, {fistful_IdentityNotFound}} = call_api('GetEvents', {<<"bad id">>, Range}).

start_challenge_token_fail(C) ->
    Ctx = #{<<"NS">> => #{}},
    EID = genlib:unique(),
    PID = create_party(),
    Name = <<"Identity Name">>,
    ProvID = <<"good-one">>,
    CID = <<"person">>,
    ChlClassID = <<"sword-initiation">>,
    IdentityState = create_identity(EID, Name, PID, ProvID, CID, Ctx),
    {Type1, Token1} = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    {Type2, _Token2} = ct_identdocstore:rus_domestic_passport(C),
    IID = IdentityState#idnt_IdentityState.id,
    Proofs = [
        #idnt_ChallengeProof{type = Type1, token = Token1},
        #idnt_ChallengeProof{type = Type2, token = <<"Token">>}
    ],
    Params = #idnt_ChallengeParams{
        id = IID,
        cls = ChlClassID,
        proofs = Proofs
    },
    {exception, #fistful_ProofNotFound{}} =
        call_api('StartChallenge', {IID, Params}).

get_challenges_ok(C) ->
    Context = #{<<"NS">> => nil},
    EID = genlib:unique(),
    PartyID = create_party(),
    ChallengeID = genlib:unique(),
    Name = <<"Identity Name">>,
    ProvID = <<"good-one">>,
    ClassID = <<"person">>,
    ChlClassID = <<"sword-initiation">>,
    Identity = create_identity(EID, Name, PartyID, ProvID, ClassID, Context),

    IID = Identity#idnt_IdentityState.id,
    Params2 = gen_challenge_param(ChlClassID, ChallengeID, C),
    {ok, Challenge} = call_api('StartChallenge', {IID, Params2}),
    {ok, Challenges} = call_api('GetChallenges', {IID}),
    CID = Challenge#idnt_ChallengeState.id,
    [Chl] = lists:filter(
        fun(Item) ->
            CID =:= Item#idnt_ChallengeState.id
        end,
        Challenges
    ),
    ?assertEqual(Chl#idnt_ChallengeState.cls, Challenge#idnt_ChallengeState.cls),
    ?assertEqual(Chl#idnt_ChallengeState.proofs, Challenge#idnt_ChallengeState.proofs).

%%----------
%% INTERNAL
%%----------
create_identity(EID, Name, PartyID, ProvID, ClassID, Ctx) ->
    create_identity(EID, Name, PartyID, ProvID, ClassID, Ctx, #{}).

create_identity(EID, Name, PartyID, ProvID, ClassID, Ctx, Metadata) ->
    Params = #idnt_IdentityParams{
        id = genlib:unique(),
        name = Name,
        party = PartyID,
        provider = ProvID,
        cls = ClassID,
        external_id = EID,
        metadata = Metadata
    },
    Context = ff_entity_context_codec:marshal(Ctx#{
        <<"com.rbkmoney.wapi">> => #{<<"name">> => Name}
    }),
    {ok, IdentityState} = call_api('Create', {Params, Context}),
    IdentityState.

gen_challenge_param(ClgClassID, ChallengeID, C) ->
    {Type1, Token1} = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    {Type2, Token2} = ct_identdocstore:rus_domestic_passport(C),

    Proofs = [
        #idnt_ChallengeProof{type = Type1, token = Token1},
        #idnt_ChallengeProof{type = Type2, token = Token2}
    ],
    #idnt_ChallengeParams{
        id = ChallengeID,
        cls = ClgClassID,
        proofs = Proofs
    }.

call_api(Fun, Args) ->
    Service = {ff_proto_identity_thrift, 'Management'},
    Request = {Service, Fun, Args},
    Client = ff_woody_client:new(#{
        url => <<"http://localhost:8022/v1/identity">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).

create_party() ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

%% CONFIGS

-include_lib("ff_cth/include/ct_domain.hrl").
