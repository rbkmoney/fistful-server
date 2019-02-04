-module(ff_identity_handler_SUITE).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_identity_ok/1]).
-export([run_challenges_ok/1]).
-export([get_challenge_event_ok/1]).
-export([get_event_unknow_identity_ok/1]).
-export([start_challenge_token_fail/1]).

-spec create_identity_ok(config()) -> test_return().
-spec run_challenges_ok(config()) -> test_return().
-spec get_challenge_event_ok(config()) -> test_return().
-spec get_event_unknow_identity_ok(config()) -> test_return().
-spec start_challenge_token_fail(config()) -> test_return().
%%

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        create_identity_ok,
        run_challenges_ok,
        get_challenge_event_ok,
        get_event_unknow_identity_ok,
        start_challenge_token_fail
    ].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    ct_helper:makeup_cfg([
        ct_helper:test_case_name(init),
        ct_payment_system:setup()
    ], C).

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C).

%%

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ff_woody_ctx:set(ct_helper:get_woody_ctx(C1)),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok = ff_woody_ctx:unset().

%%-------
%% TESTS
%%-------

create_identity_ok(_C) ->
    Ctx = #{},
    [IID, EID, _ChlID] = lists:map(fun(_) -> genlib:unique() end, [1,2,3]),
    PID = create_party(),
    ProvID     = <<"good-one">>,
    CID        = <<"person">>,

    IdentityState = create_identity(IID, EID, PID, ProvID, CID, Ctx),

    IID = IdentityState#idnt_IdentityState.id,
    {ok, _IdentityState_} = call_api('Get', [IID]),
    ProvID = _IdentityState_#idnt_IdentityState.provider_id,
    IID    = _IdentityState_#idnt_IdentityState.id,
    PID    = _IdentityState_#idnt_IdentityState.party_id,
    CID    = _IdentityState_#idnt_IdentityState.class_id,
    ok.

run_challenges_ok(C) ->
    Ctx = #{},
    [IID, EID, ChlID] = lists:map(fun(_) -> genlib:unique() end, [1,2,3]),

    PID = create_party(),
    ProvID     = <<"good-one">>,
    ClassID    = <<"person">>,
    ChlClassID = <<"sword-initiation">>,

    _IdentityState = create_identity(IID, EID, PID, ProvID, ClassID, Ctx),

    Params2 = gen_challenge_param(ChlID, ChlClassID, {IID, EID, Ctx}, C),
    {ok, IdentityState2} = call_api('StartChallenges', [Params2]),
    #{ChlID := ChgState} = IdentityState2#idnt_IdentityState.challenges,
    {pending, #idnt_ChallengePending{}}  = ChgState#idnt_ChallengeState.status,

    {completed, approved} = ct_helper:await(
        {completed, approved},
        fun () ->
            {ok, State3} = call_api('Get', [IID]),
            #{ChlID := ChgState_} = State3#idnt_IdentityState.challenges,
            CS = ChgState_#idnt_ChallengeState.status,
            case CS of
                {completed, Complited} -> {completed, Complited#idnt_ChallengeCompleted.resolution};
                Other -> Other
            end
        end
    ),
    ok.

get_challenge_event_ok(C) ->
    Ctx = #{},
    [IID2, EID2, _ChlID2] = lists:map(fun(_) -> genlib:unique() end, [1,2,3]),

    PID2 = create_party(),
    ProvID2     = <<"good-one">>,
    CID2        = <<"person">>,

    _IdentityState23 = create_identity(IID2, EID2, PID2, ProvID2, CID2, #{}),

    [IID, EID, ChlID] = lists:map(fun(_) -> genlib:unique() end, [1,2,3]),

    PID = create_party(),
    ProvID     = <<"good-one">>,
    CID        = <<"person">>,
    ChlClassID = <<"sword-initiation">>,

    _IdentityState = create_identity(IID, EID, PID, ProvID, CID, Ctx),

    Params2 = gen_challenge_param(ChlID, ChlClassID, {IID, EID, Ctx}, C),
    {ok, _IdentityState2} = call_api('StartChallenges', [Params2]),

    IdentityEventParams = #idnt_IdentityEventParams{
        identity_id = IID,
        range = #evsink_EventRange{
            limit = 1000,
            'after' = undefined
        }
    },
    Ans = call_api('GetEvents', [IdentityEventParams]),
    lager:error("Ans:~n~p~n", [Ans]),
    % {ok, [Ev]} = call_api('GetEvents', [IdentityEventParams]),

    % 1 = Ev#idnt_IdentityEvent.sequence,
    % {created, Identity} = Ev#idnt_IdentityEvent.change,
    % PID    = Identity#idnt_Identity.party,
    % ProvID = Identity#idnt_Identity.provider,
    % CID    = Identity#idnt_Identity.cls,
    ok.

get_event_unknow_identity_ok(_C) ->
    Ctx = #{},
    [IID, EID, _ChlID] = lists:map(fun(_) -> genlib:unique() end, [1,2,3]),
    PID = create_party(),
    ProvID     = <<"good-one">>,
    CID        = <<"person">>,

    _IdentityState = create_identity(IID, EID, PID, ProvID, CID, Ctx),
    IdentityEventParams = #idnt_IdentityEventParams{
        identity_id = <<"bukabjaka">>,
        range = #evsink_EventRange{
            limit = 1,
            'after' = undefined
        }
    },
    {exception, {fistful_IdentityNotFound}} = call_api('GetEvents', [IdentityEventParams]).

start_challenge_token_fail(C) ->
    Ctx = #{},
    [IID, EID, ChlID] = lists:map(fun(_) -> genlib:unique() end, [1,2,3]),
    PID = create_party(),
    ProvID     = <<"good-one">>,
    CID        = <<"person">>,
    ChlClassID = <<"sword-initiation">>,

    _IdentityState = create_identity(IID, EID, PID, ProvID, CID, Ctx),
    {Type1, _Token1} = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    {Type2, _Token2} = ct_identdocstore:rus_domestic_passport(C),

    Proofs = [
        #idnt_ChallengeProof{type = Type1, token = _Token1},
        #idnt_ChallengeProof{type = Type2, token = <<"Pisun">>}
    ],
    Params = #idnt_ChallengeParams{
        id           = IID,
        challenge_id = ChlID,
        cls          = ChlClassID,
        proofs       = Proofs,
        external_id  = EID,
        context      = Ctx
    },
    {exception, {fistful_ChallengeError, challenge_proof_notfound}} = call_api('StartChallenges', [Params]).

%%----------
%% INTERNAL
%%----------
create_identity(IID, EID, PID, ProvID, ClassID, Ctx) ->
    Params = #idnt_IdentityParams{
        id          = IID,
        party_id    = PID,
        provider_id = ProvID,
        class_id    = ClassID,
        external_id = EID,
        context     = Ctx
    },
    {ok, IdentityState} = call_api('Create', [Params]),
    IdentityState.

gen_challenge_param(ID, CID, {IID, EID, Ctx}, C) ->
    {Type1, Token1} = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    {Type2, Token2} = ct_identdocstore:rus_domestic_passport(C),

    Proofs = [
        #idnt_ChallengeProof{type = Type1, token = Token1},
        #idnt_ChallengeProof{type = Type2, token = Token2}
    ],
    #idnt_ChallengeParams{
        id           = IID,
        challenge_id = ID,
        cls          = CID,
        proofs       = Proofs,
        external_id  = EID,
        context      = Ctx
    }.

call_api(Fun, Args) ->
    Service = {ff_proto_identity_thrift, 'Management'},
    Request = {Service, Fun, Args},
    Client = ff_woody_client:new(#{
        url           => <<"http://localhost:8022/v1/identity">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).

create_party() ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.
%% CONFIGS

-include_lib("ff_cth/include/ct_domain.hrl").
