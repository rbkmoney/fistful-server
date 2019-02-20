-module(ff_identity_handler_SUITE).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_identity_ok/1]).
-export([run_challenge_ok/1]).
-export([get_challenge_event_ok/1]).
-export([get_event_unknow_identity_ok/1]).
-export([start_challenge_token_fail/1]).
-export([get_challenges_ok/1]).

-spec create_identity_ok(config())           -> test_return().
-spec run_challenge_ok(config())             -> test_return().
-spec get_challenge_event_ok(config())       -> test_return().
-spec get_event_unknow_identity_ok(config()) -> test_return().
-spec start_challenge_token_fail(config())   -> test_return().
-spec get_challenges_ok(config())            -> test_return().
%%

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        get_challenges_ok,
        create_identity_ok,
        run_challenge_ok,
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
    PartyID = create_party(),
    EID     = genlib:unique(),
    ProvID  = <<"good-one">>,
    ClassID = <<"person">>,
    Name    = <<"Ricardo Milos">>,
    Ctx = #{<<"NS">> => #{<<"owner">> => PartyID}},
    Context = ff_context:wrap(Ctx),
    Identity = create_identity(EID, PartyID, ProvID, ClassID, Name, Context),

    IID = Identity#idnt_Identity.id,
    {ok, Identity_} = call_api('Get', [IID]),

    ProvID  = Identity_#idnt_Identity.provider,
    IID     = Identity_#idnt_Identity.id,
    PartyID = Identity_#idnt_Identity.party,
    ClassID = Identity_#idnt_Identity.cls,
    Ctx     = ff_context:unwrap(Identity_#idnt_Identity.context),
    ok.

run_challenge_ok(C) ->
    Context = #{<<"NS">> => nil},
    EID = genlib:unique(),
    PartyID     = create_party(),
    ChallengeID = genlib:unique(),
    ProvID      = <<"good-one">>,
    ClassID     = <<"person">>,
    ChlClassID  = <<"sword-initiation">>,
    Name        = <<"Ricardo Milos">>,
    IdentityState = create_identity(EID, PartyID, ProvID, ClassID, Name, ff_context:wrap(Context)),

    IID = IdentityState#idnt_Identity.id,
    Params2 = gen_challenge_param(ChlClassID, ChallengeID, C),
    {ok, Challenge} = call_api('StartChallenge', [IID, Params2]),

    ChallengeID = Challenge#idnt_Challenge.id,
    ChlClassID  = Challenge#idnt_Challenge.cls,
    Proofs = Params2#idnt_ChallengeParams.proofs,
    Proofs = Challenge#idnt_Challenge.proofs,
    true = {failed, #idnt_ChallengeFailed{}} =/= Challenge#idnt_Challenge.status.


get_challenge_event_ok(C) ->
    Context = ff_context:wrap(#{<<"NS">> => #{}}),
    %% Shadow identity
    EID_= genlib:unique(),
    PartyID_ = create_party(),
    ProvID   = <<"good-one">>,
    ClassID  = <<"person">>,
    Name_    = <<"BukaBjaka">>,
    _IdentityShadow = create_identity(EID_, PartyID_, ProvID, ClassID, Name_, Context),

    EID = genlib:unique(),
    PartyID    = create_party(),
    ChlClassID = <<"sword-initiation">>,
    Name       = <<"Ricardo Milos">>,
    Identity = create_identity(EID, PartyID, ProvID, ClassID, Name, Context),

    IID = Identity#idnt_Identity.id,
    Params2 = gen_challenge_param(ChlClassID, IID, C),
    {ok, _} = call_api('StartChallenge', [IID, Params2]),
    Range = #evsink_EventRange{
        limit = 1000,
        'after' = undefined
    },

    {completed, #idnt_ChallengeCompleted{resolution = approved}} = ct_helper:await(
        {completed, #idnt_ChallengeCompleted{resolution = approved}},
        fun () ->
            {ok, Events} = call_api('GetEvents', [IID, Range]),
            lists:foldl(fun(#idnt_IdentityEvent{change =
                                {identity_challenge,  #idnt_ChallengeChange{payload = {status_changed, Status}}}
                            },   _AccIn) -> Status;
                            (_Ev, AccIn) -> AccIn end, undefined, Events)

        end,
        genlib_retry:linear(10, 1000)
    ),
    {ok, Identity2} = call_api('Get', [IID]),
    EffectiveChl = Identity2#idnt_Identity.effective_challenge,
    true  = EffectiveChl =/= undefined,
    Level = Identity2#idnt_Identity.level,
    true  = Level =/= undefined.

get_event_unknow_identity_ok(_C) ->
    Ctx = ff_context:wrap(#{<<"NS">> => #{}}),
    [EID, _ChlID] = lists:map(fun(_) -> genlib:unique() end, [1, 2]),
    PID     = create_party(),
    ProvID  = <<"good-one">>,
    ClassID = <<"person">>,
    Name    = <<"Ricardo Milos">>,
    _Identity = create_identity(EID, PID, ProvID, ClassID, Name, Ctx),
    Range = #evsink_EventRange{
            limit = 1,
            'after' = undefined
        },
    {exception, {fistful_IdentityNotFound}} = call_api('GetEvents', [<<"bad id">>, Range]).

start_challenge_token_fail(C) ->
    Ctx = ff_context:wrap(#{<<"NS">> => #{}}),
    EID = genlib:unique(),
    PID = create_party(),
    ProvID     = <<"good-one">>,
    CID        = <<"person">>,
    ChlClassID = <<"sword-initiation">>,
    Name       = <<"Ricardo Milos">>,
    IdentityState = create_identity(EID, PID, ProvID, CID, Name, Ctx),
    {Type1, _Token1} = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    {Type2, _Token2} = ct_identdocstore:rus_domestic_passport(C),
    IID = IdentityState#idnt_Identity.id,
    Proofs = [
        #idnt_ChallengeProof{type = Type1, token = _Token1},
        #idnt_ChallengeProof{type = Type2, token = <<"Token">>}
    ],
    Params = #idnt_ChallengeParams{
        id     = IID,
        cls    = ChlClassID,
        proofs = Proofs
    },
    {exception, #fistful_ProofNotFound{}}
        = call_api('StartChallenge', [IID, Params]).

get_challenges_ok(C) ->
    Context = #{<<"NS">> => nil},
    EID = genlib:unique(),
    PartyID     = create_party(),
    ChallengeID = genlib:unique(),
    ProvID      = <<"good-one">>,
    ClassID     = <<"person">>,
    ChlClassID  = <<"sword-initiation">>,
    Name        = <<"Ricardo Milos">>,
    Identity = create_identity(EID, PartyID, ProvID, ClassID, Name, ff_context:wrap(Context)),

    IID = Identity#idnt_Identity.id,
    Params2 = gen_challenge_param(ChlClassID, ChallengeID, C),
    {ok, Challenge} = call_api('StartChallenge', [IID, Params2]),
    {ok, Challenges} = call_api('GetChallenges', [IID]),
    CID = Challenge#idnt_Challenge.id,
    [Chl] = lists:filter(fun(Item) ->
            CID =:= Item#idnt_Challenge.id
        end, Challenges),
    Cls = Challenge#idnt_Challenge.cls,
    Proofs = Challenge#idnt_Challenge.proofs,
    Cls = Chl#idnt_Challenge.cls,
    Proofs = Chl#idnt_Challenge.proofs.

%%----------
%% INTERNAL
%%----------
create_identity(EID, PartyID, ProvID, ClassID, Name, Ctx) ->
    IID = genlib:unique(),
    Params = #idnt_IdentityParams{
        name        = Name,
        party       = PartyID,
        provider    = ProvID,
        cls         = ClassID,
        external_id = EID,
        context     = Ctx
    },
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tp(ff_identity_handler, decode, '_'),
    {ok, IdentityState} = call_api('Create', [IID, Params]),
    dbg:stop_clear(),
    IdentityState.

gen_challenge_param(ClgClassID, ChallengeID, C) ->
    {Type1, Token1} = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    {Type2, Token2} = ct_identdocstore:rus_domestic_passport(C),

    Proofs = [
        #idnt_ChallengeProof{type = Type1, token = Token1},
        #idnt_ChallengeProof{type = Type2, token = Token2}
    ],
    #idnt_ChallengeParams{
        id           = ChallengeID,
        cls          = ClgClassID,
        proofs       = Proofs
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
