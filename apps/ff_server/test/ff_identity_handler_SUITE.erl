-module(ff_identity_handler_SUITE).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_identity_ok/1]).
-export([run_challenges_ok/1]).

-spec create_identity_ok(config()) -> test_return().
-spec run_challenges_ok(config()) -> test_return().
%%

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        create_identity_ok,
        run_challenges_ok
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
    IdentityID = genlib:unique(),
    ProviderID = <<"good-one">>,
    ClassID = <<"person">>,
    ExternalID = genlib:unique(),
    Ctx = #{},
    Params = #idnt_IdentityParams{
        id          = IdentityID,
        party_id    = PartyID,
        provider_id = ProviderID,
        class_id    = ClassID,
        external_id = ExternalID,
        context     = Ctx
    },
    {ok, IdentityState} = api_call('Create', [Params]),
    IID = IdentityState#idnt_IdentityState.id,
    {ok, _IdentityState_} = api_call('Get', [IID]),
    ProviderID = _IdentityState_#idnt_IdentityState.provider_id,
    IdentityID = _IdentityState_#idnt_IdentityState.id,
    PartyID    = _IdentityState_#idnt_IdentityState.party_id,
    ClassID    = _IdentityState_#idnt_IdentityState.class_id,
    ok.

run_challenges_ok(_C) ->
    IdentityID = genlib:unique(),
    PartyID = create_party(),
    ProviderID = <<"good-one">>,
    ClassID = <<"person">>,
    ExternalID = genlib:unique(),
    Ctx = #{},
    Params = #idnt_IdentityParams{
        id          = IdentityID,
        party_id    = PartyID,
        provider_id = ProviderID,
        class_id    = ClassID,
        external_id = ExternalID,
        context     = Ctx
    },
    {ok, _IdentityState} = api_call('Create', [Params]),
    ChallengeID = <<"person">>,
    ChlClassID =  <<"sword-initiation">>,

    {Type1, Token1} = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), _C),
    {Type2, Token2} = ct_identdocstore:rus_domestic_passport(_C),

    Proofs = [
        #idnt_ChallengeProof{type = Type1, token = Token1},
        #idnt_ChallengeProof{type = Type2, token = Token2}
    ],
    Params2 = #idnt_ChallengeParams{
        id           = IdentityID,
        challenge_id = ChallengeID,
        cls          = ChlClassID,
        proofs       = Proofs,
        external_id  = ExternalID,
        context      = Ctx
    },
    {ok, IdentityState} = api_call('StartChallenges', [Params2]),
    lager:error("~n>>>~nState: ~n~p~n", [IdentityState]),
    ok.

%%----------
%% INTERNAL
%%----------
api_call(Fun, Args) ->
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
