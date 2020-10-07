-module(wapi_identity_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("damsel/include/dmsl_webhooker_thrift.hrl").

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("identdocstore_proto/include/identdocstore_identity_document_storage_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([
    create_identity/1,
    create_identity_provider_notfound/1,
    create_identity_class_notfound/1,
    create_identity_party_inaccessible/1,
    create_identity_thrift_name/1,
    get_identity/1,
    get_identity_notfound/1,
    create_identity_challenge/1,
    create_identity_challenge_identity_notfound/1,
    create_identity_challenge_challenge_pending/1,
    create_identity_challenge_class_notfound/1,
    create_identity_challenge_level_incorrect/1,
    create_identity_challenge_conflict/1,
    create_identity_challenge_proof_notfound/1,
    create_identity_challenge_insufficient/1,
    get_identity_challenge/1,
    list_identity_challenges/1,
    list_identity_challenges_identity_notfound/1,
    get_identity_challenge_event/1,
    poll_identity_challenge_events/1,
    poll_identity_challenge_events_identity_notfound/1
]).

% common-api is used since it is the domain used in production RN
% TODO: change to wallet-api (or just omit since it is the default one) when new tokens will be a thing
-define(DOMAIN, <<"common-api">>).
-define(badresp(Code), {error, {invalid_response_code, Code}}).
-define(emptyresp(Code), {error, {Code, #{}}}).

-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].
-type group_name()      :: atom().

-behaviour(supervisor).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() ->
    [test_case_name()].
all() ->
    [
        {group, base}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {base, [],
            [
                create_identity,
                create_identity_provider_notfound,
                create_identity_class_notfound,
                create_identity_party_inaccessible,
                create_identity_thrift_name,
                get_identity,
                get_identity_notfound,
                create_identity_challenge,
                create_identity_challenge_identity_notfound,
                create_identity_challenge_challenge_pending,
                create_identity_challenge_class_notfound,
                create_identity_challenge_level_incorrect,
                create_identity_challenge_conflict,
                create_identity_challenge_proof_notfound,
                create_identity_challenge_insufficient,
                get_identity_challenge,
                list_identity_challenges,
                list_identity_challenges_identity_notfound,
                get_identity_challenge_event,
                poll_identity_challenge_events,
                poll_identity_challenge_events_identity_notfound
            ]
        }
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config) ->
    %% TODO remove this after cut off wapi
    ok = application:set_env(wapi, transport, thrift),
    ct_helper:makeup_cfg([
        ct_helper:test_case_name(init),
        ct_payment_system:setup(#{
            optional_apps => [
                bender_client,
                wapi,
                wapi_woody_client
            ]
        })
    ], Config).

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    %% TODO remove this after cut off wapi
    ok = application:unset_env(wapi, transport),
    ok = ct_payment_system:shutdown(C).

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(Group, Config) when Group =:= base ->
    ok = ff_context:save(ff_context:create(#{
        party_client => party_client:create_client(),
        woody_context => woody_context:new(<<"init_per_group/", (atom_to_binary(Group, utf8))/binary>>)
    })),
    Party = create_party(Config),
    {ok, Token} = wapi_ct_helper:issue_token(Party, [{[party], write}], unlimited, ?DOMAIN),
    Config1 = [{party, Party} | Config],
    [{context, wapi_ct_helper:get_context(Token)} | Config1];
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) ->
    _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    [{test_sup, wapi_ct_helper:start_mocked_service_sup(?MODULE)} | C1].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    ok = ct_helper:unset_context(),
    wapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests
-spec create_identity(config()) ->
    _.
create_identity(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun('Create', _) -> {ok, ?IDENTITY(PartyID)} end}
    ], C),
    {ok, _} = create_identity_call_api(C).

-spec create_identity_provider_notfound(config()) ->
    _.
create_identity_provider_notfound(C) ->
    wapi_ct_helper:mock_services([
        {fistful_identity, fun('Create', _) -> throw(#fistful_ProviderNotFound{}) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such provider">>}}},
        create_identity_call_api(C)
    ).

-spec create_identity_class_notfound(config()) ->
    _.
create_identity_class_notfound(C) ->
    wapi_ct_helper:mock_services([
        {fistful_identity, fun('Create', _) -> throw(#fistful_IdentityClassNotFound{}) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity class">>}}},
        create_identity_call_api(C)
    ).

-spec create_identity_party_inaccessible(config()) ->
    _.
create_identity_party_inaccessible(C) ->
    wapi_ct_helper:mock_services([
        {fistful_identity, fun('Create', _) -> throw(#fistful_PartyInaccessible{}) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Identity inaccessible">>}}},
        create_identity_call_api(C)
    ).

-spec create_identity_thrift_name(config()) ->
    _.
create_identity_thrift_name(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun('Create', _) ->
            {ok, ?IDENTITY(PartyID, ?DEFAULT_CONTEXT_NO_NAME(PartyID))}
        end}
    ], C),
    {ok, #{<<"name">> := ?STRING}} = create_identity_call_api(C).

-spec get_identity(config()) ->
    _.
get_identity(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun('Get', _) -> {ok, ?IDENTITY(PartyID)} end}
    ], C),
    {ok, _} = get_identity_call_api(C).

-spec get_identity_notfound(config()) ->
    _.
get_identity_notfound(C) ->
    wapi_ct_helper:mock_services([
        {fistful_identity, fun('Get', _) -> throw(#fistful_IdentityNotFound{}) end}
    ], C),
    ?assertEqual(
        {error, {404, #{}}},
        get_identity_call_api(C)
    ).

-spec create_identity_challenge(config()) ->
    _.
create_identity_challenge(C) ->
    create_identity_challenge_start_mocks(
        C,
        fun() -> {ok, ?IDENTITY_CHALLENGE(?IDENTITY_CHALLENGE_STATUS_COMPLETED)} end
    ),
    {ok, _} = create_identity_challenge_call_api(C).

-spec create_identity_challenge_identity_notfound(config()) ->
    _.
create_identity_challenge_identity_notfound(C) ->
    create_identity_challenge_start_mocks(C, fun() -> throw(#fistful_IdentityNotFound{}) end),
    ?assertEqual(
        {error, {404, #{}}},
        create_identity_challenge_call_api(C)
    ).

-spec create_identity_challenge_challenge_pending(config()) ->
    _.
create_identity_challenge_challenge_pending(C) ->
    create_identity_challenge_start_mocks(C, fun() -> throw(#fistful_ChallengePending{}) end),
    ?assertEqual(
        {error, {409, #{}}},
        create_identity_challenge_call_api(C)
    ).

-spec create_identity_challenge_class_notfound(config()) ->
    _.
create_identity_challenge_class_notfound(C) ->
    create_identity_challenge_start_mocks(C, fun() -> throw(#fistful_ChallengeClassNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such challenge type">>}}},
        create_identity_challenge_call_api(C)
    ).

-spec create_identity_challenge_level_incorrect(config()) ->
    _.
create_identity_challenge_level_incorrect(C) ->
    create_identity_challenge_start_mocks(C, fun() -> throw(#fistful_ChallengeLevelIncorrect{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Illegal identification type for current identity level">>}}},
        create_identity_challenge_call_api(C)
    ).

-spec create_identity_challenge_conflict(config()) ->
    _.
create_identity_challenge_conflict(C) ->
    create_identity_challenge_start_mocks(C, fun() -> throw(#fistful_ChallengeConflict{}) end),
    ?assertEqual(
        {error, {409, #{}}},
        create_identity_challenge_call_api(C)
    ).

-spec create_identity_challenge_proof_notfound(config()) ->
    _.
create_identity_challenge_proof_notfound(C) ->
    create_identity_challenge_start_mocks(C, fun() -> throw(#fistful_ProofNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Proof not found">>}}},
        create_identity_challenge_call_api(C)
    ).

-spec create_identity_challenge_insufficient(config()) ->
    _.
create_identity_challenge_insufficient(C) ->
    create_identity_challenge_start_mocks(C, fun() -> throw(#fistful_ProofInsufficient{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Insufficient proof">>}}},
        create_identity_challenge_call_api(C)
    ).

-spec get_identity_challenge(config()) ->
    _.
get_identity_challenge(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun
            ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
            ('GetChallenges', _) -> {ok, [?IDENTITY_CHALLENGE(?IDENTITY_CHALLENGE_STATUS_COMPLETED)]}
        end},
        {identdoc_storage, fun('Get', _) -> {ok, ?IDENT_DOC} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_identities_api:get_identity_challenge/3,
        #{
            binding => #{
                <<"identityID">> => ?STRING,
                <<"challengeID">> => ?STRING
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec list_identity_challenges(config()) ->
    _.
list_identity_challenges(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun
            ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
            ('GetChallenges', _) -> {ok, [?IDENTITY_CHALLENGE(?IDENTITY_CHALLENGE_STATUS_COMPLETED)]}
        end},
        {identdoc_storage, fun('Get', _) -> {ok, ?IDENT_DOC} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_identities_api:list_identity_challenges/3,
        #{
            binding => #{
                <<"identityID">> => ?STRING
            },
            qs_val => #{
                <<"status">> => <<"Completed">>
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec list_identity_challenges_identity_notfound(config()) ->
    _.
list_identity_challenges_identity_notfound(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun
            ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
            ('GetChallenges', _) -> throw(#fistful_IdentityNotFound{})
        end},
        {identdoc_storage, fun('Get', _) -> {ok, ?IDENT_DOC} end}
    ], C),
    ?assertEqual(
        {error, {404, #{}}},
        call_api(
            fun swag_client_wallet_identities_api:list_identity_challenges/3,
            #{
                binding => #{
                    <<"identityID">> => ?STRING
                },
                qs_val => #{
                    <<"status">> => <<"Completed">>
                }
            },
            ct_helper:cfg(context, C)
        )
    ).

-spec get_identity_challenge_event(config()) ->
    _.
get_identity_challenge_event(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun
            ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
            ('GetEvents', _) -> {ok, [?IDENTITY_CHALLENGE_EVENT(?CHALLENGE_STATUS_CHANGE)]}
        end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_identities_api:get_identity_challenge_event/3,
        #{
            binding => #{
                <<"identityID">> => ?STRING,
                <<"challengeID">> => ?STRING,
                <<"eventID">> => ?INTEGER
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec poll_identity_challenge_events(config()) ->
    _.
poll_identity_challenge_events(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun
            ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
            ('GetEvents', _) -> {ok, [?IDENTITY_CHALLENGE_EVENT(?CHALLENGE_STATUS_CHANGE)]}
        end}
    ], C),
    {ok, _} = poll_identity_challenge_events_call_api(C).

-spec poll_identity_challenge_events_identity_notfound(config()) ->
    _.
poll_identity_challenge_events_identity_notfound(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun
            ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
            ('GetEvents', _) -> throw(#fistful_IdentityNotFound{})
        end}
    ], C),
    ?assertEqual(
        {error, {404, #{}}},
        poll_identity_challenge_events_call_api(C)
    ).

%%
create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

-spec call_api(function(), map(), wapi_client_lib:context()) ->
    {ok, term()} | {error, term()}.
call_api(F, Params, Context) ->
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = F(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).

create_identity_challenge_start_mocks(C, StartChallengeResultFun) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun
            ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
            ('StartChallenge', _) -> StartChallengeResultFun()
        end},
        {identdoc_storage, fun('Get', _) -> {ok, ?IDENT_DOC} end}
    ], C).

create_identity_call_api(C) ->
    call_api(
        fun swag_client_wallet_identities_api:create_identity/3,
        #{
            body => #{
                <<"name">> => ?STRING,
                <<"class">> => ?STRING,
                <<"provider">> => ?STRING,
                <<"metadata">> => #{
                    <<"somedata">> => ?STRING
                }
            }
        },
        ct_helper:cfg(context, C)
    ).

get_identity_call_api(C) ->
    call_api(
        fun swag_client_wallet_identities_api:get_identity/3,
        #{
            binding => #{
                <<"identityID">> => ?STRING
            }
        },
        ct_helper:cfg(context, C)
    ).

create_identity_challenge_call_api(C) ->
    call_api(
        fun swag_client_wallet_identities_api:start_identity_challenge/3,
        #{
            binding => #{
                <<"identityID">> => ?STRING
            },
            body => #{
                <<"type">> => <<"sword-initiation">>,
                <<"proofs">> => [
                    #{
                        <<"token">> => wapi_utils:map_to_base64url(#{
                            <<"type">> => <<"RUSRetireeInsuranceCertificate">>,
                            <<"token">> => ?STRING
                        })
                    },
                    #{
                        <<"token">> => wapi_utils:map_to_base64url(#{
                            <<"type">> => <<"RUSDomesticPassport">>,
                            <<"token">> => ?STRING
                        })
                    }
                ]
            }
        },
        ct_helper:cfg(context, C)
    ).

poll_identity_challenge_events_call_api(C) ->
    call_api(
        fun swag_client_wallet_identities_api:poll_identity_challenge_events/3,
        #{
            binding => #{
                <<"identityID">> => ?STRING,
                <<"challengeID">> => ?STRING
            },
            qs_val => #{
                <<"limit">> => 551,
                <<"eventCursor">> => ?INTEGER
            }
        },
        ct_helper:cfg(context, C)
    ).

