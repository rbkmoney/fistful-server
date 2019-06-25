-module(wapi_identity_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").

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
    get_identity/1,
    create_identity_challenge/1,
    get_identity_challenge/1,
    list_identity_challenges/1,
    get_identity_challenge_event/1,
    poll_identity_challenge_events/1
]).

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
                get_identity,
                create_identity_challenge,
                get_identity_challenge,
                list_identity_challenges,
                get_identity_challenge_event,
                poll_identity_challenge_events
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
    ok = ff_woody_ctx:set(woody_context:new(<<"init_per_group/", (atom_to_binary(Group, utf8))/binary>>)),
    Party = create_party(Config),
    Token = issue_token(Party, [{[party], write}], unlimited),
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
    ok = ff_woody_ctx:set(ct_helper:get_woody_ctx(C1)),
    [{test_sup, wapi_ct_helper:start_mocked_service_sup(?MODULE)} | C1].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    ok = ff_woody_ctx:unset(),
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
    {ok, _} = call_api(
        fun swag_client_wallet_identities_api:create_identity/3,
        #{
            body => #{
                <<"name">> => ?STRING,
                <<"class">> => ?STRING,
                <<"provider">> => ?STRING
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec get_identity(config()) ->
    _.
get_identity(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun('Get', _) -> {ok, ?IDENTITY(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_identities_api:get_identity/3,
        #{
            binding => #{
                <<"identityID">> => ?STRING
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec create_identity_challenge(config()) ->
    _.
create_identity_challenge(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun
            ('Get', _) -> {ok, ?IDENTITY(PartyID)};
            ('StartChallenge', _) -> {ok, ?IDENTITY_CHALLENGE(?IDENTITY_CHALLENGE_STATUS_COMPLETED)}
        end},
        {identdoc_storage, fun('Get', _) -> {ok, ?IDENT_DOC} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_identities_api:start_identity_challenge/3,
        #{
            binding => #{
                <<"identityID">> => ?STRING
            },
            body => #{
                <<"type">> => ?STRING,
                <<"proofs">> => [
                    #{
                        <<"token">> => ?STRING
                    }
                ]
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec get_identity_challenge(config()) ->
    _.
get_identity_challenge(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun
            ('Get', _) -> {ok, ?IDENTITY(PartyID)};
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
            ('Get', _) -> {ok, ?IDENTITY(PartyID)};
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

-spec get_identity_challenge_event(config()) ->
    _.
get_identity_challenge_event(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun
            ('Get', _) -> {ok, ?IDENTITY(PartyID)};
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
            ('Get', _) -> {ok, ?IDENTITY(PartyID)};
            ('GetEvents', _) -> {ok, [?IDENTITY_CHALLENGE_EVENT(?CHALLENGE_STATUS_CHANGE)]}
        end}
    ], C),
    {ok, _} = call_api(
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

%%

-spec call_api(function(), map(), wapi_client_lib:context()) ->
    {ok, term()} | {error, term()}.
call_api(F, Params, Context) ->
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = F(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

issue_token(PartyID, ACL, LifeTime) ->
    Claims = #{?STRING => ?STRING},
    {ok, Token} = wapi_authorizer_jwt:issue({{PartyID, wapi_acl:from_list(ACL)}, Claims}, LifeTime),
    Token.

