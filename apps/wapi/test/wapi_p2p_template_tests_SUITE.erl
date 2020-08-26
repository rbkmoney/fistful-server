-module(wapi_p2p_template_tests_SUITE).

-behaviour(supervisor).

-include_lib("common_test/include/ct.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").
-include_lib("fistful_proto/include/ff_proto_p2p_template_thrift.hrl").

-export([init/1]).

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_ok_test/1]).


% common-api is used since it is the domain used in production RN
% TODO: change to wallet-api (or just omit since it is the default one) when new tokens will be a thing
-define(DOMAIN, <<"common-api">>).
-define(badresp(Code), {error, {invalid_response_code, Code}}).
-define(emptyresp(Code), {error, {Code, #{}}}).


-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].
-type group_name()      :: atom().


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
                create_ok_test
            ]
        }
    ].


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
                wapi_woody_client,
                wapi
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
    BasePermissions = [
        {[party], read},
        {[party], write}
    ],
    {ok, Token} = wapi_ct_helper:issue_token(Party, BasePermissions, {deadline, 10}, ?DOMAIN),
    {ok, WapiToken} = issue_wapi_token(Party),
    Config1 = [{party, Party} | Config],
    [
        {wapi_context, wapi_ct_helper:get_context(WapiToken)},
        {context, wapi_ct_helper:get_context(Token)} | Config1
    ];
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


-spec create_ok_test(config()) ->
    _.
create_ok_test(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_identity, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_p2p_template, fun('Create', _) -> {ok, ?P2PTEMPLATE(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_p2_p_templates_api:create_p2_p_transfer_template/3,
        #{
            body => #{
                <<"identityID">> => ?STRING,
                <<"details">> => #{
                    <<"body">> => #{
                        <<"value">> => #{
                            <<"currency">> => ?RUB,
                            <<"amount">> => ?INTEGER
                        }
                    },
                    <<"metadata">> => #{
                        <<"defaultMetadata">> => #{
                            <<"some key">> => <<"some value">>
                        }
                    }
                }
            }
        },
        ct_helper:cfg(wapi_context, C)
    ).



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



issue_wapi_token(Party) ->
    Permissions = [
        {[p2p], read},
        {[p2p], write},
        {[p2p_templates], read},
        {[p2p_templates], write}
    ],
    wapi_ct_helper:issue_token(Party, Permissions, unlimited, <<"wallet-api">>).

