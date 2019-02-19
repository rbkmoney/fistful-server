-module(wapi_wallet_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").
-include_lib("fistful_reporter_proto/include/ff_reporter_reports_thrift.hrl").
-include_lib("file_storage_proto/include/fs_file_storage_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("jose/include/jose_jwk.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

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
    create_report_ok_test/1,
    get_report_ok_test/1,
    get_reports_ok_test/1,
    download_file_ok_test/1
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
        {base, [sequence],
            [
                create_report_ok_test,
                get_report_ok_test,
                get_reports_ok_test,
                download_file_ok_test
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
    ct_helper:makeup_cfg([
        ct_helper:test_case_name(init),
        ct_payment_system:setup(#{
            optional_apps => [wapi]
        })
    ], Config).
    % [{apps, wapi_ct_helper:start_wapi(Config1)} | Config1].

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    %% TODO remove this after cut off wapi
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

-spec create_report_ok_test(config()) ->
    _.
create_report_ok_test(C) ->
    {ok, Identity} = create_identity(C),
    IdentityID = maps:get(<<"id">>, Identity),
    wapi_ct_helper:mock_services([{fistful_report, fun
        ('GenerateReport', _) -> {ok, ?REPORT_ID};
        ('GetReport', _) -> {ok, ?REPORT}
    end}], C),
    {ok, _} = call_api(
        fun swag_client_wallet_reports_api:create_report/3,
        #{
            binding => #{
                <<"identityID">> => IdentityID
            },
            body => #{
                <<"reportType">> => <<"withdrawalRegistry">>,
                <<"fromTime">> => ?TIMESTAMP,
                <<"toTime">> => ?TIMESTAMP
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec get_report_ok_test(config()) ->
    _.
get_report_ok_test(C) ->
    {ok, Identity} = create_identity(C),
    IdentityID = maps:get(<<"id">>, Identity),
    wapi_ct_helper:mock_services([{fistful_report, fun
        ('GetReport', _) -> {ok, ?REPORT}
    end}], C),
    {ok, _} = call_api(
        fun swag_client_wallet_reports_api:get_report/3,
        #{
            binding => #{
                <<"identityID">> => IdentityID,
                <<"reportID">> => ?INTEGER
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec get_reports_ok_test(config()) ->
    _.
get_reports_ok_test(C) ->
    {ok, Identity} = create_identity(C),
    IdentityID = maps:get(<<"id">>, Identity),
    wapi_ct_helper:mock_services([{fistful_report, fun
        ('GetReports', _) -> {ok, [?REPORT, ?REPORT, ?REPORT]}
    end}], C),
    {ok, _} = call_api(
        fun swag_client_wallet_reports_api:get_reports/3,
        #{
            binding => #{
                <<"identityID">> => IdentityID
            },
            qs_val => #{
                <<"fromTime">> => ?TIMESTAMP,
                <<"toTime">> => ?TIMESTAMP,
                <<"type">> => <<"withdrawalRegistry">>
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec download_file_ok_test(config()) ->
    _.
download_file_ok_test(C) ->
    wapi_ct_helper:mock_services([{file_storage, fun
        ('GenerateDownloadUrl', _) -> {ok, ?STRING}
    end}], C),
    {ok, _} = call_api(
        fun swag_client_wallet_downloads_api:download_file/3,
        #{
            binding => #{
                <<"fileID">> => ?STRING
            },
            qs_val => #{
                <<"expiresAt">> => ?TIMESTAMP
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

create_identity(C) ->
    PartyID = ?config(party, C),
    Params = #{
        <<"provider">> => <<"good-one">>,
        <<"class">> => <<"person">>,
        <<"name">> => <<"HAHA NO2">>
    },
    wapi_wallet_ff_backend:create_identity(Params, create_auth_ctx(PartyID)).

create_auth_ctx(PartyID) ->
    #{
        swagger_context => #{auth_context => {{PartyID, empty}, empty}}
    }.

issue_token(PartyID, ACL, LifeTime) ->
    Claims = #{?STRING => ?STRING},
    {ok, Token} = wapi_authorizer_jwt:issue({{PartyID, wapi_acl:from_list(ACL)}, Claims}, LifeTime),
    Token.

