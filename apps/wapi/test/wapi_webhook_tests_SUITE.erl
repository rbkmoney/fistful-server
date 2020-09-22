-module(wapi_webhook_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_webhooker_thrift.hrl").

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
    create_webhook_ok_test/1,
    create_withdrawal_webhook_ok_test/1,
    get_webhooks_ok_test/1,
    get_webhook_ok_test/1,
    delete_webhook_ok_test/1
]).

-define(badresp(Code), {error, {invalid_response_code, Code}}).
-define(emptyresp(Code), {error, {Code, #{}}}).

-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].
-type group_name()      :: atom().

% common-api is used since it is the domain used in production RN
% TODO: change to wallet-api (or just omit since it is the default one) when new tokens will be a thing
-define(DOMAIN, <<"common-api">>).

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
                create_webhook_ok_test,
                create_withdrawal_webhook_ok_test,
                get_webhooks_ok_test,
                get_webhook_ok_test,
                delete_webhook_ok_test
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

-spec create_webhook_ok_test(config()) ->
    _.
create_webhook_ok_test(C) ->
    {ok, Identity} = create_identity(C),
    IdentityID = maps:get(<<"id">>, Identity),
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {webhook_manager, fun('Create', _) -> {ok, ?WEBHOOK(?DESTINATION_EVENT_FILTER)} end},
        {fistful_identity, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_webhooks_api:create_webhook/3,
        #{
            body => #{
                <<"identityID">> => IdentityID,
                <<"url">> => ?STRING,
                <<"scope">> => #{
                    <<"topic">> => <<"DestinationsTopic">>,
                    <<"eventTypes">> => [<<"DestinationCreated">>]
                }
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec create_withdrawal_webhook_ok_test(config()) ->
    _.
create_withdrawal_webhook_ok_test(C) ->
    {ok, Identity} = create_identity(C),
    IdentityID = maps:get(<<"id">>, Identity),
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {webhook_manager, fun('Create', _) -> {ok, ?WEBHOOK(?WITHDRAWAL_EVENT_FILTER)} end},
        {fistful_identity, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end}
    ], C),
    WalletID = ?STRING,
    {ok, #{<<"scope">> := #{<<"walletID">> := WalletID}}} = call_api(
        fun swag_client_wallet_webhooks_api:create_webhook/3,
        #{
            body => #{
                <<"identityID">> => IdentityID,
                <<"url">> => ?STRING,
                <<"scope">> => #{
                    <<"topic">> => <<"WithdrawalsTopic">>,
                    <<"walletID">> => WalletID,
                    <<"eventTypes">> => [<<"WithdrawalStarted">>]
                }
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec get_webhooks_ok_test(config()) ->
    _.
get_webhooks_ok_test(C) ->
    PartyID = ?config(party, C),
    {ok, Identity} = create_identity(C),
    IdentityID = maps:get(<<"id">>, Identity),
    wapi_ct_helper:mock_services([
        {webhook_manager, fun('GetList', _) -> {ok,
            [?WEBHOOK(?WITHDRAWAL_EVENT_FILTER), ?WEBHOOK(?DESTINATION_EVENT_FILTER)]} end},
        {fistful_identity, fun('GetContext', _) -> {ok,
            ?DEFAULT_CONTEXT(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_webhooks_api:get_webhooks/3,
        #{
            qs_val => #{
                <<"identityID">> => IdentityID
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec get_webhook_ok_test(config()) ->
    _.
get_webhook_ok_test(C) ->
    {ok, Identity} = create_identity(C),
    IdentityID = maps:get(<<"id">>, Identity),
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {webhook_manager, fun('Get', _) -> {ok, ?WEBHOOK(?WITHDRAWAL_EVENT_FILTER)} end},
        {fistful_identity, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_webhooks_api:get_webhook_by_id/3,
        #{
            binding => #{
                <<"webhookID">> => integer_to_binary(?INTEGER)
            },
            qs_val => #{
                <<"identityID">> => IdentityID
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec delete_webhook_ok_test(config()) ->
    _.
delete_webhook_ok_test(C) ->
    {ok, Identity} = create_identity(C),
    IdentityID = maps:get(<<"id">>, Identity),
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {webhook_manager, fun('Delete', _) -> {ok, ok} end},
        {fistful_identity, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_webhooks_api:delete_webhook_by_id/3,
        #{
            binding => #{
                <<"webhookID">> => integer_to_binary(?INTEGER)
            },
            qs_val => #{
                <<"identityID">> => IdentityID
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
    wapi_wallet_ff_backend:create_identity(Params, create_context(PartyID, C)).

create_context(PartyID, C) ->
    maps:merge(wapi_ct_helper:create_auth_ctx(PartyID), create_woody_ctx(C)).

create_woody_ctx(C) ->
    #{
        woody_context => ct_helper:get_woody_ctx(C)
    }.
