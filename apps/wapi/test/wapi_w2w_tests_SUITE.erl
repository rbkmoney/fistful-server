-module(wapi_w2w_tests_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

-include_lib("fistful_proto/include/ff_proto_w2w_transfer_thrift.hrl").

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
    create/1,
    create_fail_unauthorized_wallet/1,
    create_fail_wallet_notfound/1,
    create_fail_invalid_operation_amount/1,
    create_fail_forbidden_operation_currency/1,
    create_fail_inconsistent_w2w_transfer_currency/1,
    create_fail_wallet_inaccessible/1,
    get/1,
    get_fail_w2w_notfound/1
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
                create,
                create_fail_unauthorized_wallet,
                create_fail_wallet_notfound,
                create_fail_invalid_operation_amount,
                create_fail_forbidden_operation_currency,
                create_fail_inconsistent_w2w_transfer_currency,
                create_fail_wallet_inaccessible,
                get,
                get_fail_w2w_notfound
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
    BasePermissions = [
        {[w2w], read},
        {[w2w], write}
    ],
    {ok, Token} = wapi_ct_helper:issue_token(Party, BasePermissions, {deadline, 10}, ?DOMAIN),
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

-spec create(config()) ->
    _.
create(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {bender_thrift, fun('GenerateID', _) -> {ok, ?GENERATE_ID_RESULT} end},
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {w2w_transfer, fun('Create', _) -> {ok, ?W2W_TRANSFER(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_w2_w_api:create_w2_w_transfer/3,
        #{
            body => #{
                <<"sender">> => ?STRING,
                <<"receiver">> => ?STRING,
                <<"body">> => #{
                    <<"amount">> => ?INTEGER,
                    <<"currency">> => ?RUB
                }
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec create_fail_unauthorized_wallet(config()) ->
    _.
create_fail_unauthorized_wallet(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {bender_thrift, fun('GenerateID', _) -> {ok, ?GENERATE_ID_RESULT} end},
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(<<"someotherparty">>)} end},
        {w2w_transfer, fun('Create', _) -> {ok, ?W2W_TRANSFER(PartyID)} end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such wallet sender">>}}},
        call_api(
            fun swag_client_wallet_w2_w_api:create_w2_w_transfer/3,
            #{
                body => #{
                    <<"sender">> => ?STRING,
                    <<"receiver">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => ?INTEGER,
                        <<"currency">> => ?RUB
                    }
                }
            },
            ct_helper:cfg(context, C)
        )
    ).

-spec create_fail_wallet_notfound(config()) ->
    _.
create_fail_wallet_notfound(C) ->
    PartyID = ?config(party, C),
    WalletNotFoundException = #fistful_WalletNotFound{
        id = ?STRING
    },
    wapi_ct_helper:mock_services([
        {bender_thrift, fun('GenerateID', _) -> {ok, ?GENERATE_ID_RESULT} end},
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {w2w_transfer, fun('Create', _) -> throw(WalletNotFoundException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such wallet sender">>}}},
        call_api(
            fun swag_client_wallet_w2_w_api:create_w2_w_transfer/3,
            #{
                body => #{
                    <<"sender">> => ?STRING,
                    <<"receiver">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => ?INTEGER,
                        <<"currency">> => ?RUB
                    }
                }
            },
            ct_helper:cfg(context, C)
        )
    ).

-spec create_fail_invalid_operation_amount(config()) ->
    _.
create_fail_invalid_operation_amount(C) ->
    PartyID = ?config(party, C),
    InvalidOperationAmountException = #fistful_InvalidOperationAmount{
        amount = ?CASH
    },
    wapi_ct_helper:mock_services([
        {bender_thrift, fun('GenerateID', _) -> {ok, ?GENERATE_ID_RESULT} end},
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {w2w_transfer, fun('Create', _) -> throw(InvalidOperationAmountException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Bad transfer amount">>}}},
        call_api(
            fun swag_client_wallet_w2_w_api:create_w2_w_transfer/3,
            #{
                body => #{
                    <<"sender">> => ?STRING,
                    <<"receiver">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => ?INTEGER,
                        <<"currency">> => ?RUB
                    }
                }
            },
            ct_helper:cfg(context, C)
        )
    ).

-spec create_fail_forbidden_operation_currency(config()) ->
    _.
create_fail_forbidden_operation_currency(C) ->
    PartyID = ?config(party, C),
    ForbiddenOperationCurrencyException = #fistful_ForbiddenOperationCurrency{
        currency = #'CurrencyRef'{symbolic_code = ?USD},
        allowed_currencies = [
            #'CurrencyRef'{symbolic_code = ?RUB}
        ]
    },
    wapi_ct_helper:mock_services([
        {bender_thrift, fun('GenerateID', _) -> {ok, ?GENERATE_ID_RESULT} end},
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {w2w_transfer, fun('Create', _) -> throw(ForbiddenOperationCurrencyException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Currency not allowed">>}}},
        call_api(
            fun swag_client_wallet_w2_w_api:create_w2_w_transfer/3,
            #{
                body => #{
                    <<"sender">> => ?STRING,
                    <<"receiver">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => ?INTEGER,
                        <<"currency">> => ?RUB
                    }
                }
            },
            ct_helper:cfg(context, C)
        )
    ).

-spec create_fail_inconsistent_w2w_transfer_currency(config()) ->
    _.
create_fail_inconsistent_w2w_transfer_currency(C) ->
    PartyID = ?config(party, C),
    InconsistentWithdrawalCurrencyException = #w2w_transfer_InconsistentW2WTransferCurrency{
        w2w_transfer_currency = #'CurrencyRef'{
            symbolic_code = ?USD
        },
        wallet_from_currency = #'CurrencyRef'{
            symbolic_code = ?RUB
        },
        wallet_to_currency = #'CurrencyRef'{
            symbolic_code = ?RUB
        }
    },
    wapi_ct_helper:mock_services([
        {bender_thrift, fun('GenerateID', _) -> {ok, ?GENERATE_ID_RESULT} end},
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {w2w_transfer, fun('Create', _) -> throw(InconsistentWithdrawalCurrencyException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Inconsistent currency">>}}},
        call_api(
            fun swag_client_wallet_w2_w_api:create_w2_w_transfer/3,
            #{
                body => #{
                    <<"sender">> => ?STRING,
                    <<"receiver">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => ?INTEGER,
                        <<"currency">> => ?RUB
                    }
                }
            },
            ct_helper:cfg(context, C)
        )
    ).

-spec create_fail_wallet_inaccessible(config()) ->
    _.
create_fail_wallet_inaccessible(C) ->
    PartyID = ?config(party, C),
    WalletInaccessibleException = #fistful_WalletInaccessible{
        id = ?STRING
    },
    wapi_ct_helper:mock_services([
        {bender_thrift, fun('GenerateID', _) -> {ok, ?GENERATE_ID_RESULT} end},
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {w2w_transfer, fun('Create', _) -> throw(WalletInaccessibleException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Wallet inaccessible">>}}},
        call_api(
            fun swag_client_wallet_w2_w_api:create_w2_w_transfer/3,
            #{
                body => #{
                    <<"sender">> => ?STRING,
                    <<"receiver">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => ?INTEGER,
                        <<"currency">> => ?RUB
                    }
                }
            },
            ct_helper:cfg(context, C)
        )
    ).

-spec get(config()) ->
    _.
get(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {w2w_transfer, fun('Get', _) -> {ok, ?W2W_TRANSFER(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_w2_w_api:get_w2_w_transfer/3,
        #{
            binding => #{
                <<"w2wTransferID">> => ?STRING
            }
        },
    ct_helper:cfg(context, C)
).

-spec get_fail_w2w_notfound(config()) ->
    _.
get_fail_w2w_notfound(C) ->
    wapi_ct_helper:mock_services([
        {w2w_transfer, fun('Get', _) -> throw(#fistful_W2WNotFound{}) end}
    ], C),
    ?assertMatch(
        {error, {404, #{}}},
        call_api(
            fun swag_client_wallet_w2_w_api:get_w2_w_transfer/3,
            #{
                binding => #{
                    <<"w2wTransferID">> => ?STRING
                }
            },
        ct_helper:cfg(context, C)
        )
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
