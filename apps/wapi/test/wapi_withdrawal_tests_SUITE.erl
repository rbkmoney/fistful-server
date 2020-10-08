-module(wapi_withdrawal_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").

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
    create_ok/1,
    create_fail_wallet_notfound/1,
    create_fail_destination_notfound/1,
    create_fail_destination_unauthorized/1,
    create_fail_forbidden_operation_currency/1,
    create_fail_forbidden_operation_amount/1,
    create_fail_invalid_operation_amount/1,
    create_fail_inconsistent_withdrawal_currency/1,
    create_fail_no_destination_resource_info/1,
    create_fail_identity_providers_mismatch/1,
    create_fail_wallet_inaccessible/1,
    get_ok/1,
    get_fail_withdrawal_notfound/1,
    get_by_external_id_ok/1,
    create_quote_ok/1,
    get_quote_fail_wallet_notfound/1,
    get_quote_fail_destination_notfound/1,
    get_quote_fail_destination_unauthorized/1,
    get_quote_fail_forbidden_operation_currency/1,
    get_quote_fail_forbidden_operation_amount/1,
    get_quote_fail_invalid_operation_amount/1,
    get_quote_fail_inconsistent_withdrawal_currency/1,
    get_quote_fail_identity_provider_mismatch/1,
    get_event_ok/1,
    get_events_ok/1,
    get_events_fail_withdrawal_notfound/1
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
                create_ok,
                create_fail_wallet_notfound,
                create_fail_destination_notfound,
                create_fail_destination_unauthorized,
                create_fail_forbidden_operation_currency,
                create_fail_forbidden_operation_amount,
                create_fail_invalid_operation_amount,
                create_fail_inconsistent_withdrawal_currency,
                create_fail_no_destination_resource_info,
                create_fail_identity_providers_mismatch,
                create_fail_wallet_inaccessible,
                get_ok,
                get_fail_withdrawal_notfound,
                get_by_external_id_ok,
                create_quote_ok,
                get_quote_fail_wallet_notfound,
                get_quote_fail_destination_notfound,
                get_quote_fail_destination_unauthorized,
                get_quote_fail_forbidden_operation_currency,
                get_quote_fail_forbidden_operation_amount,
                get_quote_fail_invalid_operation_amount,
                get_quote_fail_inconsistent_withdrawal_currency,
                get_quote_fail_identity_provider_mismatch,
                get_event_ok,
                get_events_ok,
                get_events_fail_withdrawal_notfound
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
        {[party], read},
        {[party], write}
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

-spec create_ok(config()) ->
    _.
create_ok(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('Create', _) -> {ok, ?WITHDRAWAL(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
        #{
            body => genlib_map:compact(#{
                <<"wallet">> => ?STRING,
                <<"destination">> => ?STRING,
                <<"body">> => #{
                    <<"amount">> => 100,
                    <<"currency">> => <<"RUB">>
                }
        })},
        ct_helper:cfg(context, C)
    ).

-spec create_fail_wallet_notfound(config()) ->
    _.
create_fail_wallet_notfound(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('Create', _) -> throw(#fistful_WalletNotFound{}) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such wallet">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
            #{
                body => genlib_map:compact(#{
                    <<"wallet">> => ?STRING,
                    <<"destination">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).


-spec create_fail_destination_notfound(config()) ->
    _.
create_fail_destination_notfound(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('Create', _) -> throw(#fistful_DestinationNotFound{}) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such destination">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
            #{
                body => genlib_map:compact(#{
                    <<"wallet">> => ?STRING,
                    <<"destination">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec create_fail_destination_unauthorized(config()) ->
    _.
create_fail_destination_unauthorized(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('Create', _) -> throw(#fistful_DestinationUnauthorized{}) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Destination unauthorized">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
            #{
                body => genlib_map:compact(#{
                    <<"wallet">> => ?STRING,
                    <<"destination">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
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
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('Create', _) -> throw(ForbiddenOperationCurrencyException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Forbidden currency">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
            #{
                body => genlib_map:compact(#{
                    <<"wallet">> => ?STRING,
                    <<"destination">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec create_fail_forbidden_operation_amount(config()) ->
    _.
create_fail_forbidden_operation_amount(C) ->
    PartyID = ?config(party, C),
    ForbiddenOperationAmountException = #fistful_ForbiddenOperationAmount{
        amount = ?CASH,
        allowed_range = #'CashRange'{
            upper = {inclusive, ?CASH},
            lower = {inclusive, ?CASH}
        }
    },
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('Create', _) -> throw(ForbiddenOperationAmountException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Invalid cash amount">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
            #{
                body => genlib_map:compact(#{
                    <<"wallet">> => ?STRING,
                    <<"destination">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => 100500,
                        <<"currency">> => ?RUB
                    }
            })},
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
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('Create', _) -> throw(InvalidOperationAmountException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Invalid cash amount">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
            #{
                body => genlib_map:compact(#{
                    <<"wallet">> => ?STRING,
                    <<"destination">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec create_fail_inconsistent_withdrawal_currency(config()) ->
    _.
create_fail_inconsistent_withdrawal_currency(C) ->
    PartyID = ?config(party, C),
    InconsistentWithdrawalCurrencyException = #wthd_InconsistentWithdrawalCurrency{
        withdrawal_currency = #'CurrencyRef'{
            symbolic_code = ?USD
        },
        destination_currency = #'CurrencyRef'{
            symbolic_code = ?RUB
        },
        wallet_currency = #'CurrencyRef'{
            symbolic_code = ?RUB
        }
    },
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('Create', _) -> throw(InconsistentWithdrawalCurrencyException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Invalid currency">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
            #{
                body => genlib_map:compact(#{
                    <<"wallet">> => ?STRING,
                    <<"destination">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec create_fail_no_destination_resource_info(config()) ->
    _.
create_fail_no_destination_resource_info(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('Create', _) -> throw(#wthd_NoDestinationResourceInfo{}) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Unknown card issuer">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
            #{
                body => genlib_map:compact(#{
                    <<"wallet">> => ?STRING,
                    <<"destination">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec create_fail_identity_providers_mismatch(config()) ->
    _.
create_fail_identity_providers_mismatch(C) ->
    PartyID = ?config(party, C),
    IdentityProviderMismatchException = #wthd_IdentityProvidersMismatch{
        wallet_provider = ?INTEGER,
        destination_provider = ?INTEGER
    },
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('Create', _) -> throw(IdentityProviderMismatchException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"This wallet and destination cannot be used together">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
            #{
                body => genlib_map:compact(#{
                    <<"wallet">> => ?STRING,
                    <<"destination">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
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
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('Create', _) -> throw(WalletInaccessibleException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Wallet inaccessible">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
            #{
                body => genlib_map:compact(#{
                    <<"wallet">> => ?STRING,
                    <<"destination">> => ?STRING,
                    <<"body">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec get_ok(config()) ->
    _.
get_ok(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_withdrawal, fun('Get', _) -> {ok, ?WITHDRAWAL(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:get_withdrawal/3,
        #{
            binding => #{
                <<"withdrawalID">> => ?STRING
            }
        },
    ct_helper:cfg(context, C)
).


-spec get_fail_withdrawal_notfound(config()) ->
    _.
get_fail_withdrawal_notfound(C) ->
    wapi_ct_helper:mock_services([
        {fistful_withdrawal, fun('Get', _) -> throw(#fistful_WithdrawalNotFound{}) end}
    ], C),
    ?assertEqual(
        {error, {404, #{}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:get_withdrawal/3,
            #{
                binding => #{
                    <<"withdrawalID">> => ?STRING
                }
            },
        ct_helper:cfg(context, C)
        )
    ).

-spec get_by_external_id_ok(config()) ->
    _.
get_by_external_id_ok(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {bender_thrift, fun('GetInternalID', _) -> {ok, ?GET_INTERNAL_ID_RESULT} end},
        {fistful_withdrawal, fun('Get', _) -> {ok, ?WITHDRAWAL(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:get_withdrawal_by_external_id/3,
        #{
            binding => #{
                <<"externalID">> => ?STRING
            }
        },
    ct_helper:cfg(context, C)
).

-spec create_quote_ok(config()) ->
    _.
create_quote_ok(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('GetQuote', _) -> {ok, ?WITHDRAWAL_QUOTE} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:create_quote/3,
        #{
            body => genlib_map:compact(#{
                <<"walletID">> => ?STRING,
                <<"destinationID">> => ?STRING,
                <<"currencyFrom">> => <<"RUB">>,
                <<"currencyTo">> => <<"USD">>,
                <<"cash">> => #{
                    <<"amount">> => 100,
                    <<"currency">> => <<"RUB">>
                }
        })},
        ct_helper:cfg(context, C)
    ).

-spec get_quote_fail_wallet_notfound(config()) ->
    _.
get_quote_fail_wallet_notfound(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('GetQuote', _) -> throw(#fistful_WalletNotFound{}) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such wallet">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_quote/3,
            #{
                body => genlib_map:compact(#{
                    <<"walletID">> => ?STRING,
                    <<"destinationID">> => ?STRING,
                    <<"currencyFrom">> => <<"RUB">>,
                    <<"currencyTo">> => <<"USD">>,
                    <<"cash">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).


-spec get_quote_fail_destination_notfound(config()) ->
    _.
get_quote_fail_destination_notfound(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('GetQuote', _) -> throw(#fistful_DestinationNotFound{}) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such destination">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_quote/3,
            #{
                body => genlib_map:compact(#{
                    <<"walletID">> => ?STRING,
                    <<"destinationID">> => ?STRING,
                    <<"currencyFrom">> => <<"RUB">>,
                    <<"currencyTo">> => <<"USD">>,
                    <<"cash">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec get_quote_fail_destination_unauthorized(config()) ->
    _.
get_quote_fail_destination_unauthorized(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('GetQuote', _) -> throw(#fistful_DestinationUnauthorized{}) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Destination unauthorized">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_quote/3,
            #{
                body => genlib_map:compact(#{
                    <<"walletID">> => ?STRING,
                    <<"destinationID">> => ?STRING,
                    <<"currencyFrom">> => <<"RUB">>,
                    <<"currencyTo">> => <<"USD">>,
                    <<"cash">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec get_quote_fail_forbidden_operation_currency(config()) ->
    _.
get_quote_fail_forbidden_operation_currency(C) ->
    PartyID = ?config(party, C),
    ForbiddenOperationCurrencyException = #fistful_ForbiddenOperationCurrency{
        currency = #'CurrencyRef'{symbolic_code = <<"USD">>},
        allowed_currencies = [
            #'CurrencyRef'{symbolic_code = <<"RUB">>}
        ]
    },
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('GetQuote', _) -> throw(ForbiddenOperationCurrencyException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Forbidden currency">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_quote/3,
            #{
                body => genlib_map:compact(#{
                    <<"walletID">> => ?STRING,
                    <<"destinationID">> => ?STRING,
                    <<"currencyFrom">> => <<"RUB">>,
                    <<"currencyTo">> => <<"USD">>,
                    <<"cash">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec get_quote_fail_forbidden_operation_amount(config()) ->
    _.
get_quote_fail_forbidden_operation_amount(C) ->
    PartyID = ?config(party, C),
    ForbiddenOperationAmountException = #fistful_ForbiddenOperationAmount{
        amount = ?CASH,
        allowed_range = #'CashRange'{
            upper = {inclusive, ?CASH},
            lower = {inclusive, ?CASH}
        }
    },
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('GetQuote', _) -> throw(ForbiddenOperationAmountException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Invalid cash amount">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_quote/3,
            #{
                body => genlib_map:compact(#{
                    <<"walletID">> => ?STRING,
                    <<"destinationID">> => ?STRING,
                    <<"currencyFrom">> => <<"RUB">>,
                    <<"currencyTo">> => <<"USD">>,
                    <<"cash">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec get_quote_fail_invalid_operation_amount(config()) ->
    _.
get_quote_fail_invalid_operation_amount(C) ->
    PartyID = ?config(party, C),
    InvalidOperationAmountException = #fistful_InvalidOperationAmount{
        amount = ?CASH
    },
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('GetQuote', _) -> throw(InvalidOperationAmountException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Invalid cash amount">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_quote/3,
            #{
                body => genlib_map:compact(#{
                    <<"walletID">> => ?STRING,
                    <<"destinationID">> => ?STRING,
                    <<"currencyFrom">> => <<"RUB">>,
                    <<"currencyTo">> => <<"USD">>,
                    <<"cash">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec get_quote_fail_inconsistent_withdrawal_currency(config()) ->
    _.
get_quote_fail_inconsistent_withdrawal_currency(C) ->
    PartyID = ?config(party, C),
    InconsistentWithdrawalCurrencyException = #wthd_InconsistentWithdrawalCurrency{
        withdrawal_currency = #'CurrencyRef'{
            symbolic_code = ?USD
        },
        destination_currency = #'CurrencyRef'{
            symbolic_code = ?RUB
        },
        wallet_currency = #'CurrencyRef'{
            symbolic_code = ?RUB
        }
    },
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('GetQuote', _) -> throw(InconsistentWithdrawalCurrencyException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Invalid currency">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_quote/3,
            #{
                body => genlib_map:compact(#{
                    <<"walletID">> => ?STRING,
                    <<"destinationID">> => ?STRING,
                    <<"currencyFrom">> => <<"RUB">>,
                    <<"currencyTo">> => <<"USD">>,
                    <<"cash">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec get_quote_fail_identity_provider_mismatch(config()) ->
    _.
get_quote_fail_identity_provider_mismatch(C) ->
    PartyID = ?config(party, C),
    IdentityProviderMismatchException = #wthd_IdentityProvidersMismatch{
        wallet_provider = ?INTEGER,
        destination_provider = ?INTEGER
    },
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('GetQuote', _) -> throw(IdentityProviderMismatchException) end}
    ], C),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"This wallet and destination cannot be used together">>}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:create_quote/3,
            #{
                body => genlib_map:compact(#{
                    <<"walletID">> => ?STRING,
                    <<"destinationID">> => ?STRING,
                    <<"currencyFrom">> => <<"RUB">>,
                    <<"currencyTo">> => <<"USD">>,
                    <<"cash">> => #{
                        <<"amount">> => 100,
                        <<"currency">> => <<"RUB">>
                    }
            })},
            ct_helper:cfg(context, C)
        )
    ).

-spec get_event_ok(config()) ->
    _.
get_event_ok(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_withdrawal,
            fun
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
                ('GetEvents', [_, #'EventRange'{limit = 0}]) -> {ok, []};
                ('GetEvents', _) -> {ok, [?WITHDRAWAL_EVENT(?WITHDRAWAL_STATUS_CHANGE)]}
            end
        }
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:get_withdrawal_events/3,
        #{
            binding => #{
                <<"withdrawalID">> => ?STRING,
                <<"eventID">> => ?INTEGER
            }
        },
    ct_helper:cfg(context, C)
).

-spec get_events_ok(config()) ->
    _.
get_events_ok(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_withdrawal,
            fun
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
                ('GetEvents', [_, #'EventRange'{limit = 0}]) -> {ok, []};
                ('GetEvents', _) -> {ok, [?WITHDRAWAL_EVENT(?WITHDRAWAL_STATUS_CHANGE)]}
            end
        }
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:poll_withdrawal_events/3,
        #{
            binding => #{
                <<"withdrawalID">> => ?STRING
            },
            qs_val => #{
                <<"limit">> => 10
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec get_events_fail_withdrawal_notfound(config()) ->
    _.
get_events_fail_withdrawal_notfound(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_withdrawal,
            fun
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
                ('GetEvents', [_, #'EventRange'{limit = 0}]) -> throw(#fistful_WithdrawalNotFound{});
                ('GetEvents', _) -> {ok, [?WITHDRAWAL_EVENT(?WITHDRAWAL_STATUS_CHANGE)]}
            end
        }
    ], C),
    ?assertEqual(
        {error, {404, #{}}},
        call_api(
            fun swag_client_wallet_withdrawals_api:poll_withdrawal_events/3,
            #{
                binding => #{
                    <<"withdrawalID">> => ?STRING
                },
                qs_val => #{
                    <<"limit">> => 10
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
