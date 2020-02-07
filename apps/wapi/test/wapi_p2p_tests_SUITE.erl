-module(wapi_p2p_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
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
    quote_p2p_transfer_ok_test/1,
    create_p2p_transfer_ok_test/1,
    create_p2p_transfer_fail_test/1,
    create_p2p_transfer_with_token_ok_test/1,
    get_p2p_transfer_ok_test/1,
    get_p2p_transfer_not_found_test/1,
    get_p2p_transfer_events_ok_test/1
]).

-define(badresp(Code), {error, {invalid_response_code, Code}}).
-define(emptyresp(Code), {error, {Code, #{}}}).

-define(CALLBACK(Tag, Payload), #p2p_adapter_Callback{tag = Tag, payload = Payload}).

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
        {group, p2p}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {p2p, [parallel], [
            quote_p2p_transfer_ok_test,
            create_p2p_transfer_ok_test,
            create_p2p_transfer_fail_test,
            create_p2p_transfer_with_token_ok_test,
            get_p2p_transfer_ok_test,
            get_p2p_transfer_not_found_test,
            get_p2p_transfer_events_ok_test
        ]}
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
    ok = ct_payment_system:shutdown(C).

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(Group, Config) when Group =:= p2p ->
    ok = ff_context:save(ff_context:create(#{
        party_client => party_client:create_client(),
        woody_context => woody_context:new(<<"init_per_group/", (atom_to_binary(Group, utf8))/binary>>)
    })),
    Party = create_party(Config),
    Token = issue_token(Party, [{[party], write}, {[party], read}], unlimited),
    Config1 = [{party, Party} | Config],
    ContextPcidss = get_context("wapi-pcidss:8080", Token),
    [{context, wapi_ct_helper:get_context(Token)}, {context_pcidss, ContextPcidss} | Config1];
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

get_context(Endpoint, Token) ->
    wapi_client_lib:get_context(Endpoint, Token, 10000, ipv4).

%%% Tests

-spec quote_p2p_transfer_ok_test(config()) ->
    _.
quote_p2p_transfer_ok_test(C) ->
    SenderToken     = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ReceiverToken   = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    #{
        identity_id := IdentityID
    } = p2p_tests_utils:prepare_standard_environment({?INTEGER, ?RUB}, C),
    {ok, _} = call_api(
        fun swag_client_wallet_p2_p_api:quote_p2_p_transfer/3,
        #{
            body => #{
                <<"identityID">> => IdentityID,
                <<"body">> => #{
                    <<"amount">> => ?INTEGER,
                    <<"currency">> => ?RUB
                },
                <<"sender">> => #{
                    <<"type">> => <<"BankCardSenderResource">>,
                    <<"token">> => SenderToken
                },
                <<"receiver">> => #{
                    <<"type">> => <<"BankCardReceiverResource">>,
                    <<"token">> => ReceiverToken
                }
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec create_p2p_transfer_ok_test(config()) ->
    _.
create_p2p_transfer_ok_test(C) ->
    SenderToken     = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ReceiverToken   = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    #{
        identity_id := IdentityID
    } = p2p_tests_utils:prepare_standard_environment({?INTEGER, ?RUB}, C),
    {ok, _} = call_api(
        fun swag_client_wallet_p2_p_api:create_p2_p_transfer/3,
        #{
            body => #{
                <<"identityID">> => IdentityID,
                <<"body">> => #{
                    <<"amount">> => ?INTEGER,
                    <<"currency">> => ?RUB
                },
                <<"sender">> => #{
                    <<"type">> => <<"BankCardSenderResource">>,
                    <<"token">> => SenderToken
                },
                <<"receiver">> => #{
                    <<"type">> => <<"BankCardReceiverResource">>,
                    <<"token">> => ReceiverToken
                }
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec create_p2p_transfer_fail_test(config()) ->
    _.
create_p2p_transfer_fail_test(C) ->
    SenderToken     = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ReceiverToken   = <<"v1.kek_token">>,
    #{
        identity_id := IdentityID
    } = p2p_tests_utils:prepare_standard_environment({?INTEGER, ?RUB}, C),
    {error, {400, #{<<"name">> := <<"BankCardReceiverResource">>}}} = call_api(
        fun swag_client_wallet_p2_p_api:create_p2_p_transfer/3,
        #{
            body => #{
                <<"identityID">> => IdentityID,
                <<"body">> => #{
                    <<"amount">> => ?INTEGER,
                    <<"currency">> => ?RUB
                },
                <<"sender">> => #{
                    <<"type">> => <<"BankCardSenderResource">>,
                    <<"token">> => SenderToken
                },
                <<"receiver">> => #{
                    <<"type">> => <<"BankCardReceiverResource">>,
                    <<"token">> => ReceiverToken
                }
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec create_p2p_transfer_with_token_ok_test(config()) ->
    _.
create_p2p_transfer_with_token_ok_test(C) ->
    SenderToken   = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ReceiverToken = store_bank_card(C, <<"4150399999000900">>),
    #{
        identity_id := IdentityID
    } = p2p_tests_utils:prepare_standard_environment({?INTEGER, ?RUB}, C),
    {ok, #{<<"token">> := Token}} = call_api(
        fun swag_client_wallet_p2_p_api:quote_p2_p_transfer/3,
        #{
            body => #{
                <<"identityID">> => IdentityID,
                <<"body">> => #{
                    <<"amount">> => ?INTEGER,
                    <<"currency">> => ?RUB
                },
                <<"sender">> => #{
                    <<"type">> => <<"BankCardSenderResource">>,
                    <<"token">> => SenderToken
                },
                <<"receiver">> => #{
                    <<"type">> => <<"BankCardReceiverResource">>,
                    <<"token">> => ReceiverToken
                }
            }
        },
        ct_helper:cfg(context, C)
    ),
    {ok, _} = call_api(
        fun swag_client_wallet_p2_p_api:create_p2_p_transfer/3,
        #{
            body => #{
                <<"identityID">> => IdentityID,
                <<"body">> => #{
                    <<"amount">> => ?INTEGER,
                    <<"currency">> => ?RUB
                },
                <<"sender">> => #{
                    <<"type">> => <<"BankCardSenderResource">>,
                    <<"token">> => SenderToken
                },
                <<"receiver">> => #{
                    <<"type">> => <<"BankCardReceiverResource">>,
                    <<"token">> => ReceiverToken
                },
                <<"quoteToken">> => Token
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec get_p2p_transfer_ok_test(config()) ->
    _.
get_p2p_transfer_ok_test(C) ->
    SenderToken   = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ReceiverToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    #{
        identity_id := IdentityID
    } = p2p_tests_utils:prepare_standard_environment({?INTEGER, ?RUB}, C),
    {ok, #{<<"id">> := TransferID}} = call_api(
        fun swag_client_wallet_p2_p_api:create_p2_p_transfer/3,
        #{
            body => #{
                <<"identityID">> => IdentityID,
                <<"body">> => #{
                    <<"amount">> => ?INTEGER,
                    <<"currency">> => ?RUB
                },
                <<"sender">> => #{
                    <<"type">> => <<"BankCardSenderResource">>,
                    <<"token">> => SenderToken
                },
                <<"receiver">> => #{
                    <<"type">> => <<"BankCardReceiverResource">>,
                    <<"token">> => ReceiverToken
                }
            }
        },
        ct_helper:cfg(context, C)
    ),
    {ok, _} = call_api(
        fun swag_client_wallet_p2_p_api:get_p2_p_transfer/3,
        #{
            binding => #{
                <<"p2pTransferID">> => TransferID
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec get_p2p_transfer_not_found_test(config()) ->
    _.
get_p2p_transfer_not_found_test(C) ->
    {error, {404, _}} = call_api(
        fun swag_client_wallet_p2_p_api:get_p2_p_transfer/3,
        #{
            binding => #{
                <<"p2pTransferID">> => ?STRING
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec get_p2p_transfer_events_ok_test(config()) ->
    _.
get_p2p_transfer_events_ok_test(C) ->
    SenderToken   = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ReceiverToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    #{
        identity_id := IdentityID
    } = p2p_tests_utils:prepare_standard_environment({101, ?RUB}, C),
    {ok, #{<<"id">> := TransferID}} = call_api(
        fun swag_client_wallet_p2_p_api:create_p2_p_transfer/3,
        #{
            body => #{
                <<"identityID">> => IdentityID,
                <<"body">> => #{
                    <<"amount">> => 101,
                    <<"currency">> => ?RUB
                },
                <<"sender">> => #{
                    <<"type">> => <<"BankCardSenderResource">>,
                    <<"token">> => SenderToken
                },
                <<"receiver">> => #{
                    <<"type">> => <<"BankCardReceiverResource">>,
                    <<"token">> => ReceiverToken
                }
            }
        },
        ct_helper:cfg(context, C)
    ),
    {ok, #{<<"result">> := []}} = call_api(
        fun swag_client_wallet_p2_p_api:get_p2_p_transfer_events/3,
        #{
            binding => #{
                <<"p2pTransferID">> => TransferID
            }
        },
        ct_helper:cfg(context, C)
    ),
%%    Callback = ?CALLBACK(Token, <<"payload">>),
    ok = await_user_interaction_created_events(TransferID, C),
%%    _ = call_p2p_adapter(Callback),
    ok = await_successful_transfer_events(TransferID, C).

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

store_bank_card(C, Pan) ->
    store_bank_card(C, Pan, undefined, undefined).
store_bank_card(C, Pan, ExpDate, CardHolder) ->
    {ok, Res} = call_api(
        fun swag_client_payres_payment_resources_api:store_bank_card/3,
        #{body => genlib_map:compact(#{
            <<"type">>       => <<"BankCard">>,
            <<"cardNumber">> => Pan,
            <<"expDate">>    => ExpDate,
            <<"cardHolder">> => CardHolder
        })},
        ct_helper:cfg(context_pcidss, C)
    ),
    maps:get(<<"token">>, Res).

await_user_interaction_created_events(TransferID, C) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            Result = call_api(
                fun swag_client_wallet_p2_p_api:get_p2_p_transfer_events/3,
                #{
                    binding => #{
                        <<"p2pTransferID">> => TransferID
                    }
                },
                ct_helper:cfg(context, C)
            ),
            case Result of
                {ok, #{<<"result">> := [
                    #{
                        <<"change">> := #{
                            <<"changeType">> := <<"P2PTransferInteractionChanged">>,
                            <<"userInteractionChange">> := #{
                                <<"changeType">> := <<"UserInteractionCreated">>,
                                <<"userInteraction">> := #{
                                    <<"interactionType">> := <<"Redirect">>,
                                    <<"request">> := #{
                                        <<"requestType">> := <<"BrowserGetRequest">>,
                                        <<"uriTemplate">> := <<"uri">>
                                    }
                                }
                            },
                            <<"userInteractionID">> := <<"test_user_interaction">>
                        }
                    } | _]}} ->
                    finished;
                {ok, #{<<"result">> := FinalWrongResult}} ->
                    {not_finished, FinalWrongResult}
            end
        end,
        genlib_retry:linear(15, 1000)
    ),
    ok.

await_successful_transfer_events(TransferID, C) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            Result = call_api(
                fun swag_client_wallet_p2_p_api:get_p2_p_transfer_events/3,
                #{
                    binding => #{
                        <<"p2pTransferID">> => TransferID
                    }
                },
                ct_helper:cfg(context, C)
            ),
            case Result of
                {ok, #{<<"result">> := [
                    #{
                        <<"change">> := #{
                            <<"changeType">> := <<"P2PTransferInteractionChanged">>,
                            <<"userInteractionChange">> := #{
                                <<"changeType">> := <<"UserInteractionCreated">>,
                                <<"userInteraction">> := #{
                                    <<"interactionType">> := <<"Redirect">>,
                                    <<"request">> := #{
                                        <<"requestType">> := <<"BrowserGetRequest">>,
                                        <<"uriTemplate">> := <<"uri">>
                                    }
                                }
                            },
                            <<"userInteractionID">> := <<"test_user_interaction">>
                        }
                    },
                    #{
                        <<"change">> := #{
                            <<"changeType">> := <<"P2PTransferInteractionChanged">>,
                            <<"userInteractionChange">> := #{
                                <<"changeType">> := <<"UserInteractionFinished">>
                            },
                            <<"userInteractionID">> := <<"test_user_interaction">>
                        }
                    },
                    #{
                        <<"change">> := #{
                            <<"changeType">> := <<"P2PTransferStatusChanged">>,
                            <<"status">> := <<"Succeeded">>
                        }
                    }
                    ]}} ->
                    finished;
                {ok, #{<<"result">> := FinalWrongResult}} ->
                    {not_finished, FinalWrongResult}
            end
        end,
        genlib_retry:linear(15, 1000)
    ),
    ok.
