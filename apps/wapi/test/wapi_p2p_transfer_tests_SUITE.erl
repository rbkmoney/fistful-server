-module(wapi_p2p_transfer_tests_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_p2p_session_thrift.hrl").

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
    create_ok_test/1,
    create_fail_resource_token_invalid_test/1,
    create_fail_resource_token_expire_test/1,
    create_fail_unauthorized_test/1,
    create_fail_identity_notfound_test/1,
    create_fail_forbidden_operation_currency_test/1,
    create_fail_forbidden_operation_amount_test/1,
    create_fail_operation_not_permitted_test/1,
    create_fail_no_resource_info_test/1,
    create_quote_ok_test/1,
    create_quote_fail_resource_token_invalid_test/1,
    create_quote_fail_resource_token_expire_test/1,
    create_with_quote_token_ok_test/1,
    create_with_bad_quote_token_fail_test/1,
    get_quote_fail_identity_not_found_test/1,
    get_quote_fail_forbidden_operation_currency_test/1,
    get_quote_fail_forbidden_operation_amount_test/1,
    get_quote_fail_operation_not_permitted_test/1,
    get_quote_fail_no_resource_info_test/1,
    get_ok_test/1,
    get_fail_p2p_notfound_test/1,
    get_events_ok/1,
    get_events_fail/1
]).

% common-api is used since it is the domain used in production RN
% TODO: change to wallet-api (or just omit since it is the default one) when new tokens will be a thing
-define(DOMAIN, <<"common-api">>).
-define(badresp(Code), {error, {invalid_response_code, Code}}).
-define(emptyresp(Code), {error, {Code, #{}}}).

-type test_case_name() :: atom().
-type config() :: [{atom(), any()}].
-type group_name() :: atom().

-behaviour(supervisor).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() -> [test_case_name()].
all() ->
    [
        {group, base}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {base, [], [
            create_ok_test,
            create_fail_resource_token_invalid_test,
            create_fail_resource_token_expire_test,
            create_fail_unauthorized_test,
            create_fail_identity_notfound_test,
            create_fail_forbidden_operation_currency_test,
            create_fail_forbidden_operation_amount_test,
            create_fail_operation_not_permitted_test,
            create_fail_no_resource_info_test,
            create_quote_ok_test,
            create_quote_fail_resource_token_invalid_test,
            create_quote_fail_resource_token_expire_test,
            create_with_quote_token_ok_test,
            create_with_bad_quote_token_fail_test,
            get_quote_fail_identity_not_found_test,
            get_quote_fail_forbidden_operation_currency_test,
            get_quote_fail_forbidden_operation_amount_test,
            get_quote_fail_operation_not_permitted_test,
            get_quote_fail_no_resource_info_test,
            get_ok_test,
            get_fail_p2p_notfound_test,
            get_events_ok,
            get_events_fail
        ]}
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    %% TODO remove this after cut off wapi
    ok = application:set_env(wapi, transport, thrift),
    ct_helper:makeup_cfg(
        [
            ct_helper:test_case_name(init),
            ct_payment_system:setup(#{
                optional_apps => [
                    bender_client,
                    wapi_woody_client,
                    wapi
                ]
            })
        ],
        Config
    ).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    %% TODO remove this after cut off wapi
    ok = ct_payment_system:shutdown(C).

-spec init_per_group(group_name(), config()) -> config().
init_per_group(Group, Config) when Group =:= base ->
    ok = ff_context:save(
        ff_context:create(#{
            party_client => party_client:create_client(),
            woody_context => woody_context:new(<<"init_per_group/", (atom_to_binary(Group, utf8))/binary>>)
        })
    ),
    Party = create_party(Config),
    BasePermissions = [
        {[party], read},
        {[party], write}
    ],
    {ok, Token} = wapi_ct_helper:issue_token(Party, BasePermissions, {deadline, 10}, ?DOMAIN),
    Config1 = [{party, Party} | Config],
    ContextPcidss = get_context("wapi-pcidss:8080", Token),
    [
        {context_pcidss, ContextPcidss},
        {context, wapi_ct_helper:get_context(Token)}
        | Config1
    ];
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    [{test_sup, wapi_ct_helper:start_mocked_service_sup(?MODULE)} | C1].

-spec end_per_testcase(test_case_name(), config()) -> config().
end_per_testcase(_Name, C) ->
    ok = ct_helper:unset_context(),
    wapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec create_ok_test(config()) -> _.
create_ok_test(C) ->
    create_ok_start_mocks(C),
    {ok, _} = create_p2p_transfer_call_api(C).

-spec create_fail_resource_token_invalid_test(config()) -> _.
create_fail_resource_token_invalid_test(C) ->
    create_ok_start_mocks(C),
    InvalidToken = <<"v1.InvalidToken">>,
    ValidToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ?assertMatch(
        {error,
            {400, #{
                <<"errorType">> := <<"InvalidResourceToken">>,
                <<"name">> := <<"BankCardSenderResourceParams">>
            }}},
        create_p2p_transfer_call_api(C, InvalidToken, ValidToken)
    ),
    ?assertMatch(
        {error,
            {400, #{
                <<"errorType">> := <<"InvalidResourceToken">>,
                <<"name">> := <<"BankCardReceiverResourceParams">>
            }}},
        create_p2p_transfer_call_api(C, ValidToken, InvalidToken)
    ).

-spec create_fail_resource_token_expire_test(config()) -> _.
create_fail_resource_token_expire_test(C) ->
    create_ok_start_mocks(C),
    InvalidToken = store_bank_card(wapi_utils:deadline_from_timeout(0)),
    ValidToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ?assertMatch(
        {error,
            {400, #{
                <<"errorType">> := <<"InvalidResourceToken">>,
                <<"name">> := <<"BankCardSenderResourceParams">>
            }}},
        create_p2p_transfer_call_api(C, InvalidToken, ValidToken)
    ),
    ?assertMatch(
        {error,
            {400, #{
                <<"errorType">> := <<"InvalidResourceToken">>,
                <<"name">> := <<"BankCardReceiverResourceParams">>
            }}},
        create_p2p_transfer_call_api(C, ValidToken, InvalidToken)
    ).

-spec create_fail_unauthorized_test(config()) -> _.
create_fail_unauthorized_test(C) ->
    WrongPartyID = <<"SomeWrongPartyID">>,
    create_ok_start_mocks(C, WrongPartyID),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity">>}}},
        create_p2p_transfer_call_api(C)
    ).

-spec create_fail_identity_notfound_test(config()) -> _.
create_fail_identity_notfound_test(C) ->
    create_fail_start_mocks(C, fun() -> throw(#fistful_IdentityNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity">>}}},
        create_p2p_transfer_call_api(C)
    ).

-spec create_fail_forbidden_operation_currency_test(config()) -> _.
create_fail_forbidden_operation_currency_test(C) ->
    ForbiddenOperationCurrencyException = #fistful_ForbiddenOperationCurrency{
        currency = #'CurrencyRef'{symbolic_code = ?USD},
        allowed_currencies = [
            #'CurrencyRef'{symbolic_code = ?RUB}
        ]
    },
    create_fail_start_mocks(C, fun() -> throw(ForbiddenOperationCurrencyException) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Currency not allowed">>}}},
        create_p2p_transfer_call_api(C)
    ).

-spec create_fail_forbidden_operation_amount_test(config()) -> _.
create_fail_forbidden_operation_amount_test(C) ->
    ForbiddenOperationAmountException = #fistful_ForbiddenOperationAmount{
        amount = ?CASH,
        allowed_range = #'CashRange'{
            upper = {inclusive, ?CASH},
            lower = {inclusive, ?CASH}
        }
    },
    create_fail_start_mocks(C, fun() -> throw(ForbiddenOperationAmountException) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Transfer amount is out of allowed range">>}}},
        create_p2p_transfer_call_api(C)
    ).

-spec create_fail_operation_not_permitted_test(config()) -> _.
create_fail_operation_not_permitted_test(C) ->
    create_fail_start_mocks(C, fun() -> throw(#fistful_OperationNotPermitted{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Operation not permitted">>}}},
        create_p2p_transfer_call_api(C)
    ).

-spec create_fail_no_resource_info_test(config()) -> _.
create_fail_no_resource_info_test(C) ->
    NoResourceInfoException = #p2p_transfer_NoResourceInfo{
        type = sender
    },
    create_fail_start_mocks(C, fun() -> throw(NoResourceInfoException) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Invalid sender resource">>}}},
        create_p2p_transfer_call_api(C)
    ).

-spec create_quote_ok_test(config()) -> _.
create_quote_ok_test(C) ->
    IdentityID = <<"id">>,
    get_quote_start_mocks(C, fun() -> {ok, ?P2P_TRANSFER_QUOTE(IdentityID)} end),
    {ok, #{<<"token">> := Token}} = quote_p2p_transfer_call_api(C, IdentityID),
    {ok, {_, _, Payload}} = uac_authorizer_jwt:verify(Token, #{}),
    {ok, #p2p_transfer_Quote{identity_id = IdentityID}} = wapi_p2p_quote:decode_token_payload(Payload).

-spec create_quote_fail_resource_token_invalid_test(config()) -> _.
create_quote_fail_resource_token_invalid_test(C) ->
    IdentityID = <<"id">>,
    get_quote_start_mocks(C, fun() -> {ok, ?P2P_TRANSFER_QUOTE(IdentityID)} end),
    InvalidToken = <<"v1.InvalidToken">>,
    ValidToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ?assertMatch(
        {error,
            {400, #{
                <<"errorType">> := <<"InvalidResourceToken">>,
                <<"name">> := <<"BankCardSenderResource">>
            }}},
        quote_p2p_transfer_call_api(C, IdentityID, InvalidToken, ValidToken)
    ),
    ?assertMatch(
        {error,
            {400, #{
                <<"errorType">> := <<"InvalidResourceToken">>,
                <<"name">> := <<"BankCardReceiverResource">>
            }}},
        quote_p2p_transfer_call_api(C, IdentityID, ValidToken, InvalidToken)
    ).

-spec create_quote_fail_resource_token_expire_test(config()) -> _.
create_quote_fail_resource_token_expire_test(C) ->
    IdentityID = <<"id">>,
    get_quote_start_mocks(C, fun() -> {ok, ?P2P_TRANSFER_QUOTE(IdentityID)} end),
    InvalidToken = store_bank_card(wapi_utils:deadline_from_timeout(0)),
    ValidToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ?assertMatch(
        {error,
            {400, #{
                <<"errorType">> := <<"InvalidResourceToken">>,
                <<"name">> := <<"BankCardSenderResource">>
            }}},
        quote_p2p_transfer_call_api(C, IdentityID, InvalidToken, ValidToken)
    ),
    ?assertMatch(
        {error,
            {400, #{
                <<"errorType">> := <<"InvalidResourceToken">>,
                <<"name">> := <<"BankCardReceiverResource">>
            }}},
        quote_p2p_transfer_call_api(C, IdentityID, ValidToken, InvalidToken)
    ).

-spec create_with_quote_token_ok_test(config()) -> _.
create_with_quote_token_ok_test(C) ->
    IdentityID = <<"id">>,
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services(
        [
            {bender_thrift, fun('GenerateID', _) -> {ok, ?GENERATE_ID_RESULT} end},
            {fistful_identity, fun
                ('Get', _) -> {ok, ?IDENTITY(PartyID)};
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)}
            end},
            {fistful_p2p_transfer, fun
                ('GetQuote', _) -> {ok, ?P2P_TRANSFER_QUOTE(IdentityID)};
                ('Create', _) -> {ok, ?P2P_TRANSFER(PartyID)}
            end}
        ],
        C
    ),
    SenderToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ReceiverToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    {ok, #{<<"token">> := QuoteToken}} = quote_p2p_transfer_call_api(C, IdentityID, SenderToken, ReceiverToken),
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
                    <<"type">> => <<"BankCardSenderResourceParams">>,
                    <<"token">> => SenderToken,
                    <<"authData">> => <<"session id">>
                },
                <<"quoteToken">> => QuoteToken,
                <<"receiver">> => #{
                    <<"type">> => <<"BankCardReceiverResourceParams">>,
                    <<"token">> => ReceiverToken
                },
                <<"contactInfo">> => #{
                    <<"email">> => <<"some@mail.com">>,
                    <<"phoneNumber">> => <<"+79990000101">>
                }
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec create_with_bad_quote_token_fail_test(config()) -> _.
create_with_bad_quote_token_fail_test(C) ->
    create_ok_start_mocks(C),
    SenderToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ReceiverToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    {error,
        {422, #{
            <<"message">> := <<"Token can't be verified">>
        }}} = call_api(
        fun swag_client_wallet_p2_p_api:create_p2_p_transfer/3,
        #{
            body => #{
                <<"identityID">> => <<"id">>,
                <<"body">> => #{
                    <<"amount">> => ?INTEGER,
                    <<"currency">> => ?RUB
                },
                <<"sender">> => #{
                    <<"type">> => <<"BankCardSenderResourceParams">>,
                    <<"token">> => SenderToken,
                    <<"authData">> => <<"session id">>
                },
                <<"quoteToken">> => <<"bad_quote_token">>,
                <<"receiver">> => #{
                    <<"type">> => <<"BankCardReceiverResourceParams">>,
                    <<"token">> => ReceiverToken
                },
                <<"contactInfo">> => #{
                    <<"email">> => <<"some@mail.com">>,
                    <<"phoneNumber">> => <<"+79990000101">>
                }
            }
        },
        ct_helper:cfg(context, C)
    ).

-spec get_quote_fail_identity_not_found_test(config()) -> _.
get_quote_fail_identity_not_found_test(C) ->
    IdentityID = <<"id">>,
    get_quote_start_mocks(C, fun() -> throw(#fistful_IdentityNotFound{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"No such identity">>}}},
        quote_p2p_transfer_call_api(C, IdentityID)
    ).

-spec get_quote_fail_forbidden_operation_currency_test(config()) -> _.
get_quote_fail_forbidden_operation_currency_test(C) ->
    ForbiddenOperationCurrencyException = #fistful_ForbiddenOperationCurrency{
        currency = #'CurrencyRef'{symbolic_code = ?USD},
        allowed_currencies = [
            #'CurrencyRef'{symbolic_code = ?RUB}
        ]
    },
    IdentityID = <<"id">>,
    get_quote_start_mocks(C, fun() -> throw(ForbiddenOperationCurrencyException) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Currency not allowed">>}}},
        quote_p2p_transfer_call_api(C, IdentityID)
    ).

-spec get_quote_fail_forbidden_operation_amount_test(config()) -> _.
get_quote_fail_forbidden_operation_amount_test(C) ->
    ForbiddenOperationAmountException = #fistful_ForbiddenOperationAmount{
        amount = ?CASH,
        allowed_range = #'CashRange'{
            upper = {inclusive, ?CASH},
            lower = {inclusive, ?CASH}
        }
    },
    IdentityID = <<"id">>,
    get_quote_start_mocks(C, fun() -> throw(ForbiddenOperationAmountException) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Transfer amount is out of allowed range">>}}},
        quote_p2p_transfer_call_api(C, IdentityID)
    ).

-spec get_quote_fail_operation_not_permitted_test(config()) -> _.
get_quote_fail_operation_not_permitted_test(C) ->
    IdentityID = <<"id">>,
    get_quote_start_mocks(C, fun() -> throw(#fistful_OperationNotPermitted{}) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Operation not permitted">>}}},
        quote_p2p_transfer_call_api(C, IdentityID)
    ).

-spec get_quote_fail_no_resource_info_test(config()) -> _.
get_quote_fail_no_resource_info_test(C) ->
    NoResourceInfoException = #p2p_transfer_NoResourceInfo{
        type = sender
    },
    IdentityID = <<"id">>,
    get_quote_start_mocks(C, fun() -> throw(NoResourceInfoException) end),
    ?assertEqual(
        {error, {422, #{<<"message">> => <<"Invalid sender resource">>}}},
        quote_p2p_transfer_call_api(C, IdentityID)
    ).

-spec get_ok_test(config()) -> _.
get_ok_test(C) ->
    PartyID = ?config(party, C),
    get_start_mocks(C, fun() -> {ok, ?P2P_TRANSFER(PartyID)} end),
    {ok, _} = get_call_api(C).

-spec get_fail_p2p_notfound_test(config()) -> _.
get_fail_p2p_notfound_test(C) ->
    get_start_mocks(C, fun() -> throw(#fistful_P2PNotFound{}) end),
    ?assertEqual(
        {error, {404, #{}}},
        get_call_api(C)
    ).

-spec get_events_ok(config()) -> _.
get_events_ok(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services(
        [
            {fistful_p2p_transfer, fun
                ('GetContext', _) ->
                    {ok, ?DEFAULT_CONTEXT(PartyID)};
                ('Get', _) ->
                    {ok, ?P2P_TRANSFER_SESSIONS(PartyID)};
                ('GetEvents', {_ID, #'EventRange'{limit = Limit}}) ->
                    {ok, [?P2P_TRANSFER_EVENT(EventID) || EventID <- lists:seq(1, Limit)]}
            end},
            {fistful_p2p_session, fun('GetEvents', {_ID, #'EventRange'{limit = Limit}}) ->
                {ok, [?P2P_SESSION_EVENT(EventID) || EventID <- lists:seq(1, Limit)]}
            end}
        ],
        C
    ),

    {ok, #{<<"result">> := Result}} = get_events_call_api(C),

    % Limit is multiplied by two because the selection occurs twice - from session and transfer.
    {ok, Limit} = application:get_env(wapi, events_fetch_limit),
    ?assertEqual(Limit * 2, erlang:length(Result)),
    [?assertMatch(#{<<"change">> := _}, Ev) || Ev <- Result].

-spec get_events_fail(config()) -> _.
get_events_fail(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services(
        [
            {fistful_p2p_transfer, fun
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
                ('Get', _) -> throw(#fistful_P2PNotFound{})
            end}
        ],
        C
    ),

    ?assertMatch({error, {404, #{}}}, get_events_call_api(C)).

%%

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

-spec call_api(function(), map(), wapi_client_lib:context()) -> {ok, term()} | {error, term()}.
call_api(F, Params, Context) ->
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = F(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).

get_events_call_api(C) ->
    call_api(
        fun swag_client_wallet_p2_p_api:get_p2_p_transfer_events/3,
        #{
            binding => #{
                <<"p2pTransferID">> => ?STRING
            }
        },
        ct_helper:cfg(context, C)
    ).

create_p2p_transfer_call_api(C) ->
    Token = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    create_p2p_transfer_call_api(C, Token, Token).

create_p2p_transfer_call_api(C, SenderToken, ReceiverToken) ->
    call_api(
        fun swag_client_wallet_p2_p_api:create_p2_p_transfer/3,
        #{
            body => #{
                <<"identityID">> => <<"id">>,
                <<"body">> => #{
                    <<"amount">> => ?INTEGER,
                    <<"currency">> => ?RUB
                },
                <<"sender">> => #{
                    <<"type">> => <<"BankCardSenderResourceParams">>,
                    <<"token">> => SenderToken,
                    <<"authData">> => <<"session id">>
                },
                <<"receiver">> => #{
                    <<"type">> => <<"BankCardReceiverResourceParams">>,
                    <<"token">> => ReceiverToken
                },
                <<"contactInfo">> => #{
                    <<"email">> => <<"some@mail.com">>,
                    <<"phoneNumber">> => <<"+79990000101">>
                }
            }
        },
        ct_helper:cfg(context, C)
    ).

quote_p2p_transfer_call_api(C, IdentityID) ->
    Token = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    quote_p2p_transfer_call_api(C, IdentityID, Token, Token).

quote_p2p_transfer_call_api(C, IdentityID, SenderToken, ReceiverToken) ->
    call_api(
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

get_call_api(C) ->
    call_api(
        fun swag_client_wallet_p2_p_api:get_p2_p_transfer/3,
        #{
            binding => #{
                <<"p2pTransferID">> => ?STRING
            }
        },
        ct_helper:cfg(context, C)
    ).

create_ok_start_mocks(C) ->
    create_ok_start_mocks(C, ?config(party, C)).

create_ok_start_mocks(C, ContextPartyID) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services(
        [
            {bender_thrift, fun('GenerateID', _) -> {ok, ?GENERATE_ID_RESULT} end},
            {fistful_identity, fun
                ('Get', _) -> {ok, ?IDENTITY(PartyID)};
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(ContextPartyID)}
            end},
            {fistful_p2p_transfer, fun('Create', _) -> {ok, ?P2P_TRANSFER(PartyID)} end}
        ],
        C
    ).

create_fail_start_mocks(C, CreateResultFun) ->
    create_fail_start_mocks(C, ?config(party, C), CreateResultFun).

create_fail_start_mocks(C, ContextPartyID, CreateResultFun) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services(
        [
            {bender_thrift, fun('GenerateID', _) -> {ok, ?GENERATE_ID_RESULT} end},
            {fistful_identity, fun
                ('Get', _) -> {ok, ?IDENTITY(PartyID)};
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(ContextPartyID)}
            end},
            {fistful_p2p_transfer, fun('Create', _) -> CreateResultFun() end}
        ],
        C
    ).

get_quote_start_mocks(C, GetQuoteResultFun) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services(
        [
            {fistful_identity, fun
                ('Get', _) -> {ok, ?IDENTITY(PartyID)};
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)}
            end},
            {fistful_p2p_transfer, fun('GetQuote', _) -> GetQuoteResultFun() end}
        ],
        C
    ).

get_start_mocks(C, GetResultFun) ->
    wapi_ct_helper:mock_services(
        [
            {fistful_p2p_transfer, fun('Get', _) -> GetResultFun() end}
        ],
        C
    ).

store_bank_card(TokenDeadline) ->
    wapi_crypto:create_resource_token(?RESOURCE, TokenDeadline).

store_bank_card(C, Pan, ExpDate, CardHolder) ->
    {ok, Res} = call_api(
        fun swag_client_payres_payment_resources_api:store_bank_card/3,
        #{
            body => genlib_map:compact(#{
                <<"type">> => <<"BankCard">>,
                <<"cardNumber">> => Pan,
                <<"expDate">> => ExpDate,
                <<"cardHolder">> => CardHolder
            })
        },
        ct_helper:cfg(context_pcidss, C)
    ),
    maps:get(<<"token">>, Res).

get_context(Endpoint, Token) ->
    wapi_client_lib:get_context(Endpoint, Token, 10000, ipv4).
