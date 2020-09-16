-module(wapi_p2p_transfer_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").

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
    create_with_quote_token/1,
    create_with_bad_quote_token/1,
    get/1,
    fail_unauthorized/1
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
                create_with_quote_token,
                create_with_bad_quote_token,
                get,
                fail_unauthorized
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
    ContextPcidss = get_context("wapi-pcidss:8080", Token),
    [
        {context_pcidss, ContextPcidss},
        {context, wapi_ct_helper:get_context(Token)} |
        Config1
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

%%% Tests

-spec create(config()) ->
    _.
create(C) ->
    mock_services(C),
    SenderToken   = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ReceiverToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    {ok, _} = call_api(
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

-spec create_with_quote_token(config()) ->
    _.
create_with_quote_token(C) ->
    mock_services(C),
    SenderToken   = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ReceiverToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    IdentityID = <<"id">>,
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
                <<"quoteToken">> => get_quote_token(IdentityID),
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

-spec create_with_bad_quote_token(config()) ->
    _.
create_with_bad_quote_token(C) ->
    mock_services(C),
    SenderToken   = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ReceiverToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    {error, {422, #{
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

-spec get(config()) ->
    _.
get(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {p2p_transfer, fun('Get', _) -> {ok, ?P2P_TRANSFER(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_p2_p_api:get_p2_p_transfer/3,
        #{
            binding => #{
                <<"p2pTransferID">> => ?STRING
            }
        },
    ct_helper:cfg(context, C)
).

-spec fail_unauthorized(config()) ->
    _.
fail_unauthorized(C) ->
    WrongPartyID = <<"kek">>,
    mock_services(C, WrongPartyID),
    SenderToken   = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    ReceiverToken = store_bank_card(C, <<"4150399999000900">>, <<"12/2025">>, <<"Buka Bjaka">>),
    {error, {422, #{
        <<"message">> := <<"No such identity">>
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

mock_services(C) ->
    mock_services(C, ?config(party, C)).

mock_services(C, ContextPartyID) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {bender_thrift, fun('GenerateID', _) -> {ok, ?GENERATE_ID_RESULT} end},
        {fistful_identity, fun('Get', _) -> {ok, ?IDENTITY(PartyID)};
                              ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(ContextPartyID)} end},
        {p2p_transfer, fun('Create', _) -> {ok, ?P2P_TRANSFER(PartyID)} end}
    ], C).

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

get_quote_token(IdentityID) ->
    Quote = #{
        fees              => #{fees => #{}},
        amount            => {?INTEGER, ?RUB},
        party_revision    => 1,
        domain_revision   => 1,
        created_at        => 1600147637597,
        expires_on        => 1600147637597,
        identity_id       => IdentityID,
        sender            => {bank_card, #{ token => ?STRING, bin_data_id => nil }},
        receiver          => {bank_card, #{ token => ?STRING, bin_data_id => nil }}
    },
    PartyID = ?STRING,
    Payload = wapi_p2p_quote:create_token_payload(Quote, PartyID),
    {ok, QuoteToken} = uac_authorizer_jwt:issue(wapi_utils:get_unique_id(), PartyID, Payload, wapi_auth:get_signee()),
    QuoteToken.

get_context(Endpoint, Token) ->
    wapi_client_lib:get_context(Endpoint, Token, 10000, ipv4).