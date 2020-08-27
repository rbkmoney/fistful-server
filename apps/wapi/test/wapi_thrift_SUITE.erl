-module(wapi_thrift_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/ff_proto_fistful_thrift.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([wallet_check_test/1]).
-export([identity_check_test/1]).
-export([identity_challenge_check_test/1]).
-export([destination_check_test/1]).
-export([w2w_transfer_check_test/1]).

% common-api is used since it is the domain used in production RN
% TODO: change to wallet-api (or just omit since it is the default one) when new tokens will be a thing
-define(DOMAIN, <<"common-api">>).

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

% -import(ct_helper, [cfg/2]).

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        {group, default}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {default, [sequence], [
            identity_check_test,
            identity_challenge_check_test,
            wallet_check_test,
            destination_check_test,
            w2w_transfer_check_test
        ]}
    ].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
     ct_helper:makeup_cfg([
        ct_helper:test_case_name(init),
        ct_payment_system:setup(#{
            default_termset => get_default_termset(),
            optional_apps => [
                bender_client,
                wapi_woody_client,
                wapi
            ]
        })
    ], C).

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C).

%%

-spec init_per_group(group_name(), config()) -> config().

init_per_group(G, C) ->
    ok = ff_context:save(ff_context:create(#{
        party_client => party_client:create_client(),
        woody_context => woody_context:new(<<"init_per_group/", (atom_to_binary(G, utf8))/binary>>)
    })),
    Party = create_party(C),
    % Token = issue_token(Party, [{[party], write}], unlimited),
    {ok, Token} = wapi_ct_helper:issue_token(Party, [{[party], write}], {deadline, 10}, ?DOMAIN),
    Context = get_context("localhost:8080", Token),
    ContextPcidss = get_context("wapi-pcidss:8080", Token),
    [{context, Context}, {context_pcidss, ContextPcidss}, {party, Party} | C].

-spec end_per_group(group_name(), config()) -> _.

end_per_group(_, _) ->
    ok.
%%

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    ok = application:set_env(wapi, transport, not_thrift),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

-define(ID_PROVIDER, <<"good-one">>).
-define(ID_PROVIDER2, <<"good-two">>).
-define(ID_CLASS, <<"person">>).

-spec identity_check_test(config()) -> test_return().

identity_check_test(C) ->
    Name = <<"Keyn Fawkes">>,
    Provider = ?ID_PROVIDER,
    Class = ?ID_CLASS,
    IdentityID1 = create_identity(Name, Provider, Class, C),
    ok = check_identity(Name, IdentityID1, Provider, Class, C),
    Keys = maps:keys(get_identity(IdentityID1, C)),
    ok = application:set_env(wapi, transport, thrift),
    IdentityID2 = create_identity(Name, Provider, Class, C),
    ok = check_identity(Name, IdentityID2, Provider, Class, C),
    ?assertEqual(Keys, maps:keys(get_identity(IdentityID2, C))).

-spec identity_challenge_check_test(config()) -> test_return().

identity_challenge_check_test(C) ->
    Name = <<"Keyn Fawkes">>,
    Provider = ?ID_PROVIDER,
    Class = ?ID_CLASS,
    IdentityID1 = create_identity(Name, Provider, Class, C),
    ok = check_identity(Name, IdentityID1, Provider, Class, C),
    IdentityChallengeID1 = create_identity_challenge(IdentityID1, C),
    Keys = maps:keys(get_identity_challenge(IdentityID1, IdentityChallengeID1, C)),
    ok = application:set_env(wapi, transport, thrift),
    IdentityID2 = create_identity(Name, Provider, Class, C),
    ok = check_identity(Name, IdentityID2, Provider, Class, C),
    IdentityChallengeID2 = create_identity_challenge(IdentityID2, C),
    ?assertEqual(Keys, maps:keys(get_identity_challenge(IdentityID2, IdentityChallengeID2, C))).

-spec wallet_check_test(config()) -> test_return().

wallet_check_test(C) ->
    Name = <<"Keyn Fawkes">>,
    Provider = ?ID_PROVIDER,
    Class = ?ID_CLASS,
    IdentityID1 = create_identity(Name, Provider, Class, C),
    WalletID1 = create_wallet(IdentityID1, C),
    ok = check_wallet(WalletID1, C),
    Keys = maps:keys(get_wallet(WalletID1, C)),
    ok = application:set_env(wapi, transport, thrift),
    IdentityID2 = create_identity(Name, Provider, Class, C),
    WalletID2 = create_wallet(IdentityID2, C),
    ok = check_wallet(WalletID2, C),
    ?assertEqual(Keys, maps:keys(get_wallet(WalletID2, C))).

-spec destination_check_test(config()) -> test_return().

destination_check_test(C) ->
    Name = <<"Keyn Fawkes">>,
    Provider = ?ID_PROVIDER,
    Class = ?ID_CLASS,
    IdentityID1 = create_identity(Name, Provider, Class, C),
    DestinationID1 = create_destination(IdentityID1, C),
    Keys = maps:keys(get_destination(DestinationID1, C)),
    ok = application:set_env(wapi, transport, thrift),
    IdentityID2 = create_identity(Name, Provider, Class, C),
    DestinationID2 = create_destination(IdentityID2, C),
    ?assertEqual(Keys, maps:keys(get_destination(DestinationID2, C))).

-spec w2w_transfer_check_test(config()) -> test_return().

w2w_transfer_check_test(C) ->
    Name = <<"Keyn Fawkes">>,
    Provider = ?ID_PROVIDER,
    Class = ?ID_CLASS,
    IdentityID1 = create_identity(Name, Provider, Class, C),
    WalletID11 = create_wallet(IdentityID1, C),
    WalletID12 = create_wallet(IdentityID1, C),
    W2WTransferID1 = create_w2w_transfer(WalletID11, WalletID12, C),
    Keys = maps:keys(get_w2w_transfer(W2WTransferID1, C)),
    ok = application:set_env(wapi, transport, thrift),
    IdentityID2 = create_identity(Name, Provider, Class, C),
    WalletID21 = create_wallet(IdentityID2, C),
    WalletID22 = create_wallet(IdentityID2, C),
    W2WTransferID2 = create_w2w_transfer(WalletID21, WalletID22, C),
    ?assertEqual(Keys, maps:keys(get_w2w_transfer(W2WTransferID2, C))).

%%

-spec call_api(function(), map(), wapi_client_lib:context()) ->
    {ok, term()} | {error, term()}.
call_api(F, Params, Context) ->
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = F(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).

%%

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

get_context(Endpoint, Token) ->
    wapi_client_lib:get_context(Endpoint, Token, 10000, ipv4).

%%

create_identity(Name, Provider, Class, C) ->
    {ok, Identity} = call_api(
        fun swag_client_wallet_identities_api:create_identity/3,
        #{body => #{
            <<"name">>     => Name,
            <<"provider">> => Provider,
            <<"class">>    => Class,
            <<"metadata">> => #{
                ?STRING => ?STRING
            }
        }},
        ct_helper:cfg(context, C)
    ),
    maps:get(<<"id">>, Identity).

check_identity(Name, IdentityID, Provider, Class, C) ->
    Identity = get_identity(IdentityID, C),
    #{
        <<"name">>     := Name,
        <<"provider">> := Provider,
        <<"class">>    := Class,
        <<"metadata">> := #{
            ?STRING := ?STRING
        }
    } = maps:with([<<"name">>,
                   <<"provider">>,
                   <<"class">>,
                   <<"metadata">>], Identity),
    ok.

get_identity(IdentityID, C) ->
    {ok, Identity} = call_api(
        fun swag_client_wallet_identities_api:get_identity/3,
        #{binding => #{<<"identityID">> => IdentityID}},
        ct_helper:cfg(context, C)
    ),
    Identity.

create_identity_challenge(IdentityID, C) ->
    {_Cert, CertToken} = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    {_Passport, PassportToken} = ct_identdocstore:rus_domestic_passport(C),
    {ok, IdentityChallenge} = call_api(
        fun swag_client_wallet_identities_api:start_identity_challenge/3,
        #{
            binding => #{
                <<"identityID">> => IdentityID
            },
            body => #{
                <<"type">> => <<"sword-initiation">>,
                <<"proofs">> => [
                    #{
                        <<"token">> => wapi_utils:map_to_base64url(#{
                            <<"type">> => <<"RUSRetireeInsuranceCertificate">>,
                            <<"token">> => CertToken
                        })
                    },
                    #{
                        <<"token">> => wapi_utils:map_to_base64url(#{
                            <<"type">> => <<"RUSDomesticPassport">>,
                            <<"token">> => PassportToken
                        })
                    }
                ]
            }
        },
        ct_helper:cfg(context, C)
    ),
    maps:get(<<"id">>, IdentityChallenge).

get_identity_challenge(IdentityID, ChallengeID, C) ->
    {ok, IdentityChallenge} = call_api(
        fun swag_client_wallet_identities_api:get_identity_challenge/3,
        #{binding => #{<<"identityID">> => IdentityID, <<"challengeID">> => ChallengeID}},
        ct_helper:cfg(context, C)
    ),
    IdentityChallenge.

create_wallet(IdentityID, C) ->
    create_wallet(IdentityID, #{}, C).

create_wallet(IdentityID, Params, C) ->
    DefaultParams = #{
            <<"name">>     => <<"Worldwide PHP Awareness Initiative">>,
            <<"identity">> => IdentityID,
            <<"currency">> => <<"RUB">>,
            <<"metadata">> => #{
                ?STRING => ?STRING
            }
    },
    {ok, Wallet} = call_api(
        fun swag_client_wallet_wallets_api:create_wallet/3,
        #{body => maps:merge(DefaultParams, Params)},
        ct_helper:cfg(context, C)
    ),
    maps:get(<<"id">>, Wallet).

check_wallet(WalletID, C) ->
    Wallet = get_wallet(WalletID, C),
    #{
        <<"name">> := <<"Worldwide PHP Awareness Initiative">>,
        <<"currency">> := <<"RUB">>,
        <<"metadata">> := #{
            ?STRING := ?STRING
        }
    } = maps:with([<<"name">>, <<"currency">>, <<"metadata">>], Wallet),
    ok.

get_wallet(WalletID, C) ->
    {ok, Wallet} = call_api(
        fun swag_client_wallet_wallets_api:get_wallet/3,
        #{binding => #{<<"walletID">> => WalletID}},
        ct_helper:cfg(context, C)
    ),
    Wallet.

create_destination(IdentityID, C) ->
    create_destination(IdentityID, #{}, C).

create_destination(IdentityID, Params, C) ->
    DefaultParams = #{
        <<"name">> => ?STRING,
        <<"identity">> => IdentityID,
        <<"currency">> => ?RUB,
        <<"resource">> => #{
            <<"type">> => <<"BankCardDestinationResource">>,
            <<"token">> => wapi_utils:map_to_base64url(#{
                <<"token">> => ?STRING,
                <<"bin">> => <<"424242">>,
                <<"lastDigits">> => <<"4242">>
            })
        }
    },
    {ok, Destination} = call_api(
        fun swag_client_wallet_withdrawals_api:create_destination/3,
        #{body => maps:merge(DefaultParams, Params)},
        ct_helper:cfg(context, C)
    ),
    maps:get(<<"id">>, Destination).

get_destination(DestinationID, C) ->
    {ok, Destination} = call_api(
        fun swag_client_wallet_withdrawals_api:get_destination/3,
        #{binding => #{<<"destinationID">> => DestinationID}},
        ct_helper:cfg(context, C)
    ),
    Destination.

create_w2w_transfer(WalletID1, WalletID2, C) ->
    DefaultParams = #{
        <<"sender">> => WalletID1,
        <<"receiver">> => WalletID2,
        <<"body">> => #{
            <<"amount">> => 1000,
            <<"currency">> => ?RUB
        }
    },
    {ok, W2WTransfer} = call_api(
        fun swag_client_wallet_w2_w_api:create_w2_w_transfer/3,
        #{body => DefaultParams},
        ct_helper:cfg(context, C)
    ),
    maps:get(<<"id">>, W2WTransfer).

get_w2w_transfer(W2WTransferID2, C) ->
    {ok, W2WTransfer} = call_api(
        fun swag_client_wallet_w2_w_api:get_w2_w_transfer/3,
        #{binding => #{<<"w2wTransferID">> => W2WTransferID2}},
        ct_helper:cfg(context, C)
    ),
    W2WTransfer.

%%

-include_lib("ff_cth/include/ct_domain.hrl").

get_default_termset() ->
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            currencies = {value, ?ordset([?cur(<<"RUB">>)])},
            wallet_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(-10000000, <<"RUB">>)},
                        {exclusive, ?cash( 10000001, <<"RUB">>)}
                    )}
                }
            ]},
            withdrawals = #domain_WithdrawalServiceTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                cash_limit = {decisions, [
                    #domain_CashLimitDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, ?cashrng(
                            {inclusive, ?cash(       0, <<"RUB">>)},
                            {exclusive, ?cash(10000000, <<"RUB">>)}
                        )}
                    }
                ]},
                cash_flow = {decisions, [
                    #domain_CashFlowDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, [
                            ?cfpost(
                                {wallet, sender_settlement},
                                {wallet, receiver_destination},
                                ?share(1, 1, operation_amount)
                            ),
                            ?cfpost(
                                {wallet, receiver_destination},
                                {system, settlement},
                                ?share(10, 100, operation_amount)
                            )
                        ]}
                    }
                ]}
            },
            w2w = #domain_W2WServiceTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>), ?cur(<<"USD">>)])},
                allow = {constant, true},
                cash_limit = {decisions, [
                    #domain_CashLimitDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, ?cashrng(
                            {inclusive, ?cash(       0, <<"RUB">>)},
                            {exclusive, ?cash(10001, <<"RUB">>)}
                        )}
                    }
                ]},
                cash_flow = {decisions, [
                    #domain_CashFlowDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, [
                            ?cfpost(
                                {wallet, sender_settlement},
                                {wallet, receiver_settlement},
                                ?share(1, 1, operation_amount)
                            )
                        ]}
                    }
                ]},
                fees = {decisions, [
                    #domain_FeeDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, #domain_Fees{
                                    fees = #{surplus => ?share(1, 1, operation_amount)}
                                }}
                    }
                ]}
            }
        }
    }.
