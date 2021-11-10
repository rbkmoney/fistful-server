-module(ff_wallet_handler_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_ok/1]).
-export([create_error_identity_not_found/1]).
-export([create_error_currency_not_found/1]).
-export([create_error_party_blocked/1]).
-export([create_error_party_suspended/1]).
-export([get_account_balance/1]).

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            create_ok,
            create_error_identity_not_found,
            create_error_currency_not_found,
            create_error_party_blocked,
            create_error_party_suspended,
            get_account_balance
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    ct_helper:makeup_cfg(
        [
            ct_helper:test_case_name(init),
            ct_payment_system:setup()
        ],
        C
    ).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C).

%%

-spec init_per_group(group_name(), config()) -> config().
init_per_group(_, C) ->
    C.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_, _) ->
    ok.

%%

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

-spec create_ok(config()) -> test_return().
-spec create_error_identity_not_found(config()) -> test_return().
-spec create_error_currency_not_found(config()) -> test_return().
-spec create_error_party_blocked(config()) -> test_return().
-spec create_error_party_suspended(config()) -> test_return().
-spec get_account_balance(config()) -> test_return().

create_ok(C) ->
    Party = create_party(C),
    Currency = <<"RUB">>,
    ID = genlib:unique(),
    ExternalID = genlib:unique(),
    ct:print("~n~n~n********~n~n[create_person_identity] Start~n"),
    IdentityID = create_person_identity(Party, C),
    ct:print("[create_person_identity] End~n"),
    Ctx = #{<<"TEST_NS">> => {obj, #{{str, <<"KEY">>} => {b, true}}}},
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Params = construct_wallet_params(ID, IdentityID, Currency, ExternalID, Metadata),
    CreateResult = call_service('Create', {Params, Ctx}),
    GetResult = call_service('Get', {ID, #'EventRange'{}}),
    {ok, Wallet} = GetResult,
    Account = Wallet#wlt_WalletState.account,
    CurrencyRef = Account#account_Account.currency,
    ?assertMatch(CreateResult, GetResult),
    ?assertMatch(<<"Valet">>, Wallet#wlt_WalletState.name),
    ?assertMatch(unblocked, Wallet#wlt_WalletState.blocking),
    ?assertMatch(ExternalID, Wallet#wlt_WalletState.external_id),
    ?assertMatch(Metadata, Wallet#wlt_WalletState.metadata),
    ?assertMatch(Ctx, Wallet#wlt_WalletState.context),
    ?assertMatch(IdentityID, Account#account_Account.identity),
    ?assertMatch(Currency, CurrencyRef#'CurrencyRef'.symbolic_code).

create_error_identity_not_found(_C) ->
    Currency = <<"RUB">>,
    ID = genlib:unique(),
    ExternalID = genlib:unique(),
    IdentityID = genlib:unique(),
    Params = construct_wallet_params(ID, IdentityID, Currency, ExternalID),
    Result = call_service('Create', {Params, #{}}),
    ?assertMatch({exception, #fistful_IdentityNotFound{}}, Result).

create_error_currency_not_found(C) ->
    Party = create_party(C),
    Currency = <<"RBK.MONEY">>,
    ID = genlib:unique(),
    IdentityID = create_person_identity(Party, C),
    Params = construct_wallet_params(ID, IdentityID, Currency),
    Result = call_service('Create', {Params, #{}}),
    ?assertMatch({exception, #fistful_CurrencyNotFound{}}, Result).

create_error_party_blocked(C) ->
    Party = create_party(C),
    Currency = <<"RUB">>,
    ID = genlib:unique(),
    IdentityID = create_person_identity(Party, C),
    ok = block_party(Party, C),
    Params = construct_wallet_params(ID, IdentityID, Currency),
    Result = call_service('Create', {Params, #{}}),
    ?assertMatch({exception, #fistful_PartyInaccessible{}}, Result).

create_error_party_suspended(C) ->
    Party = create_party(C),
    Currency = <<"RUB">>,
    ID = genlib:unique(),
    IdentityID = create_person_identity(Party, C),
    ok = suspend_party(Party, C),
    Params = construct_wallet_params(ID, IdentityID, Currency),
    Result = call_service('Create', {Params, #{}}),
    ?assertMatch({exception, #fistful_PartyInaccessible{}}, Result).

get_account_balance(C) ->
    Party = create_party(C),
    Currency = <<"RUB">>,
    ID = genlib:unique(),
    ExternalID = genlib:unique(),
    IdentityID = create_person_identity(Party, C),
    Ctx = #{<<"TEST_NS">> => {obj, #{{str, <<"KEY">>} => {b, true}}}},
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Params = construct_wallet_params(ID, IdentityID, Currency, ExternalID, Metadata),
    {ok, Wallet} = call_service('Create', {Params, Ctx}),
    WalletID = Wallet#wlt_WalletState.id,
    {ok, AccountBalance} = call_service('GetAccountBalance', {WalletID}),
    CurrencyRef = AccountBalance#account_AccountBalance.currency,
    Account = Wallet#wlt_WalletState.account,
    AccountID = Account#account_Account.id,
    ?assertMatch(AccountID, AccountBalance#account_AccountBalance.id),
    ?assertMatch(Currency, CurrencyRef#'CurrencyRef'.symbolic_code),
    ?assertMatch(0, AccountBalance#account_AccountBalance.expected_min),
    ?assertMatch(0, AccountBalance#account_AccountBalance.current),
    ?assertMatch(0, AccountBalance#account_AccountBalance.expected_max).

%%-----------
%%  Internal
%%-----------
call_service(Fun, Args) ->
    Service = {ff_proto_wallet_thrift, 'Management'},
    Request = {Service, Fun, Args},
    Client = ff_woody_client:new(#{
        url => <<"http://localhost:8022/v1/wallet">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C) ->
    ct:print("change on churche without challenge~n"),
    create_identity(Party, <<"good-one">>, <<"church">>, C).
    % create_identity(Party, <<"good-one">>, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, C) ->
    create_identity(Party, <<"Identity Name">>, ProviderID, ClassID, C).

create_identity(Party, Name, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, name => Name, party => Party, provider => ProviderID, class => ClassID},
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
    ),
    ID.

construct_userinfo() ->
    #payproc_UserInfo{id = <<"fistful">>, type = construct_usertype()}.

construct_usertype() ->
    {service_user, #payproc_ServiceUser{}}.

suspend_party(Party, C) ->
    Service = {dmsl_payment_processing_thrift, 'PartyManagement'},
    Args = {construct_userinfo(), Party},
    Request = {Service, 'Suspend', Args},
    _ = ff_woody_client:call(partymgmt, Request, ct_helper:get_woody_ctx(C)),
    ok.

block_party(Party, C) ->
    Service = {dmsl_payment_processing_thrift, 'PartyManagement'},
    Args = {construct_userinfo(), Party, <<"BECAUSE">>},
    Request = {Service, 'Block', Args},
    _ = ff_woody_client:call(partymgmt, Request, ct_helper:get_woody_ctx(C)),
    ok.

construct_wallet_params(ID, IdentityID, Currency) ->
    #wlt_WalletParams{
        id = ID,
        name = <<"Valet">>,
        account_params = #account_AccountParams{
            identity_id = IdentityID,
            symbolic_code = Currency
        }
    }.

construct_wallet_params(ID, IdentityID, Currency, ExternalID) ->
    #wlt_WalletParams{
        id = ID,
        name = <<"Valet">>,
        external_id = ExternalID,
        account_params = #account_AccountParams{
            identity_id = IdentityID,
            symbolic_code = Currency
        }
    }.

construct_wallet_params(ID, IdentityID, Currency, ExternalID, Metadata) ->
    #wlt_WalletParams{
        id = ID,
        name = <<"Valet">>,
        external_id = ExternalID,
        metadata = Metadata,
        account_params = #account_AccountParams{
            identity_id = IdentityID,
            symbolic_code = Currency
        }
    }.
