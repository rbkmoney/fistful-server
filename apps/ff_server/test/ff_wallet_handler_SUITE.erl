-module(ff_wallet_handler_SUITE).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

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

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

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
            create_error_party_suspended
        ]}
    ].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    ct_helper:makeup_cfg([
        ct_helper:test_case_name(init),
        ct_payment_system:setup()
    ], C).

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
    ok = ff_woody_ctx:set(ct_helper:get_woody_ctx(C1)),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok = ff_woody_ctx:unset().

-spec create_ok(config())                       -> test_return().
-spec create_error_identity_not_found(config()) -> test_return().
-spec create_error_currency_not_found(config()) -> test_return().
-spec create_error_party_blocked(config())      -> test_return().
-spec create_error_party_suspended(config())    -> test_return().

create_ok(C) ->
    Party      = create_party(C),
    Currency   = <<"RUB">>,
    WalletName = <<"Valet">>,
    ID         = genlib:unique(),
    ExternalId = genlib:unique(),
    IdentityID = create_person_identity(Party, C),
    Ctx        = #{<<"TEST_NS">> => {obj, #{ {str, <<"KEY">>} => {b, true}}}},
    Params = #wlt_WalletParams{
        id = ID,
        name = WalletName,
        external_id = ExternalId,
        context = Ctx,
        account_params = #account_AccountParams{
            identity_id   = IdentityID,
            symbolic_code = Currency
        }
    },
    {ok, Wallet}  = call_service('Create', [Params]),
    {ok, Wallet2} = call_service('Get', [ID]),
    Wallet        = Wallet2,
    WalletName    = Wallet2#wlt_Wallet.name,
    unblocked     = Wallet2#wlt_Wallet.blocking,
    ExternalId    = Wallet2#wlt_Wallet.external_id,
    Ctx           = Wallet2#wlt_Wallet.context,
    Account       = Wallet2#wlt_Wallet.account,
    IdentityID    = Account#account_Account.identity,
    CurrencyRef   = Account#account_Account.currency,
    Currency      = CurrencyRef#'CurrencyRef'.symbolic_code.

create_error_identity_not_found(_C) ->
    Currency   = <<"RUB">>,
    ID         = genlib:unique(),
    ExternalId = genlib:unique(),
    IdentityID = genlib:unique(),
    Params     = #wlt_WalletParams{
        id = ID,
        name = <<"Valet">>,
        external_id = ExternalId,
        account_params = #account_AccountParams{
            identity_id = IdentityID,
            symbolic_code = Currency
        }
    },
    {exception, {fistful_IdentityNotFound}} = call_service('Create', [Params]).

create_error_currency_not_found(C) ->
    Party      = create_party(C),
    Currency   = <<"RBK.MONEY">>,
    ID         = genlib:unique(),
    IdentityID = create_person_identity(Party, C),
    Params     = #wlt_WalletParams{
        id   = ID,
        name = <<"Valet">>,
        account_params = #account_AccountParams{
            identity_id   = IdentityID,
            symbolic_code = Currency
        }

    },
    {exception, {fistful_CurrencyNotFound}} = call_service('Create', [Params]).

create_error_party_blocked(C) ->
    Party      = create_party(C),
    Currency   = <<"RBK.MONEY">>,
    ID         = genlib:unique(),
    IdentityID = create_person_identity(Party, C),
    ok         = block_party(Party, C),
    Params     = #wlt_WalletParams{
        id   = ID,
        name = <<"Valet">>,
        account_params = #account_AccountParams{
            identity_id   = IdentityID,
            symbolic_code = Currency
        }

    },
    {exception, {fistful_PartyInaccessible}} = call_service('Create', [Params]).

create_error_party_suspended(C) ->
    Party      = create_party(C),
    Currency   = <<"RBK.MONEY">>,
    ID         = genlib:unique(),
    IdentityID = create_person_identity(Party, C),
    ok         = suspend_party(Party, C),
    Params     = #wlt_WalletParams{
        id   = ID,
        name = <<"Valet">>,
        account_params = #account_AccountParams{
            identity_id   = IdentityID,
            symbolic_code = Currency
        }

    },
    {exception, {fistful_PartyInaccessible}} = call_service('Create', [Params]).

%%-----------
%%  Internal
%%-----------
call_service(Fun, Args) ->
    Service = {ff_proto_wallet_thrift, 'Management'},
    Request = {Service, Fun, Args},
    Client  = ff_woody_client:new(#{
        url           => <<"http://localhost:8022/v1/wallet">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).


create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        ID,
        #{party => Party, provider => ProviderID, class => ClassID},
        ff_ctx:new()
    ),
    ID.

construct_userinfo() ->
    #payproc_UserInfo{id = <<"fistful">>, type = construct_usertype()}.

construct_usertype() ->
    {service_user, #payproc_ServiceUser{}}.

suspend_party(Party, C) ->
    Service = {dmsl_payment_processing_thrift, 'PartyManagement'},
    Args    = [construct_userinfo(), Party],
    Request = {Service, 'Suspend', Args},
    _       = ff_woody_client:call(partymgmt, Request, ct_helper:get_woody_ctx(C)),
    ok.

block_party(Party, C) ->
    Service    = {dmsl_payment_processing_thrift, 'PartyManagement'},
    Args       = [construct_userinfo(), Party, <<"BECAUSE">>],
    Request    = {Service, 'Block', Args},
    _          = ff_woody_client:call(partymgmt, Request, ct_helper:get_woody_ctx(C)),
    ok.
