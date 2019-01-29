-module(ff_wallet_handler_SUITE).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_wallet_ok/1]).
-export([create_wallet_identity_fails/1]).
-export([create_wallet_currency_fails/1]).

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
            create_wallet_ok,
            create_wallet_identity_fails,
            create_wallet_currency_fails
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


-spec create_wallet_ok(config()) -> test_return().
-spec create_wallet_identity_fails(config()) -> test_return().
-spec create_wallet_currency_fails(config()) -> test_return().

create_wallet_ok(C) ->
    Party = create_party(C),
    Currency = <<"RUB">>,
    WalletName = <<"Valet">>,
    ID = genlib:unique(),
    ExternalId = genlib:unique(),
    IdentityID = create_person_identity(Party, C),
    Ctx = #{<<"TEST_NS">> => {obj, #{ {str, <<"KEY">>} => {b, true} }}},
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
    {ok, WalletState}  = call_service('Create', [Params]),
    {ok, WalletState2} = call_service('Get', [ID]),
    WalletState = WalletState2,
    WalletName  = WalletState2#wlt_WalletState.name,
    unblocked   = WalletState2#wlt_WalletState.blocking,
    ExternalId  = WalletState2#wlt_WalletState.external_id,
    Ctx         = WalletState2#wlt_WalletState.context,
    Account     = WalletState2#wlt_WalletState.account,
    IdentityID  = Account#account_Account.identity,
    CurrencyRef = Account#account_Account.currency,
    Currency = CurrencyRef#'CurrencyRef'.symbolic_code.

create_wallet_identity_fails(_C) ->
    Currency = <<"RUB">>,
    ID = genlib:unique(),
    ExternalId = genlib:unique(),
    IdentityID = genlib:unique(),
    Params = #wlt_WalletParams{
        id = ID,
        name = <<"Valet">>,
        external_id = ExternalId,
        account_params = #account_AccountParams{
            identity_id = IdentityID,
            symbolic_code = Currency
        }
    },
    {exception, {fistful_IdentityNotFound}} = call_service('Create', [Params]).

create_wallet_currency_fails(C) ->
    Party = create_party(C),
    Currency = <<"RBK.MONEY">>,
    ID = genlib:unique(),
    IdentityID = create_person_identity(Party, C),
    Params = #wlt_WalletParams{
        id   = ID,
        name = <<"Valet">>,
        account_params = #account_AccountParams{
            identity_id   = IdentityID,
            symbolic_code = Currency
        }

    },
    {exception, {fistful_CurrencyNotFound}} = call_service('Create', [Params]).

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