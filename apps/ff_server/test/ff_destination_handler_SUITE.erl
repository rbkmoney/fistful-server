-module(ff_destination_handler_SUITE).

-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_bank_card_destination_ok/1]).
-export([create_crypto_wallet_destination_ok/1]).
-export([create_ripple_wallet_destination_ok/1]).
-export([create_digital_wallet_destination_ok/1]).

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
            create_bank_card_destination_ok,
            create_crypto_wallet_destination_ok,
            create_ripple_wallet_destination_ok,
            create_digital_wallet_destination_ok
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

-spec create_bank_card_destination_ok(config()) -> test_return().
create_bank_card_destination_ok(C) ->
    Resource =
        {bank_card, #'ResourceBankCard'{
            bank_card = #'BankCard'{
                token = <<"TOKEN shmOKEN">>
            }
        }},
    create_destination_ok(Resource, C).

-spec create_crypto_wallet_destination_ok(config()) -> test_return().
create_crypto_wallet_destination_ok(C) ->
    Resource =
        {crypto_wallet, #'ResourceCryptoWallet'{
            crypto_wallet = #'CryptoWallet'{
                id = <<"f195298af836f41d072cb390ee62bee8">>,
                currency = bitcoin_cash,
                data = {bitcoin_cash, #'CryptoDataBitcoinCash'{}}
            }
        }},
    create_destination_ok(Resource, C).

-spec create_ripple_wallet_destination_ok(config()) -> test_return().
create_ripple_wallet_destination_ok(C) ->
    Resource =
        {crypto_wallet, #'ResourceCryptoWallet'{
            crypto_wallet = #'CryptoWallet'{
                id = <<"ab843336bf7738dc697522fbb90508de">>,
                currency = ripple,
                data = {ripple, #'CryptoDataRipple'{tag = undefined}}
            }
        }},
    create_destination_ok(Resource, C).

-spec create_digital_wallet_destination_ok(config()) -> test_return().
create_digital_wallet_destination_ok(C) ->
    Resource =
        {digital_wallet, #'ResourceDigitalWallet'{
            digital_wallet = #'DigitalWallet'{
                id = <<"f195298af836f41d072cb390ee62bee8">>,
                data = {webmoney, #'DigitalDataWebmoney'{}}
            }
        }},
    create_destination_ok(Resource, C).

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

create_destination_ok(Resource, C) ->
    Party = create_party(C),
    Currency = <<"RUB">>,
    DstName = <<"loSHara card">>,
    ID = genlib:unique(),
    ExternalId = genlib:unique(),
    IdentityID = create_identity(Party, C),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Params = #dst_DestinationParams{
        id = ID,
        identity = IdentityID,
        name = DstName,
        currency = Currency,
        resource = Resource,
        external_id = ExternalId,
        metadata = Metadata
    },
    {ok, Dst} = call_service('Create', {Params, Ctx}),
    DstName = Dst#dst_DestinationState.name,
    ID = Dst#dst_DestinationState.id,
    Resource = Dst#dst_DestinationState.resource,
    ExternalId = Dst#dst_DestinationState.external_id,
    Metadata = Dst#dst_DestinationState.metadata,
    Ctx = Dst#dst_DestinationState.context,

    Account = Dst#dst_DestinationState.account,
    IdentityID = Account#account_Account.identity,
    #'CurrencyRef'{symbolic_code = Currency} = Account#account_Account.currency,

    {unauthorized, #dst_Unauthorized{}} = Dst#dst_DestinationState.status,

    {authorized, #dst_Authorized{}} = ct_helper:await(
        {authorized, #dst_Authorized{}},
        fun() ->
            {ok, #dst_DestinationState{status = Status}} =
                call_service('Get', {ID, #'EventRange'{}}),
            Status
        end,
        genlib_retry:linear(15, 1000)
    ),

    {ok, #dst_DestinationState{}} = call_service('Get', {ID, #'EventRange'{}}).

call_service(Fun, Args) ->
    Service = {ff_proto_destination_thrift, 'Management'},
    Request = {Service, Fun, Args},
    Client = ff_woody_client:new(#{
        url => <<"http://localhost:8022/v1/destination">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, C).

create_identity(Party, ProviderID, C) ->
    create_identity(Party, <<"Identity Name">>, ProviderID, C).

create_identity(Party, Name, ProviderID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, name => Name, party => Party, provider => ProviderID},
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
    ),
    ID.
