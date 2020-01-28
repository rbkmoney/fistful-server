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
            create_bank_card_destination_ok,
            create_crypto_wallet_destination_ok,
            create_ripple_wallet_destination_ok
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
    ok = ct_helper:set_context(C1),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

-spec create_bank_card_destination_ok(config()) -> test_return().

create_bank_card_destination_ok(C) ->
    Resource = {bank_card, #'BankCard'{
        token = <<"TOKEN shmOKEN">>
    }},
    create_destination_ok(Resource, C).

-spec create_crypto_wallet_destination_ok(config()) -> test_return().

create_crypto_wallet_destination_ok(C) ->
    Resource = {crypto_wallet, #'CryptoWallet'{
        id = <<"f195298af836f41d072cb390ee62bee8">>,
        currency = bitcoin_cash,
        data = {bitcoin_cash, #'CryptoDataBitcoinCash'{}}
    }},
    create_destination_ok(Resource, C).

-spec create_ripple_wallet_destination_ok(config()) -> test_return().

create_ripple_wallet_destination_ok(C) ->
    Resource = {crypto_wallet, #'CryptoWallet'{
        id = <<"ab843336bf7738dc697522fbb90508de">>,
        currency = ripple,
        data = {ripple, #'CryptoDataRipple'{tag = undefined}}
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
    IdentityID = create_person_identity(Party, C),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Params = #dst_DestinationParams{
        id          = ID,
        identity    = IdentityID,
        name        = DstName,
        currency    = Currency,
        resource    = Resource,
        external_id = ExternalId,
        context     = Ctx
    },
    {ok, Dst}  = call_service('Create', [Params]),
    DstName     = Dst#dst_Destination.name,
    ID          = Dst#dst_Destination.id,
    Resource    = Dst#dst_Destination.resource,
    ExternalId  = Dst#dst_Destination.external_id,
    Ctx         = Dst#dst_Destination.context,

    Account = Dst#dst_Destination.account,
    IdentityID = Account#account_Account.identity,
    #'CurrencyRef'{symbolic_code = Currency} = Account#account_Account.currency,

    {unauthorized, #dst_Unauthorized{}} = Dst#dst_Destination.status,

    {authorized, #dst_Authorized{}} = ct_helper:await(
        {authorized, #dst_Authorized{}},
        fun () ->
            {ok, #dst_Destination{status = Status}}
                = call_service('Get', [ID]),
            Status
        end,
        genlib_retry:linear(15, 1000)
    ),

    {ok, #dst_Destination{}} = call_service('Get', [ID]).

call_service(Fun, Args) ->
    Service = {ff_proto_destination_thrift, 'Management'},
    Request = {Service, Fun, Args},
    Client  = ff_woody_client:new(#{
        url           => <<"http://localhost:8022/v1/destination">>,
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
        ff_entity_context:new()
    ),
    ID.
