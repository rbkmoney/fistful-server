-module(ff_withdrawal_handler_SUITE).

-include_lib("fistful_proto/include/ff_proto_fistful_admin_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_withdrawal_ok/1]).
-export([create_withdrawal_wallet_currency_fail/1]).
-export([create_withdrawal_cashrange_fail/1]).
-export([create_withdrawal_destination_fail/1]).
-export([create_withdrawal_wallet_fail/1]).
-export([get_events_ok/1]).

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
            create_withdrawal_ok
            % create_withdrawal_wallet_currency_fail,
            % create_withdrawal_cashrange_fail,
            % create_withdrawal_destination_fail,
            % create_withdrawal_wallet_fail,
            % get_events_ok
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


-spec create_withdrawal_ok(config()) -> test_return().
-spec create_withdrawal_wallet_currency_fail(config()) -> test_return().
-spec create_withdrawal_cashrange_fail(config()) -> test_return().
-spec create_withdrawal_destination_fail(config()) -> test_return().
-spec create_withdrawal_wallet_fail(config()) -> test_return().
-spec get_events_ok(config()) -> test_return().

create_withdrawal_ok(C) ->
    ID            = genlib:unique(),
    ExternalId    = genlib:unique(),
    WalletID      = create_wallet(<<"RUB">>, 10000),
    DestinationID = create_destination(C),
    Body = #'Cash'{
        amount = 1000,
        currency = #'CurrencyRef'{ symbolic_code = <<"RUB">>}
    },
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Params = #wthd_WithdrawalParams{
        id          = ID,
        source      = WalletID,
        destination = DestinationID,
        body        = Body,
        external_id = ExternalId,
        context     = Ctx
    },

    {ok, Withdrawal}  = call_service(withdrawal, 'Create', [Params]),
    ID            = Withdrawal#wthd_Withdrawal.id,
    ExternalId    = Withdrawal#wthd_Withdrawal.external_id,
    WalletID      = Withdrawal#wthd_Withdrawal.source,
    DestinationID = Withdrawal#wthd_Withdrawal.destination,
    Ctx           = Withdrawal#wthd_Withdrawal.context,
    Body          = Withdrawal#wthd_Withdrawal.body,

    {succeeded, _} = ct_helper:await(
        {succeeded, #wthd_status_Succeeded{}},
        fun() ->
            {ok, W} = call_service(withdrawal, 'Get', [ID]),
            W#wthd_Withdrawal.status
        end,
        genlib_retry:linear(30, 1000)
    ),
    ok.

create_withdrawal_wallet_currency_fail(C) ->
    ID            = genlib:unique(),
    ExternalId    = genlib:unique(),
    WalletID      = create_wallet(<<"USD">>, 10000),
    DestinationID = create_destination(C),
    Body = #'Cash'{
        amount = 1000,
        currency = #'CurrencyRef'{ symbolic_code = <<"RUB">> }
    },
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Params = #wthd_WithdrawalParams{
        id          = ID,
        source      = WalletID,
        destination = DestinationID,
        body        = Body,
        external_id = ExternalId,
        context     = Ctx
    },

    {exception, #fistful_WithdrawalCurrencyInvalid{
        withdrawal_currency = #'CurrencyRef'{ symbolic_code = <<"RUB">>},
        wallet_currency     = #'CurrencyRef'{ symbolic_code = <<"USD">>}
    }} = call_service(withdrawal, 'Create', [Params]).

create_withdrawal_cashrange_fail(C) ->
    ID            = genlib:unique(),
    ExternalId    = genlib:unique(),
    WalletID      = create_wallet(<<"RUB">>, 10000),
    DestinationID = create_destination(C),
    Body = #'Cash'{
        amount = -1000,
        currency = #'CurrencyRef'{ symbolic_code = <<"RUB">>}
    },
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Params = #wthd_WithdrawalParams{
        id          = ID,
        source      = WalletID,
        destination = DestinationID,
        body        = Body,
        external_id = ExternalId,
        context     = Ctx
    },

    {exception, #fistful_WithdrawalCashAmountInvalid{
            cash  = Cash,
            range = CashRange
        }
    } = call_service(withdrawal, 'Create', [Params]),
    ?assertEqual(Body, Cash),
    ?assertNotEqual(undefined, CashRange).

create_withdrawal_destination_fail(C) ->
    ID            = genlib:unique(),
    ExternalId    = genlib:unique(),
    WalletID      = create_wallet(<<"RUB">>, 10000),
    _DestinationID = create_destination(C),
    BadDestID     = genlib:unique(),
    Body = #'Cash'{
        amount = -1000,
        currency = #'CurrencyRef'{ symbolic_code = <<"RUB">>}
    },
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Params = #wthd_WithdrawalParams{
        id          = ID,
        source      = WalletID,
        destination = BadDestID,
        body        = Body,
        external_id = ExternalId,
        context     = Ctx
    },

    {exception, {fistful_DestinationNotFound}} = call_service(withdrawal, 'Create', [Params]).

create_withdrawal_wallet_fail(C) ->
    ID            = genlib:unique(),
    ExternalId    = genlib:unique(),
    _WalletID      = create_wallet(<<"RUB">>, 10000),
    DestinationID  = create_destination(C),
    BadWalletID     = genlib:unique(),
    Body = #'Cash'{
        amount = -1000,
        currency = #'CurrencyRef'{ symbolic_code = <<"RUB">>}
    },
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Params = #wthd_WithdrawalParams{
        id          = ID,
        source      = BadWalletID,
        destination = DestinationID,
        body        = Body,
        external_id = ExternalId,
        context     = Ctx
    },

    {exception, {fistful_WalletNotFound}} = call_service(withdrawal, 'Create', [Params]).

get_events_ok(C) ->
    ID            = genlib:unique(),
    ExternalId    = genlib:unique(),
    WalletID      = create_wallet(<<"RUB">>, 10000),
    DestinationID = create_destination(C),
    Body = #'Cash'{
        amount = 2000,
        currency = #'CurrencyRef'{ symbolic_code = <<"RUB">>}
    },
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Params = #wthd_WithdrawalParams{
        id          = ID,
        source      = WalletID,
        destination = DestinationID,
        body        = Body,
        external_id = ExternalId,
        context     = Ctx
    },

    {ok, _W} = call_service(withdrawal, 'Create', [Params]),

    Range = #'EventRange'{
        limit   = 1000,
        'after' = undefined
    },
    {succeeded, #wthd_status_Succeeded{}} = ct_helper:await(
        {succeeded, #wthd_status_Succeeded{}},
        fun () ->
            {ok, Events} = call_service(withdrawal, 'GetEvents', [ID, Range]),
            lists:foldl(fun(#wthd_Event{change = {status_changed, Status}}, _AccIn) -> Status;
                            (_Ev, AccIn) -> AccIn end, undefined, Events)
        end,
        genlib_retry:linear(10, 1000)
    ).

%%-----------
%%  Internal
%%-----------
call_service(withdrawal, Fun, Args) ->
    Service = {ff_proto_withdrawal_thrift, 'Management'},
    Request = {Service, Fun, Args},
    Client  = ff_woody_client:new(#{
        url           => <<"http://localhost:8022/v1/withdrawal">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request);
call_service(wallet, Fun, Args) ->
    Service = {ff_proto_wallet_thrift, 'Management'},
    Request = {Service, Fun, Args},
    Client  = ff_woody_client:new(#{
        url           => <<"http://localhost:8022/v1/wallet">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request);
call_service(destination, Fun, Args) ->
    Service = {ff_proto_destination_thrift, 'Management'},
    Request = {Service, Fun, Args},
    Client  = ff_woody_client:new(#{
        url           => <<"http://localhost:8022/v1/destination">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).

call_admin(Fun, Args) ->
    Service = {ff_proto_fistful_admin_thrift, 'FistfulAdmin'},
    Request = {Service, Fun, Args},
    Client  = ff_woody_client:new(#{
        url           => <<"http://localhost:8022/v1/admin">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).

create_party() ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_identity() ->
    IdentityID  = genlib:unique(),
    Party = create_party(),
    ok = ff_identity_machine:create(
        IdentityID,
        #{
            party    => Party,
            provider => <<"good-one">>,
            class    => <<"person">>
        },
        ff_entity_context:new()
    ),
    IdentityID.

create_wallet(Currency, Amount) ->
    IdentityID = create_identity(),
    WalletID = genlib:unique(), ExternalID = genlib:unique(),
    Params = #wlt_WalletParams{
        id = WalletID,
        name = <<"BigBossWallet">>,
        external_id = ExternalID,
        context = ff_entity_context:new(),
        account_params = #account_AccountParams{
            identity_id   = IdentityID,
            symbolic_code = Currency
        }
    },
    {ok, _} = call_service(wallet, 'Create', [Params]),
    add_money(WalletID, IdentityID, Amount, Currency),
    WalletID.

create_destination(C) ->
    #{token := T, payment_system := PS, bin := Bin, masked_pan := Mp} =
        ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    Resource = {bank_card, #'BankCard'{
        token = T,
        payment_system = PS,
        bin = Bin,
        masked_pan = Mp
    }},
    DstID = genlib:unique(),
    Params = #dst_DestinationParams{
        id          = DstID,
        identity    = create_identity(),
        name        = <<"BigBossDestination">>,
        currency    = <<"RUB">>,
        resource    = Resource,
        external_id = genlib:unique()
    },
    {ok, _} = call_service(destination, 'Create', [Params]),
    {authorized, #dst_Authorized{}} = ct_helper:await(
        {authorized, #dst_Authorized{}},
        fun () ->
            {ok, Dest} = call_service(destination, 'Get', [DstID]),
            Dest#dst_Destination.status
        end,
        genlib_retry:linear(15, 1000)
    ),
    DstID.

add_money(WalletID, IdentityID, Amount, Currency) ->
    SrcID = genlib:unique(),

    % Create source
    {ok, _Src1} = call_admin('CreateSource', [#ff_admin_SourceParams{
        id       = SrcID,
        name     = <<"HAHA NO">>,
        identity_id = IdentityID,
        currency = #'CurrencyRef'{symbolic_code = Currency},
        resource = {internal, #src_Internal{details = <<"Infinite source of cash">>}}
    }]),

    {authorized, #src_Authorized{}} = ct_helper:await(
        {authorized, #src_Authorized{}},
        fun () ->
            {ok, Src} = call_admin('GetSource', [SrcID]),
            Src#src_Source.status
        end
    ),

    % Process deposit
    {ok, Dep1} = call_admin('CreateDeposit', [#ff_admin_DepositParams{
        id          = genlib:unique(),
        source      = SrcID,
        destination = WalletID,
        body        = #'Cash'{
            amount   = Amount,
            currency = #'CurrencyRef'{symbolic_code = Currency}
        }
    }]),
    DepID = Dep1#deposit_Deposit.id,
    {pending, _} = Dep1#deposit_Deposit.status,
    succeeded = ct_helper:await(
        succeeded,
        fun () ->
            {ok, Dep} = call_admin('GetDeposit', [DepID]),
            {Status, _} = Dep#deposit_Deposit.status,
            Status
        end,
        genlib_retry:linear(15, 1000)
    ),
    ok.