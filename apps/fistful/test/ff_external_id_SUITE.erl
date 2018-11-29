-module(ff_external_id_SUITE).

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([idempotency_identity_ok/1]).
-export([idempotency_identity_conflict/1]).
-export([idempotency_wallet_ok/1]).
-export([idempotency_wallet_conflict/1]).
-export([idempotency_source_ok/1]).
-export([idempotency_source_conflict/1]).
-export([idempotency_destination_ok/1]).
-export([idempotency_destination_conflict/1]).
-export([idempotency_deposit_ok/1]).
-export([idempotency_deposit_conflict/1]).
-export([idempotency_withdrawal_ok/1]).
-export([idempotency_withdrawal_conflict/1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        idempotency_identity_ok,
        idempotency_identity_conflict,
        idempotency_wallet_ok,
        idempotency_wallet_conflict,
        idempotency_source_ok,
        idempotency_source_conflict,
        idempotency_destination_ok,
        idempotency_destination_conflict,
        idempotency_deposit_ok,
        idempotency_deposit_conflict,
        idempotency_withdrawal_ok,
        idempotency_withdrawal_conflict
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() -> [].

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

%%

-spec idempotency_identity_ok(config()) ->
    test_return().

idempotency_identity_ok(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    ID = ct_payment_system:create_identity(ExternalID, Party, <<"good-one">>, <<"person">>),
    ID = ct_payment_system:create_identity(ExternalID, Party, <<"good-one">>, <<"person">>).

-spec idempotency_identity_conflict(config()) ->
    test_return().

idempotency_identity_conflict(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    ID = ct_payment_system:create_identity(ExternalID, Party, <<"good-one">>, <<"person">>),
    {error, {conflict, ID}} = ct_payment_system:create_identity(ExternalID, Party, <<"good-two">>, <<"person">>).


-spec idempotency_wallet_ok(config()) ->
    test_return().

idempotency_wallet_ok(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    IdentityID = ct_payment_system:create_identity(Party, <<"good-one">>, <<"person">>),
    ID = ct_payment_system:create_wallet(ExternalID, IdentityID, <<"HAHA NO2">>, <<"RUB">>),
    ID = ct_payment_system:create_wallet(ExternalID, IdentityID, <<"HAHA NO2">>, <<"RUB">>).

-spec idempotency_wallet_conflict(config()) ->
    test_return().

idempotency_wallet_conflict(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    IdentityID = ct_payment_system:create_identity(Party, <<"good-one">>, <<"person">>),
    ID = ct_payment_system:create_wallet(ExternalID, IdentityID, <<"HAHA NO2">>, <<"RUB">>),
    {error, {conflict, ID}} =
        ct_payment_system:create_wallet(ExternalID, IdentityID, <<"HAHA NO2">>, <<"USD">>).

-spec idempotency_source_ok(config()) ->
    test_return().

idempotency_source_ok(C) ->
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    Party = create_party(C),
    ExternalID = genlib:unique(),
    IdentityID = ct_payment_system:create_identity(Party, <<"good-one">>, <<"person">>),
    SrcID = create_instrument(ExternalID, source, IdentityID,
        <<"XSource">>, <<"RUB">>, SrcResource, C),
    SrcID = create_instrument(ExternalID, source, IdentityID,
        <<"XSource">>, <<"RUB">>, SrcResource, C).

-spec idempotency_source_conflict(config()) ->
    test_return().

idempotency_source_conflict(C) ->
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    Party = create_party(C),
    ExternalID = genlib:unique(),
    IdentityID = ct_payment_system:create_identity(Party, <<"good-one">>, <<"person">>),
    SrcID = create_instrument(ExternalID, source, IdentityID,
        <<"XSource">>, <<"RUB">>, SrcResource, C),
    {error, {conflict, SrcID}} = create_instrument(ExternalID, source, IdentityID,
        <<"XSource">>, <<"USD">>, SrcResource, C).

-spec idempotency_destination_ok(config()) ->
    test_return().

idempotency_destination_ok(C) ->
    DestResource = {bank_card, ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C)},
    Party = create_party(C),
    ExternalID = genlib:unique(),
    IdentityID = ct_payment_system:create_identity(Party, <<"good-one">>, <<"person">>),
    DestID = create_instrument(ExternalID, destination, IdentityID,
        <<"XDesination">>, <<"RUB">>, DestResource, C),
    DestID = create_instrument(ExternalID, destination, IdentityID,
        <<"XDesination">>, <<"RUB">>, DestResource, C).

-spec idempotency_destination_conflict(config()) ->
    test_return().

idempotency_destination_conflict(C) ->
    DestResource = {bank_card, ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C)},
    Party = create_party(C),
    ExternalID = genlib:unique(),
    IdentityID = ct_payment_system:create_identity(Party, <<"good-one">>, <<"person">>),
    DestID = create_instrument(ExternalID, destination, IdentityID,
        <<"XDesination">>, <<"RUB">>, DestResource, C),
    {error, {conflict, DestID}} = create_instrument(ExternalID, destination, IdentityID,
        <<"XDesination">>, <<"USD">>, DestResource, C).

-spec idempotency_deposit_ok(config()) ->
    test_return().

idempotency_deposit_ok(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    IdentityID = ct_payment_system:create_identity(Party, <<"good-one">>, <<"person">>),
    WalletID = ct_payment_system:create_wallet(IdentityID, <<"HAHA NO2">>, <<"RUB">>),
    SrcID   = create_source(IdentityID, C),
    DepID = create_deposit(ExternalID, WalletID, SrcID),
    DepID = create_deposit(ExternalID, WalletID, SrcID).

-spec idempotency_deposit_conflict(config()) ->
    test_return().

idempotency_deposit_conflict(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    IdentityID1 = ct_payment_system:create_identity(Party, <<"good-one">>, <<"person">>),
    IdentityID2 = ct_payment_system:create_identity(Party, <<"good-one">>, <<"person">>),
    WalletID = ct_payment_system:create_wallet(IdentityID1, <<"HAHA NO2">>, <<"RUB">>),
    SrcID1   = create_source(IdentityID1, C),
    SrcID2   = create_source(IdentityID2, C),
    DepID = create_deposit(ExternalID, WalletID, SrcID1),
    {error, {conflict, DepID}} = create_deposit(ExternalID, WalletID, SrcID2).

-spec idempotency_withdrawal_ok(config()) ->
    test_return().

idempotency_withdrawal_ok(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    IdentityID = ct_payment_system:create_identity(Party, <<"good-one">>, <<"person">>),
    WalletID = ct_payment_system:create_wallet(IdentityID, <<"HAHA NO2">>, <<"RUB">>),
    DestID = create_destination(IdentityID, C),
    WdrID = create_withdrawal(ExternalID, WalletID, DestID),
    WdrID = create_withdrawal(ExternalID, WalletID, DestID).

-spec idempotency_withdrawal_conflict(config()) ->
    test_return().

idempotency_withdrawal_conflict(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    IdentityID1 = ct_payment_system:create_identity(Party, <<"good-one">>, <<"person">>),
    IdentityID2 = ct_payment_system:create_identity(Party, <<"good-one">>, <<"person">>),
    WalletID = ct_payment_system:create_wallet(IdentityID1, <<"HAHA NO2">>, <<"RUB">>),
    DestID1 = create_destination(IdentityID1, C),
    DestID2 = create_destination(IdentityID2, C),
    WdrID = create_withdrawal(ExternalID, WalletID, DestID1),
    {error, {conflict, WdrID}} = create_withdrawal(ExternalID, WalletID, DestID2).

%%

create_withdrawal(ExternalID, WalletID, DestID) ->
    case ff_withdrawal:create(
        ExternalID,
        #{wallet_id => WalletID, destination_id => DestID, body => {4240, <<"RUB">>}},
        ff_ctx:new()
    ) of
        {ok, Withdrawal} ->
            ff_withdrawal:id(ff_withdrawal:get(Withdrawal));
        Error ->
            Error
    end.

create_deposit(ExternalID, WalletID, SrcID) ->
    case ff_deposit:create(
        ExternalID,
        #{source_id => SrcID, wallet_id => WalletID, body => {10000, <<"RUB">>}},
        ff_ctx:new()
    ) of
        {ok, Deposit} ->
            ff_deposit:id(ff_deposit:get(Deposit));
        Error ->
            Error
    end.

create_party(_C) ->
    ID = genlib:unique(),
    _ = ff_party:create(ID),
    ID.

create_instrument(ExternalID, Type, IdentityID, Name, Currency, Resource, _C) ->
    ct_payment_system:create_instrument(
        ExternalID,
        Type,
        IdentityID,
        Name,
        Currency,
        Resource
    ).

create_instrument(Type, IdentityID, Name, Currency, Resource, _C) ->
    ct_payment_system:create_instrument(
        Type,
        IdentityID,
        Name,
        Currency,
        Resource
    ).

create_source(IID, C) ->
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    SrcID = create_instrument(source, IID, <<"XSource">>, <<"RUB">>, SrcResource, C),
    {ok, SrcM1} = ff_source:get_machine(SrcID),
    Src1 = ff_source:get(SrcM1),
    unauthorized = ff_source:status(Src1),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, SrcM} = ff_source:get_machine(SrcID),
            ff_source:status(ff_source:get(SrcM))
        end
    ),
    SrcID.

create_destination(IID, C) ->
    DestResource = {bank_card, ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C)},
    DestID = create_instrument(destination, IID, <<"XDesination">>, <<"RUB">>, DestResource, C),
    {ok, DestM1} = ff_destination:get_machine(DestID),
    Dest1 = ff_destination:get(DestM1),
    unauthorized = ff_destination:status(Dest1),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, DestM} = ff_destination:get_machine(DestID),
            ff_destination:status(ff_destination:get(DestM))
        end
    ),
    DestID.
