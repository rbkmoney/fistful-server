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
-export([idempotency_destination_ok/1]).
-export([idempotency_destination_conflict/1]).
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
        idempotency_destination_ok,
        idempotency_destination_conflict,
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
    Params = #{
        <<"provider">> => <<"good-one">>,
        <<"class">> => <<"person">>,
        <<"name">> => <<"someone">>
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_identity(ExternalID, Params, create_auth_ctx(Party)),
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_identity(ExternalID, Params, create_auth_ctx(Party)).

-spec idempotency_identity_conflict(config()) ->
    test_return().

idempotency_identity_conflict(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    Params = #{
        <<"provider">> => <<"good-one">>,
        <<"class">> => <<"person">>,
        <<"name">> => <<"HAHA NO2">>
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_identity(ExternalID, Params, create_auth_ctx(Party)),
    NewParams = Params#{<<"provider">> => <<"good-two">>},
    {error, {conflict, ID}} =
        wapi_wallet_ff_backend:create_identity(ExternalID, NewParams, create_auth_ctx(Party)).

-spec idempotency_wallet_ok(config()) ->
    test_return().

idempotency_wallet_ok(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party),
    Params = #{
        <<"identity">> => IdentityID,
        <<"currency">> => <<"RUB">>,
        <<"name">> => <<"HAHA NO2">>
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_wallet(ExternalID, Params, create_auth_ctx(Party)),
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_wallet(ExternalID, Params, create_auth_ctx(Party)).

-spec idempotency_wallet_conflict(config()) ->
    test_return().

idempotency_wallet_conflict(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party),
    Params = #{
        <<"identity">> => IdentityID,
        <<"currency">> => <<"RUB">>,
        <<"name">> => <<"HAHA NO2">>
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_wallet(ExternalID, Params, create_auth_ctx(Party)),
    NewParams = Params#{<<"currency">> => <<"USD">>},
    {error, {conflict, ID}} =
        wapi_wallet_ff_backend:create_wallet(ExternalID, NewParams, create_auth_ctx(Party)).

-spec idempotency_destination_ok(config()) ->
    test_return().

idempotency_destination_ok(C) ->
    BankCard = #{payment_system := PS, masked_pan := MP} =
        ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party),
    Params = #{
        <<"identity">>  => IdentityID,
        <<"currency">>  => <<"RUB">>,
        <<"name">>      => <<"XDesination">>,
        <<"resource">>  => #{
            <<"type">>  => <<"BankCardDestinationResource">>,
            <<"token">> => wapi_utils:map_to_base64url(BankCard#{
                paymentSystem => PS,
                lastDigits => MP})
        }
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_destination(ExternalID, Params, create_auth_ctx(Party)),
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_destination(ExternalID, Params, create_auth_ctx(Party)).

-spec idempotency_destination_conflict(config()) ->
    test_return().

idempotency_destination_conflict(C) ->
    BankCard = #{payment_system := PS, masked_pan := MP} =
        ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party),
    Params = #{
        <<"identity">>  => IdentityID,
        <<"currency">>  => <<"RUB">>,
        <<"name">>      => <<"XDesination">>,
        <<"resource">>  => #{
            <<"type">>  => <<"BankCardDestinationResource">>,
            <<"token">> => wapi_utils:map_to_base64url(BankCard#{
                paymentSystem   => PS,
                lastDigits      => MP})
        }
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_destination(ExternalID, Params, create_auth_ctx(Party)),
    NewParams = Params#{<<"currency">> => <<"USD">>},
    {error, {conflict, ID}} =
        wapi_wallet_ff_backend:create_destination(ExternalID, NewParams, create_auth_ctx(Party)).

-spec idempotency_withdrawal_ok(config()) ->
    test_return().

idempotency_withdrawal_ok(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party),
    {ok, #{<<"id">> := WalletID}}   = create_wallet(IdentityID, Party),
    {ok, #{<<"id">> := DestID}}     = create_destination(IdentityID, Party, C),

    wait_for_destination_authorized(DestID),

    Params = #{
        <<"wallet">>        => WalletID,
        <<"destination">>   => DestID,
        <<"body">>          => #{
            <<"amount">>    => <<"10">>,
            <<"currency">>  => <<"RUB">>
        }
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_withdrawal(ExternalID, Params, create_auth_ctx(Party)),
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_withdrawal(ExternalID, Params, create_auth_ctx(Party)).

-spec idempotency_withdrawal_conflict(config()) ->
    test_return().

idempotency_withdrawal_conflict(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party),
    {ok, #{<<"id">> := WalletID}}   = create_wallet(IdentityID, Party),
    {ok, #{<<"id">> := DestID}}     = create_destination(IdentityID, Party, C),

    wait_for_destination_authorized(DestID),

    Params = #{
        <<"wallet">>        => WalletID,
        <<"destination">>   => DestID,
        <<"body">>          => Body = #{
            <<"amount">>    => <<"10">>,
            <<"currency">>  => <<"RUB">>
        }
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_withdrawal(ExternalID, Params, create_auth_ctx(Party)),
    NewParams = Params#{<<"body">> => Body#{<<"amount">> => <<"100">>}},
    {error, {conflict, ID}} =
        wapi_wallet_ff_backend:create_withdrawal(ExternalID, NewParams, create_auth_ctx(Party)).

%%

wait_for_destination_authorized(DestID) ->
    {ok, DestM1} = ff_destination:get_machine(DestID),
    Dest1 = ff_destination:get(DestM1),
    unauthorized = ff_destination:status(Dest1),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, DestM} = ff_destination:get_machine(DestID),
            ff_destination:status(ff_destination:get(DestM))
        end
    ).

create_destination(IdentityID, Party, C) ->
    BankCard = #{payment_system := PS, masked_pan := MP} =
        ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    Params = #{
        <<"identity">>  => IdentityID,
        <<"currency">>  => <<"RUB">>,
        <<"name">>      => <<"XDesination">>,
        <<"resource">>  => #{
            <<"type">>  => <<"BankCardDestinationResource">>,
            <<"token">> => wapi_utils:map_to_base64url(BankCard#{
                paymentSystem   => PS,
                lastDigits      => MP})
        }
    },
    wapi_wallet_ff_backend:create_destination(undefined, Params, create_auth_ctx(Party)).

create_wallet(IdentityID, Party) ->
    create_wallet(undefined, IdentityID, Party).

create_wallet(ExternalID, IdentityID, Party) ->
    Params = #{
        <<"identity">> => IdentityID,
        <<"currency">> => <<"RUB">>,
        <<"name">> => <<"HAHA NO2">>
    },
    wapi_wallet_ff_backend:create_wallet(ExternalID, Params, create_auth_ctx(Party)).


create_identity(Party) ->
    create_identity(undefined, Party).

create_identity(ExternalID, Party) ->
    Params = #{
        <<"provider">> => <<"good-one">>,
        <<"class">> => <<"person">>,
        <<"name">> => <<"HAHA NO2">>
    },
    wapi_wallet_ff_backend:create_identity(ExternalID, Params, create_auth_ctx(Party)).

create_auth_ctx(PartyID) ->
    #{
        swagger_context => #{auth_context => {{PartyID, empty}, empty}}
    }.

create_party(_C) ->
    ID = genlib:unique(),
    _ = ff_party:create(ID),
    ID.
