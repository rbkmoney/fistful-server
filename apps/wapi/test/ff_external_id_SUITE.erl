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
-export([bender_to_fistful_sync/1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        bender_to_fistful_sync,
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
        ct_payment_system:setup(#{
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

%%

-spec idempotency_identity_ok(config()) ->
    test_return().

idempotency_identity_ok(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    Params = #{
        <<"provider">> => <<"good-one">>,
        <<"class">> => <<"person">>,
        <<"name">> => <<"someone">>,
        <<"externalID">> => ExternalID
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_identity(Params, create_context(Party, C)),
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_identity(Params, create_context(Party, C)).

-spec idempotency_identity_conflict(config()) ->
    test_return().

idempotency_identity_conflict(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    Params = #{
        <<"provider">> => <<"good-one">>,
        <<"class">> => <<"person">>,
        <<"name">> => <<"HAHA NO2">>,
        <<"externalID">> => ExternalID
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_identity(Params, create_context(Party, C)),
    NewParams = Params#{<<"provider">> => <<"good-two">>},
    {error, {external_id_conflict, ID, ExternalID}} =
        wapi_wallet_ff_backend:create_identity(NewParams, create_context(Party, C)).

-spec idempotency_wallet_ok(config()) ->
    test_return().

idempotency_wallet_ok(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party, C),
    Params = #{
        <<"identity">> => IdentityID,
        <<"currency">> => <<"RUB">>,
        <<"name">> => <<"HAHA NO2">>,
        <<"externalID">> => ExternalID
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_wallet(Params, create_context(Party, C)),
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_wallet(Params, create_context(Party, C)).

-spec idempotency_wallet_conflict(config()) ->
    test_return().

idempotency_wallet_conflict(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party, C),
    Params = #{
        <<"identity">> => IdentityID,
        <<"currency">> => <<"RUB">>,
        <<"name">> => <<"HAHA NO2">>,
        <<"externalID">> => ExternalID
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_wallet(Params, create_context(Party, C)),
    NewParams = Params#{<<"currency">> => <<"USD">>},
    {error, {external_id_conflict, ID, ExternalID}} =
        wapi_wallet_ff_backend:create_wallet(NewParams, create_context(Party, C)).

-spec idempotency_destination_ok(config()) ->
    test_return().

idempotency_destination_ok(C) ->
    BankCard = #{payment_system := PS, masked_pan := MP} =
        ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party, C),
    Params = #{
        <<"identity">>  => IdentityID,
        <<"currency">>  => <<"RUB">>,
        <<"name">>      => <<"XDesination">>,
        <<"resource">>  => #{
            <<"type">>  => <<"BankCardDestinationResource">>,
            <<"token">> => wapi_utils:map_to_base64url(BankCard#{
                paymentSystem => PS,
                lastDigits => MP})
        },
        <<"externalID">> => ExternalID
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_destination(Params, create_context(Party, C)),
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_destination(Params, create_context(Party, C)).

-spec idempotency_destination_conflict(config()) ->
    test_return().

idempotency_destination_conflict(C) ->
    BankCard = #{payment_system := PS, masked_pan := MP} =
        ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party, C),
    Params = #{
        <<"identity">>  => IdentityID,
        <<"currency">>  => <<"RUB">>,
        <<"name">>      => <<"XDesination">>,
        <<"resource">>  => #{
            <<"type">>  => <<"BankCardDestinationResource">>,
            <<"token">> => wapi_utils:map_to_base64url(BankCard#{
                paymentSystem   => PS,
                lastDigits      => MP})
        },
        <<"externalID">> => ExternalID
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_destination(Params, create_context(Party, C)),
    NewParams = Params#{<<"currency">> => <<"USD">>},
    {error, {external_id_conflict, ID, ExternalID}} =
        wapi_wallet_ff_backend:create_destination(NewParams, create_context(Party, C)).

-spec idempotency_withdrawal_ok(config()) ->
    test_return().

idempotency_withdrawal_ok(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party, C),
    {ok, #{<<"id">> := WalletID}}   = create_wallet(IdentityID, Party, C),
    {ok, #{<<"id">> := DestID}}     = create_destination(IdentityID, Party, C),

    wait_for_destination_authorized(DestID),

    Params = #{
        <<"wallet">>        => WalletID,
        <<"destination">>   => DestID,
        <<"body">>          => #{
            <<"amount">>    => <<"10">>,
            <<"currency">>  => <<"RUB">>
        },
        <<"externalID">> => ExternalID
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_withdrawal(Params, create_context(Party, C)),
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_withdrawal(Params, create_context(Party, C)),
    true =:= is_integer(binary_to_integer(ID)).

-spec idempotency_withdrawal_conflict(config()) ->
    test_return().

idempotency_withdrawal_conflict(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party, C),
    {ok, #{<<"id">> := WalletID}}   = create_wallet(IdentityID, Party, C),
    {ok, #{<<"id">> := DestID}}     = create_destination(IdentityID, Party, C),

    wait_for_destination_authorized(DestID),

    Params = #{
        <<"wallet">>        => WalletID,
        <<"destination">>   => DestID,
        <<"body">>          => Body = #{
            <<"amount">>    => <<"10">>,
            <<"currency">>  => <<"RUB">>
        },
        <<"externalID">> => ExternalID
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_withdrawal(Params, create_context(Party, C)),
    NewParams = Params#{<<"body">> => Body#{<<"amount">> => <<"100">>}},
    {error, {external_id_conflict, ID, ExternalID}} =
        wapi_wallet_ff_backend:create_withdrawal(NewParams, create_context(Party, C)).

-spec bender_to_fistful_sync(config()) ->
    test_return().

bender_to_fistful_sync(C) ->
    ExternalID = genlib:unique(),
    Params0 = #{
        <<"provider">> => <<"good-one">>,
        <<"class">> => <<"person">>,
        <<"name">> => <<"someone">>,
        <<"externalID">> => ExternalID
    },
    Party = create_party(C),
    Ctx = create_context(Party, C),
    %% Offset for migration purposes
    FFSeqStart = 42,
    BenderOffset = 100000,
    ok = offset_ff_sequence(identity, FFSeqStart),
    TargetID = integer_to_binary(FFSeqStart + BenderOffset),
    {ok, #{<<"id">> := TargetID}} = wapi_wallet_ff_backend:create_identity(Params0, Ctx),
    {ok, TargetID} = get_ff_internal_id(identity, Party, ExternalID).

%%

get_ff_internal_id(Type, PartyID, ExternalID) ->
    FistfulID = ff_external_id:construct_external_id(PartyID, ExternalID),
    ff_external_id:get_internal_id(Type, FistfulID).

offset_ff_sequence(_Type, 0) ->
    ok;
offset_ff_sequence(Type, Amount) ->
    NS = 'ff/sequence',
    _ = ff_sequence:next(NS, ff_string:join($/, [Type, id]), fistful:backend(NS)),
    offset_ff_sequence(Type, Amount - 1).

wait_for_destination_authorized(DestID) ->
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
    wapi_wallet_ff_backend:create_destination(Params, create_context(Party, C)).

create_wallet(IdentityID, Party, C) ->
    Params = #{
        <<"identity">> => IdentityID,
        <<"currency">> => <<"RUB">>,
        <<"name">> => <<"HAHA NO2">>
    },
    wapi_wallet_ff_backend:create_wallet(Params, create_context(Party, C)).

create_identity(Party, C) ->
    Params = #{
        <<"provider">> => <<"good-one">>,
        <<"class">> => <<"person">>,
        <<"name">> => <<"HAHA NO2">>
    },
    wapi_wallet_ff_backend:create_identity(Params, create_context(Party, C)).

create_context(PartyID, C) ->
    maps:merge(create_auth_ctx(PartyID), create_woody_ctx(C)).

create_woody_ctx(C) ->
    #{
        woody_context => ct_helper:get_woody_ctx(C)
    }.

create_auth_ctx(PartyID) ->
    #{
        swagger_context => #{auth_context => {{PartyID, empty}, empty}}
    }.

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.
