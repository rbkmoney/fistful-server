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
-export([fistful_to_bender_migration/1]).

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
        idempotency_withdrawal_conflict,
        fistful_to_bender_migration
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
        wapi_wallet_ff_backend:create_withdrawal(Params, create_context(Party, C)).

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

-spec fistful_to_bender_migration(config()) ->
    test_return().

fistful_to_bender_migration(C) ->
    Params0 = #{
        <<"provider">> => <<"good-one">>,
        <<"class">> => <<"person">>,
        <<"name">> => <<"someone">>,
        <<"externalID">> => genlib:unique()
    },
    Party = create_party(C),
    Ctx = create_context(Party, C),
    {ok, Identity1} =
        wapi_wallet_ff_backend:create_identity(Params0, Ctx),
    CurrentID = binary_to_integer(maps:get(<<"id">>, Identity1)),
    ok = bump_bender_seq_to(identity, CurrentID + 1000, Party, C),
    Params1 = Params0#{<<"externalID">> => genlib:unique()},
    TargetID = integer_to_binary(CurrentID + 1000 + 1),
    {ok, #{<<"id">> := TargetID}} =
        wapi_wallet_ff_backend:create_identity(Params1, Ctx).

%%

bump_bender_seq_to(SeqID, TargetID, PartyID, C) ->
    SeqBin = atom_to_binary(SeqID, utf8),
    IdempotentKey = bender_client:get_idempotent_key(<<"wapi">>, SeqID, PartyID, genlib:unique()),
    {ok, ID} = bender_client:gen_by_sequence(IdempotentKey, SeqBin, <<>>, ct_helper:get_woody_ctx(C), #{},
        #{minimum => TargetID}
    ),
    true = binary_to_integer(ID) >= TargetID,
    ok.

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
