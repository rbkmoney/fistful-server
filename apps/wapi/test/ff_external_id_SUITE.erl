-module(ff_external_id_SUITE).

-include_lib("wapi_wallet_dummy_data.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

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

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

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
    ct_helper:makeup_cfg(
        [
            ct_helper:test_case_name(init),
            ct_payment_system:setup(#{
                optional_apps => [
                    bender_client,
                    wapi_woody_client,
                    wapi
                ]
            })
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

%%

-spec idempotency_identity_ok(config()) -> test_return().
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

-spec idempotency_identity_conflict(config()) -> test_return().
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

-spec idempotency_wallet_ok(config()) -> test_return().
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

-spec idempotency_wallet_conflict(config()) -> test_return().
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

-spec idempotency_destination_ok(config()) -> test_return().
idempotency_destination_ok(C) ->
    BankCard = make_bank_card(<<"4150399999000900">>, {12, 2025}, <<"ct_cardholder_name">>),
    %NewBankCard = maps:without([exp_date, cardholder_name], BankCard),
    Party = create_party(C),
    ExternalID = genlib:unique(),
    Context = create_context(Party, C),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party, C),
    Params = #{
        <<"identity">> => IdentityID,
        <<"currency">> => <<"RUB">>,
        <<"name">> => <<"XDesination">>,
        <<"resource">> => #{
            <<"type">> => <<"BankCardDestinationResource">>,
            <<"token">> => create_resource_token(BankCard)
        },
        <<"externalID">> => ExternalID
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_destination(Params, Context),
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_destination(Params, Context),
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:get_destination_by_external_id(ExternalID, Context).

-spec idempotency_destination_conflict(config()) -> test_return().
idempotency_destination_conflict(C) ->
    BankCard = make_bank_card(<<"4150399999000900">>, {12, 2025}, <<"ct_cardholder_name">>),
    % NewBankCard = maps:without([exp_date, cardholder_name], BankCard),
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party, C),
    Params = #{
        <<"identity">> => IdentityID,
        <<"currency">> => <<"RUB">>,
        <<"name">> => <<"XDesination">>,
        <<"resource">> => #{
            <<"type">> => <<"BankCardDestinationResource">>,
            <<"token">> => create_resource_token(BankCard)
        },
        <<"externalID">> => ExternalID
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_destination(Params, create_context(Party, C)),
    NewParams = Params#{<<"currency">> => <<"USD">>},
    {error, {external_id_conflict, ID, ExternalID}} =
        wapi_wallet_ff_backend:create_destination(NewParams, create_context(Party, C)).

-spec idempotency_withdrawal_ok(config()) -> test_return().
idempotency_withdrawal_ok(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party, C),
    {ok, #{<<"id">> := WalletID}} = create_wallet(IdentityID, Party, C),
    {ok, #{<<"id">> := DestID}} = create_destination_legacy(IdentityID, Party, C),
    Context = create_context(Party, C),
    wait_for_destination_authorized(DestID),

    Params = #{
        <<"wallet">> => WalletID,
        <<"destination">> => DestID,
        <<"body">> => #{
            <<"amount">> => <<"10">>,
            <<"currency">> => <<"RUB">>
        },
        <<"externalID">> => ExternalID
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_withdrawal(Params, Context),
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_withdrawal(Params, Context),
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:get_withdrawal_by_external_id(ExternalID, Context).

-spec idempotency_withdrawal_conflict(config()) -> test_return().
idempotency_withdrawal_conflict(C) ->
    Party = create_party(C),
    ExternalID = genlib:unique(),
    {ok, #{<<"id">> := IdentityID}} = create_identity(Party, C),
    {ok, #{<<"id">> := WalletID}} = create_wallet(IdentityID, Party, C),
    {ok, #{<<"id">> := DestID}} = create_destination_legacy(IdentityID, Party, C),

    wait_for_destination_authorized(DestID),

    Params = #{
        <<"wallet">> => WalletID,
        <<"destination">> => DestID,
        <<"body">> => Body = #{
            <<"amount">> => <<"10">>,
            <<"currency">> => <<"RUB">>
        },
        <<"externalID">> => ExternalID
    },
    {ok, #{<<"id">> := ID}} =
        wapi_wallet_ff_backend:create_withdrawal(Params, create_context(Party, C)),
    NewParams = Params#{<<"body">> => Body#{<<"amount">> => <<"100">>}},
    {error, {external_id_conflict, ID, ExternalID}} =
        wapi_wallet_ff_backend:create_withdrawal(NewParams, create_context(Party, C)).

%%

wait_for_destination_authorized(DestID) ->
    authorized = ct_helper:await(
        authorized,
        fun() ->
            {ok, DestM} = ff_destination_machine:get(DestID),
            Destination = ff_destination_machine:destination(DestM),
            ff_destination:status(Destination)
        end
    ).

create_destination_legacy(IdentityID, Party, C) ->
    BankCard = make_bank_card(<<"4150399999000900">>, {12, 2025}, <<"ct_cardholder_name">>),
    Params = #{
        <<"identity">> => IdentityID,
        <<"currency">> => <<"RUB">>,
        <<"name">> => <<"XDesination">>,
        <<"resource">> => #{
            <<"type">> => <<"BankCardDestinationResource">>,
            <<"token">> => create_resource_token(BankCard)
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
    maps:merge(wapi_ct_helper:create_auth_ctx(PartyID), create_woody_ctx(C)).

create_woody_ctx(C) ->
    #{
        woody_context => ct_helper:get_woody_ctx(C)
    }.

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

make_bank_card(Pan, {MM, YYYY} = _ExpDate, CardHolder) ->
    ?BANK_CARD#'BankCard'{
        bin = ?BIN(Pan),
        masked_pan = ?LAST_DIGITS(Pan),
        cardholder_name = CardHolder,
        exp_date = #'BankCardExpDate'{
            month = MM,
            year = YYYY
        }
    }.

create_resource_token(Resource) ->
    wapi_crypto:encrypt_bankcard_token(Resource).
