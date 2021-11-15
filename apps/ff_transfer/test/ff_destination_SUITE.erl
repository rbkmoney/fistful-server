-module(ff_destination_SUITE).

-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("stdlib/include/assert.hrl").

% Common test API
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Tests
-export([create_destination_ok_test/1]).
-export([create_destination_identity_notfound_fail_test/1]).
-export([create_destination_currency_notfound_fail_test/1]).
-export([get_destination_ok_test/1]).
-export([get_destination_notfound_fail_test/1]).

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, default}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            create_destination_ok_test,
            create_destination_identity_notfound_fail_test,
            create_destination_currency_notfound_fail_test,
            get_destination_ok_test,
            get_destination_notfound_fail_test
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

%% Default group test cases

-spec create_destination_ok_test(config()) -> test_return().
create_destination_ok_test(C) ->
    Party = create_party(C),
    IID = create_identity(Party, C),
    _DestinationID = create_destination(IID, C),
    ok.

-spec create_destination_identity_notfound_fail_test(config()) -> test_return().
create_destination_identity_notfound_fail_test(C) ->
    IID = <<"BadIdentityID">>,
    DestResource = {
        bank_card,
        #{
            bank_card => ct_cardstore:bank_card(
                <<"4150399999000900">>,
                {12, 2025},
                C
            )
        }
    },
    Params = #{
        id => genlib:unique(),
        identity => IID,
        name => <<"XDestination">>,
        currency => <<"RUB">>,
        resource => DestResource
    },
    CreateResult = ff_destination_machine:create(Params, ff_entity_context:new()),
    ?assertEqual({error, {identity, notfound}}, CreateResult).

-spec create_destination_currency_notfound_fail_test(config()) -> test_return().
create_destination_currency_notfound_fail_test(C) ->
    Party = create_party(C),
    IID = create_identity(Party, C),
    DestResource = {
        bank_card,
        #{
            bank_card => ct_cardstore:bank_card(
                <<"4150399999000900">>,
                {12, 2025},
                C
            )
        }
    },
    Params = #{
        id => genlib:unique(),
        identity => IID,
        name => <<"XDestination">>,
        currency => <<"BadUnknownCurrency">>,
        resource => DestResource
    },
    CreateResult = ff_destination_machine:create(Params, ff_entity_context:new()),
    ?assertEqual({error, {currency, notfound}}, CreateResult).

-spec get_destination_ok_test(config()) -> test_return().
get_destination_ok_test(C) ->
    Party = create_party(C),
    IID = create_identity(Party, C),
    DestinationID = create_destination(IID, C),
    {ok, DestinationMachine} = ff_destination_machine:get(DestinationID),
    ?assertMatch(
        #{
            account := #{currency := <<"RUB">>},
            name := <<"XDestination">>,
            resource := {
                bank_card,
                #{
                    bank_card := #{
                        bin := <<"415039">>,
                        exp_date := {12, 2025},
                        masked_pan := <<"0900">>
                    }
                }
            },
            status := authorized
        },
        ff_destination_machine:destination(DestinationMachine)
    ).

-spec get_destination_notfound_fail_test(config()) -> test_return().
get_destination_notfound_fail_test(_C) ->
    ?assertEqual({error, notfound}, ff_destination_machine:get(<<"BadID">>)).

%% Common functions

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

create_destination(IID, C) ->
    DestResource = {bank_card, #{bank_card => ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C)}},
    DestID = create_destination(IID, <<"XDestination">>, <<"RUB">>, DestResource),
    authorized = ct_helper:await(
        authorized,
        fun() ->
            {ok, DestM} = ff_destination_machine:get(DestID),
            Destination = ff_destination_machine:destination(DestM),
            ff_destination:status(Destination)
        end
    ),
    DestID.

create_destination(IdentityID, Name, Currency, Resource) ->
    ID = genlib:unique(),
    ok = ff_destination_machine:create(
        #{id => ID, identity => IdentityID, name => Name, currency => Currency, resource => Resource},
        ff_entity_context:new()
    ),
    ID.
