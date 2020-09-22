-module(ff_instrument_SUITE).

-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_source_thrift.hrl").
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
-export([create_source_ok_test/1]).
-export([create_destination_ok_test/1]).
-export([create_source_identity_notfound_fail_test/1]).
-export([create_destination_identity_notfound_fail_test/1]).
-export([create_source_currency_notfound_fail_test/1]).
-export([create_destination_currency_notfound_fail_test/1]).
-export([get_source_ok_test/1]).
-export([get_destination_ok_test/1]).
-export([get_source_notfound_fail_test/1]).
-export([get_destination_notfound_fail_test/1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, default}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            create_source_ok_test,
            create_destination_ok_test,
            create_source_identity_notfound_fail_test,
            create_destination_identity_notfound_fail_test,
            create_source_currency_notfound_fail_test,
            create_destination_currency_notfound_fail_test,
            get_source_ok_test,
            get_destination_ok_test,
            get_source_notfound_fail_test,
            get_destination_notfound_fail_test
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

%% Default group test cases

-spec create_source_ok_test(config()) -> test_return().
create_source_ok_test(C) ->
    Party = create_party(C),
    IID = create_person_identity(Party, C),
    _SourceID = create_source(IID, C),
    ok.

-spec create_destination_ok_test(config()) -> test_return().
create_destination_ok_test(C) ->
    Party  = create_party(C),
    IID = create_person_identity(Party, C),
    _DestinationID = create_destination(IID, C),
    ok.

-spec create_source_identity_notfound_fail_test(config()) -> test_return().
create_source_identity_notfound_fail_test(_C) ->
    IID = <<"BadIdentityID">>,
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    Params = #{
        id => genlib:unique(),
        identity => IID,
        name => <<"XSource">>,
        currency => <<"RUB">>,
        resource => SrcResource
    },
    CreateResult = ff_source:create(Params, ff_entity_context:new()),
    ?assertEqual({error, {identity, notfound}}, CreateResult).

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
        )}
    },
    Params = #{
        id => genlib:unique(),
        identity => IID,
        name => <<"XDestination">>,
        currency => <<"RUB">>,
        resource => DestResource
    },
    CreateResult = ff_destination:create(Params, ff_entity_context:new()),
    ?assertEqual({error, {identity, notfound}}, CreateResult).

-spec create_source_currency_notfound_fail_test(config()) -> test_return().
create_source_currency_notfound_fail_test(C) ->
    Party = create_party(C),
    IID = create_person_identity(Party, C),
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    Params = #{
        id => genlib:unique(),
        identity => IID,
        name => <<"XSource">>,
        currency => <<"BadUnknownCurrency">>,
        resource => SrcResource
    },
    CreateResult = ff_source:create(Params, ff_entity_context:new()),
    ?assertEqual({error, {currency, notfound}}, CreateResult).

-spec create_destination_currency_notfound_fail_test(config()) -> test_return().
create_destination_currency_notfound_fail_test(C) ->
    Party = create_party(C),
    IID = create_person_identity(Party, C),
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
    CreateResult = ff_destination:create(Params, ff_entity_context:new()),
    ?assertEqual({error, {currency, notfound}}, CreateResult).

-spec get_source_ok_test(config()) -> test_return().
get_source_ok_test(C) ->
    Party = create_party(C),
    IID = create_person_identity(Party, C),
    SourceID = create_source(IID, C),
    {ok, SourceMachine} = ff_source:get_machine(SourceID),
    ?assertMatch(
        #{
            account := #{currency := <<"RUB">>},
            name := <<"XSource">>,
            resource := #{details := <<"Infinite source of cash">>, type := internal},
            status := authorized
        },
        ff_destination:get(SourceMachine)
    ).

-spec get_destination_ok_test(config()) -> test_return().
get_destination_ok_test(C) ->
    Party  = create_party(C),
    IID = create_person_identity(Party, C),
    DestinationID = create_destination(IID, C),
    {ok, DestinationMachine} = ff_destination:get_machine(DestinationID),
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
        ff_destination:get(DestinationMachine)
    ).

-spec get_source_notfound_fail_test(config()) -> test_return().
get_source_notfound_fail_test(_C) ->
    ?assertEqual({error, notfound}, ff_source:get_machine(<<"BadID">>)).

-spec get_destination_notfound_fail_test(config()) -> test_return().
get_destination_notfound_fail_test(_C) ->
    ?assertEqual({error, notfound}, ff_destination:get_machine(<<"BadID">>)).

%% Common functions

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C) ->
    create_person_identity(Party, C, <<"good-one">>).

create_person_identity(Party, C, ProviderID) ->
    create_identity(Party, ProviderID, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, C) ->
    create_identity(Party, <<"Identity Name">>, ProviderID, ClassID, C).

create_identity(Party, Name, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, name => Name, party => Party, provider => ProviderID, class => ClassID},
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
    ),
    ID.

create_instrument(Type, IdentityID, Name, Currency, Resource, C) ->
    ID = genlib:unique(),
    ok = create_instrument(
        Type,
        #{id => ID, identity => IdentityID, name => Name, currency => Currency, resource => Resource},
        ff_entity_context:new(),
        C
    ),
    ID.
create_instrument(destination, Params, Ctx, _C) ->
    ff_destination:create(Params, Ctx);
create_instrument(source, Params, Ctx, _C) ->
    ff_source:create(Params, Ctx).

create_source(IID, C) ->
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    SrcID = create_instrument(source, IID, <<"XSource">>, <<"RUB">>, SrcResource, C),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, SrcM} = ff_source:get_machine(SrcID),
            ff_source:status(ff_source:get(SrcM))
        end
    ),
    SrcID.

create_destination(IID, C) ->
    DestResource = {bank_card, #{bank_card => ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C)}},
    DestID = create_instrument(destination, IID, <<"XDestination">>, <<"RUB">>, DestResource, C),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, DestM} = ff_destination:get_machine(DestID),
            ff_destination:status(ff_destination:get(DestM))
        end
    ),
    DestID.

