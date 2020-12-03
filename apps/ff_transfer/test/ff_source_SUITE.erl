-module(ff_source_SUITE).

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

-export([create_source_identity_notfound_fail_test/1]).
-export([create_source_currency_notfound_fail_test/1]).
-export([get_source_ok_test/1]).
-export([get_source_notfound_fail_test/1]).

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
            create_source_ok_test,
            create_source_identity_notfound_fail_test,
            create_source_currency_notfound_fail_test,
            get_source_ok_test,
            get_source_notfound_fail_test
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

-spec create_source_ok_test(config()) -> test_return().
create_source_ok_test(C) ->
    Party = create_party(C),
    IID = create_person_identity(Party, C),
    _SourceID = create_source(IID, C),
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
    CreateResult = ff_source_machine:create(Params, ff_entity_context:new()),
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
    CreateResult = ff_source_machine:create(Params, ff_entity_context:new()),
    ?assertEqual({error, {currency, notfound}}, CreateResult).

-spec get_source_ok_test(config()) -> test_return().
get_source_ok_test(C) ->
    Party = create_party(C),
    IID = create_person_identity(Party, C),
    SourceID = create_source(IID, C),
    {ok, SourceMachine} = ff_source_machine:get(SourceID),
    ?assertMatch(
        #{
            account := #{currency := <<"RUB">>},
            name := <<"XSource">>,
            resource := #{details := <<"Infinite source of cash">>, type := internal},
            status := authorized
        },
        ff_source_machine:source(SourceMachine)
    ).

-spec get_source_notfound_fail_test(config()) -> test_return().
get_source_notfound_fail_test(_C) ->
    ?assertEqual({error, notfound}, ff_source_machine:get(<<"BadID">>)).

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

create_source(IID, _C) ->
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    SrcID = create_source(IID, <<"XSource">>, <<"RUB">>, SrcResource),
    authorized = ct_helper:await(
        authorized,
        fun() ->
            {ok, SrcM} = ff_source_machine:get(SrcID),
            Source = ff_source_machine:source(SrcM),
            ff_source:status(Source)
        end
    ),
    SrcID.

create_source(IdentityID, Name, Currency, Resource) ->
    ID = genlib:unique(),
    ok = ff_source_machine:create(
        #{id => ID, identity => IdentityID, name => Name, currency => Currency, resource => Resource},
        ff_entity_context:new()
    ),
    ID.
