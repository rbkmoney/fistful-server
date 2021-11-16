-module(ff_identity_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_missing_fails/1]).
-export([create_missing_fails/1]).
-export([create_ok/1]).

%%

-import(ff_pipeline, [unwrap/1]).

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        get_missing_fails,
        create_missing_fails,
        create_ok
    ].

-spec get_missing_fails(config()) -> test_return().
-spec create_missing_fails(config()) -> test_return().
-spec create_ok(config()) -> test_return().

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
    ok = ct_payment_system:shutdown(C),
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

get_missing_fails(_C) ->
    ID = genlib:unique(),
    {error, notfound} = ff_identity_machine:get(ID).

create_missing_fails(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    Name = <<"Identity Name">>,
    {error, {provider, notfound}} = ff_identity_machine:create(
        #{
            id => ID,
            name => Name,
            party => Party,
            provider => <<"who">>
        },
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
    ).

create_ok(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    Name = <<"Identity Name">>,
    ok = ff_identity_machine:create(
        #{
            id => ID,
            name => Name,
            party => Party,
            provider => <<"good-one">>
        },
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
    ),
    I1 = ff_identity_machine:identity(unwrap(ff_identity_machine:get(ID))),
    {ok, accessible} = ff_identity:is_accessible(I1),
    Party = ff_identity:party(I1).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.
