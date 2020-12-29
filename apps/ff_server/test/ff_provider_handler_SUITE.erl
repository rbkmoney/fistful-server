-module(ff_provider_handler_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/ff_proto_provider_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_provider_ok/1]).
-export([get_provider_fail_notfound/1]).
-export([list_providers_ok/1]).

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            get_provider_ok,
            get_provider_fail_notfound,
            list_providers_ok
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

-spec get_provider_ok(config()) -> test_return().
get_provider_ok(_C) ->
    {ok, Provider} = call_service('GetProvider', {<<"good-one">>}),
    ?assertEqual(<<"good-one">>, Provider#provider_Provider.id),
    ?assertEqual(<<"Generic Payment Institution">>, Provider#provider_Provider.name),
    ?assertEqual([<<"RUS">>], Provider#provider_Provider.residences).

-spec get_provider_fail_notfound(config()) -> test_return().
get_provider_fail_notfound(_C) ->
    {exception, #fistful_ProviderNotFound{}} = call_service('GetProvider', {<<"unknown-provider">>}).

-spec list_providers_ok(config()) -> test_return().
list_providers_ok(_C) ->
    {ok, [_Provider | _Rest]} = call_service('ListProviders', {}).

%%

call_service(Fun, Args) ->
    Service = ff_services:get_service(fistful_provider),
    Path = erlang:list_to_binary(ff_services:get_service_path(fistful_provider)),
    Request = {Service, Fun, Args},
    Client = ff_woody_client:new(#{
        url => <<"http://localhost:8022", Path/binary>>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).
