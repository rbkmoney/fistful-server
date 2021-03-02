-module(ff_routing_rule_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("ff_cth/include/ct_domain.hrl").

%% Common test API

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests

-export([routes_found_test/1]).
-export([no_routes_found_test/1]).
-export([rejected_by_prohibitions_table_test/1]).
-export([ruleset_misconfig_test/1]).
-export([rules_not_found_test/1]).

%% Internal types

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

%% Macro helpers

%% Common test API implementation

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, default}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [
            routes_found_test,
            no_routes_found_test,
            rejected_by_prohibitions_table_test,
            ruleset_misconfig_test,
            rules_not_found_test
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

%% Tests

-spec routes_found_test(config()) -> test_return().
routes_found_test(_C) ->
    VS = make_varset(?cash(999, <<"RUB">>), <<"12345">>),
    ?assertMatch(
        {
            [
                #{terminal_ref := ?trm(1)},
                #{terminal_ref := ?trm(2)}
            ],
            #{rejected_routes := []}
        },
        gather_routes(VS, 1)
    ).

-spec no_routes_found_test(config()) -> test_return().
no_routes_found_test(_C) ->
    VS = make_varset(?cash(1000, <<"RUB">>), <<"12345">>),
    ?assertMatch(
        {
            [],
            #{rejected_routes := []}
        },
        gather_routes(VS, 1)
    ).

-spec rejected_by_prohibitions_table_test(config()) -> test_return().
rejected_by_prohibitions_table_test(_C) ->
    VS = make_varset(?cash(1000, <<"RUB">>), <<"67890">>),
    ?assertMatch(
        {
            [
                #{terminal_ref := ?trm(3)},
                #{terminal_ref := ?trm(5)}
            ],
            #{
                rejected_routes := [
                    {_, ?trm(4), {'RoutingRule', <<"Candidate description">>}}
                ]
            }
        },
        gather_routes(VS, 1)
    ).

-spec ruleset_misconfig_test(config()) -> test_return().
ruleset_misconfig_test(_C) ->
    VS = #{party_id => <<"12345">>},
    ?assertMatch(
        {
            [],
            #{rejected_routes := []}
        },
        gather_routes(VS, 1)
    ).

-spec rules_not_found_test(config()) -> test_return().
rules_not_found_test(_C) ->
    VS = make_varset(?cash(1000, <<"RUB">>), <<"12345">>),
    ?assertMatch(
        {
            [],
            #{rejected_routes := []}
        },
        gather_routes(VS, 2)
    ).

%%

make_varset(Cash, PartyID) ->
    #{
        currency => ?cur(<<"RUB">>),
        cost => Cash,
        payment_tool =>
            {bank_card, #domain_BankCard{
                payment_system = visa
            }},
        party_id => PartyID
    }.

gather_routes(Varset, PaymentInstitutionID) ->
    Revision = ff_domain_config:head(),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, Revision),
    ff_routing_rule:gather_routes(
        PaymentInstitution,
        withdrawal_routing_rules,
        Varset,
        Revision
    ).
