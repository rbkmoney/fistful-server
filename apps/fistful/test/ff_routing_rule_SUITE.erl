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

-export([withdrawal_routes_found_test/1]).
-export([withdrawal_no_routes_found_test/1]).
-export([withdrawal_rejected_by_prohibitions_table_test/1]).
-export([withdrawal_ruleset_misconfig_test/1]).
-export([withdrawal_rules_not_found_test/1]).
-export([p2p_routes_found_test/1]).
-export([p2p_no_routes_found_test/1]).
-export([p2p_rejected_by_prohibitions_table_test/1]).
-export([p2p_ruleset_misconfig_test/1]).
-export([p2p_rules_not_found_test/1]).

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
            withdrawal_routes_found_test,
            withdrawal_no_routes_found_test,
            withdrawal_rejected_by_prohibitions_table_test,
            withdrawal_ruleset_misconfig_test,
            withdrawal_rules_not_found_test,
            p2p_routes_found_test,
            p2p_no_routes_found_test,
            p2p_rejected_by_prohibitions_table_test,
            p2p_ruleset_misconfig_test,
            p2p_rules_not_found_test
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

-spec withdrawal_routes_found_test(config()) -> test_return().
withdrawal_routes_found_test(_C) ->
    VS = make_varset(?cash(999, <<"RUB">>), <<"12345">>),
    PaymentInstitutionID = 1,
    ?assertMatch(
        {
            [
                #{terminal_ref := ?trm(1)},
                #{terminal_ref := ?trm(2)}
            ],
            #{rejected_routes := []}
        },
        gather_routes(withdrawal_routing_rules, PaymentInstitutionID, VS)
    ).

-spec withdrawal_no_routes_found_test(config()) -> test_return().
withdrawal_no_routes_found_test(_C) ->
    VS = make_varset(?cash(1000, <<"RUB">>), <<"12345">>),
    PaymentInstitutionID = 1,
    ?assertMatch(
        {
            [],
            #{rejected_routes := []}
        },
        gather_routes(withdrawal_routing_rules, PaymentInstitutionID, VS)
    ).

-spec withdrawal_rejected_by_prohibitions_table_test(config()) -> test_return().
withdrawal_rejected_by_prohibitions_table_test(_C) ->
    VS = make_varset(?cash(1000, <<"RUB">>), <<"67890">>),
    PaymentInstitutionID = 1,
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
        gather_routes(withdrawal_routing_rules, PaymentInstitutionID, VS)
    ).

-spec withdrawal_ruleset_misconfig_test(config()) -> test_return().
withdrawal_ruleset_misconfig_test(_C) ->
    VS = #{party_id => <<"12345">>},
    PaymentInstitutionID = 1,
    ?assertMatch(
        {
            [],
            #{rejected_routes := []}
        },
        gather_routes(withdrawal_routing_rules, PaymentInstitutionID, VS)
    ).

-spec withdrawal_rules_not_found_test(config()) -> test_return().
withdrawal_rules_not_found_test(_C) ->
    VS = make_varset(?cash(1000, <<"RUB">>), <<"12345">>),
    PaymentInstitutionID = 2,
    ?assertMatch(
        {
            [],
            #{rejected_routes := []}
        },
        gather_routes(withdrawal_routing_rules, PaymentInstitutionID, VS)
    ).

-spec p2p_routes_found_test(config()) -> test_return().
p2p_routes_found_test(_C) ->
    VS = make_varset(?cash(999, <<"RUB">>), <<"12345">>),
    PaymentInstitutionID = 1,
    ?assertMatch(
        {
            [
                #{terminal_ref := ?trm(101)},
                #{terminal_ref := ?trm(102)}
            ],
            #{rejected_routes := []}
        },
        gather_routes(p2p_transfer_routing_rules, PaymentInstitutionID, VS)
    ).

-spec p2p_no_routes_found_test(config()) -> test_return().
p2p_no_routes_found_test(_C) ->
    VS = make_varset(?cash(1000, <<"RUB">>), <<"12345">>),
    PaymentInstitutionID = 1,
    ?assertMatch(
        {
            [],
            #{rejected_routes := []}
        },
        gather_routes(p2p_transfer_routing_rules, PaymentInstitutionID, VS)
    ).

-spec p2p_rejected_by_prohibitions_table_test(config()) -> test_return().
p2p_rejected_by_prohibitions_table_test(_C) ->
    VS = make_varset(?cash(1000, <<"RUB">>), <<"67890">>),
    PaymentInstitutionID = 1,
    ?assertMatch(
        {
            [
                #{terminal_ref := ?trm(103)},
                #{terminal_ref := ?trm(105)}
            ],
            #{
                rejected_routes := [
                    {_, ?trm(104), {'RoutingRule', <<"Candidate description">>}}
                ]
            }
        },
        gather_routes(p2p_transfer_routing_rules, PaymentInstitutionID, VS)
    ).

-spec p2p_ruleset_misconfig_test(config()) -> test_return().
p2p_ruleset_misconfig_test(_C) ->
    VS = #{party_id => <<"12345">>},
    PaymentInstitutionID = 1,
    ?assertMatch(
        {
            [],
            #{rejected_routes := []}
        },
        gather_routes(p2p_transfer_routing_rules, PaymentInstitutionID, VS)
    ).

-spec p2p_rules_not_found_test(config()) -> test_return().
p2p_rules_not_found_test(_C) ->
    VS = make_varset(?cash(1000, <<"RUB">>), <<"12345">>),
    PaymentInstitutionID = 2,
    ?assertMatch(
        {
            [],
            #{rejected_routes := []}
        },
        gather_routes(p2p_transfer_routing_rules, PaymentInstitutionID, VS)
    ).

%%

make_varset(Cash, PartyID) ->
    #{
        currency => ?cur(<<"RUB">>),
        cost => Cash,
        payment_tool => bank_card(),
        party_id => PartyID
    }.

bank_card() ->
    {bank_card, #domain_BankCard{
        token = genlib:bsuuid(),
        bin = <<"424242">>,
        last_digits = <<"">>,
        bank_name = <<"bank">>,
        payment_system_deprecated = visa,
        issuer_country = rus
    }}.

gather_routes(RoutingRulesTag, PaymentInstitutionID, Varset) ->
    Revision = ff_domain_config:head(),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, Varset, Revision),
    ff_routing_rule:gather_routes(
        PaymentInstitution,
        RoutingRulesTag,
        Varset,
        Revision
    ).
