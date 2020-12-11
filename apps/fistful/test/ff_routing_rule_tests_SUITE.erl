-module(hg_route_rules_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_errors_thrift.hrl").
-include_lib("stdlib/include/assert.hrl").

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

-export([no_route_found_for_payment/1]).
-export([gather_route_success/1]).
-export([rejected_by_table_prohibitions/1]).
-export([empty_candidate_ok/1]).
-export([ruleset_misconfig/1]).

%% Internal types

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

%% API

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, default},
        {group, routing_with_fail_rate},
        {group, terminal_priority}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [], [
            no_route_found_for_payment,
            gather_route_success,
            rejected_by_table_prohibitions,
            empty_candidate_ok,
            ruleset_misconfig,
            handle_uncomputable_provider_terms
        ]},
        {routing_with_fail_rate, [parallel], [
            prefer_alive,
            prefer_normal_conversion,
            prefer_higher_availability,
            prefer_higher_conversion,
            prefer_weight_over_availability,
            prefer_weight_over_conversion,
            gathers_fail_rated_routes
        ]},
        {terminal_priority, [], [
            terminal_priority_for_shop
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

%% Tests

-spec no_route_found_for_payment(config()) -> test_return().
no_route_found_for_payment(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(999, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[], RejectContext} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision),
    #{
        rejected_routes := [
            {?prv(1), ?trm(1), {'PaymentsProvisionTerms', payment_tool}},
            {?prv(2), ?trm(6), {'PaymentsProvisionTerms', category}},
            {?prv(3), ?trm(10), {'PaymentsProvisionTerms', cost}}
        ]
    } = RejectContext.

-spec gather_route_success(config()) -> test_return().
gather_route_success(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[{_, {?trm(10), _, _}}], RejectContext} = hg_routing_rule:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),
    #{
        rejected_routes := [
            {?prv(1), ?trm(1), {'PaymentsProvisionTerms', payment_tool}},
            {?prv(2), ?trm(6), {'PaymentsProvisionTerms', category}}
        ]
    } = RejectContext.

-spec rejected_by_table_prohibitions(config()) -> test_return().
rejected_by_table_prohibitions(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"67890">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[], RejectContext} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision),

    #{
        rejected_routes := [
            {?prv(3), ?trm(11), {'RoutingRule', undefined}},
            {?prv(1), ?trm(1), {'PaymentsProvisionTerms', payment_tool}}
        ]
    } = RejectContext,
    ok.

-spec empty_candidate_ok(config()) -> test_return().
empty_candidate_ok(_C) ->
    BankCard = #domain_BankCard{
        token = <<"bank card token">>,
        payment_system = visa,
        bin = <<"411111">>,
        last_digits = <<"11">>
    },
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(101010, <<"RUB">>),
        payment_tool => {bank_card, BankCard},
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(2)}),
    {[], #{
        varset := VS,
        rejected_routes := [],
        rejected_providers := []
    }} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision).

-spec ruleset_misconfig(config()) -> test_return().
ruleset_misconfig(_C) ->
    VS = #{
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[], #{
        varset := VS,
        rejected_routes := [],
        rejected_providers := []
    }} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision).

-spec handle_uncomputable_provider_terms(config()) -> test_return().
handle_uncomputable_provider_terms(_C) ->
    VS0 = #{
        category => ?cat(1),
        currency => ?cur(<<"EUR">>),
        cost => ?cash(1000, <<"EUR">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        risk_score => low,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Providers0, RejectContext0} = hg_routing:gather_routes(
        payment,
        PaymentInstitution,
        VS0,
        Revision
    ),
    {[], #{
        rejected_providers := [
            {?prv(4), {'PaymentsProvisionTerms', currency}},
            {?prv(3), {'Misconfiguration', _}},
            {?prv(2), {'PaymentsProvisionTerms', currency}},
            {?prv(1), {'PaymentsProvisionTerms', currency}}
        ]
    }} = {Providers0, RejectContext0},

    VS1 = VS0#{
        currency => ?cur(<<"RUB">>),
        cost => ?cash(100, <<"RUB">>)
    },
    {Providers1, RejectContext1} = hg_routing:gather_routes(
        payment,
        PaymentInstitution,
        VS1,
        Revision
    ),
    {[], #{
        rejected_providers := [
            {?prv(4), {'PaymentsProvisionTerms', currency}},
            {?prv(2), {'PaymentsProvisionTerms', category}},
            {?prv(1), {'PaymentsProvisionTerms', payment_tool}}
        ],
        rejected_routes := [
            {?prv(3), ?trm(10), {'Misconfiguration', _}}
        ]
    }} = {Providers1, RejectContext1},
    ok.

-spec prefer_alive(config()) -> test_return().
prefer_alive(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },
    RiskScore = low,

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[
            {{?prv(200), _}, _},
            {{?prv(201), _}, _},
            {{?prv(202), _}, _}
        ] = Routes,
        RejectContext} = gather_routes(PaymentInstitution, VS, Revision),

    {ProviderRefs, TerminalData} = lists:unzip(Routes),

    Alive = {alive, 0.0},
    Dead = {dead, 1.0},
    Normal = {normal, 0.0},

    ProviderStatuses0 = [{Alive, Normal}, {Dead, Normal}, {Dead, Normal}],
    ProviderStatuses1 = [{Dead, Normal}, {Alive, Normal}, {Dead, Normal}],
    ProviderStatuses2 = [{Dead, Normal}, {Dead, Normal}, {Alive, Normal}],

    FailRatedRoutes0 = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses0),
    FailRatedRoutes1 = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses1),
    FailRatedRoutes2 = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses2),

    Result0 = hg_routing:choose_route(FailRatedRoutes0, RejectContext, RiskScore),
    Result1 = hg_routing:choose_route(FailRatedRoutes1, RejectContext, RiskScore),
    Result2 = hg_routing:choose_route(FailRatedRoutes2, RejectContext, RiskScore),

    {ok, #domain_PaymentRoute{provider = ?prv(200), terminal = ?trm(111)}, Meta0} = Result0,
    {ok, #domain_PaymentRoute{provider = ?prv(201), terminal = ?trm(111)}, Meta1} = Result1,
    {ok, #domain_PaymentRoute{provider = ?prv(202), terminal = ?trm(222)}, Meta2} = Result2,

    #{reject_reason := availability_condition, preferable_route := #{provider_ref := 202}} = Meta0,
    #{reject_reason := availability_condition, preferable_route := #{provider_ref := 202}} = Meta1,
    false = maps:is_key(reject_reason, Meta2),

    ok.

-spec prefer_normal_conversion(config()) -> test_return().
prefer_normal_conversion(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },
    RiskScore = low,

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[
            {{?prv(200), _}, _},
            {{?prv(201), _}, _},
            {{?prv(202), _}, _}
        ] = Routes,
        RC} = gather_routes(PaymentInstitution, VS, Revision),

    {Providers, TerminalData} = lists:unzip(Routes),

    Alive = {alive, 0.0},
    Normal = {normal, 0.0},
    Lacking = {lacking, 1.0},

    ProviderStatuses0 = [{Alive, Normal}, {Alive, Lacking}, {Alive, Lacking}],
    ProviderStatuses1 = [{Alive, Lacking}, {Alive, Normal}, {Alive, Lacking}],
    ProviderStatuses2 = [{Alive, Lacking}, {Alive, Lacking}, {Alive, Normal}],
    FailRatedRoutes0 = lists:zip3(Providers, TerminalData, ProviderStatuses0),
    FailRatedRoutes1 = lists:zip3(Providers, TerminalData, ProviderStatuses1),
    FailRatedRoutes2 = lists:zip3(Providers, TerminalData, ProviderStatuses2),

    Result0 = hg_routing:choose_route(FailRatedRoutes0, RC, RiskScore),
    Result1 = hg_routing:choose_route(FailRatedRoutes1, RC, RiskScore),
    Result2 = hg_routing:choose_route(FailRatedRoutes2, RC, RiskScore),

    {ok, #domain_PaymentRoute{provider = ?prv(200), terminal = ?trm(111)}, Meta0} = Result0,
    {ok, #domain_PaymentRoute{provider = ?prv(201), terminal = ?trm(111)}, Meta1} = Result1,
    {ok, #domain_PaymentRoute{provider = ?prv(202), terminal = ?trm(222)}, Meta2} = Result2,

    #{reject_reason := conversion_condition, preferable_route := #{provider_ref := 202}} = Meta0,
    #{reject_reason := conversion_condition, preferable_route := #{provider_ref := 202}} = Meta1,
    false = maps:is_key(reject_reason, Meta2),

    ok.

-spec prefer_higher_availability(config()) -> test_return().
prefer_higher_availability(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },
    RiskScore = low,

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[
            {{?prv(200), _}, _},
            {{?prv(201), _}, _},
            {{?prv(202), _}, _}
        ] = Routes,
        RC} = gather_routes(PaymentInstitution, VS, Revision),

    {ProviderRefs, TerminalData} = lists:unzip(Routes),

    ProviderStatuses = [{{alive, 0.5}, {normal, 0.5}}, {{dead, 0.8}, {lacking, 1.0}}, {{alive, 0.6}, {normal, 0.5}}],
    FailRatedRoutes = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, RiskScore),

    {ok, #domain_PaymentRoute{provider = ?prv(200), terminal = ?trm(111)}, #{
        reject_reason := availability,
        preferable_route := #{provider_ref := 202}
    }} = Result,

    ok.

-spec prefer_higher_conversion(config()) -> test_return().
prefer_higher_conversion(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[
            {{?prv(200), _}, _},
            {{?prv(201), _}, _},
            {{?prv(202), _}, _}
        ] = Routes,
        RC} = gather_routes(PaymentInstitution, VS, Revision),

    {Providers, TerminalData} = lists:unzip(Routes),

    ProviderStatuses = [{{dead, 0.8}, {lacking, 1.0}}, {{alive, 0.5}, {normal, 0.3}}, {{alive, 0.5}, {normal, 0.5}}],
    FailRatedRoutes = lists:zip3(Providers, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, undefined),
    {ok, #domain_PaymentRoute{provider = ?prv(201), terminal = ?trm(111)}, #{
        reject_reason := conversion,
        preferable_route := #{provider_ref := 202}
    }} = Result,
    ok.

-spec prefer_weight_over_availability(config()) -> test_return().
prefer_weight_over_availability(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"54321">>,
        flow => instant
    },
    RiskScore = low,

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[
            {{?prv(200), _}, _},
            {{?prv(201), _}, _},
            {{?prv(202), _}, _}
        ] = Routes,
        RC} = gather_routes(PaymentInstitution, VS, Revision),

    {Providers, TerminalData} = lists:unzip(Routes),

    ProviderStatuses = [{{alive, 0.3}, {normal, 0.3}}, {{alive, 0.5}, {normal, 0.3}}, {{alive, 0.3}, {normal, 0.3}}],
    FailRatedRoutes = lists:zip3(Providers, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, RiskScore),

    {ok, #domain_PaymentRoute{provider = ?prv(201), terminal = ?trm(111)}, _Meta} = Result,

    ok.

-spec prefer_weight_over_conversion(config()) -> test_return().
prefer_weight_over_conversion(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"54321">>,
        flow => instant
    },
    RiskScore = low,

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[
            {{?prv(200), _}, _},
            {{?prv(201), _}, _},
            {{?prv(202), _}, _}
        ] = Routes,
        RC} = gather_routes(PaymentInstitution, VS, Revision),

    {Providers, TerminalData} = lists:unzip(Routes),

    ProviderStatuses = [{{alive, 0.3}, {normal, 0.5}}, {{alive, 0.3}, {normal, 0.3}}, {{alive, 0.3}, {normal, 0.3}}],
    FailRatedRoutes = lists:zip3(Providers, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, RiskScore),

    {ok, #domain_PaymentRoute{provider = ?prv(201), terminal = ?trm(111)}, _Meta} = Result,

    ok.

-spec terminal_priority_for_shop(config()) -> test_return().
terminal_priority_for_shop(C) ->
    {ok,
        #domain_PaymentRoute{
            provider = ?prv(300),
            terminal = ?trm(111)
        },
        _Meta0} = terminal_priority_for_shop(?dummy_party_id, ?dummy_shop_id, C),
    {ok,
        #domain_PaymentRoute{
            provider = ?prv(300),
            terminal = ?trm(222)
        },
        _Meta1} = terminal_priority_for_shop(?dummy_party_id, ?dummy_another_shop_id, C).

terminal_priority_for_shop(PartyID, ShopID, _C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => PartyID,
        shop_id => ShopID,
        flow => instant
    },
    RiskScore = low,
    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    {Routes, RejectContext} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),
    FailRatedRoutes = hg_routing:gather_fail_rates(Routes),
    hg_routing:choose_route(FailRatedRoutes, RejectContext, RiskScore).


