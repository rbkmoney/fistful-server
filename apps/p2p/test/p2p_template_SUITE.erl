-module(p2p_template_SUITE).

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
-export([create_not_allow_test/1]).
-export([create_ok_test/1]).
-export([preserve_revisions_test/1]).
-export([unknown_test/1]).

-export([consume_eventsinks/1]).

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
        {group, eventsink}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            create_not_allow_test,
            create_ok_test,
            preserve_revisions_test,
            unknown_test
        ]},
        {eventsink, [], [
            consume_eventsinks
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

-spec create_not_allow_test(config()) -> test_return().
create_not_allow_test(C) ->
    #{
        identity_id := IdentityID
    } = prepare_standard_environment(C),
    P2PTemplateID = generate_id(),
    Details = #{},
    P2PTemplateParams = #{
        id => P2PTemplateID,
        identity_id => IdentityID,
        details => Details,
        external_id => P2PTemplateID
    },
    ok = p2p_template_machine:create(P2PTemplateParams, ff_entity_context:new()),
    P2PTemplate = get_p2p_template(P2PTemplateID),
    ?assertEqual(IdentityID, p2p_template:identity_id(P2PTemplate)),
    ?assertEqual(Details, p2p_template:details(P2PTemplate)),
    ?assertEqual(P2PTemplateID, p2p_template:external_id(P2PTemplate)).

-spec create_ok_test(config()) -> test_return().
create_ok_test(C) ->
    #{
        identity_id := IdentityID
    } = prepare_standard_environment(C),
    P2PTemplateID = generate_id(),
    Details = #{},
    P2PTemplateParams = #{
        id => P2PTemplateID,
        identity_id => IdentityID,
        details => Details,
        external_id => P2PTemplateID
    },
    ok = p2p_template_machine:create(P2PTemplateParams, ff_entity_context:new()),
    P2PTemplate = get_p2p_template(P2PTemplateID),
    ?assertEqual(IdentityID, p2p_template:identity_id(P2PTemplate)),
    ?assertEqual(Details, p2p_template:details(P2PTemplate)),
    ?assertEqual(P2PTemplateID, p2p_template:external_id(P2PTemplate)).

-spec preserve_revisions_test(config()) -> test_return().
preserve_revisions_test(C) ->
    #{
        identity_id := IdentityID
    } = prepare_standard_environment(C),
    P2PTemplateID = generate_id(),
    P2PTemplateParams = #{
        id => P2PTemplateID,
        identity_id => IdentityID,
        details => #{}
    },
    ok = p2p_template_machine:create(P2PTemplateParams, ff_entity_context:new()),
    P2PTemplate = get_p2p_template(P2PTemplateID),
    ?assertNotEqual(undefined, p2p_template:domain_revision(P2PTemplate)),
    ?assertNotEqual(undefined, p2p_template:party_revision(P2PTemplate)),
    ?assertNotEqual(undefined, p2p_template:created_at(P2PTemplate)).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    P2PTemplateID = <<"unknown_p2p_template">>,
    Result = p2p_template_machine:get(P2PTemplateID),
    ?assertMatch({error, {unknown_p2p_template, P2PTemplateID}}, Result).

-spec consume_eventsinks(config()) -> test_return().
consume_eventsinks(_) ->
    EventSinks = [
          p2p_template_event_sink
    ],
    [_Events = ct_eventsink:consume(1000, Sink) || Sink <- EventSinks].

%% Utils

prepare_standard_environment(C) ->
    PartyID = create_party(C),
    IdentityID = create_person_identity(PartyID, C),
    #{
        identity_id => IdentityID,
        party_id => PartyID
    }.

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C) ->
    create_person_identity(Party, C, <<"good-one">>).

create_person_identity(Party, C, ProviderID) ->
    create_identity(Party, ProviderID, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, party => Party, provider => ProviderID, class => ClassID},
        ff_entity_context:new()
    ),
    ID.

get_p2p_template(P2PTemplateID) ->
    {ok, Machine} = p2p_template_machine:get(P2PTemplateID),
    p2p_template_machine:p2p_template(Machine).

generate_id() ->
    ff_id:generate_snowflake_id().
