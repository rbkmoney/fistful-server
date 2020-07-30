-module(ff_p2p_template_handler_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_p2p_template_thrift.hrl").
-include_lib("shumpune_proto/include/shumpune_shumpune_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([block_p2p_template_ok_test/1]).
-export([get_context_test/1]).
-export([create_p2p_template_ok_test/1]).
-export([unknown_test/1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {default, [parallel], [
            block_p2p_template_ok_test,
            get_context_test,
            create_p2p_template_ok_test,
            unknown_test
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

-spec block_p2p_template_ok_test(config()) -> test_return().
block_p2p_template_ok_test(C) ->
    #{
        identity_id := IdentityID
    } = prepare_standard_environment(C),
    P2PTemplateID = generate_id(),
    ExternalID = generate_id(),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Details = make_template_details({1000, <<"RUB">>}),
    Params = #p2p_template_P2PTemplateParams{
        id = P2PTemplateID,
        identity_id = IdentityID,
        external_id = ExternalID,
        template_details = Details
    },
    {ok, _P2PTemplateState} = call_p2p_template('Create', [Params, Ctx]),
    Expected0 = get_p2p_template(P2PTemplateID),
    ?assertEqual(unblocked, p2p_template:blocking(Expected0)),
    {ok, ok} = call_p2p_template('SetBlocking', [P2PTemplateID, blocked]),
    Expected1 = get_p2p_template(P2PTemplateID),
    ?assertEqual(blocked, p2p_template:blocking(Expected1)).

-spec get_context_test(config()) -> test_return().
get_context_test(C) ->
    #{
        identity_id := IdentityID
    } = prepare_standard_environment(C),
    P2PTemplateID = generate_id(),
    ExternalID = generate_id(),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Details = make_template_details({1000, <<"RUB">>}),
    Params = #p2p_template_P2PTemplateParams{
        id = P2PTemplateID,
        identity_id = IdentityID,
        external_id = ExternalID,
        template_details = Details
    },
    {ok, _P2PTemplateState} = call_p2p_template('Create', [Params, Ctx]),
    {ok, EncodedContext} = call_p2p_template('GetContext', [P2PTemplateID]),
    ?assertEqual(Ctx, ff_entity_context_codec:unmarshal(EncodedContext)).

-spec create_p2p_template_ok_test(config()) -> test_return().
create_p2p_template_ok_test(C) ->
    #{
        identity_id := IdentityID
    } = prepare_standard_environment(C),
    P2PTemplateID = generate_id(),
    ExternalID = generate_id(),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Details = make_template_details({1000, <<"RUB">>}),
    Params = #p2p_template_P2PTemplateParams{
        id = P2PTemplateID,
        identity_id = IdentityID,
        external_id = ExternalID,
        template_details = Details
    },
    {ok, P2PTemplateState} = call_p2p_template('Create', [Params, Ctx]),

    Expected = get_p2p_template(P2PTemplateID),
    ?assertEqual(P2PTemplateID, P2PTemplateState#p2p_template_P2PTemplateState.id),
    ?assertEqual(ExternalID, P2PTemplateState#p2p_template_P2PTemplateState.external_id),
    ?assertEqual(IdentityID, P2PTemplateState#p2p_template_P2PTemplateState.identity_id),
    ?assertEqual(Details, P2PTemplateState#p2p_template_P2PTemplateState.template_details),
    ?assertEqual(
        p2p_template:domain_revision(Expected),
        P2PTemplateState#p2p_template_P2PTemplateState.domain_revision
    ),
    ?assertEqual(
        p2p_template:party_revision(Expected),
        P2PTemplateState#p2p_template_P2PTemplateState.party_revision
    ),
    ?assertEqual(
        p2p_template:created_at(Expected),
        ff_codec:unmarshal(timestamp_ms, P2PTemplateState#p2p_template_P2PTemplateState.created_at)
    ),

    {ok, FinalP2PTemplateState} = call_p2p_template('Get', [P2PTemplateID, #'EventRange'{}]),
    ?assertMatch(
        unblocked,
        FinalP2PTemplateState#p2p_template_P2PTemplateState.blocking
    ).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    P2PTemplateID = <<"unknown_p2p_template">>,
    Result = call_p2p_template('Get', [P2PTemplateID, #'EventRange'{}]),
    ExpectedError = #fistful_P2PTemplateNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

%%  Internals

make_template_details({Amount, Currency}) ->
    make_template_details({Amount, Currency}, #{<<"test key">> => <<"test value">>}).

make_template_details({Amount, Currency}, Metadata) ->
    #p2p_template_P2PTemplateDetails{
        body = ff_p2p_template_codec:marshal(template_body, #{value => #{currency => Currency, body => Amount}}),
        metadata = #p2p_template_P2PTemplateMetadata{
            value = ff_p2p_template_codec:marshal(ctx, Metadata)
        }
    }.

call_p2p_template(Fun, Args) ->
    ServiceName = p2p_template_management,
    Service = ff_services:get_service(ServiceName),
    Request = {Service, Fun, Args},
    Client  = ff_woody_client:new(#{
        url => "http://localhost:8022" ++ ff_services:get_service_path(ServiceName)
    }),
    ff_woody_client:call(Client, Request).

prepare_standard_environment(C) ->
    Party = create_party(C),
    IdentityID = create_person_identity(Party, C),
    #{
        identity_id => IdentityID,
        party_id => Party
    }.

get_p2p_template(P2PTemplateID) ->
    {ok, Machine} = p2p_template_machine:get(P2PTemplateID),
    p2p_template_machine:p2p_template(Machine).

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

generate_id() ->
    ff_id:generate_snowflake_id().
