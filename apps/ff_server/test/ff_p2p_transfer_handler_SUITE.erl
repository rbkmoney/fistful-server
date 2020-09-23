-module(ff_p2p_transfer_handler_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_p2p_session_thrift.hrl").
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
-export([get_p2p_session_context_ok_test/1]).
-export([get_p2p_session_ok_test/1]).
-export([create_adjustment_ok_test/1]).
-export([get_p2p_transfer_events_ok_test/1]).
-export([get_p2p_transfer_context_ok_test/1]).
-export([get_p2p_transfer_ok_test/1]).
-export([create_p2p_transfer_ok_test/1]).
-export([unknown_session_test/1]).
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
            get_p2p_session_context_ok_test,
            get_p2p_session_ok_test,
            create_adjustment_ok_test,
            get_p2p_transfer_events_ok_test,
            get_p2p_transfer_context_ok_test,
            get_p2p_transfer_ok_test,
            create_p2p_transfer_ok_test,
            unknown_session_test,
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

-spec get_p2p_session_context_ok_test(config()) -> test_return().
get_p2p_session_context_ok_test(C) ->
    #{
        session_id := ID
    } = prepare_standard_environment(C),
    {ok, _Context} = call_p2p_session('GetContext', [ID]).

-spec get_p2p_session_ok_test(config()) -> test_return().
get_p2p_session_ok_test(C) ->
    #{
        session_id := ID
    } = prepare_standard_environment(C),
    {ok, P2PSessionState} = call_p2p_session('Get', [ID, #'EventRange'{}]),
    ?assertEqual(ID, P2PSessionState#p2p_session_SessionState.id).

-spec create_adjustment_ok_test(config()) -> test_return().
create_adjustment_ok_test(C) ->
    #{
        p2p_transfer_id := ID
    } = prepare_standard_environment(C),
    AdjustmentID = generate_id(),
    ExternalID = generate_id(),
    Params = #p2p_adj_AdjustmentParams{
        id = AdjustmentID,
        change = {change_status, #p2p_adj_ChangeStatusRequest{
            new_status = {failed, #p2p_status_Failed{failure = #'Failure'{code = <<"Ooops">>}}}
        }},
        external_id = ExternalID
    },
    {ok, AdjustmentState} = call_p2p('CreateAdjustment', [ID, Params]),
    ExpectedAdjustment = get_adjustment(ID, AdjustmentID),

    ?assertEqual(AdjustmentID, AdjustmentState#p2p_adj_AdjustmentState.id),
    ?assertEqual(ExternalID, AdjustmentState#p2p_adj_AdjustmentState.external_id),
    ?assertEqual(
        ff_adjustment:created_at(ExpectedAdjustment),
        ff_codec:unmarshal(timestamp_ms, AdjustmentState#p2p_adj_AdjustmentState.created_at)
    ),
    ?assertEqual(
        ff_adjustment:domain_revision(ExpectedAdjustment),
        AdjustmentState#p2p_adj_AdjustmentState.domain_revision
    ),
    ?assertEqual(
        ff_adjustment:party_revision(ExpectedAdjustment),
        AdjustmentState#p2p_adj_AdjustmentState.party_revision
    ),
    ?assertEqual(
        ff_p2p_transfer_adjustment_codec:marshal(changes_plan, ff_adjustment:changes_plan(ExpectedAdjustment)),
        AdjustmentState#p2p_adj_AdjustmentState.changes_plan
    ).

-spec get_p2p_transfer_events_ok_test(config()) -> test_return().
get_p2p_transfer_events_ok_test(C) ->
    #{
        p2p_transfer_id := ID
    } = prepare_standard_environment(C),
    {ok, [#p2p_transfer_Event{change = {created, _}} | _Rest]} = call_p2p('GetEvents', [ID, #'EventRange'{}]).

-spec get_p2p_transfer_context_ok_test(config()) -> test_return().
get_p2p_transfer_context_ok_test(C) ->
    #{
        p2p_transfer_id := ID,
        context := Ctx
    } = prepare_standard_environment(C),
    {ok, Context} = call_p2p('GetContext', [ID]),
    ?assertEqual(Ctx, Context).

-spec get_p2p_transfer_ok_test(config()) -> test_return().
get_p2p_transfer_ok_test(C) ->
    #{
        p2p_transfer_id := ID
    } = prepare_standard_environment(C),
    {ok, P2PTransferState} = call_p2p('Get', [ID, #'EventRange'{}]),
    ?assertEqual(ID, P2PTransferState#p2p_transfer_P2PTransferState.id).

-spec create_p2p_transfer_ok_test(config()) -> test_return().
create_p2p_transfer_ok_test(C) ->
    #{
        identity_id := IdentityID
    } = prepare_standard_environment(C),
    P2PTransferID = generate_id(),
    ExternalID = generate_id(),
    Metadata = ff_p2p_transfer_codec:marshal(ctx,#{<<"hello">> => <<"world">>}),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Params = #p2p_transfer_P2PTransferParams{
        id = P2PTransferID,
        identity_id = IdentityID,
        sender = create_resource_raw(C),
        receiver = create_resource_raw(C),
        body = make_cash({100, <<"RUB">>}),
        client_info = #'ClientInfo'{ip_address = <<"some ip_address">>, fingerprint = <<"some fingerprint">>},
        external_id = ExternalID,
        metadata = Metadata
    },
    {ok, P2PTransferState} = call_p2p('Create', [Params, Ctx]),

    Expected = get_p2p_transfer(P2PTransferID),
    ?assertEqual(P2PTransferID, P2PTransferState#p2p_transfer_P2PTransferState.id),
    ?assertEqual(ExternalID, P2PTransferState#p2p_transfer_P2PTransferState.external_id),
    ?assertEqual(IdentityID, P2PTransferState#p2p_transfer_P2PTransferState.owner),
    % TODO: ?assertEqual(Metadata, P2PTransferState#p2p_transfer_P2PTransferState.metadata),
    ?assertEqual(
        p2p_transfer:domain_revision(Expected),
        P2PTransferState#p2p_transfer_P2PTransferState.domain_revision
    ),
    ?assertEqual(
        p2p_transfer:party_revision(Expected),
        P2PTransferState#p2p_transfer_P2PTransferState.party_revision
    ),
    ?assertEqual(
        p2p_transfer:created_at(Expected),
        ff_codec:unmarshal(timestamp_ms, P2PTransferState#p2p_transfer_P2PTransferState.created_at)
    ).

-spec unknown_session_test(config()) -> test_return().
unknown_session_test(_C) ->
    P2PSessionID = <<"unknown_p2p_session">>,
    Result = call_p2p_session('Get', [P2PSessionID, #'EventRange'{}]),
    ExpectedError = #fistful_P2PSessionNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    ID = <<"unknown_id">>,
    Result = call_p2p('Get', [ID, #'EventRange'{}]),
    ExpectedError = #fistful_P2PNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

%%  Internals

await_final_p2p_transfer_status(P2PTransferID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            {ok, Machine} = p2p_transfer_machine:get(P2PTransferID),
            P2PTransfer = p2p_transfer_machine:p2p_transfer(Machine),
            case p2p_transfer:is_finished(P2PTransfer) of
                false ->
                    {not_finished, P2PTransfer};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(10, 1000)
    ),
    get_p2p_status(P2PTransferID).

get_p2p(ID) ->
    {ok, Machine} = p2p_transfer_machine:get(ID),
    p2p_transfer_machine:p2p_transfer(Machine).

get_p2p_status(ID) ->
    p2p_transfer:status(get_p2p(ID)).

get_adjustment(ID, AdjustmentID) ->
    {ok, Adjustment} = p2p_transfer:find_adjustment(AdjustmentID, get_p2p(ID)),
    Adjustment.

make_cash({Amount, Currency}) ->
    #'Cash'{
        amount = Amount,
        currency = #'CurrencyRef'{symbolic_code = Currency}
    }.

create_resource_raw(C) ->
    StoreSource = ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    Resource = {bank_card, #{
        bank_card => StoreSource,
        auth_data => {session, #{
            session_id => <<"ID">>
        }}
    }},
    ff_p2p_transfer_codec:marshal(participant, p2p_participant:create(raw, Resource, #{})).

call_p2p_session(Fun, Args) ->
    ServiceName = p2p_session_management,
    Service = ff_services:get_service(ServiceName),
    Request = {Service, Fun, Args},
    Client  = ff_woody_client:new(#{
        url => "http://localhost:8022" ++ ff_services:get_service_path(ServiceName)
    }),
    ff_woody_client:call(Client, Request).

call_p2p(Fun, Args) ->
    ServiceName = p2p_transfer_management,
    Service = ff_services:get_service(ServiceName),
    Request = {Service, Fun, Args},
    Client  = ff_woody_client:new(#{
        url => "http://localhost:8022" ++ ff_services:get_service_path(ServiceName)
    }),
    ff_woody_client:call(Client, Request).


prepare_standard_environment(C) ->
    Party = create_party(C),
    IdentityID = create_person_identity(Party, C),
    P2PTransferID = generate_id(),
    ExternalID = generate_id(),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Params = #p2p_transfer_P2PTransferParams{
        id = P2PTransferID,
        identity_id = IdentityID,
        sender = create_resource_raw(C),
        receiver = create_resource_raw(C),
        body = make_cash({100, <<"RUB">>}),
        client_info = #'ClientInfo'{ip_address = <<"some ip_address">>, fingerprint = <<"some fingerprint">>},
        external_id = ExternalID
    },
    {ok, _State} = call_p2p('Create', [Params, Ctx]),
    succeeded = await_final_p2p_transfer_status(P2PTransferID),
    {ok, P2PTransferState} = call_p2p('Get', [P2PTransferID, #'EventRange'{}]),
    [#p2p_transfer_SessionState{id = SessionID} | _Rest] = P2PTransferState#p2p_transfer_P2PTransferState.sessions,
    #{
        identity_id => IdentityID,
        party_id => Party,
        p2p_transfer_id => P2PTransferID,
        session_id => SessionID,
        context => Ctx
    }.

get_p2p_transfer(P2PTransferID) ->
    {ok, Machine} = p2p_transfer_machine:get(P2PTransferID),
    p2p_transfer_machine:p2p_transfer(Machine).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C) ->
    create_person_identity(Party, C, <<"quote-owner">>).

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

generate_id() ->
    ff_id:generate_snowflake_id().
