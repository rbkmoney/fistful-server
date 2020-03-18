-module(p2p_transfer_adjustment_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("shumpune_proto/include/shumpune_shumpune_thrift.hrl").

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

-export([adjustment_can_change_status_to_failed_test/1]).
-export([adjustment_can_change_failure_test/1]).
-export([adjustment_can_change_status_to_succeeded_test/1]).
-export([adjustment_can_not_change_status_to_pending_test/1]).
-export([adjustment_can_not_change_status_to_same/1]).
-export([adjustment_sequence_test/1]).
-export([adjustment_idempotency_test/1]).
-export([no_parallel_adjustments_test/1]).
-export([no_pending_p2p_transfer_adjustments_test/1]).
-export([unknown_p2p_transfer_test/1]).

-export([consume_eventsinks/1]).

%% Internal types

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

%% Macro helpers

-define(final_balance(Amount, Currency), {Amount, {{inclusive, Amount}, {inclusive, Amount}}, Currency}).

%% API

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->[
    {group, default},
    {group, eventsink}
].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            adjustment_can_change_status_to_failed_test,
            adjustment_can_change_failure_test,
            adjustment_can_change_status_to_succeeded_test,
            adjustment_can_not_change_status_to_pending_test,
            adjustment_can_not_change_status_to_same,
            adjustment_sequence_test,
            adjustment_idempotency_test,
            no_parallel_adjustments_test,
            no_pending_p2p_transfer_adjustments_test,
            unknown_p2p_transfer_test
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

-spec adjustment_can_change_status_to_failed_test(config()) -> test_return().
adjustment_can_change_status_to_failed_test(C) ->
    #{
        p2p_transfer_id := P2PTransferID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Failure = #{code => <<"test">>},
    AdjustmentID = process_adjustment(P2PTransferID, #{
        change => {change_status, {failed, Failure}},
        external_id => <<"true_unique_id">>
    }),
    ?assertMatch(succeeded, get_adjustment_status(P2PTransferID, AdjustmentID)),
    ExternalID = ff_adjustment:external_id(get_adjustment(P2PTransferID, AdjustmentID)),
    ?assertEqual(<<"true_unique_id">>, ExternalID),
    ?assertEqual({failed, Failure},  get_p2p_transfer_status(P2PTransferID)),
    assert_adjustment_same_revisions(P2PTransferID, AdjustmentID).

-spec adjustment_can_change_failure_test(config()) -> test_return().
adjustment_can_change_failure_test(C) ->
    #{
        p2p_transfer_id := P2PTransferID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Failure1 = #{code => <<"one">>},
    AdjustmentID1 = process_adjustment(P2PTransferID, #{
        change => {change_status, {failed, Failure1}}
    }),
    ?assertEqual({failed, Failure1},  get_p2p_transfer_status(P2PTransferID)),
    assert_adjustment_same_revisions(P2PTransferID, AdjustmentID1),
    Failure2 = #{code => <<"two">>},
    AdjustmentID2 = process_adjustment(P2PTransferID, #{
        change => {change_status, {failed, Failure2}}
    }),
    ?assertEqual({failed, Failure2},  get_p2p_transfer_status(P2PTransferID)),
    assert_adjustment_same_revisions(P2PTransferID, AdjustmentID2).

-spec adjustment_can_change_status_to_succeeded_test(config()) -> test_return().
adjustment_can_change_status_to_succeeded_test(C) ->
    Cash = {1001, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    P2PTransferID = generate_id(),
    ClientInfo = #{
        ip_address => <<"some ip_address">>,
        fingerprint => <<"some fingerprint">>
    },
    Params = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash,
        client_info => ClientInfo,
        external_id => P2PTransferID
    },
    ok = p2p_transfer_machine:create(Params, ff_entity_context:new()),
    ?assertMatch({failed, _}, await_final_p2p_transfer_status(P2PTransferID)),
    AdjustmentID = process_adjustment(P2PTransferID, #{
        change => {change_status, succeeded}
    }),
    ?assertMatch(succeeded, get_adjustment_status(P2PTransferID, AdjustmentID)),
    ?assertMatch(succeeded, get_p2p_transfer_status(P2PTransferID)),
    assert_adjustment_same_revisions(P2PTransferID, AdjustmentID).

-spec adjustment_can_not_change_status_to_pending_test(config()) -> test_return().
adjustment_can_not_change_status_to_pending_test(C) ->
    #{
        p2p_transfer_id := P2PTransferID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Result = p2p_transfer_machine:start_adjustment(P2PTransferID, #{
        id => generate_id(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {invalid_status_change, {unavailable_status, pending}}}, Result).

-spec adjustment_can_not_change_status_to_same(config()) -> test_return().
adjustment_can_not_change_status_to_same(C) ->
    #{
        p2p_transfer_id := P2PTransferID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Result = p2p_transfer_machine:start_adjustment(P2PTransferID, #{
        id => generate_id(),
        change => {change_status, succeeded}
    }),
    ?assertMatch({error, {invalid_status_change, {already_has_status, succeeded}}}, Result).

-spec adjustment_sequence_test(config()) -> test_return().
adjustment_sequence_test(C) ->
    #{
        p2p_transfer_id := P2PTransferID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    MakeFailed = fun() ->
        _ = process_adjustment(P2PTransferID, #{
            change => {change_status, {failed, #{code => <<"test">>}}}
        })
    end,
    MakeSucceeded = fun() ->
        _ = process_adjustment(P2PTransferID, #{
            change => {change_status, succeeded}
        })
    end,
    MakeFailed(),
    MakeSucceeded(),
    MakeFailed(),
    MakeSucceeded(),
    MakeFailed().

-spec adjustment_idempotency_test(config()) -> test_return().
adjustment_idempotency_test(C) ->
    #{
        p2p_transfer_id := P2PTransferID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    Params = #{
        id => generate_id(),
        change => {change_status, {failed, #{code => <<"test">>}}}
    },
    _ = process_adjustment(P2PTransferID, Params),
    _ = process_adjustment(P2PTransferID, Params),
    _ = process_adjustment(P2PTransferID, Params),
    _ = process_adjustment(P2PTransferID, Params),
    P2PTransfer = get_p2p_transfer(P2PTransferID),
    ?assertMatch([_], p2p_transfer:adjustments(P2PTransfer)).

-spec no_parallel_adjustments_test(config()) -> test_return().
no_parallel_adjustments_test(C) ->
    #{
        p2p_transfer_id := P2PTransferID
    } = prepare_standard_environment({100, <<"RUB">>}, C),
    P2PTransfer0 = get_p2p_transfer(P2PTransferID),
    AdjustmentID0 = generate_id(),
    Params0 = #{
        id => AdjustmentID0,
        change => {change_status, {failed, #{code => <<"test">>}}}
    },
    {ok, {_, Events0}} = p2p_transfer:start_adjustment(Params0, P2PTransfer0),
    P2PTransfer1 = lists:foldl(fun p2p_transfer:apply_event/2, P2PTransfer0, Events0),
    Params1 = #{
        id => generate_id(),
        change => {change_status, succeeded}
    },
    Result = p2p_transfer:start_adjustment(Params1, P2PTransfer1),
    ?assertMatch({error, {another_adjustment_in_progress, AdjustmentID0}}, Result).

-spec no_pending_p2p_transfer_adjustments_test(config()) -> test_return().
no_pending_p2p_transfer_adjustments_test(C) ->
    Cash = {100, <<"RUB">>},
    #{
        identity_id := IdentityID,
        sender := ResourceSender,
        receiver := ResourceReceiver
    } = prepare_standard_environment(Cash, C),
    P2PTransferID = generate_id(),
    ClientInfo = #{
        ip_address => <<"some ip_address">>,
        fingerprint => <<"some fingerprint">>
    },
    P2PTransferParams = #{
        id => P2PTransferID,
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => Cash,
        client_info => ClientInfo,
        external_id => P2PTransferID
    },
    {ok, Events0} = p2p_transfer:create(P2PTransferParams),
    P2PTransfer1 = lists:foldl(fun p2p_transfer:apply_event/2, undefined, Events0),
    Params1 = #{
        id => generate_id(),
        change => {change_status, succeeded}
    },
    Result = p2p_transfer:start_adjustment(Params1, P2PTransfer1),
    ?assertMatch({error, {invalid_p2p_transfer_status, pending}}, Result).

-spec unknown_p2p_transfer_test(config()) -> test_return().
unknown_p2p_transfer_test(_C) ->
    P2PTransferID = <<"unknown_p2p_transfer">>,
    Result = p2p_transfer_machine:start_adjustment(P2PTransferID, #{
        id => generate_id(),
        change => {change_status, pending}
    }),
    ?assertMatch({error, {unknown_p2p_transfer, P2PTransferID}}, Result).

-spec consume_eventsinks(config()) -> test_return().
consume_eventsinks(_) ->
    EventSinks = [
          p2p_transfer_event_sink,
          p2p_session_event_sink
    ],
    [_Events = ct_eventsink:consume(1000, Sink) || Sink <- EventSinks].

%% Utils
prepare_standard_environment(P2PTransferCash, C) ->
    PartyID = create_party(C),
    IdentityID = create_person_identity(PartyID, C, <<"quote-owner">>),
    ResourceSender = create_resource_raw(C),
    ResourceReceiver = create_resource_raw(C),
    ClientInfo = #{
        ip_address => <<"some ip_address">>,
        fingerprint => <<"some fingerprint">>
    },
    P2PTransferParams = #{
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        body => P2PTransferCash,
        client_info => ClientInfo,
        external_id => generate_id()
    },
    P2PTransferID = process_p2p_transfer(P2PTransferParams),
    #{
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        party_id => PartyID,
        p2p_transfer_id => P2PTransferID
    }.

process_p2p_transfer(P2PTransferParams) ->
    P2PTransferID = generate_id(),
    ok = p2p_transfer_machine:create(P2PTransferParams#{id => P2PTransferID}, ff_entity_context:new()),
    succeeded = await_final_p2p_transfer_status(P2PTransferID),
    P2PTransferID.

process_adjustment(P2PTransferID, AdjustmentParams0) ->
    AdjustmentParams1 = maps:merge(#{id => generate_id()}, AdjustmentParams0),
    #{id := AdjustmentID} = AdjustmentParams1,
    ok = p2p_transfer_machine:start_adjustment(P2PTransferID, AdjustmentParams1),
    succeeded = await_final_adjustment_status(P2PTransferID, AdjustmentID),
    AdjustmentID.

get_p2p_transfer(P2PTransferID) ->
    {ok, Machine} = p2p_transfer_machine:get(P2PTransferID),
    p2p_transfer_machine:p2p_transfer(Machine).

get_p2p_transfer_status(P2PTransferID) ->
    p2p_transfer:status(get_p2p_transfer(P2PTransferID)).

get_adjustment_status(P2PTransferID, AdjustmentID) ->
    ff_adjustment:status(get_adjustment(P2PTransferID, AdjustmentID)).

get_adjustment(P2PTransferID, AdjustmentID) ->
    P2PTransfer = get_p2p_transfer(P2PTransferID),
    {ok, Adjustment} = p2p_transfer:find_adjustment(AdjustmentID, P2PTransfer),
    Adjustment.

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
    get_p2p_transfer_status(P2PTransferID).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

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

create_resource_raw(C) ->
    StoreSource = ct_cardstore:bank_card(<<"4150399999000900">>, C),
    p2p_participant:create(raw, {bank_card, #{
        bank_card => StoreSource,
        auth_data => {session, #{
            session_id => <<"ID">>
        }}
    }}).

await_final_adjustment_status(P2PTransferID, AdjustmentID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            {ok, Machine} = p2p_transfer_machine:get(P2PTransferID),
            P2PTransfer = p2p_transfer_machine:p2p_transfer(Machine),
            {ok, Adjustment} = p2p_transfer:find_adjustment(AdjustmentID, P2PTransfer),
            case ff_adjustment:is_finished(Adjustment) of
                false ->
                    {not_finished, P2PTransfer};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    get_adjustment_status(P2PTransferID, AdjustmentID).

assert_adjustment_same_revisions(P2PTransferID, AdjustmentID) ->
    Adjustment = get_adjustment(P2PTransferID, AdjustmentID),
    P2PTransfer = get_p2p_transfer(P2PTransferID),
    ?assertEqual(p2p_transfer:domain_revision(P2PTransfer), ff_adjustment:domain_revision(Adjustment)),
    ?assertEqual(p2p_transfer:party_revision(P2PTransfer), ff_adjustment:party_revision(Adjustment)),
    ?assertEqual(p2p_transfer:created_at(P2PTransfer), ff_adjustment:operation_timestamp(Adjustment)),
    ok.