-module(ff_deposit_handler_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_deposit_thrift.hrl").
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

-export([create_bad_amount_test/1]).
-export([create_currency_validation_error_test/1]).
-export([create_source_notfound_test/1]).
-export([create_wallet_notfound_test/1]).
-export([create_ok_test/1]).
-export([unknown_test/1]).
-export([get_context_test/1]).
-export([get_events_test/1]).
-export([create_adjustment_ok_test/1]).
-export([create_adjustment_unavailable_status_error_test/1]).
-export([create_adjustment_already_has_status_error_test/1]).
-export([create_revert_ok_test/1]).
-export([create_revert_inconsistent_revert_currency_error_test/1]).
-export([create_revert_insufficient_deposit_amount_error_test/1]).
-export([create_revert_invalid_revert_amount_error_test/1]).
-export([create_revert_unknown_deposit_error_test/1]).
-export([create_revert_adjustment_ok_test/1]).
-export([create_revert_adjustment_unavailable_status_error_test/1]).
-export([create_revert_adjustment_already_has_status_error_test/1]).
-export([deposit_state_content_test/1]).

%% Internal types

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

%% Macro helpers

-define(final_balance(Amount, Currency), {Amount, {{inclusive, Amount}, {inclusive, Amount}}, Currency}).

%% API

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [parallel], [
            create_bad_amount_test,
            create_currency_validation_error_test,
            create_source_notfound_test,
            create_wallet_notfound_test,
            create_ok_test,
            unknown_test,
            get_context_test,
            get_events_test,
            create_adjustment_ok_test,
            create_adjustment_unavailable_status_error_test,
            create_adjustment_already_has_status_error_test,
            create_revert_ok_test,
            create_revert_inconsistent_revert_currency_error_test,
            create_revert_insufficient_deposit_amount_error_test,
            create_revert_invalid_revert_amount_error_test,
            create_revert_unknown_deposit_error_test,
            create_revert_adjustment_ok_test,
            create_revert_adjustment_unavailable_status_error_test,
            create_revert_adjustment_already_has_status_error_test,
            deposit_state_content_test
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

-spec create_bad_amount_test(config()) -> test_return().
create_bad_amount_test(C) ->
    Body = make_cash({0, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment(Body, C),
    Params = #deposit_DepositParams{
        id = generate_id(),
        body = Body,
        source_id = SourceID,
        wallet_id = WalletID
    },
    Result = call_deposit('Create', [Params, #{}]),
    ExpectedError = #fistful_InvalidOperationAmount{
        amount = Body
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_currency_validation_error_test(config()) -> test_return().
create_currency_validation_error_test(C) ->
    Body = make_cash({100, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment(Body, C),
    Params = #deposit_DepositParams{
        id = generate_id(),
        body = make_cash({5000, <<"EUR">>}),
        source_id = SourceID,
        wallet_id = WalletID
    },
    Result = call_deposit('Create', [Params, #{}]),
    ExpectedError = #fistful_ForbiddenOperationCurrency{
        currency = #'CurrencyRef'{symbolic_code = <<"EUR">>},
        allowed_currencies = [
            #'CurrencyRef'{symbolic_code = <<"RUB">>},
            #'CurrencyRef'{symbolic_code = <<"USD">>}
        ]
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_source_notfound_test(config()) -> test_return().
create_source_notfound_test(C) ->
    Body = make_cash({100, <<"RUB">>}),
    #{
        wallet_id := WalletID
    } = prepare_standard_environment(Body, C),
    Params = #deposit_DepositParams{
        id = generate_id(),
        body = Body,
        source_id = <<"unknown_source">>,
        wallet_id = WalletID
    },
    Result = call_deposit('Create', [Params, #{}]),
    ExpectedError = #fistful_SourceNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_wallet_notfound_test(config()) -> test_return().
create_wallet_notfound_test(C) ->
    Body = make_cash({100, <<"RUB">>}),
    #{
        source_id := SourceID
    } = prepare_standard_environment(Body, C),
    Params = #deposit_DepositParams{
        id = generate_id(),
        body = Body,
        source_id = SourceID,
        wallet_id = <<"unknown_wallet">>
    },
    Result = call_deposit('Create', [Params, #{}]),
    ExpectedError = #fistful_WalletNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_ok_test(config()) -> test_return().
create_ok_test(C) ->
    Body = make_cash({100, <<"RUB">>}),
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = prepare_standard_environment(Body, C),
    DepositID = generate_id(),
    ExternalID = generate_id(),
    Context = #{<<"NS">> => #{generate_id() => generate_id()}},
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Params = #deposit_DepositParams{
        id = DepositID,
        body = Body,
        source_id = SourceID,
        wallet_id = WalletID,
        metadata = Metadata,
        external_id = ExternalID
    },
    {ok, DepositState} = call_deposit('Create', [Params, ff_entity_context_codec:marshal(Context)]),
    Expected = get_deposit(DepositID),
    ?assertEqual(DepositID, DepositState#deposit_DepositState.id),
    ?assertEqual(WalletID, DepositState#deposit_DepositState.wallet_id),
    ?assertEqual(SourceID, DepositState#deposit_DepositState.source_id),
    ?assertEqual(ExternalID, DepositState#deposit_DepositState.external_id),
    ?assertEqual(Body, DepositState#deposit_DepositState.body),
    ?assertEqual(Metadata, DepositState#deposit_DepositState.metadata),
    ?assertEqual(
        ff_deposit:domain_revision(Expected),
        DepositState#deposit_DepositState.domain_revision
    ),
    ?assertEqual(
        ff_deposit:party_revision(Expected),
        DepositState#deposit_DepositState.party_revision
    ),
    ?assertEqual(
        ff_deposit:created_at(Expected),
        ff_codec:unmarshal(timestamp_ms, DepositState#deposit_DepositState.created_at)
    ).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    DepositID = <<"unknown_deposit">>,
    Result = call_deposit('Get', [DepositID, #'EventRange'{}]),
    ExpectedError = #fistful_DepositNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

-spec get_context_test(config()) -> test_return().
get_context_test(C) ->
    #{
        deposit_id := DepositID,
        context := Context
    } = prepare_standard_environment_with_deposit(C),
    {ok, EncodedContext} = call_deposit('GetContext', [DepositID]),
    ?assertEqual(Context, ff_entity_context_codec:unmarshal(EncodedContext)).

-spec get_events_test(config()) -> test_return().
get_events_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment_with_deposit(C),
    Range = {undefined, undefined},
    EncodedRange = ff_codec:marshal(event_range, Range),
    {ok, Events} = call_deposit('GetEvents', [DepositID, EncodedRange]),
    {ok, ExpectedEvents} = ff_deposit_machine:events(DepositID, Range),
    EncodedEvents = [ff_deposit_codec:marshal(event, E) || E <- ExpectedEvents],
    ?assertEqual(EncodedEvents, Events).

-spec create_adjustment_ok_test(config()) -> test_return().
create_adjustment_ok_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment_with_deposit(C),
    AdjustmentID = generate_id(),
    ExternalID = generate_id(),
    Params = #dep_adj_AdjustmentParams{
        id = AdjustmentID,
        change =
            {change_status, #dep_adj_ChangeStatusRequest{
                new_status = {failed, #dep_status_Failed{failure = #'Failure'{code = <<"Ooops">>}}}
            }},
        external_id = ExternalID
    },
    {ok, AdjustmentState} = call_deposit('CreateAdjustment', [DepositID, Params]),
    ExpectedAdjustment = get_adjustment(DepositID, AdjustmentID),

    ?assertEqual(AdjustmentID, AdjustmentState#dep_adj_AdjustmentState.id),
    ?assertEqual(ExternalID, AdjustmentState#dep_adj_AdjustmentState.external_id),
    ?assertEqual(
        ff_adjustment:created_at(ExpectedAdjustment),
        ff_codec:unmarshal(timestamp_ms, AdjustmentState#dep_adj_AdjustmentState.created_at)
    ),
    ?assertEqual(
        ff_adjustment:domain_revision(ExpectedAdjustment),
        AdjustmentState#dep_adj_AdjustmentState.domain_revision
    ),
    ?assertEqual(
        ff_adjustment:party_revision(ExpectedAdjustment),
        AdjustmentState#dep_adj_AdjustmentState.party_revision
    ),
    ?assertEqual(
        ff_deposit_adjustment_codec:marshal(changes_plan, ff_adjustment:changes_plan(ExpectedAdjustment)),
        AdjustmentState#dep_adj_AdjustmentState.changes_plan
    ).

-spec create_adjustment_unavailable_status_error_test(config()) -> test_return().
create_adjustment_unavailable_status_error_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment_with_deposit(C),
    Params = #dep_adj_AdjustmentParams{
        id = generate_id(),
        change =
            {change_status, #dep_adj_ChangeStatusRequest{
                new_status = {pending, #dep_status_Pending{}}
            }}
    },
    Result = call_deposit('CreateAdjustment', [DepositID, Params]),
    ExpectedError = #deposit_ForbiddenStatusChange{
        target_status = {pending, #dep_status_Pending{}}
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_adjustment_already_has_status_error_test(config()) -> test_return().
create_adjustment_already_has_status_error_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment_with_deposit(C),
    Params = #dep_adj_AdjustmentParams{
        id = generate_id(),
        change =
            {change_status, #dep_adj_ChangeStatusRequest{
                new_status = {succeeded, #dep_status_Succeeded{}}
            }}
    },
    Result = call_deposit('CreateAdjustment', [DepositID, Params]),
    ExpectedError = #deposit_AlreadyHasStatus{
        deposit_status = {succeeded, #dep_status_Succeeded{}}
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_revert_ok_test(config()) -> test_return().
create_revert_ok_test(C) ->
    #{
        deposit_id := DepositID,
        body := Body
    } = prepare_standard_environment_with_deposit(C),
    RevertID = generate_id(),
    ExternalID = generate_id(),
    Reason = generate_id(),
    Params = #deposit_revert_RevertParams{
        id = RevertID,
        body = Body,
        external_id = ExternalID,
        reason = Reason
    },
    {ok, RevertState} = call_deposit('CreateRevert', [DepositID, Params]),
    Expected = get_revert(DepositID, RevertID),

    ?assertEqual(RevertID, RevertState#deposit_revert_RevertState.id),
    ?assertEqual(ExternalID, RevertState#deposit_revert_RevertState.external_id),
    ?assertEqual(Body, RevertState#deposit_revert_RevertState.body),
    ?assertEqual(Reason, RevertState#deposit_revert_RevertState.reason),
    ?assertEqual(
        ff_deposit_revert:created_at(Expected),
        ff_codec:unmarshal(timestamp_ms, RevertState#deposit_revert_RevertState.created_at)
    ),
    ?assertEqual(
        ff_deposit_revert:domain_revision(Expected),
        RevertState#deposit_revert_RevertState.domain_revision
    ),
    ?assertEqual(
        ff_deposit_revert:party_revision(Expected),
        RevertState#deposit_revert_RevertState.party_revision
    ).

-spec create_revert_inconsistent_revert_currency_error_test(config()) -> test_return().
create_revert_inconsistent_revert_currency_error_test(C) ->
    #{
        deposit_id := DepositID
    } = prepare_standard_environment_with_deposit(make_cash({1, <<"RUB">>}), C),
    Params = #deposit_revert_RevertParams{
        id = generate_id(),
        body = make_cash({1, <<"USD">>})
    },
    Result = call_deposit('CreateRevert', [DepositID, Params]),
    ExpectedError = #deposit_InconsistentRevertCurrency{
        deposit_currency = #'CurrencyRef'{symbolic_code = <<"RUB">>},
        revert_currency = #'CurrencyRef'{symbolic_code = <<"USD">>}
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_revert_insufficient_deposit_amount_error_test(config()) -> test_return().
create_revert_insufficient_deposit_amount_error_test(C) ->
    DepositBody = make_cash({100, <<"RUB">>}),
    RevertBody = make_cash({1000, <<"RUB">>}),
    #{
        deposit_id := DepositID
    } = prepare_standard_environment_with_deposit(DepositBody, C),
    Params = #deposit_revert_RevertParams{
        id = generate_id(),
        body = RevertBody
    },
    Result = call_deposit('CreateRevert', [DepositID, Params]),
    ExpectedError = #deposit_InsufficientDepositAmount{
        revert_body = RevertBody,
        deposit_amount = DepositBody
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_revert_invalid_revert_amount_error_test(config()) -> test_return().
create_revert_invalid_revert_amount_error_test(C) ->
    DepositBody = make_cash({100, <<"RUB">>}),
    RevertBody = make_cash({0, <<"RUB">>}),
    #{
        deposit_id := DepositID
    } = prepare_standard_environment_with_deposit(DepositBody, C),
    Params = #deposit_revert_RevertParams{
        id = generate_id(),
        body = RevertBody
    },
    Result = call_deposit('CreateRevert', [DepositID, Params]),
    ExpectedError = #fistful_InvalidOperationAmount{
        amount = RevertBody
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_revert_unknown_deposit_error_test(config()) -> test_return().
create_revert_unknown_deposit_error_test(C) ->
    #{
        body := Body
    } = prepare_standard_environment_with_deposit(C),
    Params = #deposit_revert_RevertParams{
        id = generate_id(),
        body = Body
    },
    Result = call_deposit('CreateRevert', [<<"unknown_deposit">>, Params]),
    ExpectedError = #fistful_DepositNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_revert_adjustment_ok_test(config()) -> test_return().
create_revert_adjustment_ok_test(C) ->
    #{
        deposit_id := DepositID,
        revert_id := RevertID
    } = prepare_standard_environment_with_revert(C),
    AdjustmentID = generate_id(),
    ExternalID = generate_id(),
    Params = #dep_rev_adj_AdjustmentParams{
        id = AdjustmentID,
        change =
            {change_status, #dep_rev_adj_ChangeStatusRequest{
                new_status = {failed, #dep_rev_status_Failed{failure = #'Failure'{code = <<"Ooops">>}}}
            }},
        external_id = ExternalID
    },
    {ok, AdjustmentState} = call_deposit('CreateRevertAdjustment', [DepositID, RevertID, Params]),
    ExpectedAdjustment = get_revert_adjustment(DepositID, RevertID, AdjustmentID),

    ?assertEqual(AdjustmentID, AdjustmentState#dep_rev_adj_AdjustmentState.id),
    ?assertEqual(ExternalID, AdjustmentState#dep_rev_adj_AdjustmentState.external_id),
    ?assertEqual(
        ff_adjustment:created_at(ExpectedAdjustment),
        ff_codec:unmarshal(timestamp_ms, AdjustmentState#dep_rev_adj_AdjustmentState.created_at)
    ),
    ?assertEqual(
        ff_adjustment:domain_revision(ExpectedAdjustment),
        AdjustmentState#dep_rev_adj_AdjustmentState.domain_revision
    ),
    ?assertEqual(
        ff_adjustment:party_revision(ExpectedAdjustment),
        AdjustmentState#dep_rev_adj_AdjustmentState.party_revision
    ),
    ?assertEqual(
        ff_deposit_revert_adjustment_codec:marshal(changes_plan, ff_adjustment:changes_plan(ExpectedAdjustment)),
        AdjustmentState#dep_rev_adj_AdjustmentState.changes_plan
    ).

-spec create_revert_adjustment_unavailable_status_error_test(config()) -> test_return().
create_revert_adjustment_unavailable_status_error_test(C) ->
    #{
        deposit_id := DepositID,
        revert_id := RevertID
    } = prepare_standard_environment_with_revert(C),
    Params = #dep_rev_adj_AdjustmentParams{
        id = generate_id(),
        change =
            {change_status, #dep_rev_adj_ChangeStatusRequest{
                new_status = {pending, #dep_rev_status_Pending{}}
            }}
    },
    Result = call_deposit('CreateRevertAdjustment', [DepositID, RevertID, Params]),
    ExpectedError = #deposit_ForbiddenRevertStatusChange{
        target_status = {pending, #dep_rev_status_Pending{}}
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec create_revert_adjustment_already_has_status_error_test(config()) -> test_return().
create_revert_adjustment_already_has_status_error_test(C) ->
    #{
        deposit_id := DepositID,
        revert_id := RevertID
    } = prepare_standard_environment_with_revert(C),
    Params = #dep_rev_adj_AdjustmentParams{
        id = generate_id(),
        change =
            {change_status, #dep_rev_adj_ChangeStatusRequest{
                new_status = {succeeded, #dep_rev_status_Succeeded{}}
            }}
    },
    Result = call_deposit('CreateRevertAdjustment', [DepositID, RevertID, Params]),
    ExpectedError = #deposit_RevertAlreadyHasStatus{
        revert_status = {succeeded, #dep_rev_status_Succeeded{}}
    },
    ?assertEqual({exception, ExpectedError}, Result).

-spec deposit_state_content_test(config()) -> test_return().
deposit_state_content_test(C) ->
    #{
        deposit_id := DepositID,
        revert_id := RevertID
    } = prepare_standard_environment_with_revert(C),
    AdjustmentParams = #dep_adj_AdjustmentParams{
        id = generate_id(),
        change =
            {change_status, #dep_adj_ChangeStatusRequest{
                new_status = {failed, #dep_status_Failed{failure = #'Failure'{code = <<"Ooops">>}}}
            }}
    },
    {ok, _} = call_deposit('CreateAdjustment', [DepositID, AdjustmentParams]),
    RevertAdjustmentParams = #dep_rev_adj_AdjustmentParams{
        id = generate_id(),
        change =
            {change_status, #dep_rev_adj_ChangeStatusRequest{
                new_status = {failed, #dep_rev_status_Failed{failure = #'Failure'{code = <<"Ooops">>}}}
            }}
    },
    {ok, _} = call_deposit('CreateRevertAdjustment', [DepositID, RevertID, RevertAdjustmentParams]),

    {ok, DepositState} = call_deposit('Get', [DepositID, #'EventRange'{}]),
    ?assertMatch([_], DepositState#deposit_DepositState.reverts),
    ?assertMatch([_], DepositState#deposit_DepositState.adjustments),
    ?assertNotEqual(undefined, DepositState#deposit_DepositState.effective_final_cash_flow),
    ?assertNotEqual(
        undefined,
        DepositState#deposit_DepositState.status
    ),

    [RevertState] = DepositState#deposit_DepositState.reverts,
    ?assertMatch([_], RevertState#deposit_revert_RevertState.adjustments).

%% Utils

call_deposit(Fun, Args) ->
    ServiceName = deposit_management,
    Service = ff_services:get_service(ServiceName),
    Request = {Service, Fun, Args},
    Client = ff_woody_client:new(#{
        url => "http://localhost:8022" ++ ff_services:get_service_path(ServiceName)
    }),
    ff_woody_client:call(Client, Request).

prepare_standard_environment(Body, C) ->
    #'Cash'{currency = #'CurrencyRef'{symbolic_code = Currency}} = Body,
    Party = create_party(C),
    IdentityID = create_person_identity(Party, C),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    SourceID = create_source(IdentityID, C),
    #{
        identity_id => IdentityID,
        party_id => Party,
        wallet_id => WalletID,
        source_id => SourceID
    }.

prepare_standard_environment_with_deposit(C) ->
    Body = make_cash({100, <<"RUB">>}),
    Env = prepare_standard_environment_with_deposit(Body, C),
    Env#{body => Body}.

prepare_standard_environment_with_deposit(Body, C) ->
    #{
        wallet_id := WalletID,
        source_id := SourceID
    } = Env = prepare_standard_environment(Body, C),
    DepositID = generate_id(),
    ExternalID = generate_id(),
    Context = #{<<"NS">> => #{generate_id() => generate_id()}},
    EncodedContext = ff_entity_context_codec:marshal(Context),
    Params = #deposit_DepositParams{
        id = DepositID,
        wallet_id = WalletID,
        source_id = SourceID,
        body = Body,
        external_id = ExternalID
    },
    {ok, _DepositState} = call_deposit('Create', [Params, EncodedContext]),
    succeeded = await_final_deposit_status(DepositID),
    Env#{
        deposit_id => DepositID,
        external_id => ExternalID,
        context => Context
    }.

prepare_standard_environment_with_revert(C) ->
    Body = make_cash({100, <<"RUB">>}),
    Env = prepare_standard_environment_with_revert(Body, C),
    Env#{body => Body}.

prepare_standard_environment_with_revert(Body, C) ->
    #{
        deposit_id := DepositID
    } = Env = prepare_standard_environment_with_deposit(Body, C),
    RevertID = generate_id(),
    ExternalID = generate_id(),
    Reason = generate_id(),
    Params = #deposit_revert_RevertParams{
        id = RevertID,
        body = Body,
        external_id = ExternalID,
        reason = Reason
    },
    {ok, _RevertState} = call_deposit('CreateRevert', [DepositID, Params]),
    succeeded = await_final_revert_status(DepositID, RevertID),
    Env#{
        revert_id => RevertID,
        revert_external_id => RevertID,
        reason => Reason
    }.

get_deposit(DepositID) ->
    {ok, Machine} = ff_deposit_machine:get(DepositID),
    ff_deposit_machine:deposit(Machine).

get_deposit_status(DepositID) ->
    ff_deposit:status(get_deposit(DepositID)).

get_adjustment(DepositID, AdjustmentID) ->
    {ok, Adjustment} = ff_deposit:find_adjustment(AdjustmentID, get_deposit(DepositID)),
    Adjustment.

get_revert(DepositID, RevertID) ->
    Deposit = get_deposit(DepositID),
    {ok, Revert} = ff_deposit:find_revert(RevertID, Deposit),
    Revert.

get_revert_adjustment(DepositID, RevertID, AdjustmentID) ->
    {ok, Adjustment} = ff_deposit_revert:find_adjustment(AdjustmentID, get_revert(DepositID, RevertID)),
    Adjustment.

await_final_deposit_status(DepositID) ->
    finished = ct_helper:await(
        finished,
        fun() ->
            {ok, Machine} = ff_deposit_machine:get(DepositID),
            Deposit = ff_deposit_machine:deposit(Machine),
            case ff_deposit:is_finished(Deposit) of
                false ->
                    {not_finished, Deposit};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    get_deposit_status(DepositID).

await_final_revert_status(DepositID, RevertID) ->
    finished = ct_helper:await(
        finished,
        fun() ->
            {ok, Machine} = ff_deposit_machine:get(DepositID),
            Deposit = ff_deposit_machine:deposit(Machine),
            {ok, Revert} = ff_deposit:find_revert(RevertID, Deposit),
            case ff_deposit_revert:is_finished(Revert) of
                false ->
                    {not_finished, Deposit};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    Revert = get_revert(DepositID, RevertID),
    ff_deposit_revert:status(Revert).

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

create_wallet(IdentityID, Name, Currency, _C) ->
    ID = genlib:unique(),
    ok = ff_wallet_machine:create(
        #{id => ID, identity => IdentityID, name => Name, currency => Currency},
        ff_entity_context:new()
    ),
    ID.

await_wallet_balance({Amount, Currency}, ID) ->
    Balance = {Amount, {{inclusive, Amount}, {inclusive, Amount}}, Currency},
    Balance = ct_helper:await(
        Balance,
        fun() -> get_wallet_balance(ID) end,
        genlib_retry:linear(3, 500)
    ),
    ok.

get_wallet_balance(ID) ->
    {ok, Machine} = ff_wallet_machine:get(ID),
    get_account_balance(ff_wallet:account(ff_wallet_machine:wallet(Machine))).

%% NOTE: This function can flap tests after switch to shumpune
%% because of potentially wrong Clock. In common case it should be passed
%% from caller after applying changes to account balance.
%% This will work fine with shumway because it return LatestClock on any
%% balance changes, therefore it will broke tests with shumpune
%% because of proper clocks.
get_account_balance(Account) ->
    {ok, {Amounts, Currency}} = ff_transaction:balance(
        Account,
        ff_clock:latest_clock()
    ),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

generate_id() ->
    ff_id:generate_snowflake_id().

create_source(IID, _C) ->
    ID = generate_id(),
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    Params = #{
        id => ID,
        identity => IID,
        name => <<"XSource">>,
        currency => <<"RUB">>,
        resource => SrcResource
    },
    ok = ff_source_machine:create(Params, ff_entity_context:new()),
    authorized = ct_helper:await(
        authorized,
        fun() ->
            {ok, SrcM} = ff_source_machine:get(ID),
            Source = ff_source_machine:source(SrcM),
            ff_source:status(Source)
        end
    ),
    ID.

make_cash({Amount, Currency}) ->
    #'Cash'{
        amount = Amount,
        currency = #'CurrencyRef'{symbolic_code = Currency}
    }.
