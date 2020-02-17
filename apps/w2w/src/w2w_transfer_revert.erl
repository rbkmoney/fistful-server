%%%
%%% W2W transfer revert
%%%

-module(w2w_transfer_revert).

-define(ACTUAL_FORMAT_VERSION, 1).

-type id() :: binary().
-type reason() :: binary().

-opaque revert() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := id(),
    body := body(),
    wallet_from_id := wallet_id(),
    wallet_to_id := wallet_id(),
    status := status(),
    party_revision := party_revision(),
    domain_revision := domain_revision(),
    created_at := ff_time:timestamp_ms(),
    p_transfer => p_transfer(),
    reason => reason(),
    external_id => id(),
    limit_checks => [limit_check_details()],
    adjustments => adjustments_index()
}.

-type params() :: #{
    id := id(),
    wallet_from_id := wallet_id(),
    wallet_to_id := wallet_id(),
    body := body(),
    reason => binary(),
    external_id => id()
}.

-type status() ::
    pending |
    succeeded |
    {failed, failure()}.

-type event() ::
    {created, revert()} |
    {p_transfer, ff_postings_transfer:event()} |
    {limit_check, limit_check_details()} |
    {status_changed, status()} |
    ff_adjustment_utils:wrapped_event().

-type limit_check_details() :: #{
    wallet_from := wallet_limit_check_details(),
    wallet_to := wallet_limit_check_details()
}.

-type wallet_limit_check_details() ::
    ok |
    {failed, wallet_limit_check_error()}.

-type wallet_limit_check_error() :: #{
    expected_range := cash_range(),
    balance := cash()
}.

-type create_error() :: none().

-type adjustment_params() :: #{
    id := adjustment_id(),
    change := adjustment_change(),
    external_id => id()
}.

-type adjustment_change() ::
    {change_status, status()}.

-type start_adjustment_error() ::
    invalid_revert_status_error() |
    invalid_status_change_error() |
    {another_adjustment_in_progress, adjustment_id()} |
    ff_adjustment:create_error().

-type unknown_adjustment_error() :: ff_adjustment_utils:unknown_adjustment_error().

-type invalid_status_change_error() ::
    {invalid_status_change, {unavailable_status, status()}} |
    {invalid_status_change, {already_has_status, status()}}.

-type invalid_revert_status_error() ::
    {invalid_revert_status, status()}.

-export_type([id/0]).
-export_type([event/0]).
-export_type([reason/0]).
-export_type([status/0]).
-export_type([revert/0]).
-export_type([params/0]).
-export_type([create_error/0]).
-export_type([adjustment_params/0]).
-export_type([start_adjustment_error/0]).
-export_type([limit_check_details/0]).

%% Accessors

-export([id/1]).
-export([wallet_from_id/1]).
-export([wallet_to_id/1]).
-export([body/1]).
-export([status/1]).
-export([reason/1]).
-export([external_id/1]).
-export([created_at/1]).
-export([party_revision/1]).
-export([domain_revision/1]).

%% API

-export([create/1]).
-export([is_active/1]).
-export([is_finished/1]).
-export([start_adjustment/2]).
-export([find_adjustment/2]).
-export([adjustments/1]).
-export([effective_final_cash_flow/1]).

%% Transfer logic callbacks

-export([process_transfer/1]).

%% Event source

-export([apply_event/2]).

%% Internal types

-type wallet_id() :: ff_wallet:id().
-type wallet() :: ff_wallet:wallet().
-type p_transfer() :: ff_postings_transfer:transfer().
-type body() :: ff_transaction:body().
-type action() :: machinery:action() | undefined.
-type process_result() :: {action(), [event()]}.
-type external_id() :: id().
-type failure() :: ff_failure:failure().
-type cash() :: ff_cash:cash().
-type cash_range() :: ff_range:range(cash()).
-type adjustment() :: ff_adjustment:adjustment().
-type adjustment_id() :: ff_adjustment:id().
-type adjustments_index() :: ff_adjustment_utils:index().
-type final_cash_flow() :: ff_cash_flow:final_cash_flow().
-type party_revision() :: ff_party:revision().
-type domain_revision() :: ff_domain_config:revision().
-type identity() :: ff_identity:identity().
-type terms() :: ff_party:terms().
-type clock() :: ff_transaction:clock().

-type wrapped_adjustment_event() :: ff_adjustment_utils:wrapped_event().

-type validation_error() ::
    {terms_violation, {wallet_limit, {cash_range, {cash(), cash_range()}}}}.

-type fail_type() ::
    limit_check.

-type activity() ::
    p_transfer_start |
    p_transfer_prepare |
    p_transfer_commit |
    p_transfer_cancel |
    limit_check |
    adjustment |
    {fail, fail_type()} |
    finish.

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec id(revert()) -> id().
id(#{id := V}) ->
    V.

-spec wallet_from_id(revert()) -> wallet_id().
wallet_from_id(#{wallet_from_id := V}) ->
    V.

-spec wallet_to_id(revert()) -> wallet_id().
wallet_to_id(#{wallet_to_id := V}) ->
    V.

-spec body(revert()) -> body().
body(#{body := V}) ->
    V.

-spec status(revert()) -> status().
status(Revert) ->
    OwnStatus = maps:get(status, Revert),
    %% `OwnStatus` is used in case of `{created, revert()}` event marshaling
    %% The event revert is not created from events, so `adjustments` can not have
    %% initial revert status.
    ff_adjustment_utils:status(adjustments_index(Revert), OwnStatus).

-spec reason(revert()) -> reason() | undefined.
reason(T) ->
    maps:get(reason, T, undefined).

-spec external_id(revert()) -> external_id() | undefined.
external_id(T) ->
    maps:get(external_id, T, undefined).

-spec party_revision(revert()) -> party_revision().
party_revision(#{party_revision := V}) ->
    V.

-spec domain_revision(revert()) -> domain_revision().
domain_revision(#{domain_revision := V}) ->
    V.

-spec created_at(revert()) -> ff_time:timestamp_ms().
created_at(#{created_at := V}) ->
    V.

%% API

-spec create(params()) ->
    {ok, process_result()}.

create(Params) ->
    #{
        id := ID,
        wallet_from_id := WalletFromID,
        wallet_to_id := WalletToID,
        body := Body
    } = Params,
    {ok, WalletFrom} = get_wallet(WalletFromID),
    PartyID = ff_identity:party(get_wallet_identity(WalletFrom)),
    {ok, PartyRevision} = ff_party:get_revision(PartyID),
    CreatedAt = ff_time:now(),
    DomainRevision = ff_domain_config:head(),
    Revert = genlib_map:compact(#{
        version => ?ACTUAL_FORMAT_VERSION,
        id => ID,
        body => Body,
        wallet_from_id => WalletFromID,
        wallet_to_id => WalletToID,
        status => pending,
        party_revision => PartyRevision,
        domain_revision => DomainRevision,
        created_at => CreatedAt,
        reason => maps:get(reason, Params, undefined),
        external_id => maps:get(external_id, Params, undefined)
    }),
    {ok, {continue, [{created, Revert}]}}.

-spec start_adjustment(adjustment_params(), revert()) ->
    {ok, process_result()} |
    {error, start_adjustment_error()}.
start_adjustment(Params, Revert) ->
    #{id := AdjustmentID} = Params,
    case find_adjustment(AdjustmentID, Revert) of
        {error, {unknown_adjustment, _}} ->
            do_start_adjustment(Params, Revert);
        {ok, _Adjustment} ->
            {ok, {undefined, []}}
    end.

-spec find_adjustment(adjustment_id(), revert()) ->
    {ok, adjustment()} | {error, unknown_adjustment_error()}.
find_adjustment(AdjustmentID, Revert) ->
    ff_adjustment_utils:get_by_id(AdjustmentID, adjustments_index(Revert)).

-spec adjustments(revert()) -> [adjustment()].
adjustments(Revert) ->
    ff_adjustment_utils:adjustments(adjustments_index(Revert)).

-spec effective_final_cash_flow(revert()) -> final_cash_flow().
effective_final_cash_flow(Revert) ->
    case ff_adjustment_utils:cash_flow(adjustments_index(Revert)) of
        undefined ->
            ff_cash_flow:make_empty_final();
        CashFlow ->
            CashFlow
    end.

%% Transfer logic callbacks

-spec process_transfer(revert()) ->
    process_result().
process_transfer(Revert) ->
    Activity = deduce_activity(Revert),
    do_process_transfer(Activity, Revert).

%% Сущность в настоящий момент нуждается в передаче ей управления для совершения каких-то действий
-spec is_active(revert()) -> boolean().
is_active(#{status := succeeded} = Revert) ->
    is_childs_active(Revert);
is_active(#{status := {failed, _}} = Revert) ->
    is_childs_active(Revert);
is_active(#{status := pending}) ->
    true.

%% Сущность завершила свою основную задачу по переводу денег. Дальше её состояние будет меняться только
%% изменением дочерних сущностей, например запуском adjustment.
-spec is_finished(revert()) -> boolean().
is_finished(#{status := succeeded}) ->
    true;
is_finished(#{status := {failed, _}}) ->
    true;
is_finished(#{status := pending}) ->
    false.

%% Events utils

-spec apply_event(event(), revert() | undefined) ->
    revert().
apply_event(Ev, T0) ->
    T1 = apply_event_(Ev, T0),
    T2 = save_adjustable_info(Ev, T1),
    T2.

-spec apply_event_(event(), revert() | undefined) ->
    revert().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, S}, T) ->
    T#{status => S};
apply_event_({limit_check, Details}, T) ->
    add_limit_check(Details, T);
apply_event_({p_transfer, Ev}, T) ->
    T#{p_transfer => ff_postings_transfer:apply_event(Ev, p_transfer(T))};
apply_event_({adjustment, _Ev} = Event, T) ->
    apply_adjustment_event(Event, T).

%% Internals

-spec do_start_adjustment(adjustment_params(), revert()) ->
    {ok, process_result()} |
    {error, start_adjustment_error()}.
do_start_adjustment(Params, Revert) ->
    do(fun() ->
        valid = unwrap(validate_adjustment_start(Params, Revert)),
        AdjustmentParams = make_adjustment_params(Params, Revert),
        #{id := AdjustmentID} = Params,
        {Action, Events} = unwrap(ff_adjustment:create(AdjustmentParams)),
        {Action, ff_adjustment_utils:wrap_events(AdjustmentID, Events)}
    end).

-spec deduce_activity(revert()) ->
    activity().
deduce_activity(Revert) ->
    Params = #{
        p_transfer => p_transfer_status(Revert),
        status => status(Revert),
        limit_check => limit_check_status(Revert),
        active_adjustment => ff_adjustment_utils:is_active(adjustments_index(Revert))
    },
    do_deduce_activity(Params).

do_deduce_activity(#{status := pending, p_transfer := undefined}) ->
    p_transfer_start;
do_deduce_activity(#{status := pending, p_transfer := created}) ->
    p_transfer_prepare;
do_deduce_activity(#{status := pending, p_transfer := prepared, limit_check := unknown}) ->
    limit_check;
do_deduce_activity(#{status := pending, p_transfer := prepared, limit_check := ok}) ->
    p_transfer_commit;
do_deduce_activity(#{status := pending, p_transfer := committed, limit_check := ok}) ->
    finish;
do_deduce_activity(#{status := pending, p_transfer := prepared, limit_check := {failed, _}}) ->
    p_transfer_cancel;
do_deduce_activity(#{status := pending, p_transfer := cancelled, limit_check := {failed, _}}) ->
    {fail, limit_check};
do_deduce_activity(#{active_adjustment := true}) ->
    adjustment.

do_process_transfer(p_transfer_start, Revert) ->
    create_p_transfer(Revert);
do_process_transfer(p_transfer_prepare, Revert) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Revert, fun ff_postings_transfer:prepare/1),
    {continue, Events};
do_process_transfer(p_transfer_commit, Revert) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Revert, fun ff_postings_transfer:commit/1),
    {continue, Events};
do_process_transfer(p_transfer_cancel, Revert) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Revert, fun ff_postings_transfer:cancel/1),
    {continue, Events};
do_process_transfer(limit_check, Revert) ->
    process_limit_check(Revert);
do_process_transfer({fail, Reason}, Revert) ->
    process_transfer_fail(Reason, Revert);
do_process_transfer(finish, Revert) ->
    process_transfer_finish(Revert);
do_process_transfer(adjustment, Revert) ->
    ff_adjustment_utils:process_adjustments(adjustments_index(Revert)).

-spec create_p_transfer(revert()) ->
    process_result().
create_p_transfer(Revert) ->
    FinalCashFlow = make_final_cash_flow(Revert),
    PTransferID = construct_p_transfer_id(id(Revert)),
    {ok, PostingsTransferEvents} = ff_postings_transfer:create(PTransferID, FinalCashFlow),
    {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}.

-spec process_limit_check(revert()) ->
    process_result().
process_limit_check(W2WTransfer) ->
    WalletFromID = wallet_from_id(W2WTransfer),
    WalletToID = wallet_to_id(W2WTransfer),

    LimitCheck = #{
        wallet_from => process_wallet_limit_check(WalletFromID, W2WTransfer),
        wallet_to => process_wallet_limit_check(WalletToID, W2WTransfer)
    },
    {continue, [{limit_check, LimitCheck}]}.

-spec process_transfer_finish(revert()) ->
    process_result().
process_transfer_finish(_Revert) ->
    {undefined, [{status_changed, succeeded}]}.

-spec process_transfer_fail(fail_type(), revert()) ->
    process_result().
process_transfer_fail(limit_check, Revert) ->
    Failure = build_failure(limit_check, Revert),
    {undefined, [{status_changed, {failed, Failure}}]}.

-spec make_final_cash_flow(revert()) ->
    final_cash_flow().
make_final_cash_flow(Revert) ->
    WalletFromID = wallet_from_id(Revert),
    {ok, WalletFromMachine} = ff_wallet_machine:get(WalletFromID),
    WalletFrom = ff_wallet_machine:wallet(WalletFromMachine),
    WalletFromAccount = ff_wallet:account(WalletFrom),
    WalletToID = wallet_to_id(Revert),
    {ok, WalletToMachine} = ff_wallet_machine:get(WalletToID),
    WalletToAccount = ff_wallet:account(ff_wallet_machine:wallet(WalletToMachine)),

    Body = body(Revert),
    {_Amount, CurrencyID} = Body,
    DomainRevision = domain_revision(Revert),
    Identity = get_wallet_identity(WalletFrom),
    Varset = genlib_map:compact(#{
        currency => ff_dmsl_codec:marshal(currency_ref, CurrencyID),
        cost => ff_dmsl_codec:marshal(cash, Body),
        wallet_id => WalletFromID
    }),
    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, DomainRevision),
    {ok, SystemAccounts} = ff_payment_institution:compute_system_accounts(PaymentInstitution, Varset),
    SystemAccount = maps:get(CurrencyID, SystemAccounts, #{}),
    SettlementAccount = maps:get(settlement, SystemAccount, undefined),
    SubagentAccount = maps:get(subagent, SystemAccount, undefined),

    Constants = #{
        operation_amount => Body
    },
    Accounts = #{
        {system, settlement} => SettlementAccount,
        {system, subagent} => SubagentAccount,
        {wallet, sender_source} => WalletFromAccount,
        {wallet, receiver_settlement} => WalletToAccount
    },

    PartyID = ff_identity:party(Identity),
    PartyRevision = party_revision(Revert),
    ContractID = ff_identity:contract(Identity),
    Timestamp = created_at(Revert),
    {ok, Terms} = ff_party:get_contract_terms(
        PartyID, ContractID, Varset, Timestamp, PartyRevision, DomainRevision
    ),
    {ok, CashFlowPlan} = ff_party:get_w2w_cash_flow_plan(Terms),

    {ok, FinalCashFlow} = ff_cash_flow:finalize(ff_cash_flow:inverse(CashFlowPlan), Accounts, Constants),
    FinalCashFlow.

%% Internal getters and setters

-spec p_transfer_status(revert()) ->
    ff_postings_transfer:status() | undefined.
p_transfer_status(Revert) ->
    case p_transfer(Revert) of
        undefined ->
            undefined;
        Transfer ->
            ff_postings_transfer:status(Transfer)
    end.

-spec adjustments_index(revert()) -> adjustments_index().
adjustments_index(Revert) ->
    case maps:find(adjustments, Revert) of
        {ok, Adjustments} ->
            Adjustments;
        error ->
            ff_adjustment_utils:new_index()
    end.

-spec p_transfer(revert()) -> p_transfer() | undefined.
p_transfer(T) ->
    maps:get(p_transfer, T, undefined).

-spec set_adjustments_index(adjustments_index(), revert()) -> revert().
set_adjustments_index(Adjustments, Revert) ->
    Revert#{adjustments => Adjustments}.

-spec is_childs_active(revert()) -> boolean().
is_childs_active(Revert) ->
    ff_adjustment_utils:is_active(adjustments_index(Revert)).

%% Validators

-spec validate_adjustment_start(adjustment_params(), revert()) ->
    {ok, valid} |
    {error, start_adjustment_error()}.
validate_adjustment_start(Params, Revert) ->
    do(fun() ->
        valid = unwrap(validate_no_pending_adjustment(Revert)),
        valid = unwrap(validate_revert_finish(Revert)),
        valid = unwrap(validate_status_change(Params, Revert))
    end).

-spec validate_revert_finish(revert()) ->
    {ok, valid} |
    {error, {invalid_revert_status, status()}}.
validate_revert_finish(Revert) ->
    case is_finished(Revert) of
        true ->
            {ok, valid};
        false ->
            {error, {invalid_revert_status, status(Revert)}}
    end.

-spec validate_no_pending_adjustment(revert()) ->
    {ok, valid} |
    {error, {another_adjustment_in_progress, adjustment_id()}}.
validate_no_pending_adjustment(Revert) ->
    case ff_adjustment_utils:get_not_finished(adjustments_index(Revert)) of
        error ->
            {ok, valid};
        {ok, AdjustmentID} ->
            {error, {another_adjustment_in_progress, AdjustmentID}}
    end.

-spec validate_status_change(adjustment_params(), revert()) ->
    {ok, valid} |
    {error, invalid_status_change_error()}.
validate_status_change(#{change := {change_status, Status}}, Revert) ->
    do(fun() ->
        valid = unwrap(invalid_status_change, validate_target_status(Status)),
        valid = unwrap(invalid_status_change, validate_change_same_status(Status, status(Revert)))
    end);
validate_status_change(_Params, _Revert) ->
    {ok, valid}.

-spec validate_target_status(status()) ->
    {ok, valid} |
    {error, {unavailable_status, status()}}.
validate_target_status(succeeded) ->
    {ok, valid};
validate_target_status({failed, _Failure}) ->
    {ok, valid};
validate_target_status(Status) ->
    {error, {unavailable_status, Status}}.

-spec validate_change_same_status(status(), status()) ->
    {ok, valid} |
    {error, {already_has_status, status()}}.
validate_change_same_status(NewStatus, OldStatus) when NewStatus =/= OldStatus ->
    {ok, valid};
validate_change_same_status(Status, Status) ->
    {error, {already_has_status, Status}}.

%% Adjustment helpers

-spec apply_adjustment_event(wrapped_adjustment_event(), revert()) -> revert().
apply_adjustment_event(WrappedEvent, Revert) ->
    Adjustments0 = adjustments_index(Revert),
    Adjustments1 = ff_adjustment_utils:apply_event(WrappedEvent, Adjustments0),
    set_adjustments_index(Adjustments1, Revert).

-spec make_adjustment_params(adjustment_params(), revert()) ->
    ff_adjustment:params().
make_adjustment_params(Params, Revert) ->
    #{id := ID, change := Change} = Params,
    genlib_map:compact(#{
        id => ID,
        changes_plan => make_adjustment_change(Change, Revert),
        external_id => genlib_map:get(external_id, Params),
        domain_revision => domain_revision(Revert),
        party_revision => party_revision(Revert),
        operation_timestamp => created_at(Revert)
    }).

-spec make_adjustment_change(adjustment_change(), revert()) ->
    ff_adjustment:changes().
make_adjustment_change({change_status, NewStatus}, Revert) ->
    CurrentStatus = status(Revert),
    make_change_status_params(CurrentStatus, NewStatus, Revert).

-spec make_change_status_params(status(), status(), revert()) ->
    ff_adjustment:changes().
make_change_status_params(succeeded, {failed, _} = NewStatus, Revert) ->
    CurrentCashFlow = effective_final_cash_flow(Revert),
    NewCashFlow = ff_cash_flow:make_empty_final(),
    #{
        new_status => #{
            new_status => NewStatus
        },
        new_cash_flow => #{
            old_cash_flow_inverted => ff_cash_flow:inverse(CurrentCashFlow),
            new_cash_flow => NewCashFlow
        }
    };
make_change_status_params({failed, _}, succeeded = NewStatus, Revert) ->
    CurrentCashFlow = effective_final_cash_flow(Revert),
    NewCashFlow = make_final_cash_flow(Revert),
    #{
        new_status => #{
            new_status => NewStatus
        },
        new_cash_flow => #{
            old_cash_flow_inverted => ff_cash_flow:inverse(CurrentCashFlow),
            new_cash_flow => NewCashFlow
        }
    };
make_change_status_params({failed, _}, {failed, _} = NewStatus, _Revert) ->
    #{
        new_status => #{
            new_status => NewStatus
        }
    }.

-spec save_adjustable_info(event(), revert()) -> revert().
save_adjustable_info({created, _Revert}, Revert) ->
    #{status := Status} = Revert,
    update_adjusment_index(fun ff_adjustment_utils:set_status/2, Status, Revert);
save_adjustable_info({status_changed, Status}, Revert) ->
    update_adjusment_index(fun ff_adjustment_utils:set_status/2, Status, Revert);
save_adjustable_info({p_transfer, {status_changed, committed}}, Revert) ->
    CashFlow = ff_postings_transfer:final_cash_flow(p_transfer(Revert)),
    update_adjusment_index(fun ff_adjustment_utils:set_cash_flow/2, CashFlow, Revert);
save_adjustable_info(_Ev, Revert) ->
    Revert.

-spec update_adjusment_index(Updater, Value, revert()) -> revert() when
    Updater :: fun((Value, adjustments_index()) -> adjustments_index()),
    Value :: any().
update_adjusment_index(Updater, Value, Revert) ->
    Index = adjustments_index(Revert),
    set_adjustments_index(Updater(Value, Index), Revert).

%% Limit helpers

-spec process_wallet_limit_check(wallet_id(), revert()) ->
    wallet_limit_check_details().

process_wallet_limit_check(WalletID, Revert) ->
    Body = body(Revert),
    {ok, Wallet} = get_wallet(WalletID),
    DomainRevision = domain_revision(Revert),
    Identity = get_wallet_identity(Wallet),
    PartyID = ff_identity:party(Identity),
    PartyRevision = party_revision(Revert),
    ContractID = ff_identity:contract(Identity),
    {_Amount, Currency} = Body,
    Timestamp = created_at(Revert),
    Varset = genlib_map:compact(#{
        currency => ff_dmsl_codec:marshal(currency_ref, Currency),
        cost => ff_dmsl_codec:marshal(cash, Body),
        wallet_id => WalletID
    }),
    {ok, Terms} = ff_party:get_contract_terms(
        PartyID, ContractID, Varset, Timestamp, PartyRevision, DomainRevision
    ),
    Clock = ff_postings_transfer:clock(p_transfer(Revert)),
    case validate_wallet_limits(Terms, Wallet, Clock) of
        {ok, valid} ->
            ok;
        {error, {terms_violation, {wallet_limit, {cash_range, {Cash, Range}}}}} ->
            Details = #{
                expected_range => Range,
                balance => Cash
            },
            {failed, Details}
    end.

-spec limit_checks(revert()) ->
    [limit_check_details()].
limit_checks(Revert) ->
    maps:get(limit_checks, Revert, []).

-spec add_limit_check(limit_check_details(), revert()) ->
    revert().
add_limit_check(Check, Revert) ->
    Checks = limit_checks(Revert),
    Revert#{limit_checks => [Check | Checks]}.

-spec limit_check_status(revert()) ->
    ok | {failed, limit_check_details()} | unknown.
limit_check_status(#{limit_checks := Checks}) ->
    case lists:dropwhile(fun is_limit_check_ok/1, Checks) of
        [] ->
            ok;
        [H | _Tail] ->
            {failed, H}
    end;
limit_check_status(Revert) when not is_map_key(limit_checks, Revert) ->
    unknown.

-spec is_limit_check_ok(limit_check_details()) -> boolean().
is_limit_check_ok(#{wallet_from := ok, wallet_to := ok}) ->
    true;
is_limit_check_ok(_) ->
    false.

-spec validate_wallet_limits(terms(), wallet(), clock()) ->
    {ok, valid} |
    {error, validation_error()}.
validate_wallet_limits(Terms, Wallet, Clock) ->
    case ff_party:validate_wallet_limits(Terms, Wallet, Clock) of
        {ok, valid} = Result ->
            Result;
        {error, {terms_violation, {cash_range, {Cash, CashRange}}}} ->
            {error, {terms_violation, {wallet_limit, {cash_range, {Cash, CashRange}}}}};
        {error, {invalid_terms, _Details} = Reason} ->
            erlang:error(Reason)
    end.

-spec build_failure(fail_type(), revert()) -> failure().
build_failure(limit_check, Revert) ->
    {failed, Details} = limit_check_status(Revert),
    #{
        code => <<"unknown">>,
        reason => genlib:format(Details)
    }.

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/w2w_transfer_revert/", ID/binary>>.

-spec get_wallet(wallet_id()) ->
    {ok, wallet()} | {error, notfound}.
get_wallet(WalletID) ->
    do(fun() ->
        WalletMachine = unwrap(ff_wallet_machine:get(WalletID)),
        ff_wallet_machine:wallet(WalletMachine)
    end).

-spec get_wallet_identity(wallet()) ->
    identity().
get_wallet_identity(Wallet) ->
    IdentityID = ff_wallet:identity(Wallet),
    {ok, IdentityMachine} = ff_identity_machine:get(IdentityID),
    ff_identity_machine:identity(IdentityMachine).