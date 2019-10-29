%%%
%%% Deposit revert
%%%

-module(ff_deposit_revert).

-define(ACTUAL_FORMAT_VERSION, 1).

-type id()       :: binary().
-type reason()   :: binary().

-opaque revert() :: #{
    version         := ?ACTUAL_FORMAT_VERSION,
    id              := id(),
    body            := body(),
    wallet_id       := wallet_id(),
    source_id       := source_id(),
    status          := status(),
    party_revision  := party_revision(),
    domain_revision := domain_revision(),
    created_at      := ff_time:timestamp_ms(),
    p_transfer      => p_transfer(),
    reason          => reason(),
    external_id     => id(),
    limit_checks    => [limit_check_details()],
    adjustments     => adjustments_index()
}.

-type params() :: #{
    id            := id(),
    wallet_id     := wallet_id(),
    source_id     := source_id(),
    body          := body(),
    reason        => binary(),
    external_id   => id()
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

-type limit_check_details() ::
    {wallet, wallet_limit_check_details()}.

-type wallet_limit_check_details() ::
    ok |
    {failed, wallet_limit_check_error()}.

-type wallet_limit_check_error() :: #{
    expected_range := cash_range(),
    balance := cash()
}.

-type create_error() :: none().

-type adjustment_params() :: #{
    id          := adjustment_id(),
    change      := adjustment_change(),
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
-export([wallet_id/1]).
-export([source_id/1]).
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

%% Transfer logic callbacks

-export([process_transfer/1]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/1]).

%% Internal types

-type wallet_id()         :: ff_wallet:id().
-type wallet()            :: ff_wallet:wallet().
-type source_id()         :: ff_source:id().
-type p_transfer()        :: ff_postings_transfer:transfer().
-type body()              :: ff_transaction:body().
-type action()            :: machinery:action() | undefined.
-type process_result()    :: {action(), [event()]}.
-type legacy_event()      :: any().
-type external_id()       :: id().
-type failure()           :: ff_failure:failure().
-type cash()              :: ff_cash:cash().
-type cash_range()        :: ff_range:range(cash()).
-type adjustment()        :: ff_adjustment:adjustment().
-type adjustment_id()     :: ff_adjustment:id().
-type adjustments_index() :: ff_adjustment_utils:index().
-type final_cash_flow()   :: ff_cash_flow:final_cash_flow().
-type party_revision()    :: ff_party:revision().
-type domain_revision()   :: ff_domain_config:revision().
-type identity()          :: ff_identity:identity().
-type terms()             :: ff_party:terms().
-type clock()             :: ff_transaction:clock().

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

-spec wallet_id(revert()) -> wallet_id().
wallet_id(#{wallet_id := V}) ->
    V.

-spec source_id(revert()) -> source_id().
source_id(#{source_id := V}) ->
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
        id            := ID,
        wallet_id     := WalletID,
        source_id     := SourceID,
        body          := Body
    } = Params,
    {ok, Wallet} = get_wallet(WalletID),
    PartyID = ff_identity:party(get_wallet_identity(Wallet)),
    {ok, PartyRevision} = ff_party:get_revision(PartyID),
    CreatedAt = ff_time:now(),
    DomainRevision = ff_domain_config:head(),
    Revert = genlib_map:compact(#{
        version         => ?ACTUAL_FORMAT_VERSION,
        id              => ID,
        body            => Body,
        wallet_id       => WalletID,
        source_id       => SourceID,
        status          => pending,
        party_revision  => PartyRevision,
        domain_revision => DomainRevision,
        created_at       => CreatedAt,
        reason          => maps:get(reason, Params, undefined),
        external_id     => maps:get(external_id, Params, undefined)
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

-spec apply_event(event() | legacy_event(), revert() | undefined) ->
    revert().
apply_event(Ev, T0) ->
    Migrated = maybe_migrate(Ev),
    T1 = apply_event_(Migrated, T0),
    T2 = save_adjustable_info(Migrated, T1),
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

-spec maybe_migrate(event() | legacy_event()) ->
    event().
maybe_migrate({adjustment, _Payload} = Event) ->
    ff_adjustment_utils:maybe_migrate(Event);
maybe_migrate(Ev) ->
    Ev.

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
    FinalCashFlow = make_final_cash_flow(wallet_id(Revert), source_id(Revert), body(Revert)),
    PTransferID = construct_p_transfer_id(id(Revert)),
    {ok, PostingsTransferEvents} = ff_postings_transfer:create(PTransferID, FinalCashFlow),
    {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}.

-spec process_limit_check(revert()) ->
    process_result().
process_limit_check(Revert) ->
    Body = body(Revert),
    WalletID = wallet_id(Revert),
    CreatedAt = created_at(Revert),
    DomainRevision = domain_revision(Revert),
    {ok, Wallet} = get_wallet(WalletID),
    Identity = get_wallet_identity(Wallet),
    PartyID = ff_identity:party(Identity),
    PartyRevision = party_revision(Revert),
    ContractID = ff_identity:contract(Identity),
    {_Amount, Currency} = Body,
    Varset = genlib_map:compact(#{
        currency => ff_dmsl_codec:marshal(currency_ref, Currency),
        cost => ff_dmsl_codec:marshal(cash, Body),
        wallet_id => WalletID
    }),
    {ok, Terms} = ff_party:get_contract_terms(
        PartyID, ContractID, Varset, CreatedAt, PartyRevision, DomainRevision
    ),
    Clock = ff_postings_transfer:clock(p_transfer(Revert)),
    Events = case validate_wallet_limits(Terms, Wallet, Clock) of
        {ok, valid} ->
            [{limit_check, {wallet, ok}}];
        {error, {terms_violation, {wallet_limit, {cash_range, {Cash, Range}}}}} ->
            Details = #{
                expected_range => Range,
                balance => Cash
            },
            [{limit_check, {wallet, {failed, Details}}}]
    end,
    {continue, Events}.

-spec process_transfer_finish(revert()) ->
    process_result().
process_transfer_finish(_Revert) ->
    {undefined, [{status_changed, succeeded}]}.

-spec process_transfer_fail(fail_type(), revert()) ->
    process_result().
process_transfer_fail(limit_check, Revert) ->
    Failure = build_failure(limit_check, Revert),
    {undefined, [{status_changed, {failed, Failure}}]}.

-spec make_final_cash_flow(wallet_id(), source_id(), body()) ->
    final_cash_flow().
make_final_cash_flow(WalletID, SourceID, Body) ->
    {ok, WalletMachine} = ff_wallet_machine:get(WalletID),
    WalletAccount = ff_wallet:account(ff_wallet_machine:wallet(WalletMachine)),
    {ok, SourceMachine} = ff_source:get_machine(SourceID),
    SourceAccount = ff_source:account(ff_source:get(SourceMachine)),
    Constants = #{
        operation_amount => Body
    },
    Accounts = #{
        {wallet, sender_source} => SourceAccount,
        {wallet, receiver_settlement} => WalletAccount
    },
    CashFlowPlan = #{
        postings => [
            #{
                sender   => {wallet, receiver_settlement},
                receiver => {wallet, sender_source},
                volume   => {share, {{1, 1}, operation_amount, default}}
            }
        ]
    },
    {ok, FinalCashFlow} = ff_cash_flow:finalize(CashFlowPlan, Accounts, Constants),
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

-spec effective_final_cash_flow(revert()) -> final_cash_flow().
effective_final_cash_flow(Revert) ->
    case ff_adjustment_utils:cash_flow(adjustments_index(Revert)) of
        undefined ->
            ff_cash_flow:make_empty_final();
        CashFlow ->
            CashFlow
    end.

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
        new_status => NewStatus,
        new_cash_flow => #{
            old_cash_flow_inverted => ff_cash_flow:inverse(CurrentCashFlow),
            new_cash_flow => NewCashFlow
        }
    };
make_change_status_params({failed, _}, succeeded = NewStatus, Revert) ->
    CurrentCashFlow = effective_final_cash_flow(Revert),
    NewCashFlow = make_final_cash_flow(wallet_id(Revert), source_id(Revert), body(Revert)),
    #{
        new_status => NewStatus,
        new_cash_flow => #{
            old_cash_flow_inverted => ff_cash_flow:inverse(CurrentCashFlow),
            new_cash_flow => NewCashFlow
        }
    };
make_change_status_params({failed, _}, {failed, _} = NewStatus, _Revert) ->
    #{
        new_status => NewStatus
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
is_limit_check_ok({wallet, ok}) ->
    true;
is_limit_check_ok({wallet, {failed, _Details}}) ->
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
    <<"ff/deposit_revert/", ID/binary>>.

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
