%%%
%%% W2WTransfer
%%%

-module(w2w_transfer).

-type id() :: binary().

-define(ACTUAL_FORMAT_VERSION, 1).
-opaque w2w_transfer() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := id(),
    body := body(),
    wallet_from_id := wallet_id(),
    wallet_to_id := wallet_id(),
    party_revision := party_revision(),
    domain_revision := domain_revision(),
    created_at := ff_time:timestamp_ms(),
    p_transfer => p_transfer(),
    status => status(),
    external_id => id(),
    limit_checks => [limit_check_details()],
    reverts => reverts_index(),
    adjustments => adjustments_index()
}.
-type params() :: #{
    id := id(),
    body := ff_transaction:body(),
    wallet_from_id := wallet_id(),
    wallet_to_id := wallet_id(),
    external_id => external_id()
}.

-type status() ::
    pending |
    succeeded |
    {failed, failure()}.

-type event() ::
    {created, w2w_transfer()} |
    {limit_check, limit_check_details()} |
    {p_transfer, ff_postings_transfer:event()} |
    wrapped_revert_event() |
    wrapped_adjustment_event() |
    {status_changed, status()}.

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

-type create_error() ::
    {wallet_from, notfound} |
    {wallet_to, notfound} |
    ff_party:validate_w2w_transfer_creation_error() |
    {inconsistent_currency, {W2WTransfer :: currency_id(), WalletFrom :: currency_id(), WalletTo :: currency_id()}}.

-type revert_params() :: #{
    id := id(),
    body := body(),
    reason => binary(),
    external_id => id()
}.

-type start_revert_error() ::
    invalid_w2w_transfer_status_error() |
    {inconsistent_revert_currency, {Revert :: currency_id(), W2WTransfer :: currency_id()}} |
    {insufficient_w2w_transfer_amount, {Revert :: body(), W2WTransfer :: body()}} |
    {invalid_revert_amount, Revert :: body()} |
    w2w_transfer_revert:create_error().

-type invalid_w2w_transfer_status_error() ::
    {invalid_w2w_transfer_status, status()}.

-type wrapped_revert_event()  :: w2w_transfer_revert_utils:wrapped_event().
-type wrapped_adjustment_event()  :: ff_adjustment_utils:wrapped_event().

-type revert_adjustment_params() :: w2w_transfer_revert:adjustment_params().

-type start_revert_adjustment_error() ::
    w2w_transfer_revert:start_adjustment_error() |
    unknown_revert_error().

-type unknown_revert_error() :: w2w_transfer_revert_utils:unknown_revert_error().

-type adjustment_params() :: #{
    id := adjustment_id(),
    change := adjustment_change(),
    external_id => id()
}.

-type adjustment_change() ::
    {change_status, status()}.

-type start_adjustment_error() ::
    invalid_w2w_transfer_status_error() |
    invalid_status_change_error() |
    {another_adjustment_in_progress, adjustment_id()} |
    ff_adjustment:create_error().

-type unknown_adjustment_error() :: ff_adjustment_utils:unknown_adjustment_error().

-type invalid_status_change_error() ::
    {invalid_status_change, {unavailable_status, status()}} |
    {invalid_status_change, {already_has_status, status()}}.

-export_type([w2w_transfer/0]).
-export_type([id/0]).
-export_type([params/0]).
-export_type([revert_params/0]).
-export_type([event/0]).
-export_type([wrapped_revert_event/0]).
-export_type([wrapped_adjustment_event/0]).
-export_type([create_error/0]).
-export_type([start_revert_error/0]).
-export_type([revert_adjustment_params/0]).
-export_type([start_revert_adjustment_error/0]).
-export_type([adjustment_params/0]).
-export_type([start_adjustment_error/0]).
-export_type([limit_check_details/0]).

%% Accessors

-export([wallet_from_id/1]).
-export([wallet_to_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([external_id/1]).
-export([party_revision/1]).
-export([domain_revision/1]).
-export([created_at/1]).

%% API
-export([create/1]).

-export([is_active/1]).
-export([is_finished/1]).

-export([start_revert/2]).
-export([start_revert_adjustment/3]).
-export([find_revert/2]).
-export([reverts/1]).

-export([start_adjustment/2]).
-export([find_adjustment/2]).
-export([adjustments/1]).
-export([effective_final_cash_flow/1]).

%% Transfer logic callbacks
-export([process_transfer/1]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Internal types

% -type account()               :: ff_account:account().
-type process_result()        :: {action(), [event()]}.
% -type cash_flow_plan()        :: ff_cash_flow:cash_flow_plan().
-type wallet_id()             :: ff_wallet:id().
-type wallet()                :: ff_wallet:wallet().
-type revert()                :: w2w_transfer_revert:revert().
-type revert_id()             :: w2w_transfer_revert:id().
-type body()                  :: ff_transaction:body().
-type cash()                  :: ff_cash:cash().
-type cash_range()            :: ff_range:range(cash()).
-type action()                :: machinery:action() | undefined.
-type p_transfer()            :: ff_postings_transfer:transfer().
-type currency_id()           :: ff_currency:id().
-type external_id()           :: id().
-type reverts_index()         :: w2w_transfer_revert_utils:index().
-type failure()               :: ff_failure:failure().
-type adjustment()            :: ff_adjustment:adjustment().
-type adjustment_id()         :: ff_adjustment:id().
-type adjustments_index()     :: ff_adjustment_utils:index().
-type final_cash_flow()       :: ff_cash_flow:final_cash_flow().
-type party_revision()        :: ff_party:revision().
-type domain_revision()       :: ff_domain_config:revision().
-type identity()              :: ff_identity:identity().
-type terms()                 :: ff_party:terms().
-type clock()                 :: ff_transaction:clock().

-type activity() ::
    p_transfer_start |
    p_transfer_prepare |
    p_transfer_commit |
    p_transfer_cancel |
    limit_check |
    revert |
    adjustment |
    {fail, fail_type()} |
    finish.

-type fail_type() ::
    limit_check.

%% Accessors

-spec id(w2w_transfer()) -> id().
id(#{id := V}) ->
    V.

-spec wallet_from_id(w2w_transfer()) -> wallet_id().
wallet_from_id(T) ->
    maps:get(wallet_from_id, T).

-spec wallet_to_id(w2w_transfer()) -> wallet_id().
wallet_to_id(T) ->
    maps:get(wallet_to_id, T).

-spec body(w2w_transfer()) -> body().
body(#{body := V}) ->
    V.

-spec status(w2w_transfer()) -> status() | undefined.
status(W2WTransfer) ->
    OwnStatus = maps:get(status, W2WTransfer, undefined),
    %% `OwnStatus` is used in case of `{created, w2w_transfer()}` event marshaling
    %% The event w2w_transfer is not created from events, so `adjustments` can not have
    %% initial w2w_transfer status.
    ff_adjustment_utils:status(adjustments_index(W2WTransfer), OwnStatus).

-spec p_transfer(w2w_transfer())  -> p_transfer() | undefined.
p_transfer(W2WTransfer) ->
    maps:get(p_transfer, W2WTransfer, undefined).

-spec external_id(w2w_transfer()) -> external_id() | undefined.
external_id(W2WTransfer) ->
    maps:get(external_id, W2WTransfer, undefined).

-spec party_revision(w2w_transfer()) -> party_revision() | undefined.
party_revision(T) ->
    maps:get(party_revision, T, undefined).

-spec domain_revision(w2w_transfer()) -> domain_revision() | undefined.
domain_revision(T) ->
    maps:get(domain_revision, T, undefined).

-spec created_at(w2w_transfer()) -> ff_time:timestamp_ms() | undefined.
created_at(T) ->
    maps:get(created_at, T, undefined).

%% API

-spec create(params()) ->
    {ok, [event()]} |
    {error, create_error()}.
create(Params) ->
    do(fun() ->
        #{id := ID, wallet_from_id := WalletFromID, wallet_to_id := WalletToID, body := Body} = Params,
        CreatedAt = ff_time:now(),
        DomainRevision = ff_domain_config:head(),
        WalletFrom = unwrap(wallet_from, get_wallet(WalletFromID)),
        WalletTo = unwrap(wallet_to, get_wallet(WalletToID)),
        Identity = get_wallet_identity(WalletFrom),
        PartyID = ff_identity:party(Identity),
        {ok, PartyRevision} = ff_party:get_revision(PartyID),
        ContractID = ff_identity:contract(Identity),
        {_Amount, Currency} = Body,
        Varset = genlib_map:compact(#{
            currency => ff_dmsl_codec:marshal(currency_ref, Currency),
            cost => ff_dmsl_codec:marshal(cash, Body),
            wallet_id => WalletFromID
        }),
        {ok, Terms} = ff_party:get_contract_terms(
            PartyID, ContractID, Varset, CreatedAt, PartyRevision, DomainRevision
        ),
        valid =  unwrap(validate_w2w_transfer_creation(Terms, Params, WalletFrom, WalletTo)),
        ExternalID = maps:get(external_id, Params, undefined),
        [
            {created, add_external_id(ExternalID, #{
                version => ?ACTUAL_FORMAT_VERSION,
                id => ID,
                body => Body,
                wallet_from_id => WalletFromID,
                wallet_to_id => WalletToID,
                party_revision => PartyRevision,
                domain_revision => DomainRevision,
                created_at => CreatedAt
            })},
            {status_changed, pending}
        ]
    end).

-spec start_revert(revert_params(), w2w_transfer()) ->
    {ok, process_result()} |
    {error, start_revert_error()}.
start_revert(Params, W2WTransfer) ->
    #{id := RevertID} = Params,
    case find_revert(RevertID, W2WTransfer) of
        {error, {unknown_revert, _}} ->
            do_start_revert(Params, W2WTransfer);
        {ok, _Revert} ->
            {ok, {undefined, []}}
    end.

-spec start_revert_adjustment(revert_id(), revert_adjustment_params(), w2w_transfer()) ->
    {ok, process_result()} |
    {error, start_revert_adjustment_error()}.
start_revert_adjustment(RevertID, Params, W2WTransfer) ->
    do(fun() ->
        Revert = unwrap(find_revert(RevertID, W2WTransfer)),
        {Action, Events} = unwrap(w2w_transfer_revert:start_adjustment(Params, Revert)),
        {Action, w2w_transfer_revert_utils:wrap_events(RevertID, Events)}
    end).

-spec find_revert(revert_id(), w2w_transfer()) ->
    {ok, revert()} | {error, unknown_revert_error()}.
find_revert(RevertID, W2WTransfer) ->
    w2w_transfer_revert_utils:get_by_id(RevertID, reverts_index(W2WTransfer)).

-spec reverts(w2w_transfer()) -> [revert()].
reverts(W2WTransfer) ->
    w2w_transfer_revert_utils:reverts(reverts_index(W2WTransfer)).

-spec start_adjustment(adjustment_params(), w2w_transfer()) ->
    {ok, process_result()} |
    {error, start_adjustment_error()}.
start_adjustment(Params, W2WTransfer) ->
    #{id := AdjustmentID} = Params,
    case find_adjustment(AdjustmentID, W2WTransfer) of
        {error, {unknown_adjustment, _}} ->
            do_start_adjustment(Params, W2WTransfer);
        {ok, _Adjustment} ->
            {ok, {undefined, []}}
    end.

-spec find_adjustment(adjustment_id(), w2w_transfer()) ->
    {ok, adjustment()} | {error, unknown_adjustment_error()}.
find_adjustment(AdjustmentID, W2WTransfer) ->
    ff_adjustment_utils:get_by_id(AdjustmentID, adjustments_index(W2WTransfer)).

-spec adjustments(w2w_transfer()) -> [adjustment()].
adjustments(W2WTransfer) ->
    ff_adjustment_utils:adjustments(adjustments_index(W2WTransfer)).

-spec effective_final_cash_flow(w2w_transfer()) -> final_cash_flow().
effective_final_cash_flow(W2WTransfer) ->
    case ff_adjustment_utils:cash_flow(adjustments_index(W2WTransfer)) of
        undefined ->
            ff_cash_flow:make_empty_final();
        CashFlow ->
            CashFlow
    end.

-spec process_transfer(w2w_transfer()) ->
    process_result().
process_transfer(W2WTransfer) ->
    Activity = deduce_activity(W2WTransfer),
    do_process_transfer(Activity, W2WTransfer).

%% Сущность в настоящий момент нуждается в передаче ей управления для совершения каких-то действий
-spec is_active(w2w_transfer()) -> boolean().
is_active(#{status := succeeded} = W2WTransfer) ->
    is_childs_active(W2WTransfer);
is_active(#{status := {failed, _}} = W2WTransfer) ->
    is_childs_active(W2WTransfer);
is_active(#{status := pending}) ->
    true.

%% Сущность завершила свою основную задачу по переводу денег. Дальше её состояние будет меняться только
%% изменением дочерних сущностей, например запуском adjustment.
-spec is_finished(w2w_transfer()) -> boolean().
is_finished(#{status := succeeded}) ->
    true;
is_finished(#{status := {failed, _}}) ->
    true;
is_finished(#{status := pending}) ->
    false.

%% Events utils

-spec apply_event(event(), w2w_transfer() | undefined) ->
    w2w_transfer().
apply_event(Ev, T0) ->
    T1 = apply_event_(Ev, T0),
    T2 = save_adjustable_info(Ev, T1),
    T2.

-spec apply_event_(event(), w2w_transfer() | undefined) ->
    w2w_transfer().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, S}, T) ->
    maps:put(status, S, T);
apply_event_({limit_check, Details}, T) ->
    add_limit_check(Details, T);
apply_event_({p_transfer, Ev}, T) ->
    T#{p_transfer => ff_postings_transfer:apply_event(Ev, p_transfer(T))};
apply_event_({revert, _Ev} = Event, T) ->
    apply_revert_event(Event, T);
apply_event_({adjustment, _Ev} = Event, T) ->
    apply_adjustment_event(Event, T).

%% Internals

-spec do_start_revert(revert_params(), w2w_transfer()) ->
    {ok, process_result()} |
    {error, start_revert_error()}.
do_start_revert(Params = #{id := RevertID}, W2WTransfer) ->
    do(fun() ->
        valid = unwrap(validate_revert_start(Params, W2WTransfer)),
        RevertParams = Params#{
            wallet_from_id => wallet_from_id(W2WTransfer),
            wallet_to_id => wallet_to_id(W2WTransfer)
        },
        {Action, Events} = unwrap(w2w_transfer_revert:create(RevertParams)),
        {Action, w2w_transfer_revert_utils:wrap_events(RevertID, Events)}
    end).

-spec do_start_adjustment(adjustment_params(), w2w_transfer()) ->
    {ok, process_result()} |
    {error, start_adjustment_error()}.
do_start_adjustment(Params, W2WTransfer) ->
    do(fun() ->
        valid = unwrap(validate_adjustment_start(Params, W2WTransfer)),
        AdjustmentParams = make_adjustment_params(Params, W2WTransfer),
        #{id := AdjustmentID} = Params,
        {Action, Events} = unwrap(ff_adjustment:create(AdjustmentParams)),
        {Action, ff_adjustment_utils:wrap_events(AdjustmentID, Events)}
    end).

add_external_id(undefined, Event) ->
    Event;
add_external_id(ExternalID, Event) ->
    Event#{external_id => ExternalID}.

-spec deduce_activity(w2w_transfer()) ->
    activity().
deduce_activity(W2WTransfer) ->
    Params = #{
        p_transfer => p_transfer_status(W2WTransfer),
        status => status(W2WTransfer),
        limit_check => limit_check_status(W2WTransfer),
        active_revert => w2w_transfer_revert_utils:is_active(reverts_index(W2WTransfer)),
        active_adjustment => ff_adjustment_utils:is_active(adjustments_index(W2WTransfer))
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
do_deduce_activity(#{p_transfer := committed, active_revert := true}) ->
    revert;
do_deduce_activity(#{active_adjustment := true}) ->
    adjustment.

-spec do_process_transfer(activity(), w2w_transfer()) ->
    process_result().
do_process_transfer(p_transfer_start, W2WTransfer) ->
    create_p_transfer(W2WTransfer);
do_process_transfer(p_transfer_prepare, W2WTransfer) ->
    {ok, Events} = ff_pipeline:with(p_transfer, W2WTransfer, fun ff_postings_transfer:prepare/1),
    {continue, Events};
do_process_transfer(p_transfer_commit, W2WTransfer) ->
    {ok, Events} = ff_pipeline:with(p_transfer, W2WTransfer, fun ff_postings_transfer:commit/1),
    {continue, Events};
do_process_transfer(p_transfer_cancel, W2WTransfer) ->
    {ok, Events} = ff_pipeline:with(p_transfer, W2WTransfer, fun ff_postings_transfer:cancel/1),
    {continue, Events};
do_process_transfer(limit_check, W2WTransfer) ->
    process_limit_check(W2WTransfer);
do_process_transfer({fail, Reason}, W2WTransfer) ->
    process_transfer_fail(Reason, W2WTransfer);
do_process_transfer(finish, W2WTransfer) ->
    process_transfer_finish(W2WTransfer);
do_process_transfer(revert, W2WTransfer) ->
    Result = w2w_transfer_revert_utils:process_reverts(reverts_index(W2WTransfer)),
    handle_child_result(Result, W2WTransfer);
do_process_transfer(adjustment, W2WTransfer) ->
    Result = ff_adjustment_utils:process_adjustments(adjustments_index(W2WTransfer)),
    handle_child_result(Result, W2WTransfer).

-spec create_p_transfer(w2w_transfer()) ->
    process_result().
create_p_transfer(W2WTransfer) ->
    FinalCashFlow = make_final_cash_flow(wallet_from_id(W2WTransfer), wallet_to_id(W2WTransfer), body(W2WTransfer)),
    PTransferID = construct_p_transfer_id(id(W2WTransfer)),
    {ok, PostingsTransferEvents} = ff_postings_transfer:create(PTransferID, FinalCashFlow),
    {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}.

-spec process_limit_check(w2w_transfer()) ->
    process_result().
process_limit_check(W2WTransfer) ->
    WalletFromID = wallet_from_id(W2WTransfer),
    WalletToID = wallet_to_id(W2WTransfer),

    LimitCheck = #{
        wallet_from => process_wallet_limit_check(WalletFromID, W2WTransfer),
        wallet_to => process_wallet_limit_check(WalletToID, W2WTransfer)
    },
    {continue, [{limit_check, LimitCheck}]}.

-spec process_transfer_finish(w2w_transfer()) ->
    process_result().
process_transfer_finish(_W2WTransfer) ->
    {undefined, [{status_changed, succeeded}]}.

-spec process_transfer_fail(fail_type(), w2w_transfer()) ->
    process_result().
process_transfer_fail(limit_check, W2WTransfer) ->
    Failure = build_failure(limit_check, W2WTransfer),
    {undefined, [{status_changed, {failed, Failure}}]}.

-spec make_final_cash_flow(wallet_id(), wallet_id(), body()) ->
    final_cash_flow().
make_final_cash_flow(WalletFromID, WalletToID, Body) ->
    {ok, WalletFromMachine} = ff_wallet_machine:get(WalletFromID),
    WalletFromAccount = ff_wallet:account(ff_wallet_machine:wallet(WalletFromMachine)),
    {ok, WalletToMachine} = ff_wallet_machine:get(WalletToID),
    WalletToAccount = ff_wallet:account(ff_wallet_machine:wallet(WalletToMachine)),
    Constants = #{
        operation_amount => Body
    },
    Accounts = #{
        {wallet, sender_source} => WalletFromAccount,
        {wallet, receiver_settlement} => WalletToAccount
    },
    CashFlowPlan = #{
        postings => [
            #{
                sender => {wallet, sender_source},
                receiver => {wallet, receiver_settlement},
                volume => {share, {{1, 1}, operation_amount, default}}
            }
        ]
    },
    {ok, FinalCashFlow} = ff_cash_flow:finalize(CashFlowPlan, Accounts, Constants),
    FinalCashFlow.

-spec handle_child_result(process_result(), w2w_transfer()) -> process_result().
handle_child_result({undefined, Events} = Result, W2WTransfer) ->
    NextW2WTransfer = lists:foldl(fun(E, Acc) -> apply_event(E, Acc) end, W2WTransfer, Events),
    case is_active(NextW2WTransfer) of
        true ->
            {continue, Events};
        false ->
            Result
    end;
handle_child_result({_OtherAction, _Events} = Result, _W2WTransfer) ->
    Result.

%% Internal getters and setters

-spec p_transfer_status(w2w_transfer()) -> ff_postings_transfer:status() | undefined.
p_transfer_status(W2WTransfer) ->
    case p_transfer(W2WTransfer) of
        undefined ->
            undefined;
        Transfer ->
            ff_postings_transfer:status(Transfer)
    end.

-spec adjustments_index(w2w_transfer()) -> adjustments_index().
adjustments_index(W2WTransfer) ->
    case maps:find(adjustments, W2WTransfer) of
        {ok, Adjustments} ->
            Adjustments;
        error ->
            ff_adjustment_utils:new_index()
    end.

-spec set_adjustments_index(adjustments_index(), w2w_transfer()) -> w2w_transfer().
set_adjustments_index(Adjustments, W2WTransfer) ->
    W2WTransfer#{adjustments => Adjustments}.

-spec is_childs_active(w2w_transfer()) -> boolean().
is_childs_active(W2WTransfer) ->
    ff_adjustment_utils:is_active(adjustments_index(W2WTransfer)) orelse
        w2w_transfer_revert_utils:is_active(reverts_index(W2WTransfer)).

-spec operation_timestamp(w2w_transfer()) -> ff_time:timestamp_ms().
operation_timestamp(W2WTransfer) ->
    ff_maybe:get_defined(created_at(W2WTransfer), ff_time:now()).

-spec operation_party_revision(w2w_transfer()) ->
    domain_revision().
operation_party_revision(W2WTransfer) ->
    case party_revision(W2WTransfer) of
        undefined ->
            {ok, Wallet} = get_wallet(wallet_from_id(W2WTransfer)),
            PartyID = ff_identity:party(get_wallet_identity(Wallet)),
            {ok, Revision} = ff_party:get_revision(PartyID),
            Revision;
        Revision ->
            Revision
    end.

-spec operation_domain_revision(w2w_transfer()) ->
    domain_revision().
operation_domain_revision(W2WTransfer) ->
    case domain_revision(W2WTransfer) of
        undefined ->
            ff_domain_config:head();
        Revision ->
            Revision
    end.

%% W2WTransfer validators

-spec validate_w2w_transfer_creation(terms(), params(), wallet(), wallet()) ->
    {ok, valid} |
    {error, create_error()}.
validate_w2w_transfer_creation(Terms, Params, WalletFrom, WalletTo) ->
    #{body := Body} = Params,
    do(fun() ->
        valid = unwrap(ff_party:validate_w2w_transfer_creation(Terms, Body)),
        valid = unwrap(validate_w2w_transfer_currency(Body, WalletFrom, WalletTo))
    end).

-spec validate_w2w_transfer_currency(body(), wallet(), wallet()) ->
    {ok, valid} |
    {error, {inconsistent_currency, {currency_id(), currency_id(), currency_id()}}}.
validate_w2w_transfer_currency(Body, WalletFrom, WalletTo) ->
    WalletFromCurrencyID = ff_account:currency(ff_wallet:account(WalletFrom)),
    WalletToCurrencyID = ff_account:currency(ff_wallet:account(WalletTo)),
    case Body of
        {_Amount, W2WTransferCurencyID} when
            W2WTransferCurencyID =:= WalletFromCurrencyID andalso
            W2WTransferCurencyID =:= WalletToCurrencyID
        ->
            {ok, valid};
        {_Amount, W2WTransferCurencyID} ->
            {error, {inconsistent_currency, {W2WTransferCurencyID, WalletFromCurrencyID, WalletToCurrencyID}}}
    end.

%% Limit helpers
-spec process_wallet_limit_check(wallet_id(), w2w_transfer()) ->
    wallet_limit_check_details().

process_wallet_limit_check(WalletID, W2WTransfer) ->
    Body = body(W2WTransfer),
    {ok, Wallet} = get_wallet(WalletID),
    DomainRevision = operation_domain_revision(W2WTransfer),
    Identity = get_wallet_identity(Wallet),
    PartyID = ff_identity:party(Identity),
    PartyRevision = operation_party_revision(W2WTransfer),
    ContractID = ff_identity:contract(Identity),
    {_Amount, Currency} = Body,
    Timestamp = operation_timestamp(W2WTransfer),
    Varset = genlib_map:compact(#{
        currency => ff_dmsl_codec:marshal(currency_ref, Currency),
        cost => ff_dmsl_codec:marshal(cash, Body),
        wallet_id => WalletID
    }),
    {ok, Terms} = ff_party:get_contract_terms(
        PartyID, ContractID, Varset, Timestamp, PartyRevision, DomainRevision
    ),
    Clock = ff_postings_transfer:clock(p_transfer(W2WTransfer)),
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

-spec limit_checks(w2w_transfer()) ->
    [limit_check_details()].
limit_checks(W2WTransfer) ->
    maps:get(limit_checks, W2WTransfer, []).

-spec add_limit_check(limit_check_details(), w2w_transfer()) ->
    w2w_transfer().
add_limit_check(Check, W2WTransfer) ->
    Checks = limit_checks(W2WTransfer),
    W2WTransfer#{limit_checks => [Check | Checks]}.

-spec limit_check_status(w2w_transfer()) ->
    ok | {failed, limit_check_details()} | unknown.
limit_check_status(#{limit_checks := Checks}) ->
    case lists:dropwhile(fun is_limit_check_ok/1, Checks) of
        [] ->
            ok;
        [H | _Tail] ->
            {failed, H}
    end;
limit_check_status(W2WTransfer) when not is_map_key(limit_checks, W2WTransfer) ->
    unknown.

-spec is_limit_check_ok(limit_check_details()) -> boolean().
is_limit_check_ok({wallet, ok}) ->
    true;
is_limit_check_ok({wallet, {failed, _Details}}) ->
    false.

-spec validate_wallet_limits(terms(), wallet(), clock()) ->
    {ok, valid} |
    {error, {terms_violation, {wallet_limit, {cash_range, {cash(), cash_range()}}}}}.
validate_wallet_limits(Terms, Wallet, Clock) ->
    case ff_party:validate_wallet_limits(Terms, Wallet, Clock) of
        {ok, valid} = Result ->
            Result;
        {error, {terms_violation, {cash_range, {Cash, CashRange}}}} ->
            {error, {terms_violation, {wallet_limit, {cash_range, {Cash, CashRange}}}}};
        {error, {invalid_terms, _Details} = Reason} ->
            erlang:error(Reason)
    end.

%% Revert validators

-spec validate_revert_start(revert_params(), w2w_transfer()) ->
    {ok, valid} |
    {error, start_revert_error()}.
validate_revert_start(Params, W2WTransfer) ->
    do(fun() ->
        valid = unwrap(validate_w2w_transfer_success(W2WTransfer)),
        valid = unwrap(validate_revert_body(Params, W2WTransfer))
    end).

-spec validate_revert_body(revert_params(), w2w_transfer()) -> {ok, valid} | {error, Error} when
    Error :: CurrencyError | AmountError,
    CurrencyError :: {inconsistent_revert_currency, {Revert :: currency_id(), W2WTransfer :: currency_id()}},
    AmountError :: {insufficient_w2w_transfer_amount, {Revert :: body(), W2WTransfer :: body()}}.
validate_revert_body(Params, W2WTransfer) ->
    do(fun() ->
        valid = unwrap(validate_revert_currency(Params, W2WTransfer)),
        valid = unwrap(validate_revert_amount(Params)),
        valid = unwrap(validate_unreverted_amount(Params, W2WTransfer))
    end).

-spec validate_w2w_transfer_success(w2w_transfer()) ->
    {ok, valid} |
    {error, invalid_w2w_transfer_status_error()}.
validate_w2w_transfer_success(W2WTransfer) ->
    case status(W2WTransfer) of
        succeeded ->
            {ok, valid};
        Other ->
            {error, {invalid_w2w_transfer_status, Other}}
    end.

-spec validate_revert_currency(revert_params(), w2w_transfer()) ->
    {ok, valid} |
    {error, {inconsistent_revert_currency, {Revert :: currency_id(), W2WTransfer :: currency_id()}}}.
validate_revert_currency(Params, W2WTransfer) ->
    {_InitialAmount, W2WTransferCurrency} = body(W2WTransfer),
    #{body := {_Amount, RevertCurrency}} = Params,
    case {RevertCurrency, W2WTransferCurrency} of
        {SameCurrency, SameCurrency} ->
            {ok, valid};
        _Other ->
            {error, {inconsistent_revert_currency, {RevertCurrency, W2WTransferCurrency}}}
    end.

-spec validate_unreverted_amount(revert_params(), w2w_transfer()) ->
    {ok, valid} |
    {error, {insufficient_w2w_transfer_amount, {Revert :: body(), W2WTransfer :: body()}}}.
validate_unreverted_amount(Params, W2WTransfer) ->
    {InitialAmount, Currency} = body(W2WTransfer),
    #{body := {RevertAmount, Currency} = RevertBody} = Params,
    {TotalReverted, Currency} = max_reverted_body_total(W2WTransfer),
    case InitialAmount - TotalReverted - RevertAmount of
        UnrevertedAmount when UnrevertedAmount >= 0 ->
            {ok, valid};
        _Other ->
            Unreverted = {InitialAmount - TotalReverted, Currency},
            {error, {insufficient_w2w_transfer_amount, {RevertBody, Unreverted}}}
    end.

-spec validate_revert_amount(revert_params()) ->
    {ok, valid} |
    {error, {invalid_revert_amount, Revert :: body()}}.
validate_revert_amount(Params) ->
    #{body := {RevertAmount, _Currency} = RevertBody} = Params,
    case RevertAmount of
        Good when Good > 0 ->
            {ok, valid};
        _Other ->
            {error, {invalid_revert_amount, RevertBody}}
    end.

%% Revert helpers

-spec reverts_index(w2w_transfer()) -> reverts_index().
reverts_index(W2WTransfer) ->
    case maps:find(reverts, W2WTransfer) of
        {ok, Reverts} ->
            Reverts;
        error ->
            w2w_transfer_revert_utils:new_index()
    end.

-spec set_reverts_index(reverts_index(), w2w_transfer()) -> w2w_transfer().
set_reverts_index(Reverts, W2WTransfer) ->
    W2WTransfer#{reverts => Reverts}.

-spec apply_revert_event(wrapped_revert_event(), w2w_transfer()) -> w2w_transfer().
apply_revert_event(WrappedEvent, W2WTransfer) ->
    Reverts0 = reverts_index(W2WTransfer),
    Reverts1 = w2w_transfer_revert_utils:apply_event(WrappedEvent, Reverts0),
    set_reverts_index(Reverts1, W2WTransfer).

-spec max_reverted_body_total(w2w_transfer()) -> body().
max_reverted_body_total(W2WTransfer) ->
    Reverts = w2w_transfer_revert_utils:reverts(reverts_index(W2WTransfer)),
    {_InitialAmount, Currency} = body(W2WTransfer),
    lists:foldl(
        fun(Revert, {TotalAmount, AccCurrency} = Acc) ->
            Status = w2w_transfer_revert:status(Revert),
            PotentialSucceeded = Status =:= succeeded orelse w2w_transfer_revert:is_active(Revert),
            case PotentialSucceeded of
                true ->
                    {RevertAmount, AccCurrency} = w2w_transfer_revert:body(Revert),
                    {TotalAmount + RevertAmount, AccCurrency};
                false ->
                    Acc
            end
        end,
        {0, Currency},
        Reverts
    ).

%% Adjustment validators

-spec validate_adjustment_start(adjustment_params(), w2w_transfer()) ->
    {ok, valid} |
    {error, start_adjustment_error()}.
validate_adjustment_start(Params, W2WTransfer) ->
    do(fun() ->
        valid = unwrap(validate_no_pending_adjustment(W2WTransfer)),
        valid = unwrap(validate_w2w_transfer_finish(W2WTransfer)),
        valid = unwrap(validate_status_change(Params, W2WTransfer))
    end).

-spec validate_w2w_transfer_finish(w2w_transfer()) ->
    {ok, valid} |
    {error, {invalid_w2w_transfer_status, status()}}.
validate_w2w_transfer_finish(W2WTransfer) ->
    case is_finished(W2WTransfer) of
        true ->
            {ok, valid};
        false ->
            {error, {invalid_w2w_transfer_status, status(W2WTransfer)}}
    end.

-spec validate_no_pending_adjustment(w2w_transfer()) ->
    {ok, valid} |
    {error, {another_adjustment_in_progress, adjustment_id()}}.
validate_no_pending_adjustment(W2WTransfer) ->
    case ff_adjustment_utils:get_not_finished(adjustments_index(W2WTransfer)) of
        error ->
            {ok, valid};
        {ok, AdjustmentID} ->
            {error, {another_adjustment_in_progress, AdjustmentID}}
    end.

-spec validate_status_change(adjustment_params(), w2w_transfer()) ->
    {ok, valid} |
    {error, invalid_status_change_error()}.
validate_status_change(#{change := {change_status, Status}}, W2WTransfer) ->
    do(fun() ->
        valid = unwrap(invalid_status_change, validate_target_status(Status)),
        valid = unwrap(invalid_status_change, validate_change_same_status(Status, status(W2WTransfer)))
    end);
validate_status_change(_Params, _W2WTransfer) ->
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

-spec apply_adjustment_event(wrapped_adjustment_event(), w2w_transfer()) -> w2w_transfer().
apply_adjustment_event(WrappedEvent, W2WTransfer) ->
    Adjustments0 = adjustments_index(W2WTransfer),
    Adjustments1 = ff_adjustment_utils:apply_event(WrappedEvent, Adjustments0),
    set_adjustments_index(Adjustments1, W2WTransfer).

-spec make_adjustment_params(adjustment_params(), w2w_transfer()) ->
    ff_adjustment:params().
make_adjustment_params(Params, W2WTransfer) ->
    #{id := ID, change := Change} = Params,
    genlib_map:compact(#{
        id => ID,
        changes_plan => make_adjustment_change(Change, W2WTransfer),
        external_id => genlib_map:get(external_id, Params),
        domain_revision => operation_domain_revision(W2WTransfer),
        party_revision => operation_party_revision(W2WTransfer),
        operation_timestamp => operation_timestamp(W2WTransfer)
    }).

-spec make_adjustment_change(adjustment_change(), w2w_transfer()) ->
    ff_adjustment:changes().
make_adjustment_change({change_status, NewStatus}, W2WTransfer) ->
    CurrentStatus = status(W2WTransfer),
    make_change_status_params(CurrentStatus, NewStatus, W2WTransfer).

-spec make_change_status_params(status(), status(), w2w_transfer()) ->
    ff_adjustment:changes().
make_change_status_params(succeeded, {failed, _} = NewStatus, W2WTransfer) ->
    CurrentCashFlow = effective_final_cash_flow(W2WTransfer),
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
make_change_status_params({failed, _}, succeeded = NewStatus, W2WTransfer) ->
    CurrentCashFlow = effective_final_cash_flow(W2WTransfer),
    NewCashFlow = make_final_cash_flow(wallet_from_id(W2WTransfer), wallet_to_id(W2WTransfer), body(W2WTransfer)),
    #{
        new_status => #{
            new_status => NewStatus
        },
        new_cash_flow => #{
            old_cash_flow_inverted => ff_cash_flow:inverse(CurrentCashFlow),
            new_cash_flow => NewCashFlow
        }
    };
make_change_status_params({failed, _}, {failed, _} = NewStatus, _W2WTransfer) ->
    #{
        new_status => #{
            new_status => NewStatus
        }
    }.

-spec save_adjustable_info(event(), w2w_transfer()) -> w2w_transfer().
save_adjustable_info({status_changed, Status}, W2WTransfer) ->
    update_adjusment_index(fun ff_adjustment_utils:set_status/2, Status, W2WTransfer);
save_adjustable_info({p_transfer, {status_changed, committed}}, W2WTransfer) ->
    CashFlow = ff_postings_transfer:final_cash_flow(p_transfer(W2WTransfer)),
    update_adjusment_index(fun ff_adjustment_utils:set_cash_flow/2, CashFlow, W2WTransfer);
save_adjustable_info(_Ev, W2WTransfer) ->
    W2WTransfer.

-spec update_adjusment_index(Updater, Value, w2w_transfer()) -> w2w_transfer() when
    Updater :: fun((Value, adjustments_index()) -> adjustments_index()),
    Value :: any().
update_adjusment_index(Updater, Value, W2WTransfer) ->
    Index = adjustments_index(W2WTransfer),
    set_adjustments_index(Updater(Value, Index), W2WTransfer).

%% Helpers

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/w2w_transfer/", ID/binary>>.

-spec build_failure(fail_type(), w2w_transfer()) -> failure().
build_failure(limit_check, W2WTransfer) ->
    {failed, Details} = limit_check_status(W2WTransfer),
    #{
        code => <<"account_limit_exceeded">>,
        reason => genlib:format(Details),
        sub => #{
            code => <<"amount">>
        }
    }.

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
