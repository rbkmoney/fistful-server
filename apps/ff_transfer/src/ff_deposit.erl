%%%
%%% Deposit
%%%

-module(ff_deposit).

-type id()    :: binary().

-define(ACTUAL_FORMAT_VERSION, 2).
-opaque deposit() :: #{
    version         := ?ACTUAL_FORMAT_VERSION,
    id              := id(),
    transfer_type   := deposit,
    body            := body(),
    params          := transfer_params(),
    party_revision  => party_revision(),
    domain_revision => domain_revision(),
    created_at      => ff_time:timestamp_ms(),
    p_transfer      => p_transfer(),
    status          => status(),
    external_id     => id(),
    limit_checks    => [limit_check_details()],
    reverts         => reverts_index(),
    adjustments     => adjustments_index()
}.
-type params() :: #{
    id            := id(),
    body          := ff_transaction:body(),
    source_id     := ff_source:id(),
    wallet_id     := ff_wallet:id(),
    external_id   => external_id()
}.

-type status() ::
    pending |
    succeeded |
    {failed, failure()} .

-type event() ::
    {created, deposit()} |
    {limit_check, limit_check_details()} |
    {p_transfer, ff_postings_transfer:event()} |
    wrapped_revert_event() |
    wrapped_adjustment_event() |
    {status_changed, status()}.

-type limit_check_details() ::
    {wallet_receiver, wallet_limit_check_details()}.

-type wallet_limit_check_details() ::
    ok |
    {failed, wallet_limit_check_error()}.

-type wallet_limit_check_error() :: #{
    expected_range := cash_range(),
    balance := cash()
}.

-type create_error() ::
    {source, notfound | unauthorized} |
    {wallet, notfound} |
    ff_party:validate_deposit_creation_error() |
    {inconsistent_currency, {Deposit :: currency_id(), Source :: currency_id(), Wallet :: currency_id()}}.

-type revert_params() :: #{
    id            := id(),
    body          := body(),
    reason        => binary(),
    external_id   => id()
}.

-type start_revert_error() ::
    invalid_deposit_status_error() |
    {inconsistent_revert_currency, {Revert :: currency_id(), Deposit :: currency_id()}} |
    {insufficient_deposit_amount, {Revert :: body(), Deposit :: body()}} |
    {invalid_revert_amount, Revert :: body()} |
    ff_deposit_revert:create_error().

-type invalid_deposit_status_error() ::
    {invalid_deposit_status, status()}.

-type wrapped_revert_event()  :: ff_deposit_revert_utils:wrapped_event().
-type wrapped_adjustment_event()  :: ff_adjustment_utils:wrapped_event().

-type revert_adjustment_params() :: ff_deposit_revert:adjustment_params().

-type start_revert_adjustment_error() ::
    ff_deposit_revert:start_adjustment_error() |
    unknown_revert_error().

-type unknown_revert_error() :: ff_deposit_revert_utils:unknown_revert_error().

-type adjustment_params() :: #{
    id          := adjustment_id(),
    change      := adjustment_change(),
    external_id => id()
}.

-type adjustment_change() ::
    {change_status, status()}.

-type start_adjustment_error() ::
    invalid_deposit_status_error() |
    invalid_status_change_error() |
    {another_adjustment_in_progress, adjustment_id()} |
    ff_adjustment:create_error().

-type unknown_adjustment_error() :: ff_adjustment_utils:unknown_adjustment_error().

-type invalid_status_change_error() ::
    {invalid_status_change, {unavailable_status, status()}} |
    {invalid_status_change, {already_has_status, status()}}.

-export_type([deposit/0]).
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

-export([wallet_id/1]).
-export([source_id/1]).
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
-export([maybe_migrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Internal types

-type account()               :: ff_account:account().
-type process_result()        :: {action(), [event()]}.
-type cash_flow_plan()        :: ff_cash_flow:cash_flow_plan().
-type source_id()             :: ff_source:id().
-type source()                :: ff_source:source().
-type wallet_id()             :: ff_wallet:id().
-type wallet()                :: ff_wallet:wallet().
-type revert()                :: ff_deposit_revert:revert().
-type revert_id()             :: ff_deposit_revert:id().
-type body()                  :: ff_transaction:body().
-type cash()                  :: ff_cash:cash().
-type cash_range()            :: ff_range:range(cash()).
-type action()                :: machinery:action() | undefined.
-type p_transfer()            :: ff_postings_transfer:transfer().
-type currency_id()           :: ff_currency:id().
-type external_id()           :: id().
-type legacy_event()          :: any().
-type reverts_index()         :: ff_deposit_revert_utils:index().
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

-type transfer_params() :: #{
    source_id             := source_id(),
    wallet_id             := wallet_id(),
    wallet_account        := account(),
    source_account        := account(),
    wallet_cash_flow_plan := cash_flow_plan()
}.

-type activity() ::
    p_transfer_start |
    p_transfer_prepare |
    p_transfer_commit |
    p_transfer_cancel |
    limit_check |
    revert |
    adjustment |
    {fail, fail_type()} |
    stop |  % Legacy activity.
    finish.

-type fail_type() ::
    limit_check.

%% Accessors

-spec id(deposit()) -> id().
id(#{id := V}) ->
    V.

-spec wallet_id(deposit()) -> wallet_id().
wallet_id(T) ->
    maps:get(wallet_id, params(T)).

-spec source_id(deposit()) -> source_id().
source_id(T) ->
    maps:get(source_id, params(T)).

-spec body(deposit()) -> body().
body(#{body := V}) ->
    V.

-spec status(deposit()) -> status() | undefined.
status(Deposit) ->
    maps:get(status, Deposit, undefined).

-spec p_transfer(deposit())  -> p_transfer() | undefined.
p_transfer(Deposit) ->
    maps:get(p_transfer, Deposit, undefined).

-spec external_id(deposit()) -> external_id() | undefined.
external_id(Deposit) ->
    maps:get(external_id, Deposit, undefined).

-spec party_revision(deposit()) -> party_revision() | undefined.
party_revision(T) ->
    maps:get(party_revision, T, undefined).

-spec domain_revision(deposit()) -> domain_revision() | undefined.
domain_revision(T) ->
    maps:get(domain_revision, T, undefined).

-spec created_at(deposit()) -> ff_time:timestamp_ms() | undefined.
created_at(T) ->
    maps:get(created_at, T, undefined).

%% API

-spec create(params()) ->
    {ok, [event()]} |
    {error, create_error()}.
create(Params) ->
    do(fun() ->
        #{id := ID, source_id := SourceID, wallet_id := WalletID, body := Body} = Params,
        Source = ff_source:get(unwrap(source, ff_source:get_machine(SourceID))),
        CreatedAt = ff_time:now(),
        DomainRevision = ff_domain_config:head(),
        Wallet = unwrap(wallet, get_wallet(WalletID)),
        Identity = get_wallet_identity(Wallet),
        PartyID = ff_identity:party(Identity),
        {ok, PartyRevision} = ff_party:get_revision(PartyID),
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
        valid =  unwrap(validate_deposit_creation(Terms, Params, Source, Wallet)),
        ExternalID = maps:get(external_id, Params, undefined),
        TransferParams = #{
            wallet_id             => WalletID,
            source_id             => SourceID,
            wallet_account        => ff_wallet:account(Wallet),
            source_account        => ff_source:account(Source),
            wallet_cash_flow_plan => #{
                postings => [
                    #{
                        sender   => {wallet, sender_source},
                        receiver => {wallet, receiver_settlement},
                        volume   => {share, {{1, 1}, operation_amount, default}}
                    }
                ]
            }
        },
        [
            {created, add_external_id(ExternalID, #{
                version         => ?ACTUAL_FORMAT_VERSION,
                id              => ID,
                transfer_type   => deposit,
                body            => Body,
                params          => TransferParams,
                party_revision  => PartyRevision,
                domain_revision => DomainRevision,
                created_at      => CreatedAt
            })},
            {status_changed, pending}
        ]
    end).

-spec start_revert(revert_params(), deposit()) ->
    {ok, process_result()} |
    {error, start_revert_error()}.
start_revert(Params, Deposit) ->
    #{id := RevertID} = Params,
    case find_revert(RevertID, Deposit) of
        {error, {unknown_revert, _}} ->
            do_start_revert(Params, Deposit);
        {ok, _Revert} ->
            {ok, {undefined, []}}
    end.

-spec start_revert_adjustment(revert_id(), revert_adjustment_params(), deposit()) ->
    {ok, process_result()} |
    {error, start_revert_adjustment_error()}.
start_revert_adjustment(RevertID, Params, Deposit) ->
    do(fun() ->
        Revert = unwrap(find_revert(RevertID, Deposit)),
        {Action, Events} = unwrap(ff_deposit_revert:start_adjustment(Params, Revert)),
        {Action, ff_deposit_revert_utils:wrap_events(RevertID, Events)}
    end).

-spec find_revert(revert_id(), deposit()) ->
    {ok, revert()} | {error, unknown_revert_error()}.
find_revert(RevertID, Deposit) ->
    ff_deposit_revert_utils:get_by_id(RevertID, reverts_index(Deposit)).

-spec reverts(deposit()) -> [revert()].
reverts(Deposit) ->
    ff_deposit_revert_utils:reverts(reverts_index(Deposit)).

-spec start_adjustment(adjustment_params(), deposit()) ->
    {ok, process_result()} |
    {error, start_adjustment_error()}.
start_adjustment(Params, Deposit) ->
    #{id := AdjustmentID} = Params,
    case find_adjustment(AdjustmentID, Deposit) of
        {error, {unknown_adjustment, _}} ->
            do_start_adjustment(Params, Deposit);
        {ok, _Adjustment} ->
            {ok, {undefined, []}}
    end.

-spec find_adjustment(adjustment_id(), deposit()) ->
    {ok, adjustment()} | {error, unknown_adjustment_error()}.
find_adjustment(AdjustmentID, Deposit) ->
    ff_adjustment_utils:get_by_id(AdjustmentID, adjustments_index(Deposit)).

-spec adjustments(deposit()) -> [adjustment()].
adjustments(Deposit) ->
    ff_adjustment_utils:adjustments(adjustments_index(Deposit)).

-spec effective_final_cash_flow(deposit()) -> final_cash_flow().
effective_final_cash_flow(Deposit) ->
    case ff_adjustment_utils:cash_flow(adjustments_index(Deposit)) of
        undefined ->
            ff_cash_flow:make_empty_final();
        CashFlow ->
            CashFlow
    end.

-spec process_transfer(deposit()) ->
    process_result().
process_transfer(Deposit) ->
    Activity = deduce_activity(Deposit),
    do_process_transfer(Activity, Deposit).

%% Сущность в настоящий момент нуждается в передаче ей управления для совершения каких-то действий
-spec is_active(deposit()) -> boolean().
is_active(#{status := succeeded} = Deposit) ->
    is_childs_active(Deposit);
is_active(#{status := {failed, _}} = Deposit) ->
    is_childs_active(Deposit);
is_active(#{status := pending}) ->
    true.

%% Сущность завершила свою основную задачу по переводу денег. Дальше её состояние будет меняться только
%% изменением дочерних сущностей, например запуском adjustment.
-spec is_finished(deposit()) -> boolean().
is_finished(#{status := succeeded}) ->
    true;
is_finished(#{status := {failed, _}}) ->
    true;
is_finished(#{status := pending}) ->
    false.

%% Events utils

-spec apply_event(event() | legacy_event(), deposit() | undefined) ->
    deposit().
apply_event(Ev, T0) ->
    T1 = apply_event_(Ev, T0),
    T2 = save_adjustable_info(Ev, T1),
    T2.

-spec apply_event_(event(), deposit() | undefined) ->
    deposit().
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

-spec do_start_revert(revert_params(), deposit()) ->
    {ok, process_result()} |
    {error, start_revert_error()}.
do_start_revert(Params, Deposit) ->
    do(fun() ->
        valid = unwrap(validate_revert_start(Params, Deposit)),
        RevertParams = Params#{
            wallet_id => wallet_id(Deposit),
            source_id => source_id(Deposit)
        },
        #{id := RevertID} = Params,
        {Action, Events} = unwrap(ff_deposit_revert:create(RevertParams)),
        {Action, ff_deposit_revert_utils:wrap_events(RevertID, Events)}
    end).

-spec do_start_adjustment(adjustment_params(), deposit()) ->
    {ok, process_result()} |
    {error, start_adjustment_error()}.
do_start_adjustment(Params, Deposit) ->
    do(fun() ->
        valid = unwrap(validate_adjustment_start(Params, Deposit)),
        AdjustmentParams = make_adjustment_params(Params, Deposit),
        #{id := AdjustmentID} = Params,
        {Action, Events} = unwrap(ff_adjustment:create(AdjustmentParams)),
        {Action, ff_adjustment_utils:wrap_events(AdjustmentID, Events)}
    end).

-spec params(deposit()) -> transfer_params().
params(#{params := V}) ->
    V.

add_external_id(undefined, Event) ->
    Event;
add_external_id(ExternalID, Event) ->
    Event#{external_id => ExternalID}.

-spec deduce_activity(deposit()) ->
    activity().
deduce_activity(Deposit) ->
    Params = #{
        p_transfer => p_transfer_status(Deposit),
        status => status(Deposit),
        limit_check => limit_check_status(Deposit),
        active_revert => ff_deposit_revert_utils:is_active(reverts_index(Deposit)),
        active_adjustment => ff_adjustment_utils:is_active(adjustments_index(Deposit))
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
    adjustment;

%% Legacy activity. Remove after first deployment
do_deduce_activity(#{status := {failed, _}, p_transfer := prepared}) ->
    p_transfer_cancel;
do_deduce_activity(#{status := succeeded, p_transfer := prepared}) ->
    p_transfer_commit;
do_deduce_activity(#{status := succeeded, p_transfer := committed, active_revert := false}) ->
    stop;
do_deduce_activity(#{status := {failed, _}, p_transfer := cancelled, active_revert := false}) ->
    stop.

-spec do_process_transfer(activity(), deposit()) ->
    process_result().
do_process_transfer(p_transfer_start, Deposit) ->
    create_p_transfer(Deposit);
do_process_transfer(p_transfer_prepare, Deposit) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Deposit, fun ff_postings_transfer:prepare/1),
    {continue, Events};
do_process_transfer(p_transfer_commit, Deposit) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Deposit, fun ff_postings_transfer:commit/1),
    {continue, Events};
do_process_transfer(p_transfer_cancel, Deposit) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Deposit, fun ff_postings_transfer:cancel/1),
    {continue, Events};
do_process_transfer(limit_check, Deposit) ->
    process_limit_check(Deposit);
do_process_transfer({fail, Reason}, Deposit) ->
    process_transfer_fail(Reason, Deposit);
do_process_transfer(finish, Deposit) ->
    process_transfer_finish(Deposit);
do_process_transfer(revert, Deposit) ->
    Result = ff_deposit_revert_utils:process_reverts(reverts_index(Deposit)),
    handle_child_result(Result, Deposit);
do_process_transfer(adjustment, Deposit) ->
    process_adjustment(Deposit);
do_process_transfer(stop, _Deposit) ->
    {undefined, []}.

-spec create_p_transfer(deposit()) ->
    process_result().
create_p_transfer(Deposit) ->
    FinalCashFlow = make_final_cash_flow(wallet_id(Deposit), source_id(Deposit), body(Deposit)),
    PTransferID = construct_p_transfer_id(id(Deposit)),
    {ok, PostingsTransferEvents} = ff_postings_transfer:create(PTransferID, FinalCashFlow),
    {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}.

-spec process_limit_check(deposit()) ->
    process_result().
process_limit_check(Deposit) ->
    Body = body(Deposit),
    WalletID = wallet_id(Deposit),
    DomainRevision = operation_domain_revision(Deposit),
    {ok, Wallet} = get_wallet(WalletID),
    Identity = get_wallet_identity(Wallet),
    PartyID = ff_identity:party(Identity),
    PartyRevision = operation_party_revision(Deposit),
    ContractID = ff_identity:contract(Identity),
    {_Amount, Currency} = Body,
    Timestamp = operation_timestamp(Deposit),
    Varset = genlib_map:compact(#{
        currency => ff_dmsl_codec:marshal(currency_ref, Currency),
        cost => ff_dmsl_codec:marshal(cash, Body),
        wallet_id => WalletID
    }),
    {ok, Terms} = ff_party:get_contract_terms(
        PartyID, ContractID, Varset, Timestamp, PartyRevision, DomainRevision
    ),
    Clock = ff_postings_transfer:clock(p_transfer(Deposit)),
    Events = case validate_wallet_limits(Terms, Wallet, Clock) of
        {ok, valid} ->
            [{limit_check, {wallet_receiver, ok}}];
        {error, {terms_violation, {wallet_limit, {cash_range, {Cash, Range}}}}} ->
            Details = #{
                expected_range => Range,
                balance => Cash
            },
            [{limit_check, {wallet_receiver, {failed, Details}}}]
    end,
    {continue, Events}.

-spec process_transfer_finish(deposit()) ->
    process_result().
process_transfer_finish(_Deposit) ->
    {undefined, [{status_changed, succeeded}]}.

-spec process_transfer_fail(fail_type(), deposit()) ->
    process_result().
process_transfer_fail(limit_check, Deposit) ->
    Failure = build_failure(limit_check, Deposit),
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
                sender   => {wallet, sender_source},
                receiver => {wallet, receiver_settlement},
                volume   => {share, {{1, 1}, operation_amount, default}}
            }
        ]
    },
    {ok, FinalCashFlow} = ff_cash_flow:finalize(CashFlowPlan, Accounts, Constants),
    FinalCashFlow.

-spec handle_child_result(process_result(), deposit()) -> process_result().
handle_child_result({undefined, Events} = Result, Deposit) ->
    NextDeposit = lists:foldl(fun(E, Acc) -> apply_event(E, Acc) end, Deposit, Events),
    case is_active(NextDeposit) of
        true ->
            {continue, Events};
        false ->
            Result
    end;
handle_child_result({_OtherAction, _Events} = Result, _Deposit) ->
    Result.

%% Internal getters and setters

-spec p_transfer_status(deposit()) -> ff_postings_transfer:status() | undefined.
p_transfer_status(Deposit) ->
    case p_transfer(Deposit) of
        undefined ->
            undefined;
        Transfer ->
            ff_postings_transfer:status(Transfer)
    end.

-spec adjustments_index(deposit()) -> adjustments_index().
adjustments_index(Deposit) ->
    case maps:find(adjustments, Deposit) of
        {ok, Adjustments} ->
            Adjustments;
        error ->
            ff_adjustment_utils:new_index()
    end.

-spec set_adjustments_index(adjustments_index(), deposit()) -> deposit().
set_adjustments_index(Adjustments, Deposit) ->
    Deposit#{adjustments => Adjustments}.

-spec is_childs_active(deposit()) -> boolean().
is_childs_active(Deposit) ->
    ff_adjustment_utils:is_active(adjustments_index(Deposit)) orelse
        ff_deposit_revert_utils:is_active(reverts_index(Deposit)).

-spec operation_timestamp(deposit()) -> ff_time:timestamp_ms().
operation_timestamp(Deposit) ->
    ff_maybe:get_defined(created_at(Deposit), ff_time:now()).

-spec operation_party_revision(deposit()) ->
    domain_revision().
operation_party_revision(Deposit) ->
    case party_revision(Deposit) of
        undefined ->
            {ok, Wallet} = get_wallet(wallet_id(Deposit)),
            PartyID = ff_identity:party(get_wallet_identity(Wallet)),
            {ok, Revision} = ff_party:get_revision(PartyID),
            Revision;
        Revision ->
            Revision
    end.

-spec operation_domain_revision(deposit()) ->
    domain_revision().
operation_domain_revision(Deposit) ->
    case domain_revision(Deposit) of
        undefined ->
            ff_domain_config:head();
        Revision ->
            Revision
    end.

%% Deposit validators

-spec validate_deposit_creation(terms(), params(), source(), wallet()) ->
    {ok, valid} |
    {error, create_error()}.
validate_deposit_creation(Terms, Params, Source, Wallet) ->
    #{body := Body} = Params,
    do(fun() ->
        valid = unwrap(ff_party:validate_deposit_creation(Terms, Body)),
        valid = unwrap(validate_deposit_currency(Body, Source, Wallet)),
        valid = unwrap(validate_source_status(Source))
    end).

-spec validate_deposit_currency(body(), source(), wallet()) ->
    {ok, valid} |
    {error, {inconsistent_currency, {currency_id(), currency_id(), currency_id()}}}.
validate_deposit_currency(Body, Source, Wallet) ->
    SourceCurrencyID = ff_account:currency(ff_source:account(Source)),
    WalletCurrencyID = ff_account:currency(ff_wallet:account(Wallet)),
    case Body of
        {_Amount, DepositCurencyID} when
            DepositCurencyID =:= SourceCurrencyID andalso
            DepositCurencyID =:= WalletCurrencyID
        ->
            {ok, valid};
        {_Amount, DepositCurencyID} ->
            {error, {inconsistent_currency, {DepositCurencyID, SourceCurrencyID, WalletCurrencyID}}}
    end.

-spec validate_source_status(source()) ->
    {ok, valid} |
    {error, {source, ff_source:status()}}.
validate_source_status(Source) ->
    case ff_source:status(Source) of
        authorized ->
            {ok, valid};
        unauthorized ->
            {error, {source, unauthorized}}
    end.

%% Limit helpers

-spec limit_checks(deposit()) ->
    [limit_check_details()].
limit_checks(Deposit) ->
    maps:get(limit_checks, Deposit, []).

-spec add_limit_check(limit_check_details(), deposit()) ->
    deposit().
add_limit_check(Check, Deposit) ->
    Checks = limit_checks(Deposit),
    Deposit#{limit_checks => [Check | Checks]}.

-spec limit_check_status(deposit()) ->
    ok | {failed, limit_check_details()} | unknown.
limit_check_status(#{limit_checks := Checks}) ->
    case lists:dropwhile(fun is_limit_check_ok/1, Checks) of
        [] ->
            ok;
        [H | _Tail] ->
            {failed, H}
    end;
limit_check_status(Deposit) when not is_map_key(limit_checks, Deposit) ->
    unknown.

-spec is_limit_check_ok(limit_check_details()) -> boolean().
is_limit_check_ok({wallet_receiver, ok}) ->
    true;
is_limit_check_ok({wallet_receiver, {failed, _Details}}) ->
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

-spec validate_revert_start(revert_params(), deposit()) ->
    {ok, valid} |
    {error, start_revert_error()}.
validate_revert_start(Params, Deposit) ->
    do(fun() ->
        valid = unwrap(validate_deposit_success(Deposit)),
        valid = unwrap(validate_revert_body(Params, Deposit))
    end).

-spec validate_revert_body(revert_params(), deposit()) -> {ok, valid} | {error, Error} when
    Error :: CurrencyError | AmountError,
    CurrencyError :: {inconsistent_revert_currency, {Revert :: currency_id(), Deposit :: currency_id()}},
    AmountError :: {insufficient_deposit_amount, {Revert :: body(), Deposit :: body()}}.
validate_revert_body(Params, Deposit) ->
    do(fun() ->
        valid = unwrap(validate_revert_currency(Params, Deposit)),
        valid = unwrap(validate_revert_amount(Params)),
        valid = unwrap(validate_unreverted_amount(Params, Deposit))
    end).

-spec validate_deposit_success(deposit()) ->
    {ok, valid} |
    {error, invalid_deposit_status_error()}.
validate_deposit_success(Deposit) ->
    case status(Deposit) of
        succeeded ->
            {ok, valid};
        Other ->
            {error, {invalid_deposit_status, Other}}
    end.

-spec validate_revert_currency(revert_params(), deposit()) ->
    {ok, valid} |
    {error, {inconsistent_revert_currency, {Revert :: currency_id(), Deposit :: currency_id()}}}.
validate_revert_currency(Params, Deposit) ->
    {_InitialAmount, DepositCurrency} = body(Deposit),
    #{body := {_Amount, RevertCurrency}} = Params,
    case {RevertCurrency, DepositCurrency} of
        {SameCurrency, SameCurrency} ->
            {ok, valid};
        _Other ->
            {error, {inconsistent_revert_currency, {RevertCurrency, DepositCurrency}}}
    end.

-spec validate_unreverted_amount(revert_params(), deposit()) ->
    {ok, valid} |
    {error, {insufficient_deposit_amount, {Revert :: body(), Deposit :: body()}}}.
validate_unreverted_amount(Params, Deposit) ->
    {InitialAmount, Currency} = body(Deposit),
    #{body := {RevertAmount, Currency} = RevertBody} = Params,
    {TotalReverted, Currency} = max_reverted_body_total(Deposit),
    case InitialAmount - TotalReverted - RevertAmount of
        UnrevertedAmount when UnrevertedAmount >= 0 ->
            {ok, valid};
        _Other ->
            Unreverted = {InitialAmount - TotalReverted, Currency},
            {error, {insufficient_deposit_amount, {RevertBody, Unreverted}}}
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

-spec reverts_index(deposit()) -> reverts_index().
reverts_index(Deposit) ->
    case maps:find(reverts, Deposit) of
        {ok, Reverts} ->
            Reverts;
        error ->
            ff_deposit_revert_utils:new_index()
    end.

-spec set_reverts_index(reverts_index(), deposit()) -> deposit().
set_reverts_index(Reverts, Deposit) ->
    Deposit#{reverts => Reverts}.

-spec apply_revert_event(wrapped_revert_event(), deposit()) -> deposit().
apply_revert_event(WrappedEvent, Deposit) ->
    Reverts0 = reverts_index(Deposit),
    Reverts1 = ff_deposit_revert_utils:apply_event(WrappedEvent, Reverts0),
    set_reverts_index(Reverts1, Deposit).

-spec max_reverted_body_total(deposit()) -> body().
max_reverted_body_total(Deposit) ->
    Reverts = ff_deposit_revert_utils:reverts(reverts_index(Deposit)),
    {_InitialAmount, Currency} = body(Deposit),
    lists:foldl(
        fun(Revert, {TotalAmount, AccCurrency} = Acc) ->
            Status = ff_deposit_revert:status(Revert),
            PotentialSucceeded = Status =:= succeeded orelse ff_deposit_revert:is_active(Revert),
            case PotentialSucceeded of
                true ->
                    {RevertAmount, AccCurrency} = ff_deposit_revert:body(Revert),
                    {TotalAmount + RevertAmount, AccCurrency};
                false ->
                    Acc
            end
        end,
        {0, Currency},
        Reverts
    ).

%% Adjustment validators

-spec validate_adjustment_start(adjustment_params(), deposit()) ->
    {ok, valid} |
    {error, start_adjustment_error()}.
validate_adjustment_start(Params, Deposit) ->
    do(fun() ->
        valid = unwrap(validate_no_pending_adjustment(Deposit)),
        valid = unwrap(validate_deposit_finish(Deposit)),
        valid = unwrap(validate_status_change(Params, Deposit))
    end).

-spec validate_deposit_finish(deposit()) ->
    {ok, valid} |
    {error, {invalid_deposit_status, status()}}.
validate_deposit_finish(Deposit) ->
    case is_finished(Deposit) of
        true ->
            {ok, valid};
        false ->
            {error, {invalid_deposit_status, status(Deposit)}}
    end.

-spec validate_no_pending_adjustment(deposit()) ->
    {ok, valid} |
    {error, {another_adjustment_in_progress, adjustment_id()}}.
validate_no_pending_adjustment(Deposit) ->
    case ff_adjustment_utils:get_not_finished(adjustments_index(Deposit)) of
        error ->
            {ok, valid};
        {ok, AdjustmentID} ->
            {error, {another_adjustment_in_progress, AdjustmentID}}
    end.

-spec validate_status_change(adjustment_params(), deposit()) ->
    {ok, valid} |
    {error, invalid_status_change_error()}.
validate_status_change(#{change := {change_status, Status}}, Deposit) ->
    do(fun() ->
        valid = unwrap(invalid_status_change, validate_target_status(Status)),
        valid = unwrap(invalid_status_change, validate_change_same_status(Status, status(Deposit)))
    end);
validate_status_change(_Params, _Deposit) ->
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

-spec apply_adjustment_event(wrapped_adjustment_event(), deposit()) -> deposit().
apply_adjustment_event(WrappedEvent, Deposit) ->
    Adjustments0 = adjustments_index(Deposit),
    Adjustments1 = ff_adjustment_utils:apply_event(WrappedEvent, Adjustments0),
    set_adjustments_index(Adjustments1, Deposit).

-spec make_adjustment_params(adjustment_params(), deposit()) ->
    ff_adjustment:params().
make_adjustment_params(Params, Deposit) ->
    #{id := ID, change := Change} = Params,
    genlib_map:compact(#{
        id => ID,
        changes_plan => make_adjustment_change(Change, Deposit),
        external_id => genlib_map:get(external_id, Params),
        domain_revision => operation_domain_revision(Deposit),
        party_revision => operation_party_revision(Deposit),
        operation_timestamp => operation_timestamp(Deposit)
    }).

-spec make_adjustment_change(adjustment_change(), deposit()) ->
    ff_adjustment:changes().
make_adjustment_change({change_status, NewStatus}, Deposit) ->
    CurrentStatus = status(Deposit),
    make_change_status_params(CurrentStatus, NewStatus, Deposit).

-spec make_change_status_params(status(), status(), deposit()) ->
    ff_adjustment:changes().
make_change_status_params(succeeded, {failed, _} = NewStatus, Deposit) ->
    CurrentCashFlow = effective_final_cash_flow(Deposit),
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
make_change_status_params({failed, _}, succeeded = NewStatus, Deposit) ->
    CurrentCashFlow = effective_final_cash_flow(Deposit),
    NewCashFlow = make_final_cash_flow(wallet_id(Deposit), source_id(Deposit), body(Deposit)),
    #{
        new_status => #{
            new_status => NewStatus
        },
        new_cash_flow => #{
            old_cash_flow_inverted => ff_cash_flow:inverse(CurrentCashFlow),
            new_cash_flow => NewCashFlow
        }
    };
make_change_status_params({failed, _}, {failed, _} = NewStatus, _Deposit) ->
    #{
        new_status => #{
            new_status => NewStatus
        }
    }.

-spec process_adjustment(deposit()) ->
    process_result().
process_adjustment(Deposit) ->
    #{
        action := Action,
        events := Events0,
        changes := Changes
    } = ff_adjustment_utils:process_adjustments(adjustments_index(Deposit)),
    Events1 = Events0 ++ handle_adjustment_changes(Changes),
    handle_child_result({Action, Events1}, Deposit).

-spec handle_adjustment_changes(ff_adjustment:changes()) ->
    [event()].
handle_adjustment_changes(Changes) ->
    StatusChange = maps:get(new_status, Changes, undefined),
    handle_adjustment_status_change(StatusChange).

-spec handle_adjustment_status_change(ff_adjustment:status_change() | undefined) ->
    [event()].
handle_adjustment_status_change(undefined) ->
    [];
handle_adjustment_status_change(#{new_status := Status}) ->
    [{status_changed, Status}].

-spec save_adjustable_info(event(), deposit()) -> deposit().
save_adjustable_info({p_transfer, {status_changed, committed}}, Deposit) ->
    CashFlow = ff_postings_transfer:final_cash_flow(p_transfer(Deposit)),
    update_adjusment_index(fun ff_adjustment_utils:set_cash_flow/2, CashFlow, Deposit);
save_adjustable_info(_Ev, Deposit) ->
    Deposit.

-spec update_adjusment_index(Updater, Value, deposit()) -> deposit() when
    Updater :: fun((Value, adjustments_index()) -> adjustments_index()),
    Value :: any().
update_adjusment_index(Updater, Value, Deposit) ->
    Index = adjustments_index(Deposit),
    set_adjustments_index(Updater(Value, Index), Deposit).

%% Helpers

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/deposit/", ID/binary>>.

-spec build_failure(fail_type(), deposit()) -> failure().
build_failure(limit_check, Deposit) ->
    {failed, Details} = limit_check_status(Deposit),
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

%% Migration

-spec maybe_migrate(event() | legacy_event(), ff_machine:migrate_params()) ->
    event().
% Actual events
maybe_migrate(Ev = {created, #{version := ?ACTUAL_FORMAT_VERSION}}, _MigrateParams) ->
    Ev;
maybe_migrate(Ev = {status_changed, {failed, #{code := _}}}, _MigrateParams) ->
    Ev;
maybe_migrate(Ev = {limit_check, {wallet_receiver, _Details}}, _MigrateParams) ->
    Ev;
maybe_migrate({p_transfer, PEvent}, _MigrateParams) ->
    {p_transfer, ff_postings_transfer:maybe_migrate(PEvent, deposit)};
maybe_migrate({revert, _Payload} = Event, _MigrateParams) ->
    ff_deposit_revert_utils:maybe_migrate(Event);
maybe_migrate({adjustment, _Payload} = Event, _MigrateParams) ->
    ff_adjustment_utils:maybe_migrate(Event);

% Old events
maybe_migrate({limit_check, {wallet, Details}}, MigrateParams) ->
    maybe_migrate({limit_check, {wallet_receiver, Details}}, MigrateParams);
maybe_migrate({created, #{version := 1, handler := ff_deposit} = T}, MigrateParams) ->
    #{
        version     := 1,
        id          := ID,
        handler     := ff_deposit,
        source      := SourceAccount,
        destination := DestinationAccount,
        body        := Body,
        params      := #{
            destination := DestinationID,
            source      := SourceID
        }
    } = T,
    maybe_migrate({created, #{
        version       => 2,
        id            => ID,
        transfer_type => deposit,
        body          => Body,
        params        => #{
            wallet_id             => DestinationID,
            source_id             => SourceID,
            wallet_account        => DestinationAccount,
            source_account        => SourceAccount,
            wallet_cash_flow_plan => #{
                postings => [
                    #{
                        sender   => {wallet, sender_source},
                        receiver => {wallet, receiver_settlement},
                        volume   => {share, {{1, 1}, operation_amount, default}}
                    }
                ]
            }
        }
    }}, MigrateParams);
maybe_migrate({transfer, PTransferEv}, MigrateParams) ->
    maybe_migrate({p_transfer, PTransferEv}, MigrateParams);
maybe_migrate({status_changed, {failed, LegacyFailure}}, MigrateParams) ->
    Failure = #{
        code => <<"unknown">>,
        reason => genlib:format(LegacyFailure)
    },
    maybe_migrate({status_changed, {failed, Failure}}, MigrateParams);
% Other events
maybe_migrate(Ev, _MigrateParams) ->
    Ev.
