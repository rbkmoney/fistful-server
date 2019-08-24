%%%
%%% Deposit revert
%%%

-module(ff_deposit_revert).

-define(ACTUAL_FORMAT_VERSION, 1).

-type id()       :: binary().
-type reason()   :: binary().

-opaque revert() :: #{
    version       := ?ACTUAL_FORMAT_VERSION,
    id            := id(),
    body          := body(),
    wallet_id     := wallet_id(),
    source_id     := source_id(),
    status        := status(),
    p_transfer    => p_transfer() | undefined,
    reason        => reason(),
    external_id   => id(),
    limit_checks  => [limit_check_details()]
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
    {status_changed, status()}.

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

-export_type([id/0]).
-export_type([event/0]).
-export_type([reason/0]).
-export_type([status/0]).
-export_type([revert/0]).
-export_type([params/0]).
-export_type([create_error/0]).
-export_type([limit_check_details/0]).

%% Accessors

-export([id/1]).
-export([wallet_id/1]).
-export([source_id/1]).
-export([body/1]).
-export([status/1]).
-export([reason/1]).
-export([external_id/1]).

%% API

-export([create/1]).
-export([is_active/1]).

%% Transfer logic callbacks

-export([process_transfer/1]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/1]).

%% Internal types

-type wallet_id()       :: ff_wallet:id().
-type wallet()          :: ff_wallet:wallet().
-type source_id()       :: ff_source:id().
-type p_transfer()      :: ff_postings_transfer:transfer().
-type body()            :: ff_transaction:body().
-type action()          :: machinery:action() | undefined.
-type process_result()  :: {action(), [event()]}.
-type legacy_event()    :: any().
-type external_id()     :: id().
-type failure()         :: ff_failure:failure().
-type cash()            :: ff_cash:cash().
-type cash_range()      :: ff_range:range(cash()).

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
    {fail, fail_type()} |
    finish.

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
status(#{status := V}) ->
    V.

-spec reason(revert()) -> reason() | undefined.
reason(T) ->
    maps:get(reason, T, undefined).

-spec external_id(revert()) -> external_id() | undefined.
external_id(T) ->
    maps:get(external_id, T, undefined).

-spec p_transfer(revert()) -> p_transfer() | undefined.
p_transfer(T) ->
    maps:get(p_transfer, T, undefined).

%% API

-spec create(params()) ->
    {ok, [event()]}.

create(Params) ->
    #{
        id            := ID,
        wallet_id     := WalletID,
        source_id     := SourceID,
        body          := Body
    } = Params,
    Revert = genlib_map:compact(#{
        version         => ?ACTUAL_FORMAT_VERSION,
        id              => ID,
        body            => Body,
        wallet_id       => WalletID,
        source_id       => SourceID,
        status          => pending,
        reason          => maps:get(reason, Params, undefined),
        external_id     => maps:get(external_id, Params, undefined)
    }),
    {ok, [{created, Revert}]}.

%% Transfer logic callbacks

-spec process_transfer(revert()) ->
    process_result().
process_transfer(Revert) ->
    Activity = deduce_activity(Revert),
    do_process_transfer(Activity, Revert).

-spec is_active(revert()) -> boolean().
is_active(#{status := succeeded}) ->
    false;
is_active(#{status := {failed, _}}) ->
    false;
is_active(#{status := pending}) ->
    true.

%% Events utils

-spec apply_event(event() | legacy_event(), revert() | undefined) ->
    revert().
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev), T).

-spec apply_event_(event(), revert() | undefined) ->
    revert().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, S}, T) ->
    T#{status => S};
apply_event_({limit_check, Details}, T) ->
    add_limit_check(Details, T);
apply_event_({p_transfer, Ev}, T = #{p_transfer := PT}) ->
    T#{p_transfer := ff_postings_transfer:apply_event(Ev, PT)};
apply_event_({p_transfer, Ev}, T) ->
    apply_event({p_transfer, Ev}, T#{p_transfer => undefined}).

-spec maybe_migrate(event() | legacy_event()) ->
    event().
maybe_migrate(Ev) ->
    Ev.

%% Internals

-spec p_transfer_status(revert()) ->
    ff_postings_transfer:status() | undefined.
p_transfer_status(Revert) ->
    case p_transfer(Revert) of
        undefined ->
            undefined;
        #{status := Status} ->
            Status
    end.

-spec deduce_activity(revert()) ->
    activity().
deduce_activity(Revert) ->
    Params = #{
        p_transfer => p_transfer_status(Revert),
        status => status(Revert),
        limit_check => limit_check_status(Revert)
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
    {fail, limit_check}.

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
    process_transfer_finish(Revert).

-spec create_p_transfer(revert()) ->
    process_result().
create_p_transfer(Revert) ->
    {ok, WalletMachine} = ff_wallet_machine:get(wallet_id(Revert)),
    WalletAccount = ff_wallet:account(ff_wallet_machine:wallet(WalletMachine)),
    {ok, SourceMachine} = ff_source:get_machine(source_id(Revert)),
    SourceAccount = ff_source:account(ff_source:get(SourceMachine)),
    Constants = #{
        operation_amount => body(Revert)
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
    PTransferID = construct_p_transfer_id(id(Revert)),
    {ok, PostingsTransferEvents} = ff_postings_transfer:create(PTransferID, FinalCashFlow),
    {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}.

-spec process_limit_check(revert()) ->
    process_result().
process_limit_check(Revert) ->
    Body = body(Revert),
    {ok, WalletMachine} = ff_wallet_machine:get(wallet_id(Revert)),
    Wallet = ff_wallet_machine:wallet(WalletMachine),
    Events = case validate_wallet_limits(Wallet, Body) of
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

-spec validate_wallet_limits(wallet(), cash()) ->
    {ok, valid} |
    {error, validation_error()}.
validate_wallet_limits(Wallet, Body) ->
    case ff_party:validate_wallet_limits(Wallet, Body) of
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
