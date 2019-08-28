%%%
%%% Deposit
%%%

-module(ff_deposit).

-type id() :: binary().

-define(ACTUAL_FORMAT_VERSION, 2).
-opaque deposit() :: #{
    version       := ?ACTUAL_FORMAT_VERSION,
    id            := id(),
    transfer_type := deposit,
    body          := body(),
    params        := transfer_params(),
    p_transfer    => p_transfer() | undefined,
    status        => status(),
    external_id   => id(),
    reverts       => reverts()
}.
-type params() :: #{
    id            := id(),
    body          := ff_transaction:body(),
    source_id     := ff_source:id(),
    wallet_id     := ff_wallet:id(),
    external_id   => external_id()
}.

-type status() ::
    pending         |
    succeeded       |
    {failed, _TODO} .

-type event() ::
    {created, deposit()} |
    {p_transfer, ff_postings_transfer:event()} |
    wrapped_revert_event() |
    {status_changed, status()}.

-type create_error() ::
    {source, notfound | unauthorized} |
    {wallet, notfound} |
    ff_party:validate_deposit_creation_error().

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

-type process_error() ::
    {terms_violation, {wallet_limit, {cash_range, {cash(), cash_range()}}}}.

-type revert_adjustment_params() :: ff_deposit_revert:adjustment_params().

-type start_revert_adjustment_error() ::
    ff_deposit_revert:start_adjustment_error() |
    unknown_revert_error().

-type unknown_revert_error() :: ff_deposit_revert_utils:unknown_revert_error().

-export_type([deposit/0]).
-export_type([id/0]).
-export_type([params/0]).
-export_type([revert_params/0]).
-export_type([event/0]).
-export_type([wrapped_revert_event/0]).
-export_type([create_error/0]).
-export_type([start_revert_error/0]).
-export_type([revert_adjustment_params/0]).
-export_type([start_revert_adjustment_error/0]).
-export_type([process_error/0]).

%% Accessors

-export([wallet_id/1]).
-export([source_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([external_id/1]).

%% API
-export([create/1]).
-export([start_revert/2]).
-export([start_revert_adjustment/3]).

-export([find_revert/2]).
-export([reverts/1]).

%% Transfer logic callbacks
-export([process_transfer/1]).
-export([process_failure/2]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type account()               :: ff_account:account().
-type process_result()        :: {action(), [event()]}.
-type cash_flow_plan()        :: ff_cash_flow:cash_flow_plan().
-type source_id()             :: ff_source:id().
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
-type reverts()               :: ff_deposit_revert_utils:index().

-type transfer_params() :: #{
    source_id             := source_id(),
    wallet_id             := wallet_id(),
    wallet_account        := account(),
    source_account        := account(),
    wallet_cash_flow_plan := cash_flow_plan()
}.

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
status(#{status := V}) ->
    V;
status(_Other) ->
    undefined.

-spec p_transfer(deposit())  -> p_transfer() | undefined.
p_transfer(#{p_transfer := V}) ->
    V;
p_transfer(_Other) ->
    undefined.

-spec external_id(deposit()) -> external_id() | undefined.
external_id(#{external_id := V}) ->
    V;
external_id(_Transfer) ->
    undefined.

%% API

-spec create(params()) ->
    {ok, [event()]} |
    {error, create_error()}.
create(Params) ->
    do(fun() ->
        #{id := ID, source_id := SourceID, wallet_id := WalletID, body := Body} = Params,
        Source = ff_source:get(unwrap(source, ff_source:get_machine(SourceID))),
        Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
        valid =  unwrap(ff_party:validate_deposit_creation(Wallet, Body)),
        ok = unwrap(source, valid(authorized, ff_source:status(Source))),
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
                version       => ?ACTUAL_FORMAT_VERSION,
                id            => ID,
                transfer_type => deposit,
                body          => Body,
                params        => TransferParams
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

%% transfer logic callbacks

-spec process_transfer(deposit()) ->
    {ok, process_result()} |
    {error, _Reason}.
process_transfer(Deposit) ->
    Activity = deduce_activity(Deposit),
    do_process_transfer(Activity, Deposit).

-spec process_failure(any(), deposit()) ->
    {ok, process_result()}.
process_failure(Reason, Deposit) ->
    {ok, ShutdownEvents} = do_process_failure(Reason, Deposit),
    {ok, {undefined, ShutdownEvents ++ [{status_changed, {failed, Reason}}]}}.

%% Events utils

-spec apply_event(event() | legacy_event(), deposit() | undefined) ->
    deposit().
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev), T).

-spec apply_event_(event(), deposit() | undefined) ->
    deposit().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, S}, T) ->
    maps:put(status, S, T);
apply_event_({p_transfer, Ev}, T = #{p_transfer := PT}) ->
    T#{p_transfer := ff_postings_transfer:apply_event(Ev, PT)};
apply_event_({revert, _Ev} = Event, T) ->
    apply_revert_event(Event, T);
apply_event_({p_transfer, Ev}, T) ->
    apply_event({p_transfer, Ev}, T#{p_transfer => undefined}).

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

-spec params(deposit()) -> transfer_params().
params(#{params := V}) ->
    V.

add_external_id(undefined, Event) ->
    Event;
add_external_id(ExternalID, Event) ->
    Event#{external_id => ExternalID}.

-type activity() ::
    p_transfer_start |
    p_transfer_prepare |
    p_transfer_commit |
    p_transfer_cancel |
    revert |
    finish.

-spec deduce_activity(deposit()) ->
    activity().
deduce_activity(Deposit) ->
    Params = #{
        p_transfer => p_transfer_status(Deposit),
        status => status(Deposit),
        active_revert => ff_deposit_revert_utils:is_active(reverts_index(Deposit))
    },
    do_deduce_activity(Params).

do_deduce_activity(#{status := pending, p_transfer := undefined}) ->
    p_transfer_start;
do_deduce_activity(#{status := pending, p_transfer := created}) ->
    p_transfer_prepare;
do_deduce_activity(#{status := pending, p_transfer := prepared}) ->
    finish;
do_deduce_activity(#{status := succeeded, p_transfer := prepared}) ->
    p_transfer_commit;
do_deduce_activity(#{status := {failed, _}, p_transfer := prepared}) ->
    p_transfer_cancel;
do_deduce_activity(#{status := succeeded, p_transfer := committed, active_revert := true}) ->
    revert.

-spec do_process_transfer(activity(), deposit()) ->
    {ok, process_result()} |
    {error, _Reason}.
do_process_transfer(p_transfer_start, Deposit) ->
    create_p_transfer(Deposit);
do_process_transfer(p_transfer_prepare, Deposit) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Deposit, fun ff_postings_transfer:prepare/1),
    {ok, {continue, Events}};
do_process_transfer(p_transfer_commit, Deposit) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Deposit, fun ff_postings_transfer:commit/1),
    {ok, {undefined, Events}};
do_process_transfer(p_transfer_cancel, Deposit) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Deposit, fun ff_postings_transfer:cancel/1),
    {ok, {undefined, Events}};
do_process_transfer(finish, Deposit) ->
    finish_transfer(Deposit);
do_process_transfer(revert, Deposit) ->
    {ok, ff_deposit_revert_utils:process_reverts(reverts_index(Deposit))}.

do_process_failure(_Reason, #{status := pending, p_transfer := #{status := created}}) ->
    {ok, []};
do_process_failure(_Reason, #{status := pending, p_transfer := #{status := prepared}} = Deposit) ->
    ff_pipeline:with(p_transfer, Deposit, fun ff_postings_transfer:cancel/1);
do_process_failure(Reason, #{status := pending, p_transfer := #{status := committed}}) ->
    erlang:error({unprocessable_failure, committed_p_transfer, Reason});
do_process_failure(_Reason, Transfer) ->
    no_p_transfer = maps:get(p_transfer, Transfer, no_p_transfer),
    {ok, []}.

-spec create_p_transfer(deposit()) ->
    {ok, process_result()} |
    {error, _Reason}.
create_p_transfer(Deposit) ->
    #{
        wallet_account := WalletAccount,
        source_account := SourceAccount,
        wallet_cash_flow_plan := CashFlowPlan
    } = params(Deposit),
    do(fun () ->
        Constants = #{
            operation_amount => body(Deposit)
        },
        Accounts = #{
            {wallet, sender_source} => SourceAccount,
            {wallet, receiver_settlement} => WalletAccount
        },
        FinalCashFlow = unwrap(cash_flow, ff_cash_flow:finalize(CashFlowPlan, Accounts, Constants)),
        PTransferID = construct_p_transfer_id(id(Deposit)),
        PostingsTransferEvents = unwrap(p_transfer, ff_postings_transfer:create(PTransferID, FinalCashFlow)),
        {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}
    end).

-spec finish_transfer(deposit()) ->
    {ok, {action(), [event()]}} |
    {error, process_error()}.
finish_transfer(Deposit) ->
    Body = body(Deposit),
    #{
        wallet_id := WalletID
    } = params(Deposit),
    {ok, WalletMachine} = ff_wallet_machine:get(WalletID),
    Wallet = ff_wallet_machine:wallet(WalletMachine),
    do(fun () ->
        valid = unwrap(validate_wallet_limits(Wallet, Body)),
        {continue, [{status_changed, succeeded}]}
    end).

-spec validate_wallet_limits(wallet(), cash()) ->
    {ok, valid} |
    {error, process_error()}.
validate_wallet_limits(Wallet, Body) ->
    case ff_party:validate_wallet_limits(Wallet, Body) of
        {ok, valid} = Result ->
            Result;
        {error, {terms_violation, {cash_range, {Cash, CashRange}}}} ->
            {error, {terms_violation, {wallet_limit, {cash_range, {Cash, CashRange}}}}};
        {error, {invalid_terms, _Details} = Reason} ->
            erlang:error(Reason)
    end.

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/deposit/", ID/binary>>.

-spec p_transfer_status(deposit()) -> ff_postings_transfer:status() | undefined.
p_transfer_status(Deposit) ->
    case p_transfer(Deposit) of
        undefined ->
            undefined;
        #{status := Status} ->
            Status
    end.

%% Validators

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
        Good when Good >= 0 ->
            {ok, valid};
        _Other ->
            {error, {invalid_revert_amount, RevertBody}}
    end.

%% Revert helpers

-spec reverts_index(deposit()) -> reverts().
reverts_index(Deposit) ->
    case maps:find(reverts, Deposit) of
        {ok, Reverts} ->
            Reverts;
        error ->
            ff_deposit_revert_utils:new_index()
    end.

-spec set_reverts_index(reverts(), deposit()) -> deposit().
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
            PotentialSucceeded = Status =:= succeeded orelse not ff_deposit_revert:is_finished(Revert),
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

%% Migration

-spec maybe_migrate(event() | legacy_event()) ->
    event().
% Actual events
maybe_migrate(Ev = {created, #{version := ?ACTUAL_FORMAT_VERSION}}) ->
    Ev;
maybe_migrate({p_transfer, PEvent}) ->
    {p_transfer, ff_postings_transfer:maybe_migrate(PEvent, deposit)};
maybe_migrate({revert, _Payload} = Event) ->
    {ID, RevertEvent} = ff_deposit_revert_utils:unwrap_event(Event),
    Migrated = ff_deposit_revert:maybe_migrate(RevertEvent),
    ff_deposit_revert_utils:wrap_event(ID, Migrated);
% Old events
maybe_migrate({created, #{version := 1, handler := ff_deposit} = T}) ->
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
    }});
maybe_migrate({transfer, PTransferEv}) ->
    maybe_migrate({p_transfer, PTransferEv});
% Other events
maybe_migrate(Ev) ->
    Ev.
