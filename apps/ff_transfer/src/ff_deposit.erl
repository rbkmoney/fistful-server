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
    external_id   => id()
}.
-type params() :: #{
    body := ff_transaction:body(),
    source_id := ff_source:id(),
    wallet_id := ff_wallet:id(),
    external_id => external_id()
}.

-type status() ::
    pending         |
    succeeded       |
    {failed, _TODO} .

-type event() ::
    {created, deposit()} |
    {p_transfer, ff_postings_transfer:event()} |
    {status_changed, status()}.

-type create_error() ::
    {source, notfound | unauthorized} |
    {wallet, notfound} |
    ff_party:validate_deposit_creation_error().

-export_type([deposit/0]).
-export_type([id/0]).
-export_type([params/0]).
-export_type([event/0]).
-export_type([create_error/0]).

%% Accessors

-export([wallet_id/1]).
-export([source_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([external_id/1]).

%% API
-export([create/2]).

%% transfer logic callbacks
-export([process_transfer/1]).
-export([process_failure/2]).

%% Event source

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type account() :: ff_account:account().
-type process_result() :: {action(), [event()]}.
-type cash_flow_plan() :: ff_cash_flow:cash_flow_plan().
-type source_id()   :: ff_source:id().
-type wallet_id()   :: ff_wallet:id().

-type body() :: ff_transaction:body().
-type action() :: ff_deposit_machine:action().
-type p_transfer() :: ff_postings_transfer:transfer().
-type external_id() :: id().
-type legacy_event() :: any().

-type transfer_params() :: #{
    source_id := source_id(),
    wallet_id := wallet_id(),
    wallet_account := account(),
    source_account := account(),
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

-spec create(id(), params()) ->
    {ok, [event()]} |
    {error, create_error()}.
create(ID, Params) ->
    do(fun() ->
        #{source_id := SourceID, wallet_id := WalletID, body := Body} = Params,
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

%% transfer logic callbacks

-spec process_transfer(deposit()) ->
    {ok, process_result()} |
    {error, _Reason}.
process_transfer(Deposit) ->
    Activity = deduce_activity(Deposit),
    do_process_transfer(Activity, Deposit).

-spec process_failure(any(), deposit()) ->
    {ok, process_result()} |
    {error, _Reason}.
process_failure(Reason, Deposit) ->
    {ok, ShutdownEvents} = do_process_failure(Reason, Deposit),
    {ok, {undefined, ShutdownEvents ++ [{status_changed, {failed, Reason}}]}}.

do_process_failure(_Reason, #{status := pending, p_transfer := #{status := created}}) ->
    {ok, []};
do_process_failure(_Reason, #{status := pending, p_transfer := #{status := prepared}} = Deposit) ->
    ff_pipeline:with(p_transfer, Deposit, fun ff_postings_transfer:cancel/1);
do_process_failure(Reason, #{status := pending, p_transfer := #{status := committed}}) ->
    erlang:error({unprocessable_failure, committed_p_transfer, Reason});
do_process_failure(_Reason, Transfer) ->
    no_p_transfer = maps:get(p_transfer, Transfer, no_p_transfer),
    {ok, []}.

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
apply_event_({p_transfer, Ev}, T) ->
    apply_event({p_transfer, Ev}, T#{p_transfer => undefined}).

%% Internals

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
    finish.

-spec deduce_activity(deposit()) ->
    activity().
deduce_activity(Deposit) ->
    Params = #{
        p_transfer => p_transfer(Deposit),
        status => status(Deposit)
    },
    do_deduce_activity(Params).

do_deduce_activity(#{status := pending, p_transfer := undefined}) ->
    p_transfer_start;
do_deduce_activity(#{status := pending, p_transfer := #{status := created}}) ->
    p_transfer_prepare;
do_deduce_activity(#{status := pending, p_transfer := #{status := prepared}}) ->
    finish;
do_deduce_activity(#{status := succeeded, p_transfer := #{status := prepared}}) ->
    p_transfer_commit;
do_deduce_activity(#{status := {failed, _}, p_transfer := #{status := prepared}}) ->
    p_transfer_cancel.

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
    finish_transfer(Deposit).

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
    {error, _Reason}.
finish_transfer(Deposit) ->
    Body = body(Deposit),
    #{
        wallet_id := WalletID,
        wallet_account := WalletAccount
    } = params(Deposit),
    do(fun () ->
        valid = unwrap(ff_party:validate_wallet_limits(WalletID, Body, WalletAccount)),
        {continue, [{status_changed, succeeded}]}
    end).

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/deposit/", ID/binary>>.

-spec maybe_migrate(event() | legacy_event()) ->
    event().
% Actual events
maybe_migrate(Ev = {created, #{version := ?ACTUAL_FORMAT_VERSION}}) ->
    Ev;
maybe_migrate({p_transfer, PEvent}) ->
    {p_transfer, ff_postings_transfer:maybe_migrate(PEvent, deposit)};
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
