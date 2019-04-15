%%%
%%% Deposit
%%%

-module(ff_deposit_new).

-type id()          :: ff_transfer_machine_new:id().
-type source_id()   :: ff_source:id().
-type wallet_id()   :: ff_wallet:id().

-type deposit() :: ff_transfer_new:transfer(transfer_params()).
-type transfer_params() :: #{
    source_id := source_id(),
    wallet_id := wallet_id(),
    wallet_account := account(),
    source_account := account(),
    wallet_cash_flow_plan := cash_flow_plan()
}.

-type machine() :: ff_transfer_machine_new:st(transfer_params()).
-type events()  :: ff_transfer_machine_new:events().
-type event()   :: ff_transfer_machine_new:event(ff_transfer_new:event(transfer_params(), route())).
-type route()   :: ff_transfer_new:route(none()).

-export_type([deposit/0]).
-export_type([machine/0]).
-export_type([transfer_params/0]).
-export_type([events/0]).
-export_type([event/0]).
-export_type([route/0]).

%% ff_transfer_new behaviour

-behaviour(ff_transfer_new).
-export([apply_event/2]).
-export([preprocess_transfer/1]).
-export([process_transfer/1]).
-export([process_failure/2]).
-export([process_call/2]).

%% Accessors

-export([wallet_id/1]).
-export([source_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([external_id/1]).

%% API

-export([create/3]).
-export([get/1]).
-export([get_machine/1]).
-export([events/2]).

%% Event source

-export([maybe_migrate/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type account() :: ff_account:account().
-type process_result() :: {ff_transfer_machine_new:action(), [event()]}.
-type cash_flow_plan() :: ff_cash_flow:cash_flow_plan().
-type ctx()            :: ff_ctx:ctx().

%% Accessors

-spec wallet_id(deposit())       -> source_id().
-spec source_id(deposit())       -> wallet_id().
-spec id(deposit())              -> ff_transfer_new:id().
-spec body(deposit())            -> ff_transfer_new:body().
-spec status(deposit())          -> ff_transfer_new:status().
-spec params(deposit())          -> transfer_params().

wallet_id(T)        -> maps:get(wallet_id, ff_transfer_new:params(T)).
source_id(T)       -> maps:get(source_id, ff_transfer_new:params(T)).
id(T)              -> ff_transfer_new:id(T).
body(T)            -> ff_transfer_new:body(T).
status(T)          -> ff_transfer_new:status(T).
params(T)          -> ff_transfer_new:params(T).

-spec external_id(deposit()) ->
    id() | undefined.
external_id(T)     -> ff_transfer_new:external_id(T).

-define(NS, 'ff/deposit_v2').

%% API

-type create_params() :: #{
    source_id   := ff_source:id(),
    wallet_id   := ff_wallet_machine:id(),
    body        := ff_transaction:body(),
    external_id => id()
}.

-spec create(id(), create_params(), ctx()) ->
    ok |
    {error,
        {source, notfound | unauthorized} |
        {destination, notfound} |
        {provider, notfound} |
        exists |
        _TransferError
    }.

create(ID, Args = #{source_id := SourceID, wallet_id := WalletID, body := Body}, Ctx) ->
    do(fun() ->
        Source = ff_source:get(unwrap(source, ff_source:get_machine(SourceID))),
        Wallet = ff_wallet_machine:wallet(unwrap(destination, ff_wallet_machine:get(WalletID))),
        valid =  unwrap(ff_party:validate_deposit_creation(Wallet, Body)),
        ok = unwrap(source, valid(authorized, ff_source:status(Source))),
        Params = #{

            transfer_type   => ff_transfer_new:handler_to_type(?MODULE),
            id              => ID,
            body            => Body,
            params          => #{
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
            external_id => maps:get(external_id, Args, undefined)
        },
        unwrap(ff_transfer_new:create(?NS, Params, Ctx))
    end).

-spec get(machine()) ->
    deposit().

get(St) ->
    ff_transfer_machine_new:transfer(St).

-spec get_machine(id()) ->
    {ok, machine()}       |
    {error, notfound}.

get_machine(ID) ->
    ff_transfer_machine_new:get(?NS, ID).

-spec events(id(), machinery:range()) ->
    {ok, events()} |
    {error, notfound}.

events(ID, Range) ->
    ff_transfer_machine_new:events(?NS, ID, Range).

%% ff_transfer_new behaviour

-spec preprocess_transfer(deposit()) ->
    ok                                                                          |
    {ok, ff_transfer_new:new_activity(), ff_transfer_new:preprocess_result()}   |
    {error, _Reason}.

preprocess_transfer(undefined) ->
    {error, cant_preprocess_undefined_transfer};
preprocess_transfer(Transfer) ->
    Activity = ff_transfer_new:activity(Transfer),
    do_preprocess_transfer(Activity, Transfer).

-spec process_transfer(deposit()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_transfer(undefined) ->
    {error, cant_process_undefined_transfer};
process_transfer(_Transfer) ->
    {ok, {undefined, []}}.

-spec process_call(_Args, deposit()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_call({revert, _Body, _Reason}, _Transfer) ->
    {ok, {undefined, []}}.

-spec process_failure(any(), deposit()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_failure(_Reason, _Transfer) ->
    {ok, {undefined, []}}.

%% Internals

do_preprocess_transfer(transaction_polling, Deposit) ->
    Transaction = ff_transfer_new:transaction(Deposit),
    case ff_transaction_new:activity(Transaction) of
        session_starting ->
            validate_wallet_limits(Deposit);
        _ ->
            ok
    end;
do_preprocess_transfer(Activity, Deposit) when
    Activity =:= routing orelse
    Activity =:= transaction_starting
->
    {ok, transaction_starting, {create_transaction, create_transaction_params(Deposit)}};
do_preprocess_transfer(_, _) ->
    ok.

create_transaction_params(Deposit) ->
    #{
        wallet_account := WalletAccount,
        source_account := SourceAccount,
        wallet_cash_flow_plan := CashFlowPlan
    } = params(Deposit),

    Constants = #{
        operation_amount => body(Deposit)
    },
    Accounts = #{
        {wallet, sender_source} => SourceAccount,
        {wallet, receiver_settlement} => WalletAccount
    },
    FinalCashFlow = unwrap(cash_flow, ff_cash_flow:finalize(CashFlowPlan, Accounts, Constants)),

    #{
        id                  => construct_transaction_id(id(Deposit)),
        body                => body(Deposit),
        final_cash_flow     => FinalCashFlow,
        session_data        => {ff_transaction_new:get_empty_session_type(), undefined, undefined}
    }.

-spec construct_transaction_id(id()) -> id().
construct_transaction_id(ID) ->
    <<"ff/deposit/", ID/binary>>.

-spec apply_event(event(), deposit()) ->
    deposit().
apply_event(_, T) ->
    T.

-spec maybe_migrate(ff_transfer_new:event() | ff_transfer_new:legacy_event()) ->
    ff_transfer_new:event().
maybe_migrate(Ev) ->
    ff_transfer_new:maybe_migrate(Ev, deposit).

validate_wallet_limits(Deposit) ->
    do(fun () ->
        #{
            wallet_account := WalletAccount,
            wallet_id := WalletID
        } = params(Deposit),
        Body = body(Deposit),
        valid = unwrap(ff_party:validate_wallet_limits(WalletID, Body, WalletAccount)),
        ok
    end).
