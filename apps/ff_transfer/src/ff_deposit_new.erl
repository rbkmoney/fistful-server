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
    wallet_id := wallet_id()
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

%% ff_transfer_machine_new behaviour
-behaviour(ff_transfer_machine_new).
-export([process_transfer/1]).
-export([process_failure/2]).

%% Accessors

-export([wallet_id/1]).
-export([source_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([external_id/1]).
-export([route/1]).
-export([transaction/1]).

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

-type process_result() :: {ff_transfer_machine_new:action(), [event()]}.

%% Accessors

-spec wallet_id(deposit())                    -> source_id().
-spec source_id(deposit())                    -> wallet_id().
-spec id(ff_transfer_new:transfer())             -> ff_transfer_new:id().
-spec body(ff_transfer_new:transfer())           -> ff_transfer_new:body().
-spec status(ff_transfer_new:transfer())         -> ff_transfer_new:status().
-spec params(ff_transfer_new:transfer())         -> transfer_params().
-spec route(ff_transfer_new:transfer())          -> ff_transfer_new:route().
-spec transaction(ff_transfer_new:transfer())    -> ff_transaction_new:transaction().

wallet_id(T)       -> maps:get(wallet_id, ff_transfer_new:params(T)).
source_id(T)       -> maps:get(source_id, ff_transfer_new:params(T)).
id(T)              -> ff_transfer_new:id(T).
body(T)            -> ff_transfer_new:body(T).
status(T)          -> ff_transfer_new:status(T).
params(T)          -> ff_transfer_new:params(T).
route(T)           -> ff_transfer_new:route(T).
transaction(T)     -> ff_transfer_new:transaction(T).

-spec external_id(deposit()) ->
    id() | undefined.
external_id(T)     -> ff_transfer_new:external_id(T).

%%

-define(NS, 'ff/deposit_v2').

-type ctx()    :: ff_ctx:ctx().
-type params() :: #{
    source_id   := ff_source:id(),
    wallet_id   := ff_wallet_machine:id(),
    body        := ff_transaction:body(),
    external_id => id()
}.

-spec create(id(), params(), ctx()) ->
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
            handler     => ?MODULE,
            body        => Body,
            params      => #{
                wallet_id             => WalletID,
                source_id             => SourceID
            },
            session_type => ff_transfer_new:get_empty_session_type(),
            external_id  => maps:get(external_id, Args, undefined)
        },
        unwrap(ff_transfer_machine_new:create(?NS, ID, Params, Ctx))
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

%% ff_transfer_machine_new behaviour

-spec process_transfer(ff_transfer_new:transfer()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_transfer(Deposit) ->
    do_process_transfer(ff_transfer_new:activity(Deposit), Deposit).

-spec process_failure(any(), ff_transfer_new:transfer()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_failure(Reason, Deposit) ->
    ff_transfer_new:process_failure(Reason, Deposit).

%% Internals

do_process_transfer(Activity, Deposit) when
    Activity =:= routing orelse
    Activity =:= transaction_starting
->
    create_transaction(Deposit);
do_process_transfer(transaction_polling, Deposit) ->
    poll_transaction_completion(Deposit).

create_transaction(Deposit) ->
    #{
        source_id := SourceID,
        wallet_id := WalletID
    } = params(Deposit),

    CashFlowPlan = unwrap(cash_flow_plan,
        ff_party:get_cash_flow_plan(ff_transfer_new:transfer_type(Deposit), #{})),

    TransactionParams = #{
        id            => construct_transaction_id(id(Deposit)),
        body          => body(Deposit),
        source        => ff_transaction_new:make_ref(source, SourceID),
        destination   => ff_transaction_new:make_ref(wallet, WalletID),
        route         => route(Deposit),
        transfer_type => ff_transfer_new:transfer_type(Deposit),
        cash_flow     => CashFlowPlan,
        session_type  => ff_transaction_new:get_empty_session_type()
    },

    ff_transfer_new:create_transaction(TransactionParams).

poll_transaction_completion(Deposit) ->
    ff_transfer_new:poll_transaction_completion(Deposit).

-spec construct_transaction_id(id()) -> id().
construct_transaction_id(ID) ->
    <<"ff/deposit/", ID/binary>>.

-spec maybe_migrate(ff_transfer_new:event() | ff_transfer_new:legacy_event()) ->
    ff_transfer_new:event().
maybe_migrate(Ev) ->
    ff_transfer_new:maybe_migrate(Ev, deposit).
