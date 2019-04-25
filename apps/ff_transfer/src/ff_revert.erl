%%%
%%% Revert
%%%

-module(ff_revert).

-type id()               :: ff_transfer_machine_new:id().
-type wallet_id()        :: ff_wallet:id().

-type revert()           :: revert(any()).
-type revert(T)          :: ff_transfer_new:transfer(transfer_params(T)).
-type transfer_params()  :: transfer_params(any()).
-type transfer_params(T) :: #{
    wallet_id            := wallet_id(),
    revert_cash_flow     := final_cash_flow(),
    target               := target(),
    session_data         := session_data(T),
    reason               => binary()
}.

-type session_data(T)    :: #{
    type                 := wallet_id(),
    params               := T
}.

-type final_cash_flow() :: ff_cash_flow:final_cash_flow().
-type create_params() :: create_params(any()).
-type create_params(T) :: #{
    wallet_id            := wallet_id(),
    revert_cash_flow     := final_cash_flow(),
    body                 := ff_transaction:body(),
    session_data         := T,
    target               := target(),
    reason               => binary(),
    external_id          => id()
}.
-type event()   :: ff_transfer_machine_new:event(ff_transfer_new:event(transfer_params(), route())).
-type route()   :: ff_transfer_new:route(none()).

-export_type([revert/0]).
-export_type([transfer_params/0]).
-export_type([create_params/1]).

%% ff_transfer_new behaviour

-behaviour(ff_transfer_new).
-export([apply_event/2]).
-export([preprocess_transfer/1]).
-export([process_transfer/1]).
-export([process_failure/2]).
-export([process_call/2]).

%% Accessors

-export([wallet_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([reason/1]).
-export([target/1]).
-export([external_id/1]).

%% API

-export([create/2]).

%% Event source

-export([maybe_migrate/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Internal types

-type process_result() :: {ff_transfer_machine_new:action(), [event()]}.
-type session_params() :: #{}.
-type target()         :: ff_transfer_new:target().

%% Accessors

-spec wallet_id(revert())       -> wallet_id().
-spec id(revert())              -> ff_transfer_new:id().
-spec body(revert())            -> ff_transfer_new:body().
-spec status(revert())          -> ff_transfer_new:status().
-spec reason(revert())          -> undefined | binary().
-spec target(revert())          -> target().
-spec params(revert(T))         -> transfer_params(T).

wallet_id(T)       -> maps:get(wallet_id, ff_transfer_new:params(T)).
id(T)              -> ff_transfer_new:id(T).
body(T)            -> ff_transfer_new:body(T).
status(T)          -> ff_transfer_new:status(T).
reason(T)          -> maps:get(reason, ff_transfer_new:params(T), undefined).
target(T)          -> maps:get(target, ff_transfer_new:params(T)).
params(T)          -> ff_transfer_new:params(T).

-spec external_id(revert()) ->
    id() | undefined.
external_id(T)     -> ff_transfer_new:external_id(T).

%% API

-spec create(id(), create_params()) ->
    {ok, [event()]}.

create(ID, Args = #{
    wallet_id           := WalletID,
    revert_cash_flow    := CashFlowPlan,
    body                := Body,
    session_data        := SessionData,
    target              := Target
}) ->
    do(fun() ->
        Params = #{

            transfer_type   => ff_transfer_new:handler_to_type(?MODULE),
            id              => ID,
            body            => Body,
            params          => #{
                wallet_id             => WalletID,
                session_data          => SessionData,
                revert_cash_flow      => CashFlowPlan,
                target                => Target,
                reason                => maps:get(reason, Args, undefined)
            },
            external_id => maps:get(external_id, Args, undefined)
        },
        ff_transfer_new:create_events(Params)
    end).

%% ff_transfer_new behaviour

-spec preprocess_transfer(revert()) ->
    ok                                                                          |
    {ok, ff_transfer_new:new_activity(), ff_transfer_new:preprocess_result(session_params())}   |
    {error, _Reason}.

preprocess_transfer(undefined) ->
    {error, cant_preprocess_undefined_transfer};
preprocess_transfer(Revert) ->
    Activity = ff_transfer_new:activity(Revert),
    do_preprocess_transfer(Activity, Revert).

-spec process_transfer(revert()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_transfer(undefined) ->
    {error, cant_process_undefined_transfer};
process_transfer(_Revert) ->
    {ok, {undefined, []}}.

-spec process_call(_Args, revert()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_call({revert, _Body, _Reason}, _Revert) ->
    {ok, {undefined, []}}.

-spec process_failure(any(), revert()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_failure(_Reason, _Revert) ->
    {ok, {undefined, []}}.

%% Internals

do_preprocess_transfer(transaction_polling, Revert) ->
    Transaction = ff_transfer_new:transaction(Revert),
    case ff_transaction_new:activity(Transaction) of
        session_starting ->
            validate_wallet_limits(Revert);
        _ ->
            ok
    end;
do_preprocess_transfer(routing, Revert = #{session_data := #{type := empty}}) ->
    {ok, transaction_starting, {create_transaction, create_transaction_params(Revert)}};
do_preprocess_transfer(transaction_starting, Revert) ->
    {ok, transaction_starting, {create_transaction, create_transaction_params(Revert)}};
do_preprocess_transfer(_, _) ->
    ok.

create_transaction_params(Revert) ->
    #{
        revert_cash_flow    := FinalCashFlow,
        session_data        := SessionData
    } = params(Revert),

    #{type := Type, params := Params} = SessionData,

    #{
        id                  => construct_transaction_id(id(Revert)),
        body                => body(Revert),
        final_cash_flow     => FinalCashFlow,
        session_data        => #{
            type    => Type,
            params  => Params
        }
    }.

-spec construct_transaction_id(id()) -> id().
construct_transaction_id(ID) ->
    <<"ff/revert/", ID/binary>>.

-spec apply_event(event(), revert()) ->
    revert().
apply_event(_, T) ->
    T.

-spec maybe_migrate(ff_transfer_new:event() | ff_transfer_new:legacy_event()) ->
    ff_transfer_new:event().
maybe_migrate(Ev) ->
    ff_transfer_new:maybe_migrate(Ev, revert).

validate_wallet_limits(Revert) ->
    do(fun () ->
        #{
            wallet_id := WalletID
        } = params(Revert),
        Body = body(Revert),
        Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
        WalletAccount = ff_wallet:account(Wallet),
        valid = unwrap(ff_party:validate_wallet_limits(WalletID, Body, WalletAccount)),
        ok
    end).
