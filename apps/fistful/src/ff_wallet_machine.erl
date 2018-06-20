%%%
%%% Wallet machine
%%%
%%% TODOs
%%%
%%%  - Pattern `NS, ID, Backend` repeats everytime.
%%%

-module(ff_wallet_machine).

-type id()        :: machinery:id().
-type timestamp() :: machinery:timestamp().
-type wallet()    :: ff_wallet:wallet().
-type ctx()       :: ff_ctx:ctx().

-type st()        :: #{
    wallet        := wallet(),
    times         => {timestamp(), timestamp()},
    ctx           => ctx()
}.

-export_type([id/0]).

-export([create/3]).
-export([get/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%%

-spec wallet(st()) -> wallet().

wallet(#{wallet := Wallet}) -> Wallet.

%%

-define(NS, wallet).

-type params() :: #{
    identity   := ff_identity_machine:id(),
    name       := binary(),
    currency   := ff_currency:id()
}.

-spec create(id(), params(), ctx()) ->
    ok |
    {error,
        {identity, notfound} |
        {currency, notfound} |
        _WalletError |
        exists
    }.

create(ID, #{identity := IdentityID, name := Name, currency := Currency}, Ctx) ->
    do(fun () ->
        Identity = unwrap(identity, ff_identity_machine:get(IdentityID)),
        _        = unwrap(currency, ff_currency:get(Currency)),
        Wallet0  = unwrap(ff_wallet:create(Identity, Name, Currency)),
        Wallet1  = unwrap(ff_wallet:setup_wallet(Wallet0)),
        unwrap(machinery:start(?NS, ID, {Wallet1, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, wallet()}    |
    {error, notfound} .

get(ID) ->
    do(fun () ->
        wallet(collapse(unwrap(machinery:get(?NS, ID, backend()))))
    end).

backend() ->
    fistful:backend(?NS).

%% machinery

-type ev()           ::
    {created, wallet()}.

-type auxst() ::
    #{ctx => ctx()}.

-type machine()      :: machinery:machine(ev(), auxst()).
-type result()       :: machinery:result(ev(), auxst()).
-type handler_opts() :: machinery:handler_opts().
-type handler_args() :: machinery:handler_args(_).

-spec init({wallet(), ctx()}, machine(), handler_args(), handler_opts()) ->
    result().

init({Wallet, Ctx}, #{}, _, _Opts) ->
    #{
        events    => emit_ts_event({created, Wallet}),
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), handler_args(), handler_opts()) ->
    result().

process_timeout(#{}, _, _Opts) ->
    #{}.

-spec process_call(_CallArgs, machine(), handler_args(), handler_opts()) ->
    {ok, result()}.

process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.

%%

collapse(#{history := History, aux_state := #{ctx := Ctx}}) ->
    collapse_history(History, #{ctx => Ctx}).

collapse_history(History, St) ->
    lists:foldl(fun merge_event/2, St, History).

merge_event({_ID, _Ts, TsEv}, St0) ->
    {EvBody, St1} = merge_ts_event(TsEv, St0),
    merge_event_body(EvBody, St1).

merge_event_body({created, Wallet}, St) ->
    St#{
        wallet => Wallet
    }.

%%

emit_ts_event(E) ->
    emit_ts_events([E]).

emit_ts_events(Es) ->
    emit_ts_events(Es, machinery_time:now()).

emit_ts_events(Es, Ts) ->
    [{ev, Ts, Body} || Body <- Es].

merge_ts_event({ev, Ts, Body}, St = #{times := {Created, _Updated}}) ->
    {Body, St#{times => {Created, Ts}}};
merge_ts_event({ev, Ts, Body}, St = #{}) ->
    {Body, St#{times => {Ts, Ts}}}.
