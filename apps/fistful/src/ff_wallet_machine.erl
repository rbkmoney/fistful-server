%%%
%%% Wallet machine
%%%
%%% TODOs
%%%
%%%  - ~~Pattern `NS, ID, Backend` repeats everytime.~~ Well, there's just `NS`
%%%    instead but it looks not so bright still.
%%%

-module(ff_wallet_machine).

-type id()        :: machinery:id().
-type wallet()    :: ff_wallet:wallet().
-type ctx()       :: ff_ctx:ctx().

-type st()        :: ff_machine:st(wallet()).

-export_type([id/0]).

-export([create/3]).
-export([get/1]).

%% Accessors

-export([wallet/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%% Accessors

-spec wallet(st())  -> wallet().

wallet(St) ->
    ff_machine:model(St).

%%

-define(NS, 'ff/wallet').

-type params() :: #{
    identity   := ff_identity_machine:id(),
    name       := binary(),
    currency   := ff_currency:id()
}.

-spec create(id(), params(), ctx()) ->
    ok |
    {error,
        _WalletCreateError |
        exists
    }.

create(ID, #{identity := IdentityID, name := Name, currency := CurrencyID}, Ctx) ->
    do(fun () ->
        Events = unwrap(ff_wallet:create(ID, IdentityID, Name, CurrencyID)),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, fistful:backend(?NS)))
    end).

-spec get(id()) ->
    {ok, st()}        |
    {error, notfound} .

get(ID) ->
    ff_machine:get(ff_wallet, ?NS, ID).

%% machinery

-type event() ::
    ff_wallet:event().

-type machine()      :: ff_machine:machine(event()).
-type result()       :: ff_machine:result(event()).
-type handler_opts() :: machinery:handler_opts(_).

-spec init({[event()], ctx()}, machine(), _, handler_opts()) ->
    result().

init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events    => ff_machine:emit_events(Events),
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

process_timeout(#{}, _, _Opts) ->
    #{}.

-spec process_call(_CallArgs, machine(), _, handler_opts()) ->
    {ok, result()}.

process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.
