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
-type wallet()    :: ff_wallet:wallet_state().
-type ctx()       :: ff_entity_context:context().

-type st()        :: ff_machine:st(wallet()).

-type params()  :: ff_wallet:params().

-export_type([id/0]).
-export_type([params/0]).

-export([create/2]).
-export([get/1]).
-export([events/2]).

%% Accessors

-export([wallet/1]).
-export([ctx/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

-define(NS, 'ff/wallet_v2').

%% Accessors

-spec wallet(st()) -> wallet().

wallet(St) ->
    ff_machine:model(St).

-spec ctx(st()) -> ctx().

ctx(St) ->
    ff_machine:ctx(St).

%%

-spec create(params(), ctx()) ->
    ok | {error, exists | ff_wallet:create_error() }.

create(Params = #{id := ID}, Ctx) ->
    do(fun () ->
        Events = unwrap(ff_wallet:create(Params)),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, st()}        |
    {error, notfound} .

get(ID) ->
    ff_machine:get(ff_wallet, ?NS, ID).

-spec events(id(), machinery:range()) ->
    {ok, [{integer(), ff_machine:timestamped_event(event())}]} |
    {error, notfound}.

events(ID, Range) ->
    do(fun () ->
        #{history := History} = unwrap(machinery:get(?NS, ID, Range, backend())),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

backend() ->
    fistful:backend(?NS).

%% machinery

-type event() ::
    ff_wallet:event().

-type machine()      :: ff_machine:machine(event()).
-type result()       :: ff_machine:result(event()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

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

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    result().

process_repair(Scenario, Machine, _Args, _Opts) ->
    ff_repair:apply_scenario(ff_wallet, Machine, Scenario).
