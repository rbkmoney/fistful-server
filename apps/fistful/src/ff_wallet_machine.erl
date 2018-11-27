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
-export([events/2]).

%% Accessors

-export([wallet/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

-define(NS, 'ff/wallet_v2').

%% Accessors

-spec wallet(st())  -> wallet().

wallet(St) ->
    ff_machine:model(St).

%%

-type params() :: #{
    identity   := ff_identity_machine:id(),
    name       := binary(),
    currency   := ff_currency:id()
}.

-spec create(id(), params(), ctx()) ->
    {ok, st()} |
    {error,
        _WalletCreateError |
        {conflict, id()} |
        {compare_error, id()}
    }.

create(ExternalID, #{identity := IdentityID, name := Name, currency := CurrencyID}, Ctx) ->
    do(fun () ->
        ID = unwrap(ff_external_id:check(wallet, ExternalID, ff_utils:get_owner(Ctx))),
        Events = unwrap(ff_wallet:create(ID, IdentityID, Name, CurrencyID)),
        case machinery:start(?NS, ID, {Events, Ctx}, backend()) of
            ok ->
                {ok, ff_machine:get(ff_wallet, ?NS, ID)};
            {error, exists} ->
                compare_events(ID, Events)
        end
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

compare_events(ID, NewEv) ->
    Limit = length(NewEv),
    case events(ID, {undefined, Limit, forward}) of
        {ok, OldEv} ->
            compare_events_(ID, NewEv, [Ev || {_, {ev, _, Ev}} <- OldEv]);
        {error, notfound} ->
            {error, {compare_error, ID}}
    end.

compare_events_(ID, NewEv, OldEv) when NewEv =:= OldEv ->
    {ok, ff_machine:get(ff_wallet, ?NS, ID)};
compare_events_(ID, _NewEv, _Ev) ->
    {error, {conflict, ID}}.

