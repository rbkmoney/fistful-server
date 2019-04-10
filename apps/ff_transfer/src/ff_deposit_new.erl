%%%
%%% Deposit
%%%

-module(ff_deposit_new).

-type id()          :: ff_transfer_machine_new:id().
-type source_id()   :: ff_source:id().
-type wallet_id()   :: ff_wallet:id().

-type deposit() :: ff_transfer:transfer(transfer_params()).
-type transfer_params() :: #{
    source_id := source_id(),
    wallet_id := wallet_id(),
    wallet_account := account(),
    source_account := account(),
    wallet_cash_flow_plan := cash_flow_plan()
}.

-type machine() :: ff_transfer_machine_new:st(transfer_params()).
-type events()  :: ff_transfer_machine_new:events().
-type event()   :: ff_transfer_machine_new:event(ff_transfer:event(transfer_params(), route())).
-type route()   :: ff_transfer:route(none()).

-export_type([deposit/0]).
-export_type([machine/0]).
-export_type([transfer_params/0]).
-export_type([events/0]).
-export_type([event/0]).
-export_type([route/0]).

%% ff_transfer_new behaviour
-behaviour(ff_transfer_new).
-export([create/2]).
-export([ns/0]).

%% Accessors

-export([wallet_id/1]).
-export([source_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([external_id/1]).

%% API

-export([get/1]).
-export([get_machine/1]).
-export([events/2]).

%% Event source

-export([maybe_migrate/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type account() :: ff_account:account().
% -type process_result() :: {ff_transfer_machine_new:action(), [event()]}.
-type cash_flow_plan() :: ff_cash_flow:cash_flow_plan().

%% Accessors

-spec wallet_id(deposit())       -> source_id().
-spec source_id(deposit())       -> wallet_id().
-spec id(deposit())              -> ff_transfer:id().
-spec body(deposit())            -> ff_transfer:body().
-spec status(deposit())          -> ff_transfer:status().
% -spec params(deposit())          -> transfer_params().

wallet_id(T)        -> maps:get(wallet_id, ff_transfer:params(T)).
source_id(T)       -> maps:get(source_id, ff_transfer:params(T)).
id(T)              -> ff_transfer:id(T).
body(T)            -> ff_transfer:body(T).
status(T)          -> ff_transfer:status(T).
% params(T)          -> ff_transfer:params(T).

-spec external_id(deposit()) ->
    id() | undefined.
external_id(T)     -> ff_transfer:external_id(T).

-define(NS, 'ff/deposit_v1').

%% API

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

%%

-spec ns() ->
    ?NS.

ns() ->
    ?NS.

-type create_params() :: #{
    id          := id(),
    source_id   := ff_source:id(),
    wallet_id   := ff_wallet_machine:id(),
    body        := ff_transaction:body(),
    external_id => id()
}.

-spec create(create_params(), ff_transfer_new:maybe(ff_transfer_new:parent())) ->
    ok |
    {error,
        {source, notfound | unauthorized} |
        {destination, notfound} |
        {provider, notfound} |
        exists |
        _TransferError
    }.

create(Args = #{id := ID, source_id := SourceID, wallet_id := WalletID, body := Body}, Parent) ->
    do(fun() ->
        Source = ff_source:get(unwrap(source, ff_source:get_machine(SourceID))),
        Wallet = ff_wallet_machine:wallet(unwrap(destination, ff_wallet_machine:get(WalletID))),
        valid =  unwrap(ff_party:validate_deposit_creation(Wallet, Body)),
        ok = unwrap(source, valid(authorized, ff_source:status(Source))),
        Params = #{
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
        TransferType = ff_transfer_new:handler_to_type(?MODULE),
        Events = unwrap(ff_transfer_new:make_default_events(#{
            id              => ID,
            transfer_type   => TransferType,
            body            => Body,
            params          => Params,
            external_id     => maps:get(external_id, Args, undefined)
        })),
        ff_transfer_new:wrap_events_for_parent(
            ID,
            TransferType,
            Events,
            Parent
        )
    end).


%% Internals

-spec maybe_migrate(ff_transfer:event() | ff_transfer:legacy_event()) ->
    ff_transfer:event().
maybe_migrate(Ev) ->
    ff_transfer:maybe_migrate(Ev, deposit).
