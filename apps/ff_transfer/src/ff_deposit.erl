%%%
%%% Deposit
%%%

-module(ff_deposit).

-type id()          :: ff_transfer_machine:id().
-type source()      :: ff_source:id(_).
-type wallet()      :: ff_wallet:id(_).

-type deposit() :: ff_transfer:transfer(transfer_params()).
-type transfer_params() :: #{
    source      := source(),
    destination := wallet()
}.

-type machine() :: ff_transfer_machine:st(transfer_params()).
-type events()  :: ff_transfer_machine:events().

-export_type([deposit/0]).
-export_type([machine/0]).
-export_type([transfer_params/0]).
-export_type([events/0]).

%% ff_transfer_machine behaviour
-behaviour(ff_transfer_machine).
-export([process_transfer/1]).

%% Accessors

-export([source/1]).
-export([destination/1]).
-export([id/1]).
-export([source_acc/1]).
-export([destination_acc/1]).
-export([body/1]).
-export([status/1]).

%% API
-export([create/3]).
-export([get/1]).
-export([get_machine/1]).
-export([events/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Accessors

-spec source(deposit())          -> source().
-spec destination(deposit())     -> wallet().
-spec id(deposit())              -> ff_transfer:id().
-spec source_acc(deposit())      -> ff_account:account().
-spec destination_acc(deposit()) -> ff_account:account().
-spec body(deposit())            -> ff_transfer:body().
-spec status(deposit())          -> ff_transfer:status().

source(T)          -> maps:get(source, ff_transfer:params(T)).
destination(T)     -> maps:get(destination, ff_transfer:params(T)).
id(T)              -> ff_transfer:id(T).
source_acc(T)      -> ff_transfer:source(T).
destination_acc(T) -> ff_transfer:destination(T).
body(T)            -> ff_transfer:body(T).
status(T)          -> ff_transfer:status(T).

%%

-define(NS, 'ff/deposit_v1').

-type ctx()    :: ff_ctx:ctx().
-type params() :: #{
    source      := ff_source:id(),
    destination := ff_wallet_machine:id(),
    body        := ff_transaction:body()
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

create(ID, #{source_id := SourceID, wallet_id := WalletID, body := Body}, Ctx) ->
    do(fun() ->
        Source = ff_source:get(unwrap(source, ff_source:get_machine(SourceID))),
        Wallet = ff_wallet_machine:wallet(unwrap(destination, ff_wallet_machine:get(WalletID))),
        ok = unwrap(source, valid(authorized, ff_source:status(Source))),
        Params = #{
            handler     => ?MODULE,
            body        => Body,
            params      => #{
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
            }
        },
        unwrap(ff_transfer_machine:create(?NS, ID, Params, Ctx))
    end).

-spec get(machine()) ->
    deposit().

get(St) ->
    ff_transfer_machine:transfer(St).

-spec get_machine(id()) ->
    {ok, machine()}       |
    {error, notfound}.

get_machine(ID) ->
    ff_transfer_machine:get(?NS, ID).

-spec events(id(), machinery:range()) ->
    {ok, events()} |
    {error, notfound}.

events(ID, Range) ->
    ff_transfer_machine:events(?NS, ID, Range).

%% ff_transfer_machine behaviour

-spec process_transfer(deposit()) ->
    {ok, {ff_transfer_machine:action(), [ff_transfer_machine:event(ff_transfer:event())]}} |
    {error, _Reason}.
process_transfer(#{status := pending, p_transfer := #{status := prepared}}) ->
    {ok, {continue, [{status_changed, succeeded}]}};
process_transfer(Transfer) ->
    ff_transfer:process_transfer(Transfer).
