%%%
%%% Withdrawal
%%%

-module(ff_withdrawal).

-type id()          :: ff_transfer_machine:id().
-type wallet()      :: ff_wallet:id(_).
-type destination() :: ff_destination:id(_).

-type withdrawal() :: ff_transfer:transfer(transfer_params()).
-type transfer_params() :: #{
    source      := wallet(),
    destination := destination()
}.

-type machine() :: ff_transfer_machine:st(transfer_params()).
-type events() :: ff_transfer_machine:events().

-export_type([withdrawal/0]).
-export_type([machine/0]).
-export_type([transfer_params/0]).
-export_type([events/0]).

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

-spec source(withdrawal())          -> wallet().
-spec destination(withdrawal())     -> destination().
-spec id(withdrawal())              -> ff_transfer:id().
-spec source_acc(withdrawal())      -> ff_account:account().
-spec destination_acc(withdrawal()) -> ff_account:account().
-spec body(withdrawal())            -> ff_transfer:body().
-spec status(withdrawal())          -> ff_transfer:status().

source(T)          -> maps:get(source, ff_transfer:params(T)).
destination(T)     -> maps:get(destination, ff_transfer:params(T)).
id(T)              -> ff_transfer:id(T).
source_acc(T)      -> ff_transfer:source(T).
destination_acc(T) -> ff_transfer:destination(T).
body(T)            -> ff_transfer:body(T).
status(T)          -> ff_transfer:status(T).


%%

-define(NS, 'ff/withdrawal_v2').

-type ctx()    :: ff_ctx:ctx().
-type params() :: #{
    source      := ff_wallet_machine:id(),
    destination := ff_destination_machine:id(),
    body        := ff_transaction:body()
}.

-spec create(id(), params(), ctx()) ->
    ok |
    {error,
        {source, notfound} |
        {destination, notfound | unauthorized} |
        {provider, notfound} |
        exists |
        _TransferError

    }.

create(ID, #{source := SourceID, destination := DestinationID, body := Body}, Ctx) ->
    do(fun() ->
        Source = ff_wallet_machine:wallet(unwrap(source, ff_wallet_machine:get(SourceID))),
        Destination = ff_destination_machine:destination(
            unwrap(destination, ff_destination_machine:get(DestinationID))
        ),
        ok = unwrap(destination, valid(authorized, ff_destination:status(Destination))),
        Params = #{
            type        => withdrawal,
            source      => ff_wallet:account(Source),
            destination => ff_destination:account(Destination),
            body        => Body,
            params      => #{
                source      => SourceID,
                destination => DestinationID
            }
        },
        unwrap(ff_transfer_machine:create(?NS, ID, Params, Ctx))
    end).

-spec get(machine()) ->
    withdrawal().

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
