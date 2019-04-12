%%%
%%% Transfer between 2 accounts
%%%

-module(ff_transfer_new).

-type handler()     :: module().

-define(ACTUAL_FORMAT_VERSION, 2).

-opaque transfer(T) :: #{
    version             := ?ACTUAL_FORMAT_VERSION,
    id                  := id(),
    body                := body(),
    activity            => activity(),
    route               => route(),
    transaction         => transaction(),
    status              => status(),
    external_id         => id(),

    params              := params(T),
    transfer_type       := transfer_type(),
    %% init on apply event time
    parent              => parent(),
    childs              => list(transfer())
}.

-type route(T) :: T.
-type parent() :: {transfer_type(), id()}.

-type status() ::
    pending         |
    succeeded       |
    {failed, _TODO} .

-type event(Params, Route)                 ::
    {created, transfer(Params)}             |
    {route_changed, route(Route)}           |
    {status_changed, status()}              |
    {transaction, transaction_event()}      |
    {child_transfer, signed_event()}        .

-type signed_event() :: #{
    type    := transfer_type(),
    id      := id(),
    event   := event(),
    parent  := parent()
}.

-type event()        :: event(Params :: any(), Route :: any()).

-type args() :: #{
    id            := id(),
    body          := body(),
    params        := params(),
    transfer_type := transfer_type(),

    status        => status(),
    external_id   => external_id()
}.
-type create_params()       :: any().
-type preprocess_result()   ::
    {create_transaction, ff_transaction_new:create_params()}.

-type activity()            ::
    new                      |
    routing                  |
    transaction_starting     |
    transaction_polling      |
    transfer                 .
-type new_activity()        ::
    undefined                |
    activity()               .

-export_type([args/0]).
-export_type([transfer/1]).
-export_type([handler/0]).
-export_type([params/1]).
-export_type([event/0]).
-export_type([event/2]).
-export_type([status/0]).
-export_type([route/1]).
-export_type([maybe/1]).

-export_type([preprocess_result/0]).
-export_type([new_activity/0]).

-export([gen/1]).
-export([id/1]).
-export([transfer_type/1]).
-export([body/1]).
-export([params/1]).
-export([status/1]).
-export([route/1]).
-export([external_id/1]).
-export([activity/1]).
-export([transaction/1]).

%% API

-export([create_deposit/2]).
-export([create_withdrawal/2]).

%% Internal

-export([make_default_events/2]).
-export([wrap_events_for_parent/3]).
-export([wrap_events_for_parent/4]).
-export([handler_to_type/1]).

%% ff_transfer behaviour
-callback create(create_params(), maybe(parent())) ->
    {ok, [event()]} |
    {error, _Reason}.
-callback ns() ->
    atom().
-callback apply_event(event(), transfer()) ->
    transfer().

%% tunnel for ff_transfer_machine_new behaviour
-callback preprocess_transfer(transfer()) ->
    ok                          |
    {ok, new_activity(), preprocess_result()}   |
    {error, _Reason}.
-callback process_transfer(transfer()) ->
    {ok, {action(), [event()]}} |
    {error, _Reason}.
-callback process_call(_CallArgs, transfer()) ->
    {ok, {action(), [event()]}} |
    {error, _Reason}.

-callback process_failure(_Reason, transfer()) ->
    {ok, {action(), [event()]}} |
    {error, _Reason}.


%% ff_transfer_machine behaviour
-behaviour(ff_transfer_machine).
-export([process_transfer/1]).
-export([process_failure/2]).
-export([process_call/2]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%% Internal types

-type id()                  :: binary().
-type external_id()         :: id() | undefined.
-type body()                :: ff_transaction:body().
-type route()               :: route(any()).
-type maybe(T)              :: ff_maybe:maybe(T).
-type transaction()         :: ff_transaction_new:transaction().
-type transaction_event()   :: ff_transaction_new:event().
-type params()              :: params(any()).
-type params(T)             :: T.
-type transfer()            :: transfer(any()).
-type legacy_event()        :: any().
-type transfer_type()       :: atom().
-type ctx()                 :: ff_ctx:ctx().
-type action()              :: ff_transfer_machine_new:action().

%% Constructor

-spec gen(args()) -> transfer().
gen(Args) ->
    TypeKeys = [id, transfer_type, body, params, status, external_id],
    genlib_map:compact(maps:with(TypeKeys, Args)).

%% Accessors

-spec id(transfer()) -> id().
id(#{id := V}) ->
    V.

-spec transfer_type(transfer()) -> transfer_type().
transfer_type(#{transfer_type := V}) ->
    V.

-spec body(transfer()) -> body().
body(#{body := V}) ->
    V.

-spec params(transfer(T)) -> params(T).
params(#{params := V}) ->
    V.

-spec status(transfer()) -> maybe(status()).
status(#{status := V}) ->
    V;
status(_Other) ->
    undefined.

-spec route(transfer())  -> maybe(route()).
route(#{route := V}) ->
    V;
route(_Other) ->
    undefined.

-spec external_id(transfer()) ->
    external_id().

external_id(#{external_id := ExternalID}) ->
    ExternalID;
external_id(_Transfer) ->
    undefined.

-spec transaction(transfer()) ->
    maybe(transaction()).

transaction(#{transaction := Transaction}) ->
    Transaction;
transaction(_Transfer) ->
    undefined.

-spec activity(transfer()) ->
    maybe(activity()).

activity(#{activity := Activity}) ->
    Activity;
activity(_Transfer) ->
    undefined.

-spec childs(transfer()) ->
    maybe([transfer()]).

childs(#{childs := Activity}) ->
    Activity;
childs(_Transfer) ->
    [].

%% API

-spec create_deposit(create_params(), ctx()) ->
    {ok, [event()]} |
    {error,
        _PostingsTransferError
    }.

create_deposit(Params, Ctx) ->
    create(deposit, Params, Ctx).

-spec create_withdrawal(create_params(), ctx()) ->
    {ok, [event()]} |
    {error,
        _PostingsTransferError
    }.

create_withdrawal(Params, Ctx) ->
    create(withdrawal, Params, Ctx).

create(TransferType, Params, Ctx) ->
    create(TransferType, Params, Ctx, undefined).

create(TransferType, Params = #{id := ID}, Ctx, Parent) ->
    do(fun () ->
        Handler = type_to_handler(TransferType),
        Events = unwrap(Handler:create(Params, Parent)),
        NS = Handler:ns(),
        ff_transfer_machine_new:create(NS, ID, Events, Ctx)
    end).

-spec make_default_events(create, map()) ->
    {ok, [event()]} |
    {error,
        _PostingsTransferError
    }.

make_default_events(create, #{
    id := ID,
    transfer_type := TransferType,
    body := Body,
    params := Params,
    external_id := ExternalID
}) ->
    do(fun () ->
        [
            {created, add_external_id(ExternalID, #{
                %% TODO get version from handler module
                version       => ?ACTUAL_FORMAT_VERSION,
                id            => ID,
                transfer_type => TransferType,
                body          => Body,
                params        => Params
            })},
            {status_changed, pending}
        ]
    end).

add_external_id(undefined, Event) ->
    Event;
add_external_id(ExternalID, Event) ->
    Event#{external_id => ExternalID}.

-spec wrap_events_for_parent(transfer(), [event()], maybe(transfer())) ->
    [event()].

wrap_events_for_parent(Child, Events, Parent) ->
    wrap_events_for_parent(id(Child), transfer_type(Child), Events, make_parent(Parent)).

-spec wrap_events_for_parent(id(), transfer_type(), [event()], maybe(parent())) ->
    [event()].

wrap_events_for_parent(_ID, _TransferType, Events, undefined) ->
    Events;
wrap_events_for_parent(ID, TransferType, Events, Parent) ->
    [{child_transfer, sign_event(ID, TransferType, Ev, Parent)} || Ev <- Events].

sign_event(ID, TransferType, Event, Parent) ->
    #{id => ID, type => TransferType, event => Event, parent => Parent}.

make_parent(undefined) ->
    undefined;
make_parent(Transfer) ->
    {transfer_type(Transfer), id(Transfer)}.

%% Handler convertors

-spec handler_to_type(module()) ->
    transfer_type().
handler_to_type(ff_deposit_new) ->
    deposit;
handler_to_type(ff_withdrawal_new) ->
    withdrawal.

-spec type_to_handler(transfer_type()) ->
    module().
type_to_handler(deposit) ->
    ff_deposit;
type_to_handler(withdrawal) ->
    ff_withdrawal.

%% ff_transfer_machine behaviour

-spec process_transfer(maybe(transfer())) ->
    {ok, {ff_transfer_machine:action(), [ff_transfer_machine:event(event())]}} |
    {error, _Reason}.

process_transfer(undefined) ->
    {error, cant_process_undefined_transfer};
process_transfer(Transfer) ->
    do(fun () ->
        {Activity, Data} = handle_preprocess_result(preprocess_transfer(Transfer), activity(Transfer)),
        unwrap(do_process_transfer(Activity, Transfer, Data))
    end).

-spec process_call(_Args, transfer()) ->
    {ok, {ff_transfer_machine:action(), [ff_transfer_machine:event(event())]}} |
    {error, _Reason}.

process_call(Args, Transfer) ->
    Handler = type_to_handler(transfer_type(Transfer)),
    Handler:process_call(Args, Transfer).

-spec process_failure(any(), maybe(transfer())) ->
    {ok, {ff_transfer_machine_new:action(), [ff_transfer_machine_new:event(event())]}} |
    {error, _Reason}.

process_failure(Reason, undefined) ->
    {error, {cant_fail_undefined_transfer_with_reason, Reason}};
process_failure(Reason, Transfer) ->
    Activity = activity(Transfer),
    do_process_failure(Activity, Reason, Transfer).

%%

preprocess_transfer(Transfer) ->
    Handler = type_to_handler(transfer_type(Transfer)),
    Handler:preprocess_transfer(Transfer).

handle_preprocess_result(ok, Activity) ->
    {Activity, undefined};
handle_preprocess_result({ok, NewActivity, Data}, Activity) ->
    {choose_new_activity(NewActivity, Activity), Data};
handle_preprocess_result(Result, Activity) ->
    {Activity, unwrap(Result)}.

choose_new_activity(undefined, Activity) ->
    Activity;
choose_new_activity(Activity, _) ->
    Activity.

do_process_transfer(transfer, Transfer, _Data) ->
    Child = get_last_child(childs(Transfer)),
    case process_transfer(Child) of
        {error, _} = Error ->
            Error;
        {ok, {Action, Events}} ->
            {ok, {Action, wrap_events_for_parent(Child, Events, Transfer)}}
    end;
do_process_transfer(transaction_starting, _Transfer, {create_transaction, Params}) ->
    do(fun () ->
        TransactionEvents = unwrap(ff_transaction_new:create(Params)),
        {continue, wrap_transaction_events(TransactionEvents)}
    end);
do_process_transfer(transaction_polling, Transfer, _Data) ->
    Handler = type_to_handler(transfer_type(Transfer)),
    Handler:process_transfer(Transfer),
    poll_transaction_completion(Transfer);
do_process_transfer(_Activity, Transfer, _Data) ->
    Handler = type_to_handler(transfer_type(Transfer)),
    Handler:process_transfer(Transfer).

%%

do_process_failure(Activity, Reason, _Transfer) when
    Activity =:= routing orelse
    Activity =:= transaction_starting
->
    {ok, {undefined, [{status_changed, {failed, Reason}}]}};
do_process_failure(transaction_polling, Reason, Transfer) ->
    {_Action, TransactionEvents} = unwrap_transaction_call_res(
        ff_transaction_new:process_failure(Reason, transaction(Transfer))),
    {ok, {undefined,
        TransactionEvents ++
        [{status_changed, {failed, Reason}}]
    }};
do_process_failure(transfer, Reason, Transfer) ->
    Child = get_last_child(childs(Transfer)),
    {Action, Events} = unwrap(process_failure(Reason, Child)),
    {ok, {Action, wrap_events_for_parent(Child, Events, Transfer)}};
do_process_failure(_Activity, Reason, Transfer) ->
    Handler = type_to_handler(transfer_type(Transfer)),
    {Action, Events} = unwrap(Handler:process_failure(Reason, Transfer)),
    {ok, {
        Action,
        Events ++ [{status_changed, {failed, Reason}}]
    }}.

%%

get_last_child(Childs) when
    Childs =:= undefined orelse
    Childs =:= []
->
    undefined;
get_last_child([H | _T]) ->
    H.

-spec poll_transaction_completion(transfer()) ->
    {ok, {ff_transfer_machine_new:action(), [ff_transfer_machine_new:event(event())]}}.
poll_transaction_completion(Transfer) ->
    Transaction = transaction(Transfer),
    {Action, Events} = case ff_transaction_new:process_transaction(Transaction) of
        {ok, _} = Result ->
            unwrap_transaction_call_res(Result);
        {error, Reason} ->
            unwrap_transaction_call_res(ff_transaction_new:process_failure(Reason, Transaction))
    end,
    TransferEvents = case check_transaction_complete(Events) of
        true ->
            [{status_changed, succeeded}];
        _ ->
            []
    end,

    {ok, {Action, Events ++ TransferEvents}}.

unwrap_transaction_call_res({ok, {Action, Events}}) ->
    {Action, wrap_transaction_events(Events)}.

wrap_transaction_events(Events) ->
    [{transaction, Ev} || Ev <- Events].

check_transaction_complete(Events) ->
    lists:foldl(fun (Ev, Acc) ->
        case Ev of
            {cmd, {transaction, {p_transfer, {status_changed, committed}}}} ->
                true;
            _ ->
                Acc
        end
    end, false, Events).

%%

-spec apply_event(event() | legacy_event(), ff_maybe:maybe(transfer(T))) ->
    transfer(T).
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev, maybe_transfer_type(T)), T).

-spec apply_event_(event(), ff_maybe:maybe(transfer(T))) ->
    transfer(T).
apply_event_({created, T}, undefined) ->
    set_activity(routing, T);
apply_event_({status_changed, S}, T) ->
    maps:put(status, S, T);
apply_event_({route_changed, R}, T) ->
    set_activity(transaction_starting, maps:put(route, R, T));
apply_event_({transaction, Ev}, T) ->
    {Action, Transaction} = ff_transaction_new:apply_event(Ev, transaction(T)),
    Activity = action_to_activity(transaction, Action),
    set_activity(Activity, maps:put(transaction, Transaction, T));
apply_event_({child_transfer, SignedEvent = #{event := Ev}}, T) ->
    Child0 = find_child(SignedEvent, childs(T)),
    Child1 = apply_event(Ev, Child0),
    Child2 = set_parent(Child1, T),
    update_childs(Child0, Child2, T);
apply_event_(Ev, T) ->
    Handler = type_to_handler(transfer_type(T)),
    Handler:apply_event(Ev, T).

find_child(_, []) ->
    undefined;
find_child(Ev = #{id := ID}, [Child | Rest]) ->
    case id(Child) =:= ID of
        true ->
            Child;
        false ->
            find_child(Ev, Rest)
    end.

-spec set_parent(transfer(), transfer()) ->
    transfer().

set_parent(Child, Transfer) ->
    Parent = make_parent(Transfer),
    maps:put(parent, Parent, Child).

-spec update_childs(maybe(transfer()), transfer(), transfer()) ->
    transfer().

update_childs(undefined, Child, T) ->
    T#{childs => [Child | childs(T)]};
update_childs(_, Child0, T) ->
    Childs = lists:foldl(fun(Child, Acc) ->
        case id(Child0) =:= id(Child) of
            true ->
                [Child0 | Acc];
            false ->
                [Child | Acc]
        end
    end, [], childs(T)),
    T#{childs => Childs}.

maybe_transfer_type(undefined) ->
    undefined;
maybe_transfer_type(T) ->
    transfer_type(T).

set_activity(undefined, T) ->
    T;
set_activity(Activity, T) ->
    maps:put(activity, Activity, T).

-spec action_to_activity(transaction, undefined | next) ->
    undefined | transaction_polling.

action_to_activity(transaction, undefined) ->
    undefined;
action_to_activity(transaction, next) ->
    transaction_polling.

-spec maybe_migrate(event() | legacy_event(), transfer_type() | undefined) ->
    event().
% Actual events
maybe_migrate(Ev = {created, #{version := ?ACTUAL_FORMAT_VERSION}}, _) ->
    Ev;
maybe_migrate(Ev = {p_transfer, _PEvent}, EventType) ->
    {transaction, {p_transfer, ff_postings_transfer:maybe_migrate(Ev, EventType)}};
% Old events
maybe_migrate({created, #{version := 1, handler := ff_withdrawal} = T}, EventType) ->
    #{
        version     := 1,
        id          := ID,
        handler     := ff_withdrawal,
        source      := SourceAccount,
        destination := DestinationAccount,
        body        := Body,
        params      := #{
            destination := DestinationID,
            source      := SourceID
        }
    } = T,
    maybe_migrate({created, #{
        version       => 2,
        id            => ID,
        transfer_type => withdrawal,
        body          => Body,
        params        => #{
            wallet_id             => SourceID,
            destination_id        => DestinationID,
            wallet_account        => SourceAccount,
            destination_account   => DestinationAccount,
            wallet_cash_flow_plan => #{
                postings => [
                    #{
                        sender   => {wallet, sender_settlement},
                        receiver => {wallet, receiver_destination},
                        volume   => {share, {{1, 1}, operation_amount, default}}
                    }
                ]
            }
        }
    }}, EventType);
maybe_migrate({created, #{version := 1, handler := ff_deposit} = T}, EventType) ->
    #{
        version     := 1,
        id          := ID,
        handler     := ff_deposit,
        source      := SourceAccount,
        destination := DestinationAccount,
        body        := Body,
        params      := #{
            destination := DestinationID,
            source      := SourceID
        }
    } = T,
    maybe_migrate({created, #{
        version       => 2,
        id            => ID,
        transfer_type => deposit,
        body          => Body,
        params        => #{
            wallet_id             => DestinationID,
            source_id             => SourceID,
            wallet_account        => DestinationAccount,
            source_account        => SourceAccount,
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
    }}, EventType);
maybe_migrate({created, T}, EventType) ->
    DestinationID = maps:get(destination, T),
    {ok, DestinationSt} = ff_destination:get_machine(DestinationID),
    DestinationAcc = ff_destination:account(ff_destination:get(DestinationSt)),
    SourceID = maps:get(source, T),
    {ok, SourceSt} = ff_wallet_machine:get(SourceID),
    SourceAcc = ff_wallet:account(ff_wallet_machine:wallet(SourceSt)),
    maybe_migrate({created, T#{
        version     => 1,
        handler     => ff_withdrawal,
        source      => SourceAcc,
        destination => DestinationAcc,
        params => #{
            destination => DestinationID,
            source      => SourceID
        }
    }}, EventType);
maybe_migrate({transfer, PTransferEv}, EventType) ->
    maybe_migrate({p_transfer, PTransferEv}, EventType);
% Other events
maybe_migrate(Ev, _) ->
    Ev.
