%%%
%%% Transfer between 2 accounts
%%%

-module(ff_transfer_new).

-type handler()     :: module().

-define(ACTUAL_FORMAT_VERSION, 3).

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
-type create_params()       :: #{
    transfer_type := transfer_type(),
    id            := id(),
    body          := body(),
    params        := params(),
    external_id   := external_id()
}.
-type preprocess_result()   :: preprocess_result(any()).
-type preprocess_result(T)   ::
    {create_transaction, ff_transaction_new:create_params(T)}.

-type activity()            ::
    new                      |
    routing                  |
    transaction_starting     |
    transaction_polling      |
    transfer                 .
-type new_activity()        ::
    undefined                |
    activity()               .

-type target()           :: #{
    root_id   := id(),
    root_type := transfer_type(),
    target_id := id()
}.

-export_type([args/0]).
-export_type([transfer/1]).
-export_type([handler/0]).
-export_type([params/1]).
-export_type([event/0]).
-export_type([event/2]).
-export_type([status/0]).
-export_type([route/1]).
-export_type([maybe/1]).
-export_type([target/0]).

-export_type([preprocess_result/1]).
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

-export([target_get_id/1]).
-export([target_get_root_id/1]).
-export([target_get_root_type/1]).

%% API

-export([create/3]).
-export([create_events/1]).
-export([revert/1]).
-export([get_revert/1]).

%% Internal

-export([wrap_events_for_parent/3]).
-export([wrap_events_for_parent/4]).
-export([handler_to_type/1]).
-export([collapse/2]).

%% ff_transfer behaviour

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

-callback get_ns() ->
    ns().

-optional_callbacks([get_ns/0]).

%% ff_transfer_machine_new behaviour
-behaviour(ff_transfer_machine_new).
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
-type transfer_type()       :: withdrawal | deposit | revert.
-type ctx()                 :: ff_ctx:ctx().
-type action()              :: ff_transfer_machine_new:action().
-type ns()                  :: machinery:namespace().

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

-spec target_get_root_id(target()) ->
    id().
target_get_root_id(#{root_id := V}) ->
    V.
-spec target_get_root_type(target()) ->
    transfer_type().
target_get_root_type(#{root_type := V}) ->
    V.
-spec target_get_id(target()) ->
    id().
target_get_id(#{target_id := V}) ->
    V.

%% API

-spec create(ns(), create_params(), ctx()) ->
    {ok, [event()]} |
    {error,
        _PostingsTransferError
    }.

create(NS, Params = #{id := ID}, Ctx) ->
    do(fun () ->
        Events = create_events(Params),
        unwrap(ff_transfer_machine_new:create(NS, ID, Events, Ctx))
    end).

-spec create_events(create_params()) ->
    [event()].

create_events(#{
    transfer_type := TransferType,
    id := ID,
    body := Body,
    params := Params,
    external_id := ExternalID
}) ->
    [
        {created, add_external_id(ExternalID, #{
            version       => ?ACTUAL_FORMAT_VERSION,
            id            => ID,
            transfer_type => TransferType,
            body          => Body,
            params        => Params
        })},
        {status_changed, pending}
    ].

-type revert_params()           :: #{
    revert_id     := id(),
    target        := target(),
    reason        := binary(),
    body          := body()
}.

-spec revert(revert_params()) ->
    ok | {error, {not_found, id()} | _TransferError}.

revert(Params = #{target := Target}) ->
    Handler = type_to_handler(target_get_root_type(Target)),
    %% TODO check result and make valid error
    do(fun() ->
        unwrap(ff_transfer_machine_new:revert(Handler:get_ns(), Params))
    end).

-spec get_revert(target()) ->
    {ok, transfer()} | {error, {not_found, id()}}.

get_revert(Target) ->
    Handler     = type_to_handler(target_get_root_type(Target)),
    ID          = target_get_root_id(Target),
    TargetID    = target_get_id(Target),
    do(fun() ->
        Machine = unwrap(ff_transfer_machine_new:get(Handler:get_ns(), ID)),
        Transfer = ff_transfer_machine_new:transfer(Machine),
        case find_child(TargetID, childs(Transfer)) of
            undefined ->
                %% TODO try to find in deep layer
                {error, {not_found, TargetID}};
            Child ->
                Child
        end
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
handler_to_type(ff_revert) ->
    revert;
handler_to_type(ff_deposit_new) ->
    deposit;
handler_to_type(ff_withdrawal_new) ->
    withdrawal.

-spec type_to_handler(transfer_type()) ->
    module().
type_to_handler(revert) ->
    ff_revert;
type_to_handler(deposit) ->
    ff_deposit_new;
type_to_handler(withdrawal) ->
    ff_withdrawal_new.

%% ff_transfer_machine_new behaviour

-spec process_transfer(maybe(transfer())) ->
    {ok, {ff_transfer_machine_new:action(), [ff_transfer_machine_new:event(event())]}} |
    {error, _Reason}.

process_transfer(undefined) ->
    {error, cant_process_undefined_transfer};
process_transfer(Transfer) ->
    do(fun () ->
        {Activity, Data} = handle_preprocess_result(preprocess_transfer(Transfer), activity(Transfer)),
        unwrap(do_process_transfer(Activity, Transfer, Data))
    end).

-spec process_call(_Args, transfer()) ->
    {ok, {ff_transfer_machine_new:action(), [ff_transfer_machine_new:event(event())]}} |
    {error, _Reason}.

process_call({revert, Params}, Transfer) ->
    process_revert(Params, Transfer);
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
    unwrap(Handler:process_transfer(Transfer)),
    poll_transaction_completion(Transfer);
do_process_transfer(_Activity, Transfer, _Data) ->
    Handler = type_to_handler(transfer_type(Transfer)),
    Handler:process_transfer(Transfer).

-spec process_revert(revert_params(), transfer()) ->
    {ok, {ff_transfer_machine_new:action(), [ff_transfer_machine_new:event(event())]}} |
    {error, _Reason}.

process_revert(Params = #{target := Target}, Transfer) ->
    RootID = target_get_root_id(Target),
    ID = target_get_id(Target),
    process_revert_(RootID, ID, Params, Transfer).

process_revert_(RootID, RootID, Params, Transfer) ->
    Type = transfer_type(Transfer),
    Handler = type_to_handler(Type),
    Handler:process_call({revert, Params}, Transfer);
process_revert_(_RootID, ID, Params, Transfer) ->
    case find_child(ID, childs(Transfer)) of
        undefined ->
            %% TODO try to find in deep layer
            {error, {not_found, ID}};
        Child ->
            do(fun () ->
                Type = transfer_type(Child),
                Handler = type_to_handler(Type),
                {Action, Events} = unwrap(Handler:process_call({revert, Params}, Child)),
                {ok, {Action, wrap_events_for_parent(Child, Events, Transfer)}}
            end)
    end.

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
    [wrap_transaction_event(Ev) || Ev <- Events].

wrap_transaction_event(Event) ->
    {transaction, Event}.

check_transaction_complete(Events) ->
    lists:foldl(fun (Ev, Acc) ->
        case Ev of
            {transaction, {p_transfer, {status_changed, committed}}} ->
                true;
            _ ->
                Acc
        end
    end, false, Events).

%%

-spec collapse([event()], undefined | transfer()) ->
    transfer().

collapse(Events, Transfer0) ->
    lists:foldl(fun (Ev, Transfer) -> apply_event(Ev, Transfer) end, Transfer0, Events).

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
    #{id := ID} = SignedEvent,
    Child0 = find_child(ID, childs(T)),
    Child1 = apply_event(Ev, Child0),
    Child2 = set_parent(Child1, T),
    set_activity(transfer, update_childs(Child0, Child2, T));
apply_event_(Ev, T) ->
    Handler = type_to_handler(transfer_type(T)),
    Handler:apply_event(Ev, T).

find_child(_, []) ->
    undefined;
find_child(ID, [Child | Rest]) ->
    case id(Child) =:= ID of
        true ->
            Child;
        false ->
            find_child(ID, Rest)
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
% Old events
maybe_migrate(Ev = {p_transfer, _Event}, EventType) ->
    wrap_transaction_event(migrate_to_transaction(Ev, EventType));
maybe_migrate(Ev = {session_started, _Event}, _EventType) ->
    wrap_transaction_event(Ev);
maybe_migrate(Ev = {session_finished, _Event}, _EventType) ->
    wrap_transaction_event(Ev);
maybe_migrate({created, #{version := 2, transfer_type := withdrawal} = T}, EventType) ->
    #{
        version         := 2,
        id              := ID,
        transfer_type   := withdrawal,
        body            := Body,
        params          := #{
            wallet_id             := SourceID,
            destination_id        := DestinationID
        }
    } = T,
    maybe_migrate({created, #{
        version       => 3,
        id            => ID,
        transfer_type => withdrawal,
        body          => Body,
        params        => #{
            wallet_id             => SourceID,
            destination_id        => DestinationID
        }
    }}, EventType);
maybe_migrate({created, #{version := 2, transfer_type := deposit} = T}, EventType) ->
    maybe_migrate({created, T#{
        version       => 3
    }}, EventType);
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

-spec migrate_to_transaction(legacy_event(), transfer_type()) ->
    ff_transaction_new:event().

%% For Transfer VERSION =:= 3

% При миграции мы не можем заполнить body, status транзакции,
% так как для этого нужно больше событий, которых нет.
% Это нужно учитывать при потреблении мигрированных событий.

migrate_to_transaction({p_transfer, PEvent}, EventType) ->
    case ff_postings_transfer:maybe_migrate(PEvent, EventType) of
        {created, #{id := ID} = PT}->
            {created, #{
                version         => 1,
                id              => ID,
                body            => {0, <<"">>},
                final_cash_flow => [],
                session_data    => [empty, undefined, undefined],
                status          => pending,
                p_transfer      => PT
            }};
        Other ->
            {p_transfer, Other}
    end.
