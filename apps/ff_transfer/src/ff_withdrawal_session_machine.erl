%%%
%%% Withdrawal session machine
%%%
%%% TODOs
%%%
%%%  - The way we ask `fistful` for a machinery backend smells like a circular
%%%    dependency injection.
%%%  - Dehydrate events upon saving.
%%%

-module(ff_withdrawal_session_machine).
-behaviour(machinery).

-define(NS, 'ff/withdrawal/session_v2').

%% ff_transfer behaviour

-behaviour(ff_transfer_session).

-export([create/3]).
-export([get/1]).
-export([status/1]).

%% machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%%
%% Types
%%
-type id() :: machinery:id().

-type session() :: ff_transfer_session:session(#{
    id         => id(),
    status     => session_status(),
    withdrawal => withdrawal(),
    provider   => ff_withdrawal_provider:provider(),
    adapter    => ff_withdrawal_provider:adapter()
}).

-type session_result() :: {success, trx_info()} | {failed, ff_adapter_withdrawal:failure()}.

-type session_status() :: ff_transfer_session:status().


-type ev() :: {created, session()}
    | {next_state, ff_adapter:state()}
    | {finished, session_result()}.

-type data()   :: ff_transfer_session:data().
-type params() :: ff_transfer_session:params(ff_withdrawal:transfer_params()).

%%
%% Internal types
%%

-type trx_info() :: dmsl_domain_thrift:'TransactionInfo'().

-type auxst()        :: undefined.

-type withdrawal()   :: ff_adapter_withdrawal:withdrawal().
-type machine()      :: machinery:machine(ev(), auxst()).
-type result()       :: machinery:result(ev(), auxst()).
-type handler_opts() :: machinery:handler_opts(_).

-type st() :: #{
    session       => session(),
    adapter_state => ff_adapter:state()
}.

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%
%% API
%%

-spec status(session()) ->
    session_status().

status(#{status := V}) -> V.

%%

-spec create(id(), data(), params()) ->
    ok | {error, exists}.
create(ID, Data, Params) ->
    Session = create_session(ID, Data, Params),
    do(fun () ->
        unwrap(machinery:start(?NS, ID, Session, backend()))
    end).

-spec get(id()) ->
    ff_map:result(session()).
get(ID) ->
    do(fun () ->
        session(collapse(unwrap(machinery:get(?NS, ID, backend()))))
    end).

backend() ->
    fistful:backend(?NS).

%%
%% machinery callbacks
%%

-spec init(session(), machine(), _, handler_opts()) ->
    result().
init(Session, #{}, _, _Opts) ->
    #{
        events => emit_ts_event({created, Session}),
        action => continue
    }.

-spec process_timeout(machine(), _, handler_opts()) ->
    result().
process_timeout(Machine, _, _Opts) ->
    State = collapse(Machine),
    process_session(State).

-spec process_call(any(), machine(), _, handler_opts()) ->
    {_, result()}.
process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.

%%
%% Internals
%%

-spec process_session(st()) -> result().
process_session(#{session := #{status := active} = Session} = St) ->
    #{
        adapter := {Adapter, AdapterOpts},
        withdrawal := Withdrawal
    } = Session,
    ASt = maps:get(adapter_state, St, undefined),
    case ff_adapter_withdrawal:process_withdrawal(Adapter, Withdrawal, ASt, AdapterOpts) of
        {ok, Intent, NextState} ->
            process_intent(Intent, NextState);
        {ok, Intent} ->
            process_intent(Intent)
    end.

process_intent(Intent, NextState) ->
    #{events := Events0} = Result = process_intent(Intent),
    Events1 = Events0 ++ emit_ts_event({next_state, NextState}),
    Result#{events => Events1}.

process_intent({finish, Result}) ->
    #{
        events => emit_ts_event({finished, Result})
    };
process_intent({sleep, Timer}) ->
    #{
        events => [],
        action => timer_action(Timer)
    }.

%%

-spec create_session(id(), data(), params()) ->
    session().
create_session(ID, Data = #{cash := Cash}, #{destination := DestinationID}) ->
    {ok, DestinationSt} = ff_destination:get_machine(DestinationID),
    Destination = ff_destination:get(DestinationSt),
    ProviderID = get_provider(Destination, Cash),
    #{
        id         => ID,
        withdrawal => create_adapter_withdrawal(Data, Destination),
        provider   => ProviderID,
        adapter    => get_adapter(ProviderID),
        status     => active
    }.

get_provider(Destination, Cash) ->
    {ok, ProviderID} = ff_withdrawal_provider:choose(Destination, Cash),
    ProviderID.

get_adapter(ProviderID) ->
    {ok, Adapter} = ff_withdrawal_provider:get_adapter(ProviderID),
    Adapter.

create_adapter_withdrawal(Data, Destination) ->
    Data#{destination => Destination}.

-spec set_session_status(session_status(), session()) -> session().
set_session_status(SessionState, Session) ->
    Session#{status => SessionState}.

-spec timer_action({deadline, binary()} | {timeout, non_neg_integer()}) -> machinery:action().
timer_action(Timer) ->
    {set_timer, Timer}.

%%

-spec session(st()) -> session().
session(#{session := Session}) ->
    Session.

collapse(#{history := History}) ->
    collapse_history(History, #{}).

collapse_history(History, St) ->
    lists:foldl(fun merge_event/2, St, History).

merge_event({_ID, _, {ev, _Ts, EvBody}}, St) ->
    merge_event_body(EvBody, St).

-spec merge_event_body(ev(), st()) -> st().
merge_event_body({created, Session}, St) ->
    St#{session => Session};
merge_event_body({next_state, AdapterState}, St) ->
    St#{adapter_state => AdapterState};
merge_event_body({finished, Result}, #{session := Session} = St) ->
    St#{session => set_session_status({finished, Result}, Session)}.

%%

emit_ts_event(E) ->
    emit_ts_events([E]).

emit_ts_events(Es) ->
    emit_ts_events(Es, machinery_time:now()).

emit_ts_events(Es, Ts) ->
    [{ev, Ts, Body} || Body <- Es].

