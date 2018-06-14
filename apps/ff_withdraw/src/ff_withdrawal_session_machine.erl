%%%
%%% Withdrawal session machine
%%%
-module(ff_withdrawal_session_machine).
-behaviour(machinery).

-define(NS, 'withdrawal/session').

%% API
-export([create/4]).
-export([get/2]).

%% machinery
-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%%
%% Types
%%

-type session() :: #{
    id => id(),
    status => session_status(),
    withdrawal => withdrawal(),
    adapter => adapter()
}.

-type session_result() :: {success, trx_info()} | {failed, ff_adpt:failure()}.

-type session_status() :: active
    | {finished, session_result()}.

-type ev() :: {created, session()}
    | {next_state, ff_adpt:adapter_state()}
    | {finished, session_result()}.

-type adapter() :: {ff_adapter:adapter(), Opts :: #{binary() => binary()}}.

%%
%% Internal types
%%

-type id() :: machinery:id().

-type trx_info() :: dmsl_domain_thrift:'TransactionInfo'().

-type auxst()        :: #{}.

-type withdrawal() :: ff_adapter_withdrawal:withdrawal().
-type machine() :: machinery:machine(ev(), auxst()).
-type result() :: machinery:result(ev(), auxst()).
-type handler_opts() :: machinery:handler_opts().
-type handler_args() :: machinery:handler_args(_).
-type backend() :: machinery:backend(_).

-type st() :: #{
    session => session(),
    adapter_state => ff_adapter:state()
}.

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%
%% API
%%

-spec create(ID, Adapter, Withdrawal, Be) -> ok | Error when
    ID :: id(),
    Adapter :: adapter(),
    Withdrawal :: withdrawal(),
    Be :: backend(),
    Error :: {error, exists}.
create(ID, Adapter, Withdrawal, Be) ->
    Session = create_session(ID, Adapter, Withdrawal),
    do(fun () ->
        unwrap(machinery:start(?NS, ID, Session, Be))
    end).

-spec get(id(), backend()) -> {ok, session()} | {error, notfound}.
get(ID, Be) ->
    do(fun () ->
        session(collapse(unwrap(machinery:get(?NS, ID, Be))))
    end).

%%
%% machinery callbacks
%%

-spec init(session(), machine(), handler_args(), handler_opts()) ->
    result().
init(Session, #{}, _, _Opts) ->
    #{
        events => emit_ts_event({created, Session}),
        action => continue
    }.

-spec process_timeout(machine(), handler_args(), handler_opts()) ->
    result().
process_timeout(Machine, _, _Opts) ->
    State = collapse(Machine),
    process_session(State).

-spec process_call(any(), machine(), handler_args(), handler_opts()) ->
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

-spec create_session(id(), adapter(), ff_adpt:withdrawal()) -> session().
create_session(ID, Adapter, Withdrawal) ->
    #{
        id => ID,
        withdrawal => Withdrawal,
        adapter => Adapter,
        status => active
    }.

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

merge_event({_ID, _Ts, {ev, _Ts, EvBody}}, St) ->
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

