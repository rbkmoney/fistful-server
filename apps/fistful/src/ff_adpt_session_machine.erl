%%%
%%% Withdrawal session machine
%%%
-module(ff_adpt_session_machine).
-behaviour(machinery).

-define(NS, adpt_session).

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

-type session_status() :: new
    | active
    | {finished, session_result()}.

-type ev() :: {created, session()}
    | started
    | {next_state, ff_adpt:adapter_state()}
    | {finished, session_result()}.

-type adapter() :: {ff_adpt:adapter(), map()}.

%%
%% Internal types
%%

-type id() :: machinery:id().

-type trx_info() :: dmsl_domain_thrift:'TransactionInfo'().

-type auxst()        :: #{}.

-type withdrawal() :: ff_adpt_withdrawal:withdrawal().
-type machine() :: machinery:machine(ev(), auxst()).
-type result() :: machinery:result(ev(), auxst()).
-type handler_opts() :: machinery:handler_opts().
-type handler_args() :: machinery:handler_args(_).
-type namespace() :: machinery:namespace().
-type backend() :: machinery:backend(_).

-type st() :: #{
    session => session(),
    adapter_state => ff_adpt:adapter_state()
}.

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%
%% API
%%

-spec create(Id, Adapter, Withdrawal, Be) -> {ok, session()} | Error when
    Id :: id(),
    Adapter :: adapter(),
    Withdrawal :: withdrawal(),
    Be :: backend(),
    Error :: {error, exists}.
create(Id, Adapter, Withdrawal, Be) ->
    Session = create_session(Id, Adapter, Withdrawal),
    do(fun () ->
        ok = unwrap(machinery:start(?NS, Id, Session, Be)),
        unwrap(get(Id, Be))
    end).

-spec get(id(), backend()) -> {ok, session()} | {error, any()}.
get(Id, Be) ->
    do(fun () ->
        session(collapse(unwrap(machinery:get(?NS, Id, Be))))
    end).

%%
%% machinery callbacks
%%

-spec init(session(), machine(), handler_args(), handler_opts()) ->
    result().
init(Session, #{}, _, _Opts) ->
    #{
        events => emit_ts_event({created, Session}),
        action => timer_action({timeout, 0})
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
    ASt = maps:get(adapter_state, St, []),
    case ff_adpt_client:process_withdrawal(Adapter, Withdrawal, ASt, AdapterOpts) of
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
create_session(Id, Adapter, Withdrawal) ->
    #{
        id => Id,
        withdrawal => Withdrawal,
        adapter => Adapter,
        status => new
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
merge_event_body(started, #{session := Session} = St) ->
    St#{session => set_session_status(active, Session)};
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

