%%%
%%% Deposit session machine
%%%
%%% TODOs
%%%
%%%  - The way we ask `fistful` for a machinery backend smells like a circular
%%%    dependency injection.
%%%  - Dehydrate events upon saving.
%%%

-module(ff_deposit_session_machine).
-behaviour(machinery).

-define(NS, 'ff/deposit/session_v1').

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
    resource   => ff_source:resource()
}).

-type session_result() :: {success, ok}.

-type session_status() :: ff_transfer_session:status().


-type ev() :: {created, session()}
    | {finished, session_result()}.

-type data()   :: ff_transfer_session:data().
-type params() :: ff_transfer_session:params(ff_deposit:transfer_params()).

%%
%% Internal types
%%

-type auxst()        :: undefined.

-type machine()      :: machinery:machine(ev(), auxst()).
-type result()       :: machinery:result(ev(), auxst()).
-type handler_opts() :: machinery:handler_opts(_).

-type st() :: #{
    session       => session()
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
process_session(#{session := #{status := active}}) ->
    #{
        events => emit_ts_event({finished, {success, ok}})
    }.

%%

-spec create_session(id(), data(), params()) ->
    session().
create_session(ID, _Data, #{source := SourceID}) ->
    #{
        id         => ID,
        status     => active,
        resource   => SourceID
    }.

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
merge_event_body({finished, Result}, #{session := Session} = St) ->
    St#{session => set_session_status({finished, Result}, Session)}.

-spec set_session_status(session_status(), session()) -> session().
set_session_status(SessionState, Session) ->
    Session#{status => SessionState}.

%%

emit_ts_event(E) ->
    emit_ts_events([E]).

emit_ts_events(Es) ->
    emit_ts_events(Es, machinery_time:now()).

emit_ts_events(Es, Ts) ->
    [{ev, Ts, Body} || Body <- Es].
