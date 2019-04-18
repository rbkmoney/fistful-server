%%%
%%% Session model
%%%

-module(ff_session).

-define(ACTUAL_FORMAT_VERSION, 1).

%% Accessors
-export([status/1]).
-export([params/1]).
-export([get_empty_session_type/0]).
-export([get_session_type/1]).

%% API
-export([get/2]).
-export([create/3]).

-export([create/4]).
-export([process_session/1]).
-export([handler_to_type/1]).
-export([type_to_handler/1]).

%% ff_machine
-export([apply_event/2]).

%% ff_repair
-export([set_session_result/2]).

%% ff_session behaviour
-callback process_session(session()) ->
    result().
-callback set_session_result(session_result(), session()) ->
    result().
-callback apply_event(event(), session()) ->
    session().

%%
%% Types
%%

-type session(T) :: #{
    version       := ?ACTUAL_FORMAT_VERSION,
    type          := session_type(),
    id            := id(),
    status        := status(),
    params        := params(T)
}.

-type session_result(OK, FAIL) :: {success, OK} | {failed, FAIL}.

-type status() ::
    active                      |
    {finished, session_result()}.

-type event() ::
    {created, session()}                |
    {next_state, ff_adapter:state()}    |
    {finished, session_result()}.

-type session_type() ::
    empty             |
    withdrawal.

-type session()         :: session(any()).
-type session_result()  :: session_result(any(), any()).
-type params(T)         :: T.
-type params()          :: params(any()).
-type result()          :: machinery:result(event(), auxst()).

-type create_params()  :: #{
    session_type  := session_type(),
    params        := params()
}.

-export_type([event/0]).
-export_type([params/0]).
-export_type([status/0]).
-export_type([session/0]).
-export_type([session/1]).
-export_type([session_type/0]).
-export_type([session_result/0]).
-export_type([session_result/2]).
-export_type([result/0]).

%%
%% Internal types
%%
-type id()       :: machinery:id().
-type ns()       :: machinery:namespace().
-type ctx()      :: ff_ctx:ctx().
-type auxst()    :: undefined.

%% Pipeline

-import(ff_pipeline, [unwrap/1, do/1]).

%%
%% API
%%

-spec status(session()) ->
    status().

status(#{status := V}) ->
    V.

-spec params(session(T)) -> params(T).
params(#{params := V}) ->
    V.

-spec get_empty_session_type() ->
    session_type().

get_empty_session_type() ->
    empty.

-spec get_session_type(withdrawal) ->
    session_type().

get_session_type(withdrawal) ->
    withdrawal.

%%

-spec get(session_type(), id()) ->
    ff_map:result(ff_session_machine:st()).
get(Type, ID) ->
    Handler = type_to_handler(Type),
    Handler:get(ID).

-spec create(session_type(), id(), params()) ->
    ok | {error, exists}.
create(Type, ID, Params) ->
    Handler = type_to_handler(Type),
    Handler:create(ID, Params, #{}).

%%

-spec create(ns(), id(), create_params(), ctx()) ->
    ok | {error, exists}.
create(NS, ID, #{
    session_type    := SessionType,
    params          := Params
}, Ctx) ->
    do(fun() ->
        Events = [{created, #{
            version     => ?ACTUAL_FORMAT_VERSION,
            id          => ID,
            type        => SessionType,
            status      => active,
            params      => Params
        }}],
        unwrap(ff_session_machine:create(NS, ID, Events, Ctx))
    end).

%% Handler convertors

-spec handler_to_type(module()) ->
    session_type().
handler_to_type(ff_empty_session) ->
    empty;
handler_to_type(ff_withdrawal_session_new) ->
    withdrawal.

-spec type_to_handler(session_type()) ->
    module().
type_to_handler(empty) ->
    ff_empty_session;
type_to_handler(withdrawal) ->
    ff_withdrawal_session_new.

-spec process_session(session()) -> result().
process_session(#{status := active, type := Type} = Session) ->
    Handler = type_to_handler(Type),
    Handler:process_session(Session).

-spec set_session_result(session_result(), session()) ->
    result().
set_session_result(Result, Session = #{type := Type}) ->
    Handler = type_to_handler(Type),
    Handler:set_session_result(Result, Session).

-spec apply_event(event(), undefined | session()) ->
    session().
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev), T).

apply_event_({created, Session}, undefined) ->
    Session;
apply_event_({finished, Result}, Session) ->
    set_session_status({finished, Result}, Session);
apply_event_(Ev, Session = #{type := Type}) ->
    Handler = type_to_handler(Type),
    Handler:apply_event(Ev, Session).

maybe_migrate(Ev = {created, #{version := ?ACTUAL_FORMAT_VERSION}}) ->
    Ev;
% Old events
maybe_migrate({created, T}) ->
    #{
        id         := ID,
        withdrawal := Withdrawal,
        provider   := ProviderID,
        adapter    := Adapter,
        status     := Status
    } = T,
    maybe_migrate({created, #{
        version       => ?ACTUAL_FORMAT_VERSION,
        id            => ID,
        type          => withdrawal,
        status        => Status,
        params        => #{
            withdrawal  => Withdrawal,
            provider    => ProviderID,
            adapter     => Adapter
        }
    }});
% Other events
maybe_migrate(Ev) ->
    Ev.

%%

-spec set_session_status(status(), session()) -> session().
set_session_status(SessionState, Session) ->
    Session#{status => SessionState}.
