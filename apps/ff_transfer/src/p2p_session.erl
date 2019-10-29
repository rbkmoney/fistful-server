%%%
%%% P2P session model
%%%

-module(p2p_session).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% API

-export([status/1]).

-export([create/3]).
-export([process_session/1]).

-export([get_adapter_with_opts/1]).

-export([process_callback/2]).

%% ff_machine
-export([apply_event/2]).
-export([maybe_migrate/1]).

%% ff_repair
-export([set_session_result/2]).

%%
%% Types
%%

-type session() :: #{
    id := id(),
    status := status(),
    transfer_params := transfer_params(),
    provider_id := ff_p2p_provider:id(),
    adapter := adapter_with_opts(),
    adapter_state => ff_adapter:state(),
    callbacks => callbacks_index(),
    user_interactions => user_interactions_index()
}.

-type session_result() :: {success, ff_adapter:trx_info()} | {failed, ff_adapter:failure()}.

-type status() ::
    active |
    {finished, session_result()}.

-type event() ::
    {created, session()} |
    {next_state, ff_adapter:state()} |
    {finished, session_result()} |
    wrapped_callback_event().

-type wrapped_callback_event() :: p2p_callback_utils:wrapped_event().

-type transfer_params() :: #{
    id := id(),
    cash := ff_transaction:body(),
    sender := p2p_transfer:resource_full(),
    receiver := p2p_transfer:resource_full(),
    deadline => deadline()
}.

-type deadline() :: binary().

-type params() :: #{
    provider_id := ff_p2p_provider:id()
}.

-type callback_params() :: p2p_callback:params().
-type process_callback_response() :: p2p_callback:response().
-type process_callback_error() ::
    {session_already_finished, id()} |
    p2p_callback_utils:unknown_callback_error().

-export_type([event/0]).
-export_type([transfer_params/0]).
-export_type([params/0]).
-export_type([status/0]).
-export_type([session/0]).
-export_type([session_result/0]).
-export_type([deadline/0]).
-export_type([callback_params/0]).
-export_type([process_callback_response/0]).
-export_type([process_callback_error/0]).

%%
%% Internal types
%%
-type id() :: machinery:id().

-type auxst() :: undefined.
-type action() :: poll | continue | undefined.

-type result() :: machinery:result(event(), auxst()).
-type process_result() :: {action(), [event()]}.
-type adapter_with_opts() :: {ff_p2p_provider:adapter(), ff_p2p_provider:adapter_opts()}.
-type legacy_event() :: any().

-type callbacks_index() :: p2p_callback_utils:index().
-type unknown_p2p_callback_error() :: p2p_callback_utils:unknown_callback_error().
-type p2p_callback() :: p2p_callback:callback().
-type p2p_callback_tag() :: p2p_callback:tag().

-type user_interactions_index() :: p2p_user_interaction_utils:index().

%% Pipeline

-import(ff_pipeline, [unwrap/1, do/1]).

%%
%% API
%%

-spec id(session()) ->
    id().

id(#{id := V}) ->
    V.

-spec status(session()) ->
    status().

status(#{status := V}) ->
    V.

%%

-spec create(id(), transfer_params(), params()) ->
    {ok, [event()]}.
create(ID, TransferParams, #{provider_id := ProviderID}) ->
    Session = #{
        id => ID,
        transfer_params => TransferParams,
        provider_id => ProviderID,
        adapter => get_adapter_with_opts(ProviderID),
        status => active
    },
    {ok, [{created, Session}]}.

-spec get_adapter_with_opts(ff_p2p_provider:id()) -> adapter_with_opts().
get_adapter_with_opts(ProviderID) ->
    Provider =  unwrap(ff_p2p_provider:get(ProviderID)),
    {ff_p2p_provider:adapter(Provider), ff_p2p_provider:adapter_opts(Provider)}.

-spec process_session(session()) -> result().
process_session(_Session) ->
    % ASt = maps:get(adapter_state, Session, undefined),
    % TODO add here p2p adapter call
    process_intent({finish, {success, #{id => <<"Some trx id">>, extra => #{}}}}).

% process_intent(Intent, NextASt) ->
%     #{events := Events0} = Result = process_intent(Intent),
%     Events1 = Events0 ++ [{next_state, NextASt}],
%     Result#{events => Events1}.
% process_intent({sleep, Timer}) ->
%     #{
%         events => [],
%         action => timer_action(Timer)
%     };
process_intent({finish, Result}) ->
    #{
        events => [{finished, Result}]
    }.

% -spec timer_action({deadline, binary()} | {timeout, non_neg_integer()}) -> machinery:action().
% timer_action(Timer) ->
%     {set_timer, Timer}.

-spec set_session_result(session_result(), session()) ->
    result().
set_session_result(Result, #{status := active}) ->
    #{
        events => [{finished, Result}],
        action => unset_timer
    }.

-spec process_callback(callback_params(), session()) ->
    {ok, {process_callback_response(), process_result()}} |
    {error, process_callback_error()}.
process_callback(#{tag := CallbackTag}, Session) ->
    case find_callback(CallbackTag, Session) of
        %% TODO add exeption to proto
        {error, {unknown_callback, _}} = Error ->
            Error;
        {ok, Callback} ->
            do_process_callback(CallbackTag, Callback, Session)
    end.

-spec find_callback(p2p_callback_tag(), session()) ->
    {ok, p2p_callback()} | {error, unknown_p2p_callback_error()}.
find_callback(CallbackTag, Session) ->
    p2p_callback_utils:get_by_tag(CallbackTag, callbacks_index(Session)).

-spec do_process_callback(p2p_callback_tag(), p2p_callback(), session()) ->
    {ok, {process_callback_response(), process_result()}} |
    {error, process_callback_error()}.

do_process_callback(CallbackTag, Callback, Session) ->
    do(fun() ->
        valid = unwrap(validate_process_callback(Session)),
        p2p_callback_utils:process_callback(CallbackTag, Callback)
    end).

-spec callbacks_index(session()) -> callbacks_index().
callbacks_index(Session) ->
    case maps:find(callbacks, Session) of
        {ok, Callbacks} ->
            Callbacks;
        error ->
            p2p_callback_utils:new_index()
    end.

%% Callback helpers

-spec validate_process_callback(session()) ->
    {ok, valid} | {error, process_callback_error()}.

validate_process_callback(Session) ->
    case status(Session) of
        active ->
            {ok, valid};
        _Finished ->
            {error, {session_already_finished, id(Session)}}
    end.

%% Events apply

-spec apply_event(event(), undefined | session()) ->
    session().
apply_event(Ev, S) ->
    apply_event_(maybe_migrate(Ev), S).

-spec apply_event_(event(), undefined | session()) ->
    session().
apply_event_({created, Session}, undefined) ->
    Session;
apply_event_({next_state, AdapterState}, Session) ->
    Session#{adapter_state => AdapterState};
apply_event_({finished, Result}, Session) ->
    set_session_status({finished, Result}, Session);
apply_event_({callback, _Ev} = Event, T) ->
    apply_callback_event(Event, T).

-spec apply_callback_event(wrapped_callback_event(), session()) -> session().
apply_callback_event(WrappedEvent, Session) ->
    Adjustments0 = callbacks_index(Session),
    Adjustments1 = p2p_callback_utils:apply_event(WrappedEvent, Adjustments0),
    set_callbacks_index(Adjustments1, Session).

-spec set_callbacks_index(callbacks_index(), session()) -> session().
set_callbacks_index(Callbacks, Session) ->
    Session#{callbacks => Callbacks}.

-spec set_session_status(status(), session()) -> session().
set_session_status(SessionState, Session) ->
    Session#{status => SessionState}.

-spec maybe_migrate(event() | legacy_event()) ->
    event().
% Other events
maybe_migrate(Ev) ->
    Ev.