%%%
%%% P2P session model
%%%

-module(p2p_session).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% API

-export([status/1]).
-export([is_finished/1]).

-export([create/3]).
-export([process_session/1]).

-export([get_adapter_with_opts/1]).

-export([process_callback/2]).

%% Accessors

-export([id/1]).
-export([transfer_params/1]).
-export([adapter/1]).
-export([adapter_state/1]).

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
    adapter_state => adapter_state(),
    callbacks => callbacks_index(),
    user_interactions => user_interactions_index()
}.

-type status() ::
    active |
    {finished, session_result()}.

-type event() ::
    {created, session()} |
    {next_state, adapter_state()} |
    {transaction_bound, ff_adapter:trx_info()} |
    {finished, session_result()} |
    wrapped_callback_event() |
    wrapped_user_interaction_event().

-type wrapped_callback_event() :: p2p_callback_utils:wrapped_event().
-type wrapped_user_interaction_event() :: p2p_user_interaction_utils:wrapped_event().

-type transfer_params() :: p2p_adapter:transfer_params().
-type adapter_state() :: p2p_adapter:adapter_state().
-type session_result() :: p2p_adapter:finish_status().

-type deadline() :: binary().

-type params() :: #{
    provider_id := ff_p2p_provider:id()
}.

-type p2p_callback_params() :: p2p_callback:process_params().
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
-export_type([p2p_callback_params/0]).
-export_type([process_callback_response/0]).
-export_type([process_callback_error/0]).

%%
%% Internal types
%%

-type id() :: machinery:id().

-type auxst() :: undefined.

-type result() :: machinery:result(event(), auxst()).
-type adapter_with_opts() :: {ff_p2p_provider:adapter(), ff_p2p_provider:adapter_opts()}.
-type legacy_event() :: any().

-type callbacks_index() :: p2p_callback_utils:index().
-type unknown_p2p_callback_error() :: p2p_callback_utils:unknown_callback_error().
-type p2p_callback() :: p2p_callback:callback().
-type p2p_callback_tag() :: p2p_callback:tag().
-type p2p_callback_status() :: p2p_callback:status().

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

%% Сущность завершила свою основную задачу по переводу денег. Дальше её состояние будет меняться только
%% изменением дочерних сущностей, например запуском adjustment.
-spec is_finished(session()) -> boolean().
is_finished(#{status := {finished, _}}) ->
    true;
is_finished(#{status := active}) ->
    false.

%% Accessors

-spec transfer_params(session()) -> transfer_params().
transfer_params(#{transfer_params := V}) ->
    V.

-spec adapter(session()) -> adapter_with_opts().
adapter(#{adapter := V}) ->
    V.

-spec adapter_state(session()) -> adapter_state() | undefined.
adapter_state(Session = #{}) ->
    maps:get(adapter_state, Session, undefined).

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
process_session(Session) ->
    ASt = adapter_state(Session),
    {Adapter, Opts} = adapter(Session),
    {ok, {Intent, Data}} = p2p_adapter:process(Adapter, transfer_params(Session), ASt, Opts),
    Events0 = process_next_state(Data, []),
    Events1 = process_trx_info(Data, Events0),
    process_intent(Intent, Events1, Session).

process_next_state(#{next_state := NextState}, Events) ->
    Events ++ [{next_state, NextState}];
process_next_state(_, Events) ->
    Events.

process_trx_info(#{transaction_info := TrxInfo}, Events) ->
    Events ++ [{transaction_bound, TrxInfo}];
process_trx_info(_, Events) ->
    Events.

process_intent({sleep, #{timer := Timer, callback_tag := Tag} = Data}, Events0, Session) ->
    UserInteraction = maps:get(user_interaction, Data, undefined),
    Events1 = process_intent_callback(Tag, Session, Events0),
    Events2 = process_user_interaction(UserInteraction, Session, Events1),
    #{
        events => Events2,
        action => [tag_action(Tag), timer_action(Timer)]
    };
process_intent({finish, Result}, Events, _Session) ->
    #{
        events => Events ++ [{finished, Result}]
    }.

process_intent_callback(Tag, Session, Events) ->
    case p2p_callback_utils:get_by_tag(Tag, callbacks_index(Session)) of
        {ok, _Callback} ->
            Events;
        {error, {unknown_callback, Tag}} ->
            {ok, CallbackEvents} = p2p_callback:create(#{tag => Tag}),
            CBEvents = p2p_callback_utils:wrap_events(Tag, CallbackEvents),
            Events ++ CBEvents
    end.

process_user_interaction(undefined, _Session, Events) ->
    Events;
process_user_interaction({ID, finish}, Session, Events) ->
    case p2p_user_interaction_utils:get_by_id(ID, user_interactions_index(Session)) of
        {ok, UserInteraction} ->
            Events ++ p2p_user_interaction_utils:finish(ID, UserInteraction);
        {error, {unknown_user_interaction, ID} = Error} ->
            _ = logger:warning("Process user interaction failed: ~p", [Error]),
            erlang:error(Error)
    end;
process_user_interaction({ID, {create, Content}}, Session, Events) ->
    case p2p_user_interaction_utils:get_by_id(ID, user_interactions_index(Session)) of
        {ok, _UserInteraction} ->
            %% TODO check if this uint same
            Events;
        {error, {unknown_user_interaction, ID}} ->
            {ok, UserInteractionEvents} = p2p_user_interaction:create(#{id => ID, content => Content}),
            Events ++ p2p_user_interaction_utils:wrap_events(ID, UserInteractionEvents)
    end.

-spec timer_action({deadline, binary()} | {timeout, non_neg_integer()}) -> machinery:action().
timer_action(Timer) ->
    {set_timer, Timer}.

-spec tag_action(machinery:tag()) -> machinery:action().
tag_action(Tag) ->
    {tag, Tag}.

-spec set_session_result(session_result(), session()) ->
    result().
set_session_result(Result, #{status := active}) ->
    #{
        events => [{finished, Result}],
        action => unset_timer
    }.

-spec process_callback(p2p_callback_params(), session()) ->
    {ok, {process_callback_response(), result()}} |
    {error, process_callback_error()}.
process_callback(Params, Session) ->
    case status(Session) of
        active ->
            process_callback_(Params, Session);
        {finished, _} ->
            {error, {session_already_finished, id(Session)}}
    end.

process_callback_(#{tag := CallbackTag} = Params, Session) ->
    case find_callback(CallbackTag, Session) of
        %% TODO add exeption to proto
        {error, {unknown_callback, _}} = Error ->
            Error;
        {ok, Callback} ->
            Status = p2p_callback:status(Callback),
            do_process_callback(Status, Params, Callback, Session)
    end.

-spec find_callback(p2p_callback_tag(), session()) ->
    {ok, p2p_callback()} | {error, unknown_p2p_callback_error()}.
find_callback(CallbackTag, Session) ->
    p2p_callback_utils:get_by_tag(CallbackTag, callbacks_index(Session)).

-spec do_process_callback(p2p_callback_status(), p2p_callback_params(), p2p_callback(), session()) ->
    {ok, {process_callback_response(), result()}} |
    {error, process_callback_error()}.

do_process_callback(succeeded, _Params, Callback, _Session) ->
    {ok, {p2p_callback:response(Callback), #{}}};
do_process_callback(pending, Params, Callback, Session) ->
    do(fun() ->
        valid = unwrap(validate_process_callback(Session)),
        ASt = adapter_state(Session),
        {Adapter, Opts} = adapter(Session),
        {ok, {Intent, Response, Data}} = p2p_adapter:handle_callback(
            Adapter,
            Params,
            transfer_params(Session),
            ASt,
            Opts
        ),
        Events0 = p2p_callback_utils:process_response(Response, Callback),
        Events1 = process_next_state(Data, Events0),
        Events2 = process_trx_info(Data, Events1),
        {Response, process_intent(Intent, Events2, Session)}
    end).

-spec callbacks_index(session()) -> callbacks_index().
callbacks_index(Session) ->
    case maps:find(callbacks, Session) of
        {ok, Callbacks} ->
            Callbacks;
        error ->
            p2p_callback_utils:new_index()
    end.

-spec user_interactions_index(session()) -> user_interactions_index().
user_interactions_index(Session) ->
    case maps:find(user_interactions, Session) of
        {ok, Callbacks} ->
            Callbacks;
        error ->
            p2p_user_interaction_utils:new_index()
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
apply_event_({callback, _Ev} = Event, Session) ->
    apply_callback_event(Event, Session);
apply_event_({user_interaction, _Ev} = Event, Session) ->
    apply_user_interaction_event(Event, Session).

-spec apply_callback_event(wrapped_callback_event(), session()) -> session().
apply_callback_event(WrappedEvent, Session) ->
    Callbacks0 = callbacks_index(Session),
    Callbacks1 = p2p_callback_utils:apply_event(WrappedEvent, Callbacks0),
    set_callbacks_index(Callbacks1, Session).

-spec set_callbacks_index(callbacks_index(), session()) -> session().
set_callbacks_index(Callbacks, Session) ->
    Session#{callbacks => Callbacks}.

-spec apply_user_interaction_event(wrapped_user_interaction_event(), session()) -> session().
apply_user_interaction_event(WrappedEvent, Session) ->
    UserInteractions0 = user_interactions_index(Session),
    UserInteractions1 = p2p_user_interaction_utils:apply_event(WrappedEvent, UserInteractions0),
    set_user_interactions_index(UserInteractions1, Session).

-spec set_user_interactions_index(user_interactions_index(), session()) -> session().
set_user_interactions_index(UserInteractions, Session) ->
    Session#{user_interactions => UserInteractions}.

-spec set_session_status(status(), session()) -> session().
set_session_status(SessionState, Session) ->
    Session#{status => SessionState}.

-spec maybe_migrate(event() | legacy_event()) ->
    event().
% Other events
maybe_migrate(Ev) ->
    Ev.
