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
-export([party_revision/1]).
-export([domain_revision/1]).

%% ff_machine
-export([apply_event/2]).
-export([maybe_migrate/1]).
-export([check_deadline/1]).

%% ff_repair
-export([set_session_result/2]).

%%
%% Types
%%
-define(ACTUAL_FORMAT_VERSION, 1).

-opaque session() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := id(),
    status := status(),
    transfer_params := transfer_params(),
    provider_id := ff_p2p_provider:id(),
    domain_revision := domain_revision(),
    party_revision := party_revision(),
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
    {transaction_bound, ff_adapter:transaction_info()} |
    {finished, session_result()} |
    wrapped_callback_event() |
    wrapped_user_interaction_event().

-type wrapped_callback_event() :: p2p_callback_utils:wrapped_event().
-type wrapped_user_interaction_event() :: p2p_user_interaction_utils:wrapped_event().

-type transfer_params() :: #{
    id       := id(),
    body     := body(),
    sender   := ff_resource:resource(),
    receiver := ff_resource:resource(),
    deadline => deadline()
}.

-type body() :: ff_transaction:body().

-type adapter_state() :: p2p_adapter:adapter_state().
-type session_result() :: p2p_adapter:finish_status().

-type deadline() :: p2p_adapter:deadline().

-type params() :: #{
    provider_id := ff_p2p_provider:id(),
    domain_revision := domain_revision(),
    party_revision := party_revision()
}.

-type p2p_callback_params() :: p2p_callback:process_params().
-type process_callback_response() :: p2p_callback:response().
-type process_callback_error() ::
    {session_already_finished, p2p_adapter:context()}.

-type timeout_error() :: {deadline_reached, deadline()}.

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
-export_type([timeout_error/0]).

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

-type user_interactions_index() :: p2p_user_interaction_utils:index().
-type party_revision() :: ff_party:revision().
-type domain_revision() :: ff_domain_config:revision().

%% Pipeline

-import(ff_pipeline, [unwrap/1]).

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

-spec adapter_state(session()) -> adapter_state() | undefined.
adapter_state(Session = #{}) ->
    maps:get(adapter_state, Session, undefined).

-spec party_revision(session()) -> party_revision().
party_revision(#{party_revision := PartyRevision}) ->
    PartyRevision.

-spec domain_revision(session()) -> domain_revision().
domain_revision(#{domain_revision := DomainRevision}) ->
    DomainRevision.

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

%%
-spec create(id(), transfer_params(), params()) ->
    {ok, [event()]}.
create(ID, TransferParams, #{
    provider_id := ProviderID,
    domain_revision := DomainRevision,
    party_revision := PartyRevision
}) ->
    Session = #{
        version => ?ACTUAL_FORMAT_VERSION,
        id => ID,
        transfer_params => TransferParams,
        provider_id => ProviderID,
        domain_revision => DomainRevision,
        party_revision => PartyRevision,
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
    {Adapter, _AdapterOpts} = adapter(Session),
    Context = p2p_adapter:build_context(collect_build_context_params(Session)),
    {ok, ProcessResult} = p2p_adapter:process(Adapter, Context),
    #{intent := Intent} = ProcessResult,
    Events0 = process_next_state(ProcessResult, []),
    Events1 = process_transaction_info(ProcessResult, Events0),
    process_intent(Intent, Events1, Session).

process_next_state(#{next_state := NextState}, Events) ->
    Events ++ [{next_state, NextState}];
process_next_state(_, Events) ->
    Events.

process_transaction_info(#{transaction_info := TrxInfo}, Events) ->
    Events ++ [{transaction_bound, TrxInfo}];
process_transaction_info(_, Events) ->
    Events.

process_intent({sleep, #{timer := Timer} = Data}, Events0, Session) ->
    UserInteraction = maps:get(user_interaction, Data, undefined),
    Tag = maps:get(callback_tag, Data, undefined),
    Events1 = process_intent_callback(Tag, Session, Events0),
    Events2 = process_user_interaction(UserInteraction, Session, Events1),
    #{
        events => Events2,
        action => maybe_add_tag_action(Tag, [timer_action(Timer)])
    };
process_intent({finish, Result}, Events, _Session) ->
    #{
        events => Events ++ [{finished, Result}],
        action => unset_timer
    }.

process_intent_callback(undefined, _Session, Events) ->
    Events;
process_intent_callback(Tag, Session, Events) ->
    case p2p_callback_utils:get_by_tag(Tag, callbacks_index(Session)) of
        {error, {unknown_callback, Tag}} ->
            {ok, CallbackEvents} = p2p_callback:create(#{tag => Tag}),
            CBEvents = p2p_callback_utils:wrap_events(Tag, CallbackEvents),
            Events ++ CBEvents;
        {ok, Callback} ->
            erlang:error({callback_already_exists, Callback})
    end.

process_user_interaction(undefined, _Session, Events) ->
    Events;
process_user_interaction({ID, finish}, Session, Events) ->
    case p2p_user_interaction_utils:get_by_id(ID, user_interactions_index(Session)) of
        {ok, UserInteraction} ->
            Events ++ p2p_user_interaction_utils:finish(ID, UserInteraction);
        {error, {unknown_user_interaction, ID} = Error} ->
            erlang:error(Error)
    end;
process_user_interaction({ID, {create, Content}}, Session, Events) ->
    case p2p_user_interaction_utils:get_by_id(ID, user_interactions_index(Session)) of
        {error, {unknown_user_interaction, ID}} ->
            {ok, UserInteractionEvents} = p2p_user_interaction:create(#{id => ID, content => Content}),
            Events ++ p2p_user_interaction_utils:wrap_events(ID, UserInteractionEvents);
        {ok, UI} ->
            erlang:error({user_interaction_already_exists, UI})
    end.

-spec timer_action({deadline, binary()} | {timeout, non_neg_integer()}) -> machinery:action().
timer_action(Timer) ->
    {set_timer, Timer}.

-spec maybe_add_tag_action(machinery:tag(), [machinery:action()]) -> [machinery:action()].
maybe_add_tag_action(undefined, Actions) ->
    Actions;
maybe_add_tag_action(Tag, Actions) ->
    [{tag, Tag} | Actions].

-spec set_session_result(session_result(), session()) ->
    result().
set_session_result(Result, #{status := active}) ->
    #{
        events => [{finished, Result}],
        action => unset_timer
    }.

-spec process_callback(p2p_callback_params(), session()) ->
    {ok, {process_callback_response(), result()}} |
    {error, {process_callback_error(), result()}}.
process_callback(#{tag := CallbackTag} = Params, Session) ->
    {ok, Callback} = find_callback(CallbackTag, Session),
    case p2p_callback:status(Callback) of
        succeeded ->
           {ok, {p2p_callback:response(Callback), #{}}};
        pending ->
            case status(Session) of
                active ->
                    do_process_callback(Params, Callback, Session);
                {finished, _} ->
                    {_Adapter, _AdapterOpts} = adapter(Session),
                    Context = p2p_adapter:build_context(collect_build_context_params(Session)),
                    {error, {{session_already_finished, Context}, #{}}}
            end
    end.

-spec find_callback(p2p_callback_tag(), session()) ->
    {ok, p2p_callback()} | {error, unknown_p2p_callback_error()}.
find_callback(CallbackTag, Session) ->
    p2p_callback_utils:get_by_tag(CallbackTag, callbacks_index(Session)).

-spec do_process_callback(p2p_callback_params(), p2p_callback(), session()) ->
    {ok, {process_callback_response(), result()}}.

do_process_callback(Params, Callback, Session) ->
    {Adapter, _AdapterOpts} = adapter(Session),
    Context = p2p_adapter:build_context(collect_build_context_params(Session)),
    {ok, HandleCallbackResult} = p2p_adapter:handle_callback(Adapter, Params, Context),
    #{intent := Intent, response := Response} = HandleCallbackResult,
    Events0 = p2p_callback_utils:process_response(Response, Callback),
    Events1 = process_next_state(HandleCallbackResult, Events0),
    Events2 = process_transaction_info(HandleCallbackResult, Events1),
    {ok, {Response, process_intent(Intent, Events2, Session)}}.

build_failure({deadline_reached, _Deadline} = Details) ->
    #{
        code => <<"authorization_failed">>,
        sub => #{
            code => <<"deadline_reached">>
        },
        reason => genlib:format(Details)
    }.

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

-spec collect_build_context_params(session()) ->
    p2p_adapter:build_context_params().
collect_build_context_params(Session) ->
    {_Adapter, AdapterOpts} = adapter(Session),
    #{
        adapter_state   => adapter_state(Session),
        transfer_params => transfer_params(Session),
        adapter_opts    => AdapterOpts,
        domain_revision => domain_revision(Session),
        party_revision  => party_revision(Session)
    }.

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

-spec check_deadline(event()) ->
    ok | {error, ff_failure:failure()}.

check_deadline({created, Session}) ->
    case to_timeout(maps:get(deadline, transfer_params(Session), undefined)) of
        {ok, _Timeout} ->
            ok;
        {error, {deadline_reached, _Deadline} = Error} ->
            {error, build_failure(Error)}
    end;
check_deadline(_) ->
    ok.

-spec to_timeout(deadline() | undefined) ->
    {ok, timeout() | infinity} | {error, timeout_error()}.
to_timeout(undefined) ->
    {ok, infinity};
to_timeout(Deadline) ->
    case Deadline - ff_time:now() of
        Timeout when Timeout > 0 ->
            {ok, Timeout};
        _ ->
            {error, {deadline_reached, Deadline}}
    end.
