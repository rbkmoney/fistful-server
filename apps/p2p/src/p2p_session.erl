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

-export([process_callback/2]).

%% Accessors

-export([id/1]).
-export([transfer_params/1]).
-export([adapter_state/1]).
-export([party_revision/1]).
-export([domain_revision/1]).
-export([route/1]).

%% ff_machine
-export([apply_event/2]).
-export([init/2]).

%% ff_repair
-export([set_session_result/2]).

%%
%% Types
%%
-define(ACTUAL_FORMAT_VERSION, 3).

-opaque session_state() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := id(),
    status := status(),
    transfer_params := transfer_params(),
    route := route(),
    domain_revision := domain_revision(),
    party_revision := party_revision(),
    adapter_state => adapter_state(),
    callbacks => callbacks_index(),
    user_interactions => user_interactions_index(),
    transaction_info => transaction_info(),

    % Deprecated. Remove after MSPF-560 finish
    provider_id_legacy := ff_p2p_provider:id()
}.

-opaque session() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := id(),
    status := status(),
    transfer_params := transfer_params(),
    route := route(),
    domain_revision := domain_revision(),
    party_revision := party_revision(),

    % Deprecated. Remove after MSPF-560 finish
    provider_id_legacy := ff_p2p_provider:id()
}.

-type status() ::
    active |
    {finished, session_result()}.

-type event() ::
    {created, session()} |
    {next_state, adapter_state()} |
    {transaction_bound, transaction_info()} |
    {finished, session_result()} |
    wrapped_callback_event() |
    wrapped_user_interaction_event().

-type wrapped_callback_event() :: p2p_callback_utils:wrapped_event().
-type wrapped_user_interaction_event() :: p2p_user_interaction_utils:wrapped_event().

-type transfer_params() :: #{
    id            := id(),
    body          := body(),
    sender        := ff_resource:resource(),
    receiver      := ff_resource:resource(),
    deadline      => deadline(),
    merchant_fees => ff_fees:final(),
    provider_fees => ff_fees:final()
}.

-type route() :: #{
    provider_id := ff_p2p_provider:id()
}.

-type body() :: ff_transaction:body().

-type transaction_info() :: ff_adapter:transaction_info().
-type adapter_state() :: p2p_adapter:adapter_state().
-type session_result() :: p2p_adapter:finish_status().

-type deadline() :: p2p_adapter:deadline().

-type params() :: #{
    route := route(),
    domain_revision := domain_revision(),
    party_revision := party_revision()
}.

-type p2p_callback_params() :: p2p_callback:process_params().
-type process_callback_response() :: p2p_callback:response().
-type process_callback_error() ::
    {session_already_finished, p2p_adapter:context()}.

-type timeout_error() :: {deadline_reached, deadline()}.

-export_type([id/0]).
-export_type([event/0]).
-export_type([transfer_params/0]).
-export_type([params/0]).
-export_type([status/0]).
-export_type([session_state/0]).
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
-type action() :: machinery:action().
-type adapter_with_opts() :: {ff_p2p_provider:adapter(), ff_p2p_provider:adapter_opts()}.

-type callbacks_index() :: p2p_callback_utils:index().
-type unknown_p2p_callback_error() :: p2p_callback_utils:unknown_callback_error().
-type p2p_callback() :: p2p_callback:callback().
-type p2p_callback_tag() :: p2p_callback:tag().

-type user_interactions_index() :: p2p_user_interaction_utils:index().
-type party_revision() :: ff_party:revision().
-type domain_revision() :: ff_domain_config:revision().
%%
%% API
%%

-spec id(session_state()) ->
    id().

id(#{id := V}) ->
    V.

-spec status(session_state()) ->
    status().

status(#{status := V}) ->
    V.

-spec adapter_state(session_state()) -> adapter_state() | undefined.
adapter_state(SessionState = #{}) ->
    maps:get(adapter_state, SessionState, undefined).

-spec transaction_info(session_state()) -> transaction_info() | undefined.
transaction_info(SessionState = #{}) ->
    maps:get(transaction_info, SessionState, undefined).

-spec party_revision(session_state()) -> party_revision().
party_revision(#{party_revision := PartyRevision}) ->
    PartyRevision.

-spec domain_revision(session_state()) -> domain_revision().
domain_revision(#{domain_revision := DomainRevision}) ->
    DomainRevision.

-spec route(session_state()) -> route().
route(#{route := Route}) ->
    Route.

%% Сущность завершила свою основную задачу по переводу денег. Дальше её состояние будет меняться только
%% изменением дочерних сущностей, например запуском adjustment.
-spec is_finished(session_state()) -> boolean().
is_finished(#{status := {finished, _}}) ->
    true;
is_finished(#{status := active}) ->
    false.

%% Accessors

-spec transfer_params(session_state()) -> transfer_params().
transfer_params(#{transfer_params := V}) ->
    V.

%%

-spec create(id(), transfer_params(), params()) ->
    {ok, [event()]}.
create(ID, TransferParams, #{
    route := Route,
    domain_revision := DomainRevision,
    party_revision := PartyRevision
}) ->
    Session = #{
        version => ?ACTUAL_FORMAT_VERSION,
        id => ID,
        route => Route,
        transfer_params => TransferParams,
        domain_revision => DomainRevision,
        party_revision => PartyRevision,
        status => active
    },
    {ok, [{created, Session}]}.

-spec get_adapter_with_opts(session_state()) -> adapter_with_opts().
get_adapter_with_opts(SessionState) ->
    #{provider_id := ProviderID} = route(SessionState),
    {ok, Provider} =  ff_p2p_provider:get(head, ProviderID),
    {ff_p2p_provider:adapter(Provider), ff_p2p_provider:adapter_opts(Provider)}.

-spec process_session(session_state()) -> result().
process_session(SessionState) ->
    {Adapter, _AdapterOpts} = get_adapter_with_opts(SessionState),
    Context = p2p_adapter:build_context(collect_build_context_params(SessionState)),
    {ok, ProcessResult} = p2p_adapter:process(Adapter, Context),
    #{intent := Intent} = ProcessResult,
    Events0 = process_next_state(ProcessResult, []),
    Events1 = process_transaction_info(ProcessResult, Events0, SessionState),
    process_intent(Intent, Events1, SessionState).

process_next_state(#{next_state := NextState}, Events) ->
    Events ++ [{next_state, NextState}];
process_next_state(_, Events) ->
    Events.

process_transaction_info(#{transaction_info := TrxInfo}, Events, SessionState) ->
    ok = assert_transaction_info(TrxInfo, transaction_info(SessionState)),
    Events ++ [{transaction_bound, TrxInfo}];
process_transaction_info(_, Events, _Session) ->
    Events.

process_intent({sleep, #{timer := Timer} = Data}, Events0, SessionState) ->
    UserInteraction = maps:get(user_interaction, Data, undefined),
    Tag = maps:get(callback_tag, Data, undefined),
    Events1 = process_intent_callback(Tag, SessionState, Events0),
    Events2 = process_user_interaction(UserInteraction, SessionState, Events1),
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
process_intent_callback(Tag, SessionState, Events) ->
    case p2p_callback_utils:get_by_tag(Tag, callbacks_index(SessionState)) of
        {error, {unknown_callback, Tag}} ->
            {ok, CallbackEvents} = p2p_callback:create(#{tag => Tag}),
            CBEvents = p2p_callback_utils:wrap_events(Tag, CallbackEvents),
            Events ++ CBEvents;
        {ok, Callback} ->
            erlang:error({callback_already_exists, Callback})
    end.

process_user_interaction(undefined, _Session, Events) ->
    Events;
process_user_interaction({ID, finish}, SessionState, Events) ->
    case p2p_user_interaction_utils:get_by_id(ID, user_interactions_index(SessionState)) of
        {ok, UserInteraction} ->
            Events ++ p2p_user_interaction_utils:finish(ID, UserInteraction);
        {error, {unknown_user_interaction, ID} = Error} ->
            erlang:error(Error)
    end;
process_user_interaction({ID, {create, Content}}, SessionState, Events) ->
    case p2p_user_interaction_utils:get_by_id(ID, user_interactions_index(SessionState)) of
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

-spec set_session_result(session_result(), session_state()) ->
    result().
set_session_result(Result, #{status := active}) ->
    #{
        events => [{finished, Result}],
        action => unset_timer
    }.

-spec process_callback(p2p_callback_params(), session_state()) ->
    {ok, {process_callback_response(), result()}} |
    {error, {process_callback_error(), result()}}.
process_callback(#{tag := CallbackTag} = Params, SessionState) ->
    {ok, Callback} = find_callback(CallbackTag, SessionState),
    case p2p_callback:status(Callback) of
        succeeded ->
           {ok, {p2p_callback:response(Callback), #{}}};
        pending ->
            case status(SessionState) of
                active ->
                    do_process_callback(Params, Callback, SessionState);
                {finished, _} ->
                    {_Adapter, _AdapterOpts} = get_adapter_with_opts(SessionState),
                    Context = p2p_adapter:build_context(collect_build_context_params(SessionState)),
                    {error, {{session_already_finished, Context}, #{}}}
            end
    end.

-spec find_callback(p2p_callback_tag(), session_state()) ->
    {ok, p2p_callback()} | {error, unknown_p2p_callback_error()}.
find_callback(CallbackTag, SessionState) ->
    p2p_callback_utils:get_by_tag(CallbackTag, callbacks_index(SessionState)).

-spec do_process_callback(p2p_callback_params(), p2p_callback(), session_state()) ->
    {ok, {process_callback_response(), result()}}.

do_process_callback(Params, Callback, SessionState) ->
    {Adapter, _AdapterOpts} = get_adapter_with_opts(SessionState),
    Context = p2p_adapter:build_context(collect_build_context_params(SessionState)),
    {ok, HandleCallbackResult} = p2p_adapter:handle_callback(Adapter, Params, Context),
    #{intent := Intent, response := Response} = HandleCallbackResult,
    Events0 = p2p_callback_utils:process_response(Response, Callback),
    Events1 = process_next_state(HandleCallbackResult, Events0),
    Events2 = process_transaction_info(HandleCallbackResult, Events1, SessionState),
    {ok, {Response, process_intent(Intent, Events2, SessionState)}}.

build_failure({deadline_reached, _Deadline} = Details) ->
    #{
        code => <<"authorization_failed">>,
        sub => #{
            code => <<"deadline_reached">>
        },
        reason => genlib:format(Details)
    }.

-spec callbacks_index(session_state()) -> callbacks_index().
callbacks_index(SessionState) ->
    case maps:find(callbacks, SessionState) of
        {ok, Callbacks} ->
            Callbacks;
        error ->
            p2p_callback_utils:new_index()
    end.

-spec user_interactions_index(session_state()) -> user_interactions_index().
user_interactions_index(SessionState) ->
    case maps:find(user_interactions, SessionState) of
        {ok, UserInteractions} ->
            UserInteractions;
        error ->
            p2p_user_interaction_utils:new_index()
    end.

-spec collect_build_context_params(session_state()) ->
    p2p_adapter:build_context_params().
collect_build_context_params(SessionState) ->
    {_Adapter, AdapterOpts} = get_adapter_with_opts(SessionState),
    #{
        id              => id(SessionState),
        adapter_state   => adapter_state(SessionState),
        transfer_params => transfer_params(SessionState),
        adapter_opts    => AdapterOpts,
        domain_revision => domain_revision(SessionState),
        party_revision  => party_revision(SessionState)
    }.

assert_transaction_info(_TrxInfo, undefined) ->
    ok;
assert_transaction_info(TrxInfo, TrxInfo) ->
    ok;
assert_transaction_info(TrxInfoNew, _TrxInfo) ->
    erlang:error({transaction_info_is_different, TrxInfoNew}).

%% Events apply

-spec apply_event(event(), undefined | session_state()) ->
    session_state().

apply_event({created, SessionState}, undefined) ->
    SessionState;
apply_event({next_state, AdapterState}, SessionState) ->
    SessionState#{adapter_state => AdapterState};
apply_event({transaction_bound, TransactionInfo}, SessionState) ->
    SessionState#{transaction_info => TransactionInfo};
apply_event({finished, Result}, SessionState) ->
    set_session_status({finished, Result}, SessionState);
apply_event({callback, _Ev} = Event, SessionState) ->
    apply_callback_event(Event, SessionState);
apply_event({user_interaction, _Ev} = Event, SessionState) ->
    apply_user_interaction_event(Event, SessionState).

-spec apply_callback_event(wrapped_callback_event(), session_state()) -> session_state().
apply_callback_event(WrappedEvent, SessionState) ->
    Callbacks0 = callbacks_index(SessionState),
    Callbacks1 = p2p_callback_utils:apply_event(WrappedEvent, Callbacks0),
    set_callbacks_index(Callbacks1, SessionState).

-spec set_callbacks_index(callbacks_index(), session_state()) -> session_state().
set_callbacks_index(Callbacks, SessionState) ->
    SessionState#{callbacks => Callbacks}.

-spec apply_user_interaction_event(wrapped_user_interaction_event(), session_state()) -> session_state().
apply_user_interaction_event(WrappedEvent, SessionState) ->
    UserInteractions0 = user_interactions_index(SessionState),
    UserInteractions1 = p2p_user_interaction_utils:apply_event(WrappedEvent, UserInteractions0),
    set_user_interactions_index(UserInteractions1, SessionState).

-spec set_user_interactions_index(user_interactions_index(), session_state()) -> session_state().
set_user_interactions_index(UserInteractions, SessionState) ->
    SessionState#{user_interactions => UserInteractions}.

-spec set_session_status(status(), session_state()) -> session_state().
set_session_status(Status, SessionState) ->
    SessionState#{status => Status}.

-spec init(session_state(), action()) ->
    {list(event()), action() | undefined}.

init(SessionState, Action) ->
    case to_timeout(maps:get(deadline, transfer_params(SessionState), undefined)) of
        {ok, _Timeout} ->
            {[], Action};
        {error, {deadline_reached, _Deadline} = Error} ->
            {[{finished, {failure, build_failure(Error)}}], undefined}
    end.

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
