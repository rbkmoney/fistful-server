%%%
%%% Withdrawal session model
%%%

-module(ff_withdrawal_session).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% Accessors

-export([id/1]).
-export([status/1]).
-export([adapter_state/1]).
-export([route/1]).
-export([withdrawal/1]).
-export([result/1]).

%% API

-export([create/3]).
-export([process_session/1]).
-export([process_callback/2]).

-export([get_adapter_with_opts/1]).
-export([get_adapter_with_opts/2]).

%% ff_machine
-export([apply_event/2]).

%% ff_repair
-export([set_session_result/2]).

%%
%% Types
%%

-define(ACTUAL_FORMAT_VERSION, 5).
-type session_state() :: #{
    id            := id(),
    status        := status(),
    withdrawal    := withdrawal(),
    route         := route(),
    adapter_state => ff_adapter:state(),
    callbacks     => callbacks_index(),
    result        => session_result(),

    % Deprecated. Remove after MSPF-560 finish
    provider_legacy => binary() | ff_payouts_provider:id()
}.

-type session() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := id(),
    status := status(),
    withdrawal := withdrawal(),
    route := route(),

    % Deprecated. Remove after MSPF-560 finish
    provider_legacy => binary() | ff_payouts_provider:id()
}.

-type session_result() :: {success, ff_adapter_withdrawal:transaction_info()}
                        | {failed, ff_adapter_withdrawal:failure()}.

-type status() :: active
    | {finished, success | {failed, ff_adapter_withdrawal:failure()}}.

-type event() :: {created, session()}
    | {next_state, ff_adapter:state()}
    | {finished, session_result()}
    | wrapped_callback_event().

-type wrapped_callback_event() :: ff_withdrawal_callback_utils:wrapped_event().

-type data() :: #{
    id         := id(),
    cash       := ff_transaction:body(),
    sender     := ff_identity:identity_state(),
    receiver   := ff_identity:identity_state(),
    quote_data => ff_adapter_withdrawal:quote_data()
}.

-type route() :: ff_withdrawal_routing:route().

-type params() :: #{
    resource := ff_destination:resource_full(),
    route := route(),
    withdrawal_id := ff_withdrawal:id()
}.


-type callback_params() :: ff_withdrawal_callback:process_params().
-type process_callback_response() :: ff_withdrawal_callback:response().
-type process_callback_error() :: {session_already_finished, session_finished_params()}.

-type session_finished_params() :: #{
    withdrawal := withdrawal(),
    state := ff_adapter:state(),
    opts := ff_withdrawal_provider:adapter_opts()
}.

-export_type([id/0]).
-export_type([data/0]).
-export_type([event/0]).
-export_type([route/0]).
-export_type([params/0]).
-export_type([status/0]).
-export_type([session_state/0]).
-export_type([session/0]).
-export_type([session_result/0]).
-export_type([callback_params/0]).
-export_type([process_callback_response/0]).
-export_type([process_callback_error/0]).

%%
%% Internal types
%%
-type id() :: machinery:id().

-type auxst()        :: undefined.

-type result() :: machinery:result(event(), auxst()).
-type withdrawal() :: ff_adapter_withdrawal:withdrawal().
-type callbacks_index() :: ff_withdrawal_callback_utils:index().
-type adapter_with_opts() :: {ff_withdrawal_provider:adapter(), ff_withdrawal_provider:adapter_opts()}.

%%
%% Accessors
%%

-spec id(session_state()) ->
    id().

id(#{id := V}) ->
    V.

-spec status(session_state()) ->
    status().

status(#{status := V}) ->
    V.

-spec route(session_state()) ->
    route().

route(#{route := V}) ->
    V.

-spec withdrawal(session_state()) ->
    withdrawal().

withdrawal(#{withdrawal := V}) ->
    V.

-spec adapter_state(session_state()) -> ff_adapter:state().

adapter_state(Session) ->
    maps:get(adapter_state, Session, undefined).

-spec callbacks_index(session_state()) -> callbacks_index().
callbacks_index(Session) ->
    case maps:find(callbacks, Session) of
        {ok, Callbacks} ->
            Callbacks;
        error ->
            ff_withdrawal_callback_utils:new_index()
    end.

-spec result(session_state()) ->
    session_result() | undefined.

result(Session) ->
    maps:get(result, Session, undefined).

%%
%% API
%%

-spec create(id(), data(), params()) ->
    {ok, [event()]}.
create(ID, Data, Params) ->
    Session = create_session(ID, Data, Params),
    {ok, [{created, Session}]}.

-spec apply_event(event(), undefined | session_state()) ->
    session_state().

apply_event({created, Session}, undefined) ->
    Session;
apply_event({next_state, AdapterState}, Session) ->
    Session#{adapter_state => AdapterState};
apply_event({finished, Result}, Session0) ->
    Session1 = Session0#{result => Result},
    set_session_status({finished, Result}, Session1);
apply_event({callback, _Ev} = WrappedEvent, Session) ->
    Callbacks0 = callbacks_index(Session),
    Callbacks1 = ff_withdrawal_callback_utils:apply_event(WrappedEvent, Callbacks0),
    set_callbacks_index(Callbacks1, Session).

-spec process_session(session_state()) -> result().
process_session(#{status := active, withdrawal := Withdrawal, route := Route} = SessionState) ->
    {Adapter, AdapterOpts} = get_adapter_with_opts(Route),
    ASt = maps:get(adapter_state, SessionState, undefined),
    case ff_adapter_withdrawal:process_withdrawal(Adapter, Withdrawal, ASt, AdapterOpts) of
        {ok, Intent, ASt} ->
            process_intent(Intent, SessionState);
        {ok, Intent, NextASt} ->
            Events = process_next_state(NextASt),
            process_intent(Intent, SessionState, Events);
        {ok, Intent} ->
            process_intent(Intent, SessionState)
    end.

-spec set_session_result(session_result(), session_state()) ->
    result().
set_session_result(Result, #{status := active}) ->
    #{
        events => [{finished, Result}],
        action => unset_timer
    }.

-spec process_callback(callback_params(), session_state()) ->
    {ok, {process_callback_response(), result()}} |
    {error, {process_callback_error(), result()}}.
process_callback(#{tag := CallbackTag} = Params, Session) ->
    {ok, Callback} = find_callback(CallbackTag, Session),
    case ff_withdrawal_callback:status(Callback) of
        succeeded ->
           {ok, {ff_withdrawal_callback:response(Callback), #{}}};
        pending ->
            case status(Session) of
                active ->
                    do_process_callback(Params, Callback, Session);
                {finished, _} ->
                    {error, {{session_already_finished, make_session_finish_params(Session)}, #{}}}
            end
    end.

%%
%% Internals
%%

find_callback(CallbackTag, Session) ->
    ff_withdrawal_callback_utils:get_by_tag(CallbackTag, callbacks_index(Session)).

do_process_callback(CallbackParams, Callback, Session) ->
    {Adapter, AdapterOpts} = get_adapter_with_opts(route(Session)),
    Withdrawal = withdrawal(Session),
    AdapterState = adapter_state(Session),
    {ok, #{
        intent := Intent,
        response := Response
    } = Result} = ff_adapter_withdrawal:handle_callback(Adapter, CallbackParams, Withdrawal, AdapterState, AdapterOpts),
    Events0 = process_next_state(genlib_map:get(next_state, Result)),
    Events1 = ff_withdrawal_callback_utils:process_response(Response, Callback),
    {ok, {Response, process_intent(Intent, Session, Events0 ++ Events1)}}.

make_session_finish_params(Session) ->
    {_Adapter, AdapterOpts} = get_adapter_with_opts(route(Session)),
    #{
        withdrawal => withdrawal(Session),
        state => adapter_state(Session),
        opts => AdapterOpts
    }.

process_next_state(undefined) ->
    [];
process_next_state(NextASt) ->
    [{next_state, NextASt}].

process_intent(Intent, Session, AdditionalEvents) ->
    #{events := Events0} = Result = process_intent(Intent, Session),
    Events1 = Events0 ++ AdditionalEvents,
    Result#{events => Events1}.

process_intent({finish, Result}, _Session) ->
    #{
        events => [{finished, Result}],
        action => unset_timer
    };
process_intent({sleep, #{timer := Timer} = Params}, Session) ->
    CallbackEvents = create_callback(Params, Session),
    #{
        events => CallbackEvents,
        action => maybe_add_tag_action(Params, [timer_action(Timer)])
    }.

%%

-spec create_session(id(), data(), params()) ->
    session().
create_session(ID, Data, #{withdrawal_id := WdthID, resource := Res, route := Route}) ->
    #{
        version    => ?ACTUAL_FORMAT_VERSION,
        id         => ID,
        withdrawal => create_adapter_withdrawal(Data, Res, WdthID),
        route      => Route,
        status     => active
    }.

create_callback(#{tag := Tag}, Session) ->
    case ff_withdrawal_callback_utils:get_by_tag(Tag, callbacks_index(Session)) of
        {error, {unknown_callback, Tag}} ->
            {ok, CallbackEvents} = ff_withdrawal_callback:create(#{tag => Tag}),
            ff_withdrawal_callback_utils:wrap_events(Tag, CallbackEvents);
        {ok, Callback} ->
            erlang:error({callback_already_exists, Callback})
    end;
create_callback(_, _) ->
    [].

-spec convert_identity_state_to_adapter_identity(ff_identity:identity_state()) ->
    ff_adapter_withdrawal:identity().

convert_identity_state_to_adapter_identity(IdentityState) ->
    Identity = #{
        id => ff_identity:id(IdentityState)
    },
    case ff_identity:effective_challenge(IdentityState) of
        {ok, ChallengeID} ->
            case ff_identity:challenge(ChallengeID, IdentityState) of
                {ok, Challenge} ->
                    Identity#{effective_challenge => #{
                        id => ChallengeID,
                        proofs => ff_identity_challenge:proofs(Challenge)
                    }};
                _ ->
                    Identity
            end;
        _ ->
            Identity
    end.

-spec get_adapter_with_opts(ff_withdrawal_routing:route()) ->
    adapter_with_opts().
get_adapter_with_opts(Route) ->
    ProviderID = ff_withdrawal_routing:get_provider(Route),
    TerminalID = ff_withdrawal_routing:get_terminal(Route),
    get_adapter_with_opts(ProviderID, TerminalID).

-spec get_adapter_with_opts(ProviderID, TerminalID) -> adapter_with_opts() when
    ProviderID :: ff_payouts_provider:id(),
    TerminalID :: ff_payouts_terminal:id() | undefined.
get_adapter_with_opts(ProviderID, TerminalID) when is_integer(ProviderID) ->
    {ok, Provider} = ff_payouts_provider:get(ProviderID),
    ProviderOpts = ff_payouts_provider:adapter_opts(Provider),
    TerminalOpts = get_adapter_terminal_opts(TerminalID),
    {ff_payouts_provider:adapter(Provider), maps:merge(ProviderOpts, TerminalOpts)}.

get_adapter_terminal_opts(undefined) ->
    #{};
get_adapter_terminal_opts(TerminalID) ->
    {ok, Terminal} = ff_payouts_terminal:get(TerminalID),
    ff_payouts_terminal:adapter_opts(Terminal).

create_adapter_withdrawal(#{id := SesID, sender := Sender, receiver := Receiver} = Data, Resource, WdthID) ->
    Data#{
        sender => convert_identity_state_to_adapter_identity(Sender),
        receiver => convert_identity_state_to_adapter_identity(Receiver),
        resource => Resource,
        id => WdthID,
        session_id => SesID
    }.

-spec set_session_status({finished, session_result()}, session_state()) -> session_state().
set_session_status({finished, {success, _}}, SessionState) ->
    SessionState#{status => {finished, success}};
set_session_status(Status = {finished, {failed, _}}, SessionState) ->
    SessionState#{status => Status}.

-spec set_callbacks_index(callbacks_index(), session_state()) -> session_state().
set_callbacks_index(Callbacks, Session) ->
    Session#{callbacks => Callbacks}.

-spec timer_action({deadline, binary()} | {timeout, non_neg_integer()}) -> machinery:action().
timer_action(Timer) ->
    {set_timer, Timer}.

-spec maybe_add_tag_action(SleepIntentParams :: map(), [machinery:action()]) -> [machinery:action()].
maybe_add_tag_action(#{tag := Tag}, Actions) ->
    [{tag, Tag} | Actions];
maybe_add_tag_action(_, Actions) ->
    Actions.
