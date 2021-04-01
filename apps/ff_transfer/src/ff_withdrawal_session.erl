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
-export([transaction_info/1]).

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
    id := id(),
    status := status(),
    withdrawal := withdrawal(),
    route := route(),
    adapter_state => ff_adapter:state(),
    callbacks => callbacks_index(),
    result => session_result(),
    % For validate outstanding TransactionsInfo
    transaction_info => transaction_info(),

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

-type transaction_info() :: ff_adapter_withdrawal:transaction_info().
-type session_result() :: success | {success, transaction_info()} | {failed, ff_adapter_withdrawal:failure()}.
-type status() :: active | {finished, success | {failed, ff_adapter_withdrawal:failure()}}.

-type event() ::
    {created, session()}
    | {next_state, ff_adapter:state()}
    | {transaction_bound, transaction_info()}
    | {finished, session_result()}
    | wrapped_callback_event().

-type wrapped_callback_event() :: ff_withdrawal_callback_utils:wrapped_event().

-type data() :: #{
    id := id(),
    cash := ff_transaction:body(),
    sender := ff_identity:identity_state(),
    receiver := ff_identity:identity_state(),
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
    opts := ff_adapter:opts()
}.

-type id() :: machinery:id().

-type action() ::
    undefined
    | continue
    | {setup_callback, machinery:tag(), machinery:timer()}
    | {setup_timer, machinery:timer()}
    | retry
    | finish.

-type process_result() :: {action(), [event()]}.

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
-export_type([process_result/0]).
-export_type([action/0]).

%%
%% Internal types
%%
-type withdrawal() :: ff_adapter_withdrawal:withdrawal().
-type callbacks_index() :: ff_withdrawal_callback_utils:index().
-type adapter_with_opts() :: {ff_adapter:adapter(), ff_adapter:opts()}.

%%
%% Accessors
%%

-spec id(session_state()) -> id().
id(#{id := V}) ->
    V.

-spec status(session_state()) -> status().
status(#{status := V}) ->
    V.

-spec route(session_state()) -> route().
route(#{route := V}) ->
    V.

-spec withdrawal(session_state()) -> withdrawal().
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

-spec result(session_state()) -> session_result() | undefined.
result(#{result := Result}) ->
    Result;
result(_) ->
    undefined.

-spec transaction_info(session_state()) -> transaction_info() | undefined.
transaction_info(Session = #{}) ->
    maps:get(transaction_info, Session, undefined).

%%
%% API
%%

-spec create(id(), data(), params()) -> {ok, [event()]}.
create(ID, Data, Params) ->
    Session = create_session(ID, Data, Params),
    {ok, [{created, Session}]}.

-spec apply_event(event(), undefined | session_state()) -> session_state().
apply_event({created, Session}, undefined) ->
    Session;
apply_event({next_state, AdapterState}, Session) ->
    Session#{adapter_state => AdapterState};
apply_event({transaction_bound, TransactionInfo}, Session) ->
    Session#{transaction_info => TransactionInfo};
apply_event({finished, success = Result}, Session) ->
    Session#{status => {finished, success}, result => Result};
apply_event({finished, {success, TransactionInfo} = Result}, Session) ->
    %% for backward compatibility with events stored in DB - take TransactionInfo here.
    %% @see ff_adapter_withdrawal:rebind_transaction_info/1
    Session#{status => {finished, success}, result => Result, transaction_info => TransactionInfo};
apply_event({finished, {failed, _} = Result} = Status, Session) ->
    Session#{status => Status, result => Result};
apply_event({callback, _Ev} = WrappedEvent, Session) ->
    Callbacks0 = callbacks_index(Session),
    Callbacks1 = ff_withdrawal_callback_utils:apply_event(WrappedEvent, Callbacks0),
    set_callbacks_index(Callbacks1, Session).

-spec process_session(session_state()) -> process_result().
process_session(#{status := {finished, _}, id := ID, result := Result, withdrawal := Withdrawal}) ->
    % Session has finished, it should notify the withdrawal machine about the fact
    WithdrawalID = ff_adapter_withdrawal:id(Withdrawal),
    case ff_withdrawal_machine:notify_session_finished(WithdrawalID, ID, Result) of
        ok ->
            {finish, []};
        {error, session_not_found} ->
            {retry, []};
        {error, _} = Error ->
            erlang:error({unable_to_finish_session, Error})
    end;
process_session(#{status := active, withdrawal := Withdrawal, route := Route} = SessionState) ->
    {Adapter, AdapterOpts} = get_adapter_with_opts(Route),
    ASt = adapter_state(SessionState),
    {ok, ProcessResult} = ff_adapter_withdrawal:process_withdrawal(Adapter, Withdrawal, ASt, AdapterOpts),
    #{intent := Intent} = ProcessResult,
    Events0 = process_next_state(ProcessResult, [], ASt),
    Events1 = process_transaction_info(ProcessResult, Events0, SessionState),
    process_adapter_intent(Intent, SessionState, Events1).

process_transaction_info(#{transaction_info := TrxInfo}, Events, SessionState) ->
    ok = assert_transaction_info(TrxInfo, transaction_info(SessionState)),
    Events ++ [{transaction_bound, TrxInfo}];
process_transaction_info(_, Events, _Session) ->
    Events.

%% Only one static TransactionInfo within one session

assert_transaction_info(_NewTrxInfo, undefined) ->
    ok;
assert_transaction_info(TrxInfo, TrxInfo) ->
    ok;
assert_transaction_info(NewTrxInfo, _TrxInfo) ->
    erlang:error({transaction_info_is_different, NewTrxInfo}).

-spec set_session_result(session_result(), session_state()) -> process_result().
set_session_result(Result, Session = #{status := active}) ->
    process_adapter_intent({finish, Result}, Session).

-spec process_callback(callback_params(), session_state()) ->
    {ok, {process_callback_response(), process_result()}}
    | {error, {process_callback_error(), process_result()}}.
process_callback(#{tag := CallbackTag} = Params, Session) ->
    {ok, Callback} = find_callback(CallbackTag, Session),
    case ff_withdrawal_callback:status(Callback) of
        succeeded ->
            {ok, {ff_withdrawal_callback:response(Callback), {undefined, []}}};
        pending ->
            case status(Session) of
                active ->
                    do_process_callback(Params, Callback, Session);
                {finished, _} ->
                    {error, {{session_already_finished, make_session_finish_params(Session)}, {undefined, []}}}
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
    {ok, HandleCallbackResult} = ff_adapter_withdrawal:handle_callback(
        Adapter,
        CallbackParams,
        Withdrawal,
        AdapterState,
        AdapterOpts
    ),
    #{intent := Intent, response := Response} = HandleCallbackResult,
    Events0 = ff_withdrawal_callback_utils:process_response(Response, Callback),
    Events1 = process_next_state(HandleCallbackResult, Events0, AdapterState),
    Events2 = process_transaction_info(HandleCallbackResult, Events1, Session),
    {ok, {Response, process_adapter_intent(Intent, Session, Events2)}}.

make_session_finish_params(Session) ->
    {_Adapter, AdapterOpts} = get_adapter_with_opts(route(Session)),
    #{
        withdrawal => withdrawal(Session),
        state => adapter_state(Session),
        opts => AdapterOpts
    }.

process_next_state(#{next_state := NextState}, Events, AdapterState) when NextState =/= AdapterState ->
    Events ++ [{next_state, NextState}];
process_next_state(_Result, Events, _AdapterState) ->
    Events.

process_adapter_intent(Intent, Session, Events0) ->
    {Action, Events1} = process_adapter_intent(Intent, Session),
    {Action, Events0 ++ Events1}.

process_adapter_intent({finish, {success, _TransactionInfo}}, _Session) ->
    %% we ignore TransactionInfo here
    %% @see ff_adapter_withdrawal:rebind_transaction_info/1
    {continue, [{finished, success}]};
process_adapter_intent({finish, Result}, _Session) ->
    {continue, [{finished, Result}]};
process_adapter_intent({sleep, #{timer := Timer, tag := Tag}}, Session) ->
    Events = create_callback(Tag, Session),
    {{setup_callback, Tag, Timer}, Events};
process_adapter_intent({sleep, #{timer := Timer}}, _Session) ->
    {{setup_timer, Timer}, []}.

%%

-spec create_session(id(), data(), params()) -> session().
create_session(ID, Data, #{withdrawal_id := WdthID, resource := Res, route := Route}) ->
    #{
        version => ?ACTUAL_FORMAT_VERSION,
        id => ID,
        withdrawal => create_adapter_withdrawal(Data, Res, WdthID),
        route => Route,
        status => active
    }.

create_callback(Tag, Session) ->
    case ff_withdrawal_callback_utils:get_by_tag(Tag, callbacks_index(Session)) of
        {error, {unknown_callback, Tag}} ->
            {ok, CallbackEvents} = ff_withdrawal_callback:create(#{tag => Tag}),
            ff_withdrawal_callback_utils:wrap_events(Tag, CallbackEvents);
        {ok, Callback} ->
            erlang:error({callback_already_exists, Callback})
    end.

-spec convert_identity_state_to_adapter_identity(ff_identity:identity_state()) -> ff_adapter_withdrawal:identity().
convert_identity_state_to_adapter_identity(IdentityState) ->
    Identity = #{
        id => ff_identity:id(IdentityState)
    },
    case ff_identity:effective_challenge(IdentityState) of
        {ok, ChallengeID} ->
            case ff_identity:challenge(ChallengeID, IdentityState) of
                {ok, Challenge} ->
                    Identity#{
                        effective_challenge => #{
                            id => ChallengeID,
                            proofs => ff_identity_challenge:proofs(Challenge)
                        }
                    };
                _ ->
                    Identity
            end;
        _ ->
            Identity
    end.

-spec get_adapter_with_opts(ff_withdrawal_routing:route()) -> adapter_with_opts().
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

-spec set_callbacks_index(callbacks_index(), session_state()) -> session_state().
set_callbacks_index(Callbacks, Session) ->
    Session#{callbacks => Callbacks}.
