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
    adapter_state => ff_adapter:state()
}.

-type session_result() :: {success, ff_adapter:trx_info()} | {failed, ff_adapter:failure()}.

-type status() :: active
    | {finished, session_result()}.

-type event() :: {created, session()}
    | {next_state, ff_adapter:state()}
    | {finished, session_result()}.

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

-export_type([event/0]).
-export_type([transfer_params/0]).
-export_type([params/0]).
-export_type([status/0]).
-export_type([session/0]).
-export_type([session_result/0]).
-export_type([deadline/0]).

%%
%% Internal types
%%
-type id() :: machinery:id().

-type auxst() :: undefined.

-type result() :: machinery:result(event(), auxst()).
-type adapter_with_opts() :: {ff_p2p_provider:adapter(), ff_p2p_provider:adapter_opts()}.
-type legacy_event() :: any().

%% Pipeline

-import(ff_pipeline, [unwrap/1]).

%%
%% API
%%

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
    set_session_status({finished, Result}, Session).

-spec set_session_status(status(), session()) -> session().
set_session_status(SessionState, Session) ->
    Session#{status => SessionState}.

-spec maybe_migrate(event() | legacy_event()) ->
    event().
% Other events
maybe_migrate(Ev) ->
    Ev.