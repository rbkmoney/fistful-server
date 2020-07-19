%%%
%%% Withdrawal session model
%%%

-module(ff_withdrawal_session).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% API

-export([status/1]).

-export([create/3]).
-export([process_session/1]).

-export([get_adapter_with_opts/1]).
-export([get_adapter_with_opts/2]).

%% ff_machine
-export([apply_event/2]).

%% ff_repair
-export([set_session_result/2]).

%%
%% Types
%%

-define(ACTUAL_FORMAT_VERSION, 4).
-type session() :: #{
    version       := ?ACTUAL_FORMAT_VERSION,
    id            := id(),
    status        := status(),
    withdrawal    := withdrawal(),
    route         := route(),
    adapter_state => ff_adapter:state(),

    % Deprecated. Remove after MSPF-560 finish
    provider_legacy => binary() | ff_payouts_provider:id()
}.

-type session_result() :: {success, ff_adapter_withdrawal:transaction_info()}
                        | {failed, ff_adapter_withdrawal:failure()}.

-type status() :: active
    | {finished, session_result()}.

-type event() :: {created, session()}
    | {next_state, ff_adapter:state()}
    | {finished, session_result()}.

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

-export_type([data/0]).
-export_type([event/0]).
-export_type([route/0]).
-export_type([params/0]).
-export_type([status/0]).
-export_type([session/0]).
-export_type([session_result/0]).

%%
%% Internal types
%%
-type id() :: machinery:id().

-type auxst()        :: undefined.

-type result() :: machinery:result(event(), auxst()).
-type withdrawal() :: ff_adapter_withdrawal:withdrawal().
-type adapter_with_opts() :: {ff_withdrawal_provider:adapter(), ff_withdrawal_provider:adapter_opts()}.

%%
%% API
%%

-spec status(session()) ->
    status().

status(#{status := V}) ->
    V.

%%

-spec create(id(), data(), params()) ->
    {ok, [event()]}.
create(ID, Data, Params) ->
    Session = create_session(ID, Data, Params),
    {ok, [{created, Session}]}.

-spec apply_event(event(), undefined | session()) ->
    session().

apply_event({created, Session}, undefined) ->
    Session;
apply_event({next_state, AdapterState}, Session) ->
    Session#{adapter_state => AdapterState};
apply_event({finished, Result}, Session) ->
    set_session_status({finished, Result}, Session).

-spec process_session(session()) -> result().
process_session(#{status := active, withdrawal := Withdrawal, route := Route} = Session) ->
    {Adapter, AdapterOpts} = get_adapter_with_opts(Route),
    ASt = maps:get(adapter_state, Session, undefined),
    case ff_adapter_withdrawal:process_withdrawal(Adapter, Withdrawal, ASt, AdapterOpts) of
        {ok, Intent, ASt} ->
            process_intent(Intent);
        {ok, Intent, NextASt} ->
            process_intent(Intent, NextASt);
        {ok, Intent} ->
            process_intent(Intent)
    end.

-spec set_session_result(session_result(), session()) ->
    result().
set_session_result(Result, #{status := active}) ->
    #{
        events => [{finished, Result}],
        action => unset_timer
    }.

%%
%% Internals
%%

process_intent(Intent, NextASt) ->
    #{events := Events0} = Result = process_intent(Intent),
    Events1 = Events0 ++ [{next_state, NextASt}],
    Result#{events => Events1}.

process_intent({finish, Result}) ->
    #{
        events => [{finished, Result}]
    };
process_intent({sleep, Timer}) ->
    #{
        events => [],
        action => timer_action(Timer)
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
    {ff_payouts_provider:adapter(Provider), maps:merge(TerminalOpts, ProviderOpts)}.

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

-spec set_session_status(status(), session()) -> session().
set_session_status(SessionState, Session) ->
    Session#{status => SessionState}.

-spec timer_action({deadline, binary()} | {timeout, non_neg_integer()}) -> machinery:action().
timer_action(Timer) ->
    {set_timer, Timer}.
