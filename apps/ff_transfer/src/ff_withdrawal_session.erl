%%%
%%% Withdrawal session model
%%%

-module(ff_withdrawal_session).

%% API

-export([status/1]).

-export([create/3]).
-export([process_session/1]).

%% ff_machine
-export([apply_event/2]).

%% ff_repair
-export([set_session_result/2]).

%%
%% Types
%%

-type session() :: #{
    id            := id(),
    status        := status(),
    withdrawal    := withdrawal(),
    provider      := ff_withdrawal_provider:id(),
    adapter       := adapter_with_opts(),
    adapter_state => ff_adapter:state()
}.

-type session_result() :: {success, trx_info()} | {failed, ff_adapter_withdrawal:failure()}.

-type status() :: active
    | {finished, session_result()}.

-type event() :: {created, session()}
    | {next_state, ff_adapter:state()}
    | {finished, session_result()}.

-type data() :: #{
    id       := id(),
    cash     := ff_transaction:body(),
    sender   := ff_identity:identity(),
    receiver := ff_identity:identity()
}.

-type params() :: #{
    destination := ff_destination:id(),
    provider_id := ff_withdrawal_provider:id()
}.

-export_type([data/0]).
-export_type([event/0]).
-export_type([params/0]).
-export_type([status/0]).
-export_type([session/0]).
-export_type([session_result/0]).

%%
%% Internal types
%%
-type id() :: machinery:id().

-type trx_info() :: dmsl_domain_thrift:'TransactionInfo'().

-type auxst()        :: undefined.

-type result() :: machinery:result(event(), auxst()).
-type withdrawal() :: ff_adapter_withdrawal:withdrawal().
-type adapter_with_opts() :: {ff_withdrawal_provider:adapter(), ff_withdrawal_provider:adapter_opts()}.

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
process_session(#{status := active} = Session) ->
    #{
        adapter := {Adapter, AdapterOpts},
        withdrawal := Withdrawal
    } = Session,
    ASt = maps:get(adapter_state, Session, undefined),
    case ff_adapter_withdrawal:process_withdrawal(Adapter, Withdrawal, ASt, AdapterOpts) of
        {ok, Intent, NextState} ->
            process_intent(Intent, NextState);
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

process_intent(Intent, NextState) ->
    #{events := Events0} = Result = process_intent(Intent),
    Events1 = Events0 ++ [{next_state, NextState}],
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
create_session(ID, Data, #{destination := DestinationID, provider_id := ProviderID}) ->
    {ok, DestinationSt} = ff_destination:get_machine(DestinationID),
    Destination = ff_destination:get(DestinationSt),
    #{
        id         => ID,
        withdrawal => create_adapter_withdrawal(Data, Destination),
        provider   => ProviderID,
        adapter    => get_adapter_with_opts(ProviderID),
        status     => active
    }.

-spec get_adapter_with_opts(ff_payouts_provider:id() | ff_withdrawal_provider:id()) -> adapter_with_opts().
get_adapter_with_opts(ProviderID) when is_integer(ProviderID) ->
    %% new_style
    Provider =  unwrap(ff_payouts_provider:get(ProviderID)),
    {ff_payouts_provider:adapter(Provider), ff_payouts_provider:adapter_opts(Provider)};
get_adapter_with_opts(ProviderID) when is_binary(ProviderID) ->
    %% old style
    %% TODO remove after update
    {ok, Provider} = ff_withdrawal_provider:get(ProviderID),
    {ff_withdrawal_provider:adapter(Provider), ff_withdrawal_provider:adapter_opts(Provider)}.

create_adapter_withdrawal(Data, Destination) ->
    Data#{destination => Destination}.

-spec set_session_status(status(), session()) -> session().
set_session_status(SessionState, Session) ->
    Session#{status => SessionState}.

-spec timer_action({deadline, binary()} | {timeout, non_neg_integer()}) -> machinery:action().
timer_action(Timer) ->
    {set_timer, Timer}.
