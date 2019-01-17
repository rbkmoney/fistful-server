%%%
%%% Withdrawal session machine
%%%
%%% TODOs
%%%
%%%  - The way we ask `fistful` for a machinery backend smells like a circular
%%%    dependency injection.
%%%  - Dehydrate events upon saving.
%%%

-module(ff_withdrawal_session_machine).
-behaviour(machinery).

-define(NS, 'ff/withdrawal/session_v2').

%% API

-export([create/3]).
-export([get/1]).
-export([status/1]).
-export([events/2]).

%% machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%%
%% Types
%%
-type id() :: machinery:id().

-type session() :: #{
    id         => id(),
    status     => status(),
    withdrawal => withdrawal(),
    provider   => ff_withdrawal_provider:id(),
    adapter    => adapter_with_opts()
}.

-type session_result() :: {success, trx_info()} | {failed, ff_adapter_withdrawal:failure()}.

-type status() :: active
    | {finished, {success, _} | {failed, _}}.


-type ev() :: {created, session()}
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

%%
%% Internal types
%%

-type trx_info() :: dmsl_domain_thrift:'TransactionInfo'().

-type auxst()        :: undefined.

-type withdrawal()   :: ff_adapter_withdrawal:withdrawal().
-type machine()      :: machinery:machine(ev(), auxst()).
-type result()       :: machinery:result(ev(), auxst()).
-type handler_opts() :: machinery:handler_opts(_).
-type adapter_with_opts() :: {ff_withdrawal_provider:adapter(), ff_withdrawal_provider:adapter_opts()}.

-type st() :: #{
    session       => session(),
    adapter_state => ff_adapter:state()
}.

-type events()   :: [{integer(), ff_machine:timestamped_event(ev())}].

-export_type([ev/0]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%
%% API
%%

-spec status(session()) ->
    status().

status(#{status := V}) -> V.

%%

-spec create(id(), data(), params()) ->
    ok | {error, exists}.
create(ID, Data, Params) ->
    Session = create_session(ID, Data, Params),
    do(fun () ->
        unwrap(machinery:start(?NS, ID, Session, backend()))
    end).

-spec get(id()) ->
    ff_map:result(session()).
get(ID) ->
    do(fun () ->
        session(collapse(unwrap(machinery:get(?NS, ID, backend()))))
    end).

-spec events(id(), machinery:range()) ->
    {ok, events()} |
    {error, notfound}.

events(ID, Range) ->
    do(fun () ->
        #{history := History} = unwrap(machinery:get(?NS, ID, Range, backend())),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

backend() ->
    fistful:backend(?NS).

%%
%% machinery callbacks
%%

-spec init(session(), machine(), _, handler_opts()) ->
    result().
init(Session, #{}, _, _Opts) ->
    #{
        events => emit_ts_event({created, Session}),
        action => continue
    }.

-spec process_timeout(machine(), _, handler_opts()) ->
    result().
process_timeout(Machine, _, _Opts) ->
    State = collapse(Machine),
    process_session(State).

-spec process_call(any(), machine(), _, handler_opts()) ->
    {_, result()}.
process_call(_CallArgs, #{}, _, _Opts) ->
    {ok, #{}}.

%%
%% Internals
%%

-spec process_session(st()) -> result().
process_session(#{session := #{status := active} = Session} = St) ->
    #{
        adapter := {Adapter, AdapterOpts},
        withdrawal := Withdrawal
    } = Session,
    ASt = maps:get(adapter_state, St, undefined),
    case ff_adapter_withdrawal:process_withdrawal(Adapter, Withdrawal, ASt, AdapterOpts) of
        {ok, Intent, NextState} ->
            process_intent(Intent, NextState);
        {ok, Intent} ->
            process_intent(Intent)
    end.

process_intent(Intent, NextState) ->
    #{events := Events0} = Result = process_intent(Intent),
    Events1 = Events0 ++ emit_ts_event({next_state, NextState}),
    Result#{events => Events1}.

process_intent({finish, Result}) ->
    #{
        events => emit_ts_event({finished, Result})
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

%%

-spec session(st()) -> session().
session(#{session := Session}) ->
    Session.

collapse(#{history := History}) ->
    collapse_history(History, #{}).

collapse_history(History, St) ->
    lists:foldl(fun merge_event/2, St, History).

merge_event({_ID, _, {ev, _Ts, EvBody}}, St) ->
    merge_event_body(EvBody, St).

-spec merge_event_body(ev(), st()) -> st().
merge_event_body({created, Session}, St) ->
    St#{session => Session};
merge_event_body({next_state, AdapterState}, St) ->
    St#{adapter_state => AdapterState};
merge_event_body({finished, Result}, #{session := Session} = St) ->
    St#{session => set_session_status({finished, Result}, Session)}.

%%

emit_ts_event(E) ->
    emit_ts_events([E]).

emit_ts_events(Es) ->
    emit_ts_events(Es, machinery_time:now()).

emit_ts_events(Es, Ts) ->
    [{ev, Ts, Body} || Body <- Es].
