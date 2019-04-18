%%%
%%% Withdrawal session model
%%%

-module(ff_withdrawal_session_new).

%% API

-export([status/1]).
-export([create/3]).
-export([get/1]).
-export([events/2]).
-export([repair/2]).

%% ff_session behaviour

-behaviour(ff_session).
-export([apply_event/2]).
-export([process_session/1]).
-export([set_session_result/2]).

%%
%% Types
%%
-type session() :: ff_session:session(session_params()).

-type session_params() :: #{
    withdrawal    := withdrawal(),
    provider      := ff_withdrawal_provider:id(),
    adapter       := adapter_with_opts(),
    adapter_state => ff_adapter:state()
}.

-type session_result() :: ff_session:session_result(trx_info(), ff_adapter_withdrawal:failure()).

-type status() :: ff_session:status().

-type params() :: #{
    cash        := ff_transaction:body(),
    sender      := ff_identity:id(),
    receiver    := ff_identity:id(),
    destination := ff_destination:id(),
    provider_id := ff_withdrawal_provider:id()
}.

-export_type([status/0]).
-export_type([session/0]).
-export_type([session_result/0]).

%%
%% Internal types
%%

-type id()                  :: machinery:id().
-type trx_info()            :: dmsl_domain_thrift:'TransactionInfo'().
-type auxst()               :: undefined.
-type result()              :: machinery:result(event(), auxst()).
-type withdrawal()          :: ff_adapter_withdrawal:withdrawal().
-type adapter_with_opts()   :: {ff_withdrawal_provider:adapter(), ff_withdrawal_provider:adapter_opts()}.
-type event()               :: ff_session:event().
-type ctx()                 :: ff_ctx:ctx().

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-define(NS, 'ff/withdrawal/session_v3').

%%
%% API
%%

-spec status(session()) ->
    status().

status(Session) ->
    ff_session:status(Session).

-spec params(session()) ->
    session_params().

params(Session) ->
    ff_session:params(Session).

-spec create(id(), params(), ctx()) ->
    ok | {error, exists}.
create(ID, #{
    cash        := Cash,
    sender      := Sender,
    receiver    := Receiver,
    destination := DestinationID,
    provider_id := ProviderID
}, Ctx) ->
    do(fun() ->
        {ok, DestinationSt} = ff_destination:get_machine(DestinationID),
        Destination = ff_destination:get(DestinationSt),
        Args = #{
            session_type   => ff_session:handler_to_type(?MODULE),
            params => #{
                withdrawal => create_adapter_withdrawal(#{
                    id       => ID,
                    cash     => Cash,
                    sender   => Sender,
                    receiver => Receiver
                }, Destination),
                provider   => ProviderID,
                adapter    => get_adapter_with_opts(ProviderID)
            }
        },
        unwrap(ff_session:create(?NS, ID, Args, Ctx))
    end).

-spec get(id()) ->
    ff_map:result(ff_session_machine:st()).
get(ID) ->
    ff_session_machine:get(?NS, ID).

-spec events(id(), machinery:range()) ->
    {ok, [{integer(), ff_machine:timestamped_event(event())}]} |
    {error, notfound}.

events(ID, Range) ->
    ff_session_machine:events(?NS, ID, Range).

-spec repair(id(), ff_repair:scenario()) ->
    ok | {error, notfound | working}.
repair(ID, Scenario) ->
    ff_session_machine:repair(?NS, ID, Scenario).

%% Behaviour

-spec process_session(session()) -> result().
process_session(#{status := active} = Session) ->
    #{
        adapter := {Adapter, AdapterOpts},
        withdrawal := Withdrawal
    } = params(Session),
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

-spec apply_event(event(), session()) ->
    session().
apply_event({next_state, AdapterState}, Session) ->
    Params = params(Session),
    Session#{params => Params#{adapter_state => AdapterState}}.

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

-spec timer_action({deadline, binary()} | {timeout, non_neg_integer()}) -> machinery:action().
timer_action(Timer) ->
    {set_timer, Timer}.
