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

%% ff_machine
-export([apply_event/2]).
-export([maybe_migrate/2]).

%% ff_repair
-export([set_session_result/2]).

%%
%% Types
%%

-define(ACTUAL_FORMAT_VERSION, 1).
-type session() :: #{
    version       := ?ACTUAL_FORMAT_VERSION,
    id            := id(),
    status        := status(),
    withdrawal    := withdrawal(),
    provider      := ff_withdrawal_provider:id(),
    adapter       := adapter_with_opts(),
    adapter_state => ff_adapter:state()
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
    sender     := ff_identity:identity(),
    receiver   := ff_identity:identity(),
    quote_data => ff_adapter_withdrawal:quote_data()
}.

-type params() :: #{
    resource := ff_destination:resource_full(),
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

-type auxst()        :: undefined.

-type result() :: machinery:result(event(), auxst()).
-type withdrawal() :: ff_adapter_withdrawal:withdrawal().
-type adapter_with_opts() :: {ff_withdrawal_provider:adapter(), ff_withdrawal_provider:adapter_opts()}.
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

-spec maybe_migrate(event() | legacy_event(), ff_machine:migrate_params()) ->
    event().

maybe_migrate(Event = {created, #{version := ?ACTUAL_FORMAT_VERSION}}) ->
    Event;
maybe_migrate({created, Session = #{
    withdrawal := Withdrawal = #{
        destination := #{resource := OldResource}
    }
}}) ->
    {ok, Resource} = ff_destination:process_resource_full(ff_instrument:maybe_migrate_resource(OldResource), undefined),
    NewWithdrawal0 = maps:without([destination], Withdrawal),
    NewWithdrawal1 = NewWithdrawal0#{resource => Resource},
    maybe_migrate({created, Session#{withdrawal => NewWithdrawal1}});
maybe_migrate({created, Session = #{
    withdrawal := Withdrawal = #{
        resource := Resource
    }
}}) ->
    NewResource = ff_instrument:maybe_migrate_resource(Resource),
    maybe_migrate({created, Session#{
        version => 1,
        withdrawal => Withdrawal#{
            resource => NewResource
    }}});
maybe_migrate({next_state, Value}) when Value =/= undefined ->
    {next_state, try_unmarshal_msgpack(Value)};
maybe_migrate({finished, {failed, {'domain_Failure', Code, Reason, SubFailure}}}, _MigrateParams) ->
    {finished, {failed, genlib_map:compact(#{
        code => migrate_unmarshal(string, Code),
        reason => maybe_migrate_unmarshal(string, Reason),
        sub => maybe_migrate_unmarshal(sub_failure, SubFailure)
    })}};
maybe_migrate({finished, {success, {'domain_TransactionInfo', ID, Timestamp, Extra, AddInfo}}}, _MigrateParams) ->
    {finished, {success, genlib_map:compact(#{
        id => ID,
        timestamp => Timestamp,
        extra => Extra,
        additional_info => maybe_migrate_unmarshal(additional_transaction_info, AddInfo)
    })}};
maybe_migrate({finished, {success, {'domain_TransactionInfo', ID, Timestamp, Extra}}}, _MigrateParams) ->
    {finished, {success, genlib_map:compact(#{
        id => ID,
        timestamp => Timestamp,
        extra => Extra
    })}};
% Other events
maybe_migrate(Ev, _MigrateParams) ->
    Ev.

migrate_unmarshal(sub_failure, {'domain_SubFailure', Code, SubFailure}) ->
    genlib_map:compact(#{
        code => migrate_unmarshal(string, Code),
        sub => maybe_migrate_unmarshal(sub_failure, SubFailure)
    });
migrate_unmarshal(additional_transaction_info, AddInfo) ->
    {
        'domain_AdditionalTransactionInfo',
        RRN,
        ApprovalCode,
        AcsURL,
        Pareq,
        MD,
        TermURL,
        Pares,
        ECI,
        CAVV,
        XID,
        CAVVAlgorithm,
        ThreeDSVerification
    } = AddInfo,
    genlib_map:compact(#{
        rrn => maybe_migrate_unmarshal(string, RRN),
        approval_code => maybe_migrate_unmarshal(string, ApprovalCode),
        acs_url => maybe_migrate_unmarshal(string, AcsURL),
        pareq => maybe_migrate_unmarshal(string, Pareq),
        md => maybe_migrate_unmarshal(string, MD),
        term_url => maybe_migrate_unmarshal(string, TermURL),
        pares => maybe_migrate_unmarshal(string, Pares),
        eci => maybe_migrate_unmarshal(string, ECI),
        cavv => maybe_migrate_unmarshal(string, CAVV),
        xid => maybe_migrate_unmarshal(string, XID),
        cavv_algorithm => maybe_migrate_unmarshal(string, CAVVAlgorithm),
        three_ds_verification => maybe_migrate_unmarshal(
            three_ds_verification,
            ThreeDSVerification
        )
    });
migrate_unmarshal(three_ds_verification, Value) when
    Value =:= authentication_successful orelse
    Value =:= attempts_processing_performed orelse
    Value =:= authentication_failed orelse
    Value =:= authentication_could_not_be_performed
->
    Value;
migrate_unmarshal(string, V) when is_binary(V) ->
    V.

maybe_migrate_unmarshal(_Type, undefined) ->
    undefined;
maybe_migrate_unmarshal(Type, V) ->
    migrate_unmarshal(Type, V).

try_unmarshal_msgpack({nl, {'msgpack_Nil'}}) ->
    nil;
try_unmarshal_msgpack({b, V}) when is_boolean(V) ->
    V;
try_unmarshal_msgpack({i, V}) when is_integer(V) ->
    V;
try_unmarshal_msgpack({flt, V}) when is_float(V) ->
    V;
try_unmarshal_msgpack({str, V}) when is_binary(V) ->
    V;
try_unmarshal_msgpack({bin, V}) when is_binary(V) ->
    {binary, V};
try_unmarshal_msgpack({arr, V}) when is_list(V) ->
    [try_unmarshal_msgpack(ListItem) || ListItem <- V];
try_unmarshal_msgpack({obj, V}) when is_map(V) ->
    maps:fold(
        fun(Key, Value, Map) ->
            Map#{try_unmarshal_msgpack(Key) => try_unmarshal_msgpack(Value)}
        end,
        #{},
        V
    );
% Not msgpack value
try_unmarshal_msgpack(V) ->
    V.

-spec process_session(session()) -> result().
process_session(#{status := active} = Session) ->
    #{
        adapter := {Adapter, AdapterOpts},
        withdrawal := Withdrawal
    } = Session,
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
create_session(ID, Data, #{resource := Resource, provider_id := ProviderID}) ->
    #{
        version    => ?ACTUAL_FORMAT_VERSION,
        id         => ID,
        withdrawal => create_adapter_withdrawal(Data, Resource),
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

create_adapter_withdrawal(Data, Resource) ->
    Data#{resource => Resource}.

-spec set_session_status(status(), session()) -> session().
set_session_status(SessionState, Session) ->
    Session#{status => SessionState}.

-spec timer_action({deadline, binary()} | {timeout, non_neg_integer()}) -> machinery:action().
timer_action(Timer) ->
    {set_timer, Timer}.
