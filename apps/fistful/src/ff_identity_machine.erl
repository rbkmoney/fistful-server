%%%
%%% Identity machine
%%%
%%% TODOs
%%%
%%%  - I'm still not sure how to intertwine two concepts together which are:
%%%     * it's better to persist only IDs / prototypes,
%%%     * it's easier to work with rich data in runtime.
%%%
%%%    It seems that the more concise way will be to keep refdatas around which
%%%    are IDs until they are materialised into ID + Data tuple any time a model
%%%    updated with these IDs.
%%%
%%%  - We do not handle challenge expiration yet.
%%%

-module(ff_identity_machine).

%% API

-type id()        :: machinery:id().
-type identity()  :: ff_identity:identity().
-type timestamp() :: machinery:timestamp().
-type ctx()       :: ff_ctx:ctx().

-type activity() ::
    {challenge, challenge_id()} |
    undefined                   .

-type st()        :: #{
    activity      := activity(),
    identity      := identity(),
    ctx           := ctx(),
    times         => {timestamp(), timestamp()}
}.

-type challenge_id() ::
    machinery:id().

-export_type([id/0]).

-export([create/3]).
-export([get/1]).
-export([start_challenge/2]).

%% Accessors

-export([identity/1]).
-export([activity/1]).
-export([ctx/1]).
-export([created/1]).
-export([updated/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, do/2, unwrap/1, unwrap/2]).

-define(NS, 'ff/identity').

-type params() :: #{
    party    := ff_party:id(),
    provider := ff_provider:id(),
    class    := ff_identity:class_id()
}.

-spec create(id(), params(), ctx()) ->
    ok |
    {error,
        {provider, notfound} |
        {identity_class, notfound} |
        _SetupContractError |
        exists
    }.

create(ID, #{party := Party, provider := ProviderID, class := IdentityClassID}, Ctx) ->
    do(fun () ->
        Provider      = unwrap(provider, ff_provider:get(ProviderID)),
        IdentityClass = unwrap(identity_class, ff_provider:get_identity_class(IdentityClassID, Provider)),
        Events0       = unwrap(ff_identity:create(ID, Party, Provider, IdentityClass)),
        Identity      = ff_identity:collapse_events(Events0),
        Events1       = unwrap(ff_identity:setup_contract(Identity)),
        unwrap(machinery:start(?NS, ID, {Events0 ++ Events1, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, st()}        |
    {error, notfound} .

get(ID) ->
    do(fun () ->
        collapse(unwrap(machinery:get(?NS, ID, backend())))
    end).

-type challenge_params() :: #{
    id     := challenge_id(),
    class  := ff_identity_class:challenge_class_id(),
    proofs := [ff_identity_challenge:proof()]
}.

-spec start_challenge(id(), challenge_params()) ->
    ok |
    {error,
        notfound |
        {challenge,
            {pending, challenge_id()} |
            {class, notfound} |
            _IdentityChallengeError
        }
    }.

start_challenge(ID, Params) ->
    case machinery:call(?NS, ID, {start_challenge, Params}, backend()) of
        {ok, Reply} ->
            Reply;
        Error ->
            Error
    end.

backend() ->
    fistful:backend(?NS).

%% Accessors

-spec identity(st()) -> identity().
-spec activity(st()) -> activity().
-spec ctx(st())      -> ctx().
-spec created(st())  -> timestamp() | undefined.
-spec updated(st())  -> timestamp() | undefined.

identity(#{identity := V}) -> V.
activity(#{activity := V}) -> V.
ctx(#{ctx := V})           -> V.
created(St)                -> erlang:element(1, times(St)).
updated(St)                -> erlang:element(2, times(St)).

times(St) ->
    genlib_map:get(times, St, {undefined, undefined}).

%% Machinery

-type ts_ev(T) ::
    {ev, timestamp(), T}.

-type ev() ::
    ff_identity:ev().

-type auxst() ::
    #{ctx => ctx()}.

-type machine()      :: machinery:machine(ts_ev(ev()), auxst()).
-type result()       :: machinery:result(ts_ev(ev()), auxst()).
-type handler_opts() :: machinery:handler_opts(_).

-spec init({[ev()], ctx()}, machine(), _, handler_opts()) ->
    result().

init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events    => emit_ts_events(Events),
        aux_state => #{ctx => Ctx}
    }.

%%

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

process_timeout(Machine, _, _Opts) ->
    process_activity(collapse(Machine)).

process_activity(#{activity := {challenge, ChallengeID}} = St) ->
    Identity = identity(St),
    {ok, Events} = ff_identity:poll_challenge_completion(ChallengeID, Identity),
    case Events of
        [] ->
            #{action => set_poll_timer(St)};
        _Some ->
            #{events => emit_ts_events(Events)}
    end.

set_poll_timer(St) ->
    Now = machinery_time:now(),
    Timeout = erlang:max(1, machinery_time:interval(Now, updated(St)) div 1000),
    {set_timer, {timeout, Timeout}}.

%%

-type call() ::
    {start_challenge, challenge_params()}.

-spec process_call(call(), machine(), _, handler_opts()) ->
    {_TODO, result()}.

process_call({start_challenge, Params}, Machine, _, _Opts) ->
    do_start_challenge(Params, collapse(Machine)).

do_start_challenge(Params, #{activity := undefined} = St) ->
    Identity = identity(St),
    handle_result(do(challenge, fun () ->
        #{
            id     := ChallengeID,
            class  := ChallengeClassID,
            proofs := Proofs
        } = Params,
        Class          = ff_identity:class(Identity),
        ChallengeClass = unwrap(class, ff_identity_class:challenge_class(ChallengeClassID, Class)),
        Events         = unwrap(ff_identity:start_challenge(ChallengeID, ChallengeClass, Proofs, Identity)),
        #{
            events => emit_ts_events(Events),
            action => continue
        }
    end));
do_start_challenge(_Params, #{activity := {challenge, ChallengeID}}) ->
    handle_result({error, {challenge, {pending, ChallengeID}}}).

handle_result({ok, R}) ->
    {ok, R};
handle_result({error, _} = Error) ->
    {Error, #{}}.

%%

collapse(#{history := History, aux_state := #{ctx := Ctx}}) ->
    apply_events(History, #{ctx => Ctx}).

apply_events(History, St) ->
    lists:foldl(fun apply_event/2, St, History).

apply_event({_ID, _Ts, TsEv}, St0) ->
    {EvBody, St1} = apply_ts_event(TsEv, St0),
    apply_event_body(ff_identity:hydrate(EvBody, maps:get(identity, St1, undefined)), St1).

apply_event_body(IdentityEv, St) ->
    St#{
        activity => deduce_activity(IdentityEv),
        identity => ff_identity:apply_event(IdentityEv, maps:get(identity, St, undefined))
    }.

deduce_activity({created, _}) ->
    undefined;
deduce_activity({contract_set, _}) ->
    undefined;
deduce_activity({level_changed, _}) ->
    undefined;
deduce_activity({effective_challenge_changed, _}) ->
    undefined;
deduce_activity({challenge, _ChallengeID, {created, _}}) ->
    undefined;
deduce_activity({challenge, ChallengeID, {status_changed, pending}}) ->
    {challenge, ChallengeID};
deduce_activity({challenge, _ChallengeID, {status_changed, _}}) ->
    undefined.

%%

emit_ts_events(Es) ->
    emit_ts_events(Es, machinery_time:now()).

emit_ts_events(Es, Ts) ->
    [{ev, Ts, ff_identity:dehydrate(Body)} || Body <- Es].

apply_ts_event({ev, Ts, Body}, St = #{times := {Created, _Updated}}) ->
    {Body, St#{times => {Created, Ts}}};
apply_ts_event({ev, Ts, Body}, St = #{}) ->
    {Body, St#{times => {Ts, Ts}}}.
