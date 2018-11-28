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
-type ctx()       :: ff_ctx:ctx().

-type st() :: ff_machine:st(identity()).

-type challenge_id() ::
    machinery:id().

-export_type([id/0]).

-export([create/3]).
-export([get/1]).
-export([events/2]).

-export([start_challenge/2]).

%% Accessors

-export([identity/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, do/2, unwrap/1]).

-define(NS, 'ff/identity').

-type params() :: #{
    party    := ff_party:id(),
    provider := ff_provider:id(),
    class    := ff_identity:class_id()
}.

-spec create(id() | undefined, params(), ctx()) ->
    {ok, st()} |
    {error,
        _IdentityCreateError |
        {conflict, id()}
    }.

create(ExternalID, #{party := Party, provider := ProviderID, class := IdentityClassID}, Ctx) ->
    do(fun () ->
        ID = unwrap(ff_external_id:check(identity, ExternalID)),
        Events = unwrap(ff_identity:create(ID, Party, ProviderID, IdentityClassID)),
        {ok, Result} = case machinery:start(?NS, ID, {Events, Ctx}, backend()) of
            ok ->
                ff_machine:get(ff_identity, ?NS, ID);
            {error, exists} ->
                compare_events(ID, Events)
        end,
        Result
    end).

-spec get(id()) ->
    {ok, st()}        |
    {error, notfound} .

get(ID) ->
    ff_machine:get(ff_identity, ?NS, ID).

-spec events(id(), machinery:range()) ->
    {ok, [{integer(), ff_machine:timestamped_event(event())}]} |
    {error, notfound}.

events(ID, Range) ->
    do(fun () ->
        #{history := History} = unwrap(machinery:get(?NS, ID, Range, backend())),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
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
        _IdentityChallengeError
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

-spec identity(st()) ->
    identity().

identity(St) ->
    ff_machine:model(St).

%% Machinery

-type event() ::
    ff_identity:event().

-type machine()      :: ff_machine:machine(event()).
-type result()       :: ff_machine:result(event()).
-type handler_opts() :: machinery:handler_opts(_).

-spec init({[event()], ctx()}, machine(), _, handler_opts()) ->
    result().

init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events    => ff_machine:emit_events(Events),
        aux_state => #{ctx => Ctx}
    }.

%%

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

process_timeout(Machine, _, _Opts) ->
    St = ff_machine:collapse(ff_identity, Machine),
    process_activity(deduce_activity(identity(St)), St).

process_activity({challenge, ChallengeID}, St) ->
    Identity = identity(St),
    {ok, Events} = ff_identity:poll_challenge_completion(ChallengeID, Identity),
    case Events of
        [] ->
            #{action => set_poll_timer(St)};
        _Some ->
            #{events => ff_machine:emit_events(Events)}
    end.

set_poll_timer(St) ->
    Now = machinery_time:now(),
    Timeout = erlang:max(1, machinery_time:interval(Now, ff_machine:updated(St)) div 1000),
    {set_timer, {timeout, Timeout}}.

%%

-type call() ::
    {start_challenge, challenge_params()}.

-spec process_call(call(), machine(), _, handler_opts()) ->
    {_TODO, result()}.

process_call({start_challenge, Params}, Machine, _, _Opts) ->
    St = ff_machine:collapse(ff_identity, Machine),
    case deduce_activity(identity(St)) of
        undefined ->
            do_start_challenge(Params, St);
        {challenge, ChallengeID} ->
            handle_result({error, {challenge, {pending, ChallengeID}}})
    end.

do_start_challenge(Params, St) ->
    Identity = identity(St),
    handle_result(do(challenge, fun () ->
        #{
            id     := ChallengeID,
            class  := ChallengeClassID,
            proofs := Proofs
        } = Params,
        Events = unwrap(ff_identity:start_challenge(ChallengeID, ChallengeClassID, Proofs, Identity)),
        #{
            events => ff_machine:emit_events(Events),
            action => continue
        }
    end)).

handle_result({ok, R}) ->
    {ok, R};
handle_result({error, _} = Error) ->
    {Error, #{}}.

%%

deduce_activity(#{challenges := Challenges}) ->
    Filter = fun (_, Challenge) -> ff_identity_challenge:status(Challenge) == pending end,
    case maps:keys(maps:filter(Filter, Challenges)) of
        [ChallengeID] ->
            {challenge, ChallengeID};
        [] ->
            undefined
    end;
deduce_activity(#{}) ->
    undefined.

compare_events(ID, NewEv) ->
    Limit = length(NewEv),
    {ok, OldEv} = events(ID, {undefined, Limit, forward}),
    compare_events_(ID, NewEv, [Ev || {_, {ev, _, Ev}} <- OldEv]).

compare_events_(ID, NewEv, OldEv) when NewEv =:= OldEv ->
    ff_machine:get(ff_identity, ?NS, ID);
compare_events_(ID, _NewEv, _Ev) ->
    {error, {conflict, ID}}.
