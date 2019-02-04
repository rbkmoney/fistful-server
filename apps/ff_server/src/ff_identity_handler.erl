-module(ff_identity_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Context, Opts) ->
    scoper:scope(fistful, #{function => Func},
        fun() ->
            ok = ff_woody_ctx:set(Context),
            try
                handle_function_(Func, Args, Context, Opts)
            after
                ff_woody_ctx:unset()
            end
        end
    ).

%%
%% Internals
%%
handle_function_('Create', [Params], Context, Opts) ->
    IdentityID = Params#idnt_IdentityParams.id,
    case ff_identity_machine:create(IdentityID,
        decode(identity, Params),
        decode(context, Params#idnt_IdentityParams.context))
    of
        ok ->
            handle_function_('Get', [IdentityID], Context, Opts);
        {error, {provider, notfound}} ->
            woody_error:raise(business, #fistful_ProviderNotFound{});
        {error, {identity_class, notfound}} ->
            woody_error:raise(business, #fistful_IdentityClassNotFound{});
        {error, {party, _Inaccessible}} ->
            woody_error:raise(business, #fistful_PartyInaccessible{});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('Get', [ID], _Context, _Opts) ->
    case ff_identity_machine:get(ID) of
        {ok, Machine} ->
            {ok, encode(identity, Machine)};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end;
handle_function_('StartChallenges', [Params], Context, Opts) ->
    IdentityID = Params#idnt_ChallengeParams.id,
    case ff_identity_machine:start_challenge(IdentityID,
        decode(challenge, Params))
    of
        ok ->
            handle_function_('Get', [IdentityID], Context, Opts);
        {error, {identity, _R} = Error} ->
            woody_error:raise(business, #fistful_ChallengeError{ error_type = encode(error, Error)});
        {error, {challenge, _Reason} = Error} ->
            woody_error:raise(business, #fistful_ChallengeError{ error_type = encode(error, Error)});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('GetEvents', [Params], _Context, _Opts) ->
    IdentityID = Params#idnt_IdentityEventParams.identity_id,
    Range = decode(range, Params#idnt_IdentityEventParams.range),
    case ff_identity_machine:events(IdentityID, Range) of
        {ok, Events} ->
            lager:error("~n~nEvents~n~n~p~n", [Events]),
            {ok, encode(events, Events)};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end.

encode(identity, Machine) ->
    Identity = ff_machine:model(Machine),
    % Ctx = ff_machine:ctx(Machine),
    Challenges = ff_identity:challenges(Identity),
    #idnt_IdentityState{
        id          = ff_identity:id(Identity),
        party_id    = ff_identity:party(Identity),
        provider_id = ff_identity:provider(Identity),
        class_id    = ff_identity:class(Identity),
        contract_id = ff_identity:contract(Identity),
        level       = ff_identity:level(Identity),
        challenges  = encode(challenges, Challenges),
        % context     = encode(context, Ctx),
        external_id = ff_identity:external_id(Identity)
    };
encode(challenges, Challenges)
    when map_size(Challenges) == 0 ->
        undefined;
encode(challenges, Challenges) ->
    maps:map(fun(_ID, Value) -> encode(challenge, Value) end, Challenges);
encode(challenge, Challenge) ->
    Proofs = ff_identity_challenge:proofs(Challenge),
    Status = ff_identity_challenge:status(Challenge),
    #idnt_ChallengeState{
        id     = ff_identity_challenge:id(Challenge),
        proofs = encode(proofs, Proofs),
        status = encode(challenge_status, Status, Challenge)
    };
encode(proofs, Proofs) ->
    [#idnt_ChallengeProof{ type = Type, token = Token } || {Type, Token} <- Proofs];
encode(context, _Ctx) -> #{<<"NS">> => nil}; %% TODO after merge PR FF-44; impl it
encode(events, Events) ->
    GenIdentityEvent = fun({ID, {ev, Timestamp, Ev}}) ->
        #idnt_IdentityEvent{
            sequence   = ff_identity_eventsink_publisher:marshal(event_id, ID),
            occured_at = ff_identity_eventsink_publisher:marshal(timestamp, Timestamp),
            change     = ff_identity_eventsink_publisher:marshal(event, Ev)
        }
    end,
    [ GenIdentityEvent(Event) || Event <- Events];

%% ERROR
encode(error, {identity, notfound})                     -> identity_notfound;
encode(error, {identity, unauthorized})                 -> identity_unauthorized;
encode(error, {challenge, {challenge_pending, _}})      -> challenge_pending;
encode(error, {challenge, {challenge_class, notfound}}) -> challenge_class_notfound;
encode(error, {challenge, {proof, notfound}})           -> challenge_proof_notfound;
encode(error, {challenge, {proof, insufficient}})       -> challenge_proof_insufficient;
encode(error, {challenge, {level, _}})                  -> challenge_level_incorrect;
encode(error, {challenge, conflict})                    -> challenge_conflict.

%% CHALLENGE_STATUS
encode(challenge_status, pending,   _Challenge) ->
    {pending, #idnt_ChallengePending{}};
encode(challenge_status, cancelled, _Challenge) ->
    {cancelled, #idnt_ChallengeCancelled{}};
encode(challenge_status, {completed, _}, Challenge) ->
    {ok, Resolution} = ff_identity_challenge:resolution(Challenge),
    {ok, Time}       = ff_identity_challenge:valid_until(Challenge),
    {completed, #idnt_ChallengeCompleted{
        resolution = Resolution,
        valid_until = Time
    }};
encode(challenge_status, {failed, _}, _Challenge) ->
    {failed, #idnt_ChallengeFailed{}}.

decode(identity, P) -> #{
    party       => P#idnt_IdentityParams.party_id,
    provider    => P#idnt_IdentityParams.provider_id,
    class       => P#idnt_IdentityParams.class_id,
    external_id => P#idnt_IdentityParams.external_id};
decode(challenge, Params) -> #{
        id     => Params#idnt_ChallengeParams.challenge_id,
        class  => Params#idnt_ChallengeParams.cls,
        proofs => decode(proofs, Params#idnt_ChallengeParams.proofs)
    };
decode(proofs, Proofs) ->
    [{P#idnt_ChallengeProof.type, P#idnt_ChallengeProof.token} || P <- Proofs];
decode(range, Range) ->
    Cursor = Range#evsink_EventRange.'after',
    Limit  = Range#evsink_EventRange.limit,
    {Cursor, Limit, forward};
decode(context,  _P) -> #{}. %% TODO after merge PR FF-44; impl it

