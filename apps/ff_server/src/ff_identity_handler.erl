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

handle_function_('Create', [IdentityID, IdentityParams], WoodyCtx, Opts) ->
    Params  = decode(identity, IdentityParams),
    Context = decode(context, IdentityParams#idnt_IdentityParams.context),
    case ff_identity_machine:create(IdentityID, Params, Context) of
        ok ->
            handle_function_('Get', [IdentityID], WoodyCtx, Opts);
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
handle_function_('StartChallenge', [IdentityID, Params], _WoodyCtx, _Opts) ->
    %% Не используем ExternalID тк идемпотентность реал-на через challengeID
    ChallengeParams = decode(challenge, Params),
    case ff_identity_machine:start_challenge(IdentityID, ChallengeParams) of
        ok ->
            ChallengeID = Params#idnt_ChallengeParams.id,
            {ok, Machine}   = ff_identity_machine:get(IdentityID),
            Identity        = ff_identity_machine:identity(Machine),
            {ok, Challenge} = ff_identity:challenge(ChallengeID, Identity),
            {ok, encode(challenge, Challenge)};
        {error, {identity, notfound}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {challenge, {challenge_pending, _}}} ->
            woody_error:raise(business, #fistful_ChallengePending{});
        {error, {challenge, {challenge_class, notfound}}} ->
            woody_error:raise(business, #fistful_ChallengeClassNotFound{});
        {error, {challenge, {proof, notfound}}} ->
            woody_error:raise(business, #fistful_ProofNotFound{});
        {error, {challenge, {proof, insufficient}}} ->
            woody_error:raise(business, #fistful_ProofInsufficient{});
        {error, {challenge, {level, _}}} ->
            woody_error:raise(business, #fistful_ChallengeLevelIncorrect{});
        {error, {challenge, conflict}} ->
            woody_error:raise(business, #fistful_ChallengeConflict{});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('GetChallenges', [ID], _WoodCtx, _Opts) ->
    case ff_identity_machine:get(ID) of
        {ok, Machine} ->
            Identity = ff_identity_machine:identity(Machine),
            {ok, encode(challenges, ff_identity:challenges(Identity))};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end;
handle_function_('GetEvents', [IdentityID, RangeParams], _Context, _Opts) ->
    Range = decode(range, RangeParams),
    case ff_identity_machine:events(IdentityID, Range) of
        {ok, Events} ->
            {ok, encode(events, Events)};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end.

%%
%% Encode
%%

encode(identity, Machine) ->
    Identity = ff_identity_machine:identity(Machine),
    Ctx      = ff_identity_machine:ctx(Machine),
    Party    = ff_identity:party(Identity),
    IID      = ff_identity:id(Identity),
    IsAccessible = {ok, accessible} =:= ff_party:is_accessible(Party),
    #idnt_Identity{
        id          = IID,
        party       = ff_identity:party(Identity),
        provider    = ff_identity:provider(Identity),
        cls         = ff_identity:class(Identity),
        contract    = ff_identity:contract(Identity),
        level       = ff_identity:level(Identity),
        blocked     = IsAccessible,
        context     = encode(context, Ctx),
        external_id = ff_identity:external_id(Identity),
        effective_challenge = get_effective_challenge(Identity)
    };
encode(challenge, Challenge) ->
    Proofs = ff_identity_challenge:proofs(Challenge),
    Status = ff_identity_challenge:status(Challenge),
    #idnt_Challenge{
        id     = ff_identity_challenge:id(Challenge),
        cls    = ff_identity_challenge:class(Challenge),
        proofs = encode(proofs, Proofs),
        status = encode(challenge_status, Status, Challenge)
    };
encode(challenges, Challenges)
    when map_size(Challenges) == 0 ->
        undefined;
encode(challenges, Challenges) ->
    maps:fold(fun(_ID, Value, AccIn) ->
        [encode(challenge, Value) | AccIn]
    end, [], Challenges);
encode(proofs, Proofs) ->
    [#idnt_ChallengeProof{ type = Type, token = Token } || {Type, Token} <- Proofs];
encode(context, undefined) -> undefined;
encode(context, Ctx) -> ff_context:wrap(Ctx);
encode(events, Events) ->
    GenIdentityEvent = fun({ID, {ev, Timestamp, Ev}}) ->
        #idnt_IdentityEvent{
            sequence   = ff_identity_eventsink_publisher:marshal(event_id, ID),
            occured_at = ff_identity_eventsink_publisher:marshal(timestamp, Timestamp),
            change     = ff_identity_eventsink_publisher:marshal(event, Ev)
        }
    end,
    [ GenIdentityEvent(Event) || Event <- Events].

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

%%
%% Decode
%%

decode(identity, P) -> #{
    party       => P#idnt_IdentityParams.party,
    provider    => P#idnt_IdentityParams.provider,
    class       => P#idnt_IdentityParams.cls,
    external_id => P#idnt_IdentityParams.external_id};
decode(challenge, Params) -> #{
    id     => Params#idnt_ChallengeParams.id,
    class  => Params#idnt_ChallengeParams.cls,
    proofs => decode(proofs, Params#idnt_ChallengeParams.proofs)};
decode(proofs, Proofs) ->
    [{P#idnt_ChallengeProof.type, P#idnt_ChallengeProof.token} || P <- Proofs];
decode(range, Range) ->
    Cursor = Range#evsink_EventRange.'after',
    Limit  = Range#evsink_EventRange.limit,
    {Cursor, Limit, forward};
decode(context, undefined) ->
    undefined;
decode(context,  Context) ->
    ff_context:unwrap(Context).

get_effective_challenge(Identity) ->
    case ff_identity:effective_challenge(Identity) of
        {ok, EffectiveChallenge} -> EffectiveChallenge;
        {error, notfound} -> undefined
    end.