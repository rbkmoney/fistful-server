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
    Params = ff_identity_codec:unmarshal(identity_params, IdentityParams),
    Context = ff_identity_codec:unmarshal(context, IdentityParams),
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
    ChallengeParams = ff_identity_codec:unmarshal(challenge_params, Params),
    case ff_identity_machine:start_challenge(IdentityID, ChallengeParams) of
        ok ->
            ChallengeID = maps:get(id, ChallengeParams),
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
    Range = ff_identity_codec:unmarshal(range, RangeParams),
    case ff_identity_machine:events(IdentityID, Range) of
        {ok, Events} ->
            {ok, ff_identity_codec:marshal(identity_events, Events)};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end.

get_effective_challenge(Identity) ->
    case ff_identity:effective_challenge(Identity) of
        {ok, EffectiveChallenge} -> EffectiveChallenge;
        {error, notfound} -> undefined
    end.

%%
%% Encode
%%

encode(identity, Machine) ->
    Identity = ff_identity_machine:identity(Machine),
    Ctx      = ff_identity_machine:ctx(Machine),
    Party    = ff_identity:party(Identity),
    IsAccessible = {ok, accessible} =:= ff_party:is_accessible(Party),

    ff_identity_codec:marshal(identity, #{
        id       => ff_identity:id(Identity),
        party    => ff_identity:party(Identity),
        provider => ff_identity:provider(Identity),
        class    => ff_identity:class(Identity),
        contract => ff_identity:contract(Identity),
        level    => ff_identity:level(Identity),
        blocked  => IsAccessible,
        context  => Ctx,
        external_id => ff_identity:external_id(Identity),
        effective_challenge => get_effective_challenge(Identity)
    });

encode(challenge, Challenge) ->
    Proofs = ff_identity_challenge:proofs(Challenge),
    Status = ff_identity_challenge:status(Challenge),
    ff_identity_codec:marshal(challenge, #{
        id     => ff_identity_challenge:id(Challenge),
        cls    => ff_identity_challenge:class(Challenge),
        proofs => Proofs,
        status => Status
    });

encode(challenges, Challenges)
    when map_size(Challenges) == 0 ->
        undefined;
encode(challenges, Challenges) ->
    maps:fold(fun(_ID, Value, AccIn) ->
        [encode(challenge, Value) | AccIn]
    end, [], Challenges).