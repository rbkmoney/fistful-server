-module(ff_identity_handler).
-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    [IdentityID| _ ] = Args,
    scoper:scope(identity, #{identity_id => IdentityID},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%

handle_function_('Create', [IdentityID, IdentityParams], Opts) ->
    Params  = ff_identity_codec:unmarshal_identity_params(IdentityParams),
    Context = ff_identity_codec:unmarshal(ctx, IdentityParams#idnt_IdentityParams.context),
    case ff_identity_machine:create(IdentityID, Params, Context) of
        ok ->
            handle_function_('Get', [IdentityID], Opts);
        {error, {provider, notfound}} ->
            woody_error:raise(business, #fistful_ProviderNotFound{});
        {error, {identity_class, notfound}} ->
            woody_error:raise(business, #fistful_IdentityClassNotFound{});
        {error, {inaccessible, _}} ->
            woody_error:raise(business, #fistful_PartyInaccessible{});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('Get', [ID], _Opts) ->
    case ff_identity_machine:get(ID) of
        {ok, Machine} ->
            Identity = ff_identity:set_blocked(ff_identity_machine:identity(Machine)),
            Ctx      = ff_identity_codec:marshal(ctx, ff_identity_machine:ctx(Machine)),
            Response = ff_identity_codec:marshal_identity(Identity),
            {ok, Response#idnt_Identity{context = Ctx}};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end;
handle_function_('StartChallenge', [IdentityID, Params], _Opts) ->
    %% Не используем ExternalID тк идемпотентность реал-на через challengeID
    ChallengeParams = ff_identity_codec:unmarshal_challenge_params(Params),
    case ff_identity_machine:start_challenge(IdentityID, ChallengeParams) of
        ok ->
            ChallengeID = maps:get(id, ChallengeParams),
            {ok, Machine}   = ff_identity_machine:get(IdentityID),
            Identity        = ff_identity_machine:identity(Machine),
            {ok, Challenge} = ff_identity:challenge(ChallengeID, Identity),
            {ok, ff_identity_codec:marshal_challenge(Challenge)};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {challenge, {pending, _}}} ->
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
handle_function_('GetChallenges', [ID], _Opts) ->
    case ff_identity_machine:get(ID) of
        {ok, Machine} ->
            Identity = ff_identity_machine:identity(Machine),
            Challenges = ff_identity:challenges(Identity),
            {ok, [ff_identity_codec:marshal_challenge(C) || C <- maps:values(Challenges)]};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end;

handle_function_('GetEvents', [IdentityID, RangeParams], _Opts) ->
    Range = ff_identity_codec:unmarshal(range, RangeParams),
    case ff_identity_machine:events(IdentityID, Range) of
        {ok, EventList} ->
            Events = [ff_identity_codec:marshal_identity_event(Event) || Event <- EventList],
            {ok, Events};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end.
