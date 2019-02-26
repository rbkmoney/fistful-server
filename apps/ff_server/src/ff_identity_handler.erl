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
    [IdentityID| _ ] = Args,
    scoper:scope(fistful, #{function => Func, identity => IdentityID},
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
    Params  = ff_identity_codec:decode_identity_params(IdentityParams),
    Context = ff_identity_codec:unmarshal(context, IdentityParams#idnt_IdentityParams.context),
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
            Identity = ff_identity:is_blocked(ff_identity_machine:identity(Machine)),
            Ctx      = ff_identity_machine:ctx(Machine),
            {ok, ff_identity_codec:marshal_identity({Identity, Ctx})};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end;
handle_function_('StartChallenge', [IdentityID, Params], _WoodyCtx, _Opts) ->
    %% Не используем ExternalID тк идемпотентность реал-на через challengeID
    ChallengeParams = ff_identity_codec:decode_challenge_params(Params),
    case ff_identity_machine:start_challenge(IdentityID, ChallengeParams) of
        ok ->
            ChallengeID = maps:get(id, ChallengeParams),
            {ok, Machine}   = ff_identity_machine:get(IdentityID),
            Identity        = ff_identity_machine:identity(Machine),
            {ok, Challenge} = ff_identity:challenge(ChallengeID, Identity),
            {ok, ff_identity_codec:encode_challenge(Challenge)};
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
            Challenges = ff_identity:challenges(Identity),
            {ok, [ff_identity_codec:encode_challenge(C) || C <- maps:values(Challenges)]};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end;
handle_function_('GetEvents', [IdentityID, RangeParams], _Context, _Opts) ->
    Range = ff_identity_codec:unmarshal(range, RangeParams),
    case ff_identity_machine:events(IdentityID, Range) of
        {ok, EventList} ->
            Events = [ff_identity_codec:encode_identity_event(Event) || Event <- EventList],
            {ok, Events};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end.