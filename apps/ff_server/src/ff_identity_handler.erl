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
handle_function_('Create', [Params], _Context, _Opts) ->
    IdentityID = Params#idnt_IdentityParams.id,
    case ff_identity_machine:create(IdentityID,
        decode(identity, Params),
        decode(context, Params#idnt_IdentityParams.context))
    of
        ok ->
            {ok, Machine} = ff_identity_machine:get(IdentityID),
            {ok, encode(identity, Machine)};
            % handle_function_('Get', [IdentityID], Context, Opts);
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
    lager:error(">>> Start challenges ~n~n~n"),
    case ff_identity_machine:start_challenge(IdentityID,
        decode(challenge, Params))
    of
        ok ->
            lager:error(">>> GET~n~n"),
            handle_function_('Get', [IdentityID], Context, Opts);
        {error, {identity, _R} = Error} ->
            lager:error("CRITICAL Error: ~p~n", [Error]),
            woody_error:raise(business, #fistful_ChallengeError{ error_type = encode(error, Error)});
        {error, {challenge, _Reason} = Error} ->
            lager:error("CRITICAL Error: ~p~n", [Error]),
            woody_error:raise(business, #fistful_ChallengeError{ error_type = encode(error, Error)});
        {error, Error} ->
            lager:error("CRITICAL Error: ~p~n", [Error]),
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_(_Func, _Args, _Ctx, _Opts) -> not_implement.

encode(identity, Machine) ->
    Identity = ff_machine:model(Machine),
    _Ctx = ff_machine:ctx(Machine),
    #idnt_IdentityState{
        id          = ff_identity:id(Identity),
        party_id    = ff_identity:party(Identity),
        provider_id = ff_identity:provider(Identity),
        class_id    = ff_identity:class(Identity),
        contract_id = ff_identity:contract(Identity)
    };
encode(context, _Ctx) -> #{<<"NS">> => nil}; %% TODO after merge PR FF-44; impl it
%% ERROR
encode(error, {identity, notfound})                     -> identity_notfound;
encode(error, {identity, unauthorized})                 -> identity_unauthorized;
encode(error, {challenge, {challenge_pending, _}})      -> challenge_pending;
encode(error, {challenge, {challenge_class, notfound}}) -> challenge_class_notfound;
encode(error, {challenge, {proof, notfound}})           -> challenge_proof_notfound;
encode(error, {challenge, {proof, insufficient}})       -> challenge_proof_insufficient;
encode(error, {challenge, {level, _}})                  -> challenge_level_incorrect;
encode(error, {challenge, conflict})                    -> challenge_conflict.

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
decode(context,  _P) -> #{}.

