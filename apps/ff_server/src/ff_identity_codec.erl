-module(ff_identity_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

-export([unmarshal_identity_params/1]).
-export([unmarshal_challenge_params/1]).

-export([marshal_identity_event/1]).
-export([marshal_challenge_state/1]).
-export([marshal_identity_state/2]).

-export([marshal/2]).
-export([unmarshal/2]).


%% This special functions hasn't got opposite functions.
-spec unmarshal_identity_params(ff_proto_identity_thrift:'IdentityParams'()) ->
    ff_identity_machine:params().

unmarshal_identity_params(#idnt_IdentityParams{
    id          = ID,
    party       = PartyID,
    provider    = ProviderID,
    cls         = ClassID,
    external_id = ExternalID,
    metadata    = Metadata
}) ->
    genlib_map:compact(#{
        id          => unmarshal(id, ID),
        party       => unmarshal(id, PartyID),
        provider    => unmarshal(id, ProviderID),
        class       => unmarshal(id, ClassID),
        external_id => maybe_unmarshal(id, ExternalID),
        metadata    => maybe_unmarshal(ctx, Metadata)
    }).

-spec unmarshal_challenge_params(ff_proto_identity_thrift:'ChallengeParams'()) ->
    ff_identity_machine:challenge_params().

unmarshal_challenge_params(#idnt_ChallengeParams{
    id     = ID,
    cls    = ClassID,
    proofs = Proofs
}) ->
    genlib_map:compact(#{
        id     => unmarshal(id, ID),
        class  => unmarshal(id, ClassID),
        proofs => unmarshal({list, challenge_proofs}, Proofs)
    }).

-spec marshal_identity_event({integer(), ff_machine:timestamped_event(ff_identity:event())}) ->
    ff_proto_identity_thrift:'Event'().

marshal_identity_event({ID, {ev, Timestamp, Ev}}) ->
    #idnt_Event{
        sequence   = marshal(event_id, ID),
        occured_at = marshal(timestamp, Timestamp),
        change     = marshal(change, Ev)
    }.

-spec marshal_challenge_state(ff_identity_challenge:challenge_state()) -> ff_proto_identity_thrift:'ChallengeState'().

marshal_challenge_state(ChallengeState) ->
    Proofs = ff_identity_challenge:proofs(ChallengeState),
    Status = ff_identity_challenge:status(ChallengeState),
    #idnt_ChallengeState{
        id     = ff_identity_challenge:id(ChallengeState),
        cls    = ff_identity_challenge:class(ChallengeState),
        proofs = marshal({list, challenge_proofs}, Proofs),
        status = marshal(challenge_payload_status_changed, Status)
    }.

-spec marshal_identity_state(ff_identity:identity_state(), ff_entity_context:context()) ->
    ff_proto_identity_thrift:'IdentityState'().

marshal_identity_state(IdentityState, Context) ->
    EffectiveChallengeID = case ff_identity:effective_challenge(IdentityState) of
        {ok, ID} -> maybe_marshal(id, ID);
        {error, notfound} -> undefined
    end,
    #idnt_IdentityState{
        id = maybe_marshal(id, ff_identity:id(IdentityState)),
        party_id = marshal(id, ff_identity:party(IdentityState)),
        provider_id = marshal(id, ff_identity:provider(IdentityState)),
        class_id = marshal(id, ff_identity:class(IdentityState)),
        contract_id = maybe_marshal(id, ff_identity:contract(IdentityState)),
        level_id = maybe_marshal(id, ff_identity:level(IdentityState)),
        blocking = maybe_marshal(blocking, ff_identity:blocking(IdentityState)),
        created_at = maybe_marshal(created_at, ff_identity:created_at(IdentityState)),
        external_id = maybe_marshal(id, ff_identity:external_id(IdentityState)),
        metadata = maybe_marshal(ctx, ff_identity:metadata(IdentityState)),
        effective_challenge_id = EffectiveChallengeID,
        context = maybe_marshal(ctx, Context)
    }.

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) -> ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(timestamped_change, {ev, Timestamp, Change}) ->
   #idnt_TimestampedChange{
        change = marshal(change, Change),
        occured_at = ff_codec:marshal(timestamp, Timestamp)
    };

marshal(change, {created, Identity}) ->
    {created, marshal(identity, Identity)};
marshal(change, {level_changed, LevelID}) ->
    {level_changed, marshal(id, LevelID)};
marshal(change, {{challenge, ChallengeID}, ChallengeChange}) ->
    {identity_challenge, marshal(challenge_change, #{
        id => ChallengeID,
        payload => ChallengeChange
    })};
marshal(change, {effective_challenge_changed, ChallengeID}) ->
    {effective_challenge_changed, marshal(id, ChallengeID)};

marshal(identity, Identity) ->
    #idnt_Identity{
        id = maybe_marshal(id, ff_identity:id(Identity)),
        party = marshal(id, ff_identity:party(Identity)),
        provider = marshal(id, ff_identity:provider(Identity)),
        cls = marshal(id, ff_identity:class(Identity)),
        contract = maybe_marshal(id, ff_identity:contract(Identity)),
        created_at = maybe_marshal(created_at, ff_identity:created_at(Identity)),
        external_id = maybe_marshal(id, ff_identity:external_id(Identity)),
        metadata = maybe_marshal(ctx, ff_identity:metadata(Identity))
    };

marshal(challenge_change, #{
    id       := ID,
    payload  := Payload
}) ->
    #idnt_ChallengeChange{
        id      = marshal(id, ID),
        payload = marshal(challenge_payload, Payload)
    };
marshal(challenge_payload, {created, Challenge}) ->
    {created, marshal(challenge_payload_created, Challenge)};
marshal(challenge_payload, {status_changed, ChallengeStatus}) ->
    {status_changed, marshal(challenge_payload_status_changed, ChallengeStatus)};
marshal(challenge_payload_created, Challenge = #{
    id := ID
} = C) ->
    ct:log("Challenge: ~p", [C]),
    Proofs = maps:get(proofs, Challenge, []),
    #idnt_Challenge{
        cls    = marshal(id, ID),
        provider_id = marshal(id, maps:get(provider, Challenge, undefined)),
        class_id = marshal(id, maps:get(challenge_class, Challenge, undefined)),
        proofs = marshal({list, challenge_proofs}, Proofs),
        claim_id =  marshal(id, maps:get(claim_id, Challenge, undefined))
    };

marshal(challenge_proofs, {Type, Token}) ->
    #idnt_ChallengeProof{
        type = Type,
        token = Token
    };

marshal(challenge_payload_status_changed, pending) ->
    {pending, #idnt_ChallengePending{}};
marshal(challenge_payload_status_changed, cancelled) ->
    {cancelled, #idnt_ChallengeCancelled{}};
marshal(challenge_payload_status_changed, {completed, Status = #{
    resolution := Resolution
}}) ->
    ValidUntil = maps:get(valid_until, Status, undefined),
    ct:log("valid until: ~p", [ValidUntil]),
    NewStatus = #idnt_ChallengeCompleted{
        resolution = marshal(resolution, Resolution),
        valid_until = marshal(timestamp, ValidUntil)
    },
    {completed, NewStatus};
marshal(challenge_payload_status_changed, {failed, _Status}) ->
    {failed, #idnt_ChallengeFailed{}};

marshal(resolution, approved) ->
    approved;
marshal(resolution, denied) ->
    denied;

marshal(ctx, Ctx) ->
    maybe_marshal(context, Ctx);

marshal(created_at, TimeMS) ->
    marshal(string, ff_time:to_rfc3339(TimeMS));

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(timestamped_change, TimestampedChange) ->
    Timestamp = ff_codec:unmarshal(timestamp, TimestampedChange#idnt_TimestampedChange.occured_at),
    Change = unmarshal(change, TimestampedChange#idnt_TimestampedChange.change),
    {ev, Timestamp, Change};

unmarshal(repair_scenario, {add_events, #idnt_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, change}, Events),
        action => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(change, {created, Identity}) ->
    {created, unmarshal(identity, Identity)};
unmarshal(change, {level_changed, LevelID}) ->
    {level_changed, unmarshal(id, LevelID)};
unmarshal(change, {identity_challenge, #idnt_ChallengeChange{id = ID, payload = Payload}}) ->
    {{challenge, unmarshal(id, ID)}, unmarshal(challenge_payload, Payload)};
unmarshal(change, {effective_challenge_changed, ChallengeID}) ->
    {effective_challenge_changed, unmarshal(id, ChallengeID)};

unmarshal(identity, #idnt_Identity{
    id          = ID,
    party       = PartyID,
    provider    = ProviderID,
    cls         = ClassID,
    contract    = ContractID,
    external_id = ExternalID,
    created_at  = CreatedAt,
    metadata    = Metadata
}) ->
    genlib_map:compact(#{
        id          => unmarshal(id, ID),
        party       => unmarshal(id, PartyID),
        provider    => unmarshal(id, ProviderID),
        class       => unmarshal(id, ClassID),
        contract    => unmarshal(id, ContractID),
        external_id => maybe_unmarshal(id, ExternalID),
        created_at  => maybe_unmarshal(created_at, CreatedAt),
        metadata    => maybe_unmarshal(ctx, Metadata),
        version     => 2
    });

unmarshal(challenge_payload, {created, Challenge}) ->
    {created, unmarshal(challenge_payload_created, Challenge)};
unmarshal(challenge_payload, {status_changed, ChallengeStatus}) ->
    {status_changed, unmarshal(challenge_payload_status_changed, ChallengeStatus)};
unmarshal(challenge_payload_created, #idnt_Challenge{
    cls    = ID,
    proofs = Proofs,
    claim_id = ClaimID,
    class_id = ChallengeClassID
} = I) ->
    ct:log("Challenge: ~p", [I]),
    #{
        id     => unmarshal(id, ID),
        claim_id => unmarshal(id, ClaimID),
        challenge_class => unmarshal(id, ChallengeClassID),
        proofs => unmarshal({list, challenge_proofs}, Proofs)
    };

unmarshal(challenge_proofs, Proof) -> {
        unmarshal(proof_type, Proof#idnt_ChallengeProof.type),
        unmarshal(id, Proof#idnt_ChallengeProof.token)
    };

unmarshal(proof_type, rus_domestic_passport) ->
    rus_domestic_passport;
unmarshal(proof_type, rus_retiree_insurance_cert) ->
    rus_retiree_insurance_cert;

unmarshal(challenge_payload_status_changed, {pending, #idnt_ChallengePending{}}) ->
    pending;
unmarshal(challenge_payload_status_changed, {cancelled, #idnt_ChallengeCancelled{}}) ->
    cancelled;
unmarshal(challenge_payload_status_changed, {completed, #idnt_ChallengeCompleted{
    resolution = Resolution,
    valid_until = ValidUntil
}}) ->
    {completed, genlib_map:compact(#{
        resolution => unmarshal(resolution, Resolution),
        valid_until => maybe_unmarshal(timestamp, ValidUntil)
    })};
unmarshal(challenge_payload_status_changed, {failed, #idnt_ChallengeFailed{}}) ->
    % FIXME: Describe failures in protocol
    {failed, unknown};
unmarshal(resolution, approved) ->
    approved;
unmarshal(resolution, denied) ->
    denied;

unmarshal(effective_challenge, undefined) ->
    {error, notfound};
unmarshal(effective_challenge, EffectiveChallengeID) ->
    {ok, unmarshal(id, EffectiveChallengeID)};

unmarshal(created_at, Timestamp) ->
    unmarshal(integer, ff_time:from_rfc3339(Timestamp));

unmarshal(ctx, Ctx) ->
    maybe_unmarshal(context, Ctx);

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec identity_test() -> _.
identity_test() ->
    IdentityIn = #{
        id          => genlib:unique(),
        party       => genlib:unique(),
        provider    => genlib:unique(),
        class       => genlib:unique(),
        contract    => genlib:unique(),
        external_id => genlib:unique()
    },
    IdentityOut = unmarshal(identity, marshal(identity, IdentityIn)),
    ?assertEqual(IdentityOut, IdentityIn).

-spec challenge_test() -> _.
challenge_test() ->
    ChallengeIn = #{
        id     => genlib:unique(),
        proofs => [{rus_retiree_insurance_cert, <<"Bananazzzz">>}]
    },
    ChallengeOut = unmarshal(challenge_payload_created, marshal(challenge_payload_created, ChallengeIn)),
    ?assertEqual(ChallengeIn, ChallengeOut).

-endif.
