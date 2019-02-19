-module(ff_identity_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(event, {created, Identity}) ->
    {created, marshal(identity, Identity)};
marshal(event, {level_changed, LevelID}) ->
    {level_changed, marshal(id, LevelID)};
marshal(event, {{challenge, ChallengeID}, ChallengeChange}) ->
    {identity_challenge, marshal(challenge_change, #{
        id => ChallengeID,
        payload => ChallengeChange
    })};
marshal(event, {effective_challenge_changed, ChallengeID}) ->
    {effective_challenge_changed, marshal(id, ChallengeID)};

marshal(identity, Identity = #{
    party       := PartyID,
    provider    := ProviderID,
    class       := ClassID
}) ->
    ContractID = maps:get(contract, Identity, undefined),
    ExternalID = maps:get(external_id, Identity, undefined),
    #idnt_Identity{
        party     = marshal(id, PartyID),
        provider  = marshal(id, ProviderID),
        cls       = marshal(id, ClassID),
        contract  = marshal(id, ContractID),
        external_id = marshal(id, ExternalID)
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
    id   := ID
}) ->
    Proofs = maps:get(proofs, Challenge, undefined),
    #idnt_Challenge{
        cls    = marshal(id, ID),
        proofs = marshal({list, challenge_proofs}, Proofs)
    };
marshal(challenge_proofs, _) ->
    #idnt_ChallengeProof{};
marshal(challenge_payload_status_changed, pending) ->
    {pending, #idnt_ChallengePending{}};
marshal(challenge_payload_status_changed, cancelled) ->
    {cancelled, #idnt_ChallengeCancelled{}};
marshal(challenge_payload_status_changed, {completed, Status = #{
    resolution := Resolution
}}) ->
    ValidUntil = maps:get(valid_until, Status, undefined),
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

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #idnt_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, event}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(event, {created, Identity}) ->
    {created, unmarshal(identity, Identity)};
unmarshal(event, {level_changed, LevelID}) ->
    {level_changed, unmarshal(id, LevelID)};
unmarshal(event, {identity_challenge, #idnt_ChallengeChange{id = ID, payload = Payload}}) ->
    {{challenge, unmarshal(id, ID)}, unmarshal(challenge_payload, Payload)};
unmarshal(event, {effective_challenge_changed, ChallengeID}) ->
    {effective_challenge_changed, unmarshal(id, ChallengeID)};

unmarshal(identity, #idnt_Identity{
    party       = PartyID,
    provider    = ProviderID,
    cls         = ClassID,
    contract    = ContractID,
    external_id = ExternalID
}) ->
    genlib_map:compact(#{
        party       => unmarshal(id, PartyID),
        provider    => unmarshal(id, ProviderID),
        class       => unmarshal(id, ClassID),
        contract    => unmarshal(id, ContractID),
        external_id => unmarshal(id, ExternalID)
    });

unmarshal(challenge_payload, {created, Challenge}) ->
    {created, unmarshal(challenge_payload_created, Challenge)};
unmarshal(challenge_payload, {status_changed, ChallengeStatus}) ->
    {status_changed, unmarshal(challenge_payload_status_changed, ChallengeStatus)};
unmarshal(challenge_payload_created, #idnt_Challenge{
    cls    = ID,
    proofs = Proofs
}) ->
    #{
        id     => unmarshal(id, ID),
        proofs => unmarshal({list, challenge_proofs}, Proofs)
    };
unmarshal(challenge_proofs, #idnt_ChallengeProof{} = Proof) ->
    % FIXME: Describe challenge_proofs in protocol
    erlang:error({not_implemented, {unmarshal, challenge_proofs}}, [challenge_proofs, Proof]);
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
        valid_until => unmarshal(timestamp, ValidUntil)
    })};
unmarshal(challenge_payload_status_changed, {failed, #idnt_ChallengeFailed{}}) ->
    % FIXME: Describe failures in protocol
    {failed, unknown};
unmarshal(resolution, approved) ->
    approved;
unmarshal(resolution, denied) ->
    denied;

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).
