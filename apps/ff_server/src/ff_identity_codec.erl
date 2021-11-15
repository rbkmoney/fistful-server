-module(ff_identity_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

-export([unmarshal_identity_params/1]).

-export([marshal_identity_event/1]).
-export([marshal_identity_state/2]).

-export([marshal/2]).
-export([unmarshal/2]).

%% This special functions hasn't got opposite functions.
-spec unmarshal_identity_params(ff_proto_identity_thrift:'IdentityParams'()) -> ff_identity_machine:params().
unmarshal_identity_params(#idnt_IdentityParams{
    id = ID,
    name = Name,
    party = PartyID,
    provider = ProviderID,
    external_id = ExternalID,
    metadata = Metadata
}) ->
    genlib_map:compact(#{
        id => unmarshal(id, ID),
        name => unmarshal(string, Name),
        party => unmarshal(id, PartyID),
        provider => unmarshal(id, ProviderID),
        external_id => maybe_unmarshal(id, ExternalID),
        metadata => maybe_unmarshal(ctx, Metadata)
    }).

-spec marshal_identity_event({integer(), ff_machine:timestamped_event(ff_identity:event())}) ->
    ff_proto_identity_thrift:'Event'().
marshal_identity_event({ID, {ev, Timestamp, Ev}}) ->
    #idnt_Event{
        sequence = marshal(event_id, ID),
        occured_at = marshal(timestamp, Timestamp),
        change = marshal(change, Ev)
    }.

-spec marshal_identity_state(ff_identity:identity_state(), ff_entity_context:context()) ->
    ff_proto_identity_thrift:'IdentityState'().
marshal_identity_state(IdentityState, Context) ->
    #idnt_IdentityState{
        id = maybe_marshal(id, ff_identity:id(IdentityState)),
        name = marshal(string, ff_identity:name(IdentityState)),
        party_id = marshal(id, ff_identity:party(IdentityState)),
        provider_id = marshal(id, ff_identity:provider(IdentityState)),
        contract_id = maybe_marshal(id, ff_identity:contract(IdentityState)),
        blocking = maybe_marshal(blocking, ff_identity:blocking(IdentityState)),
        created_at = maybe_marshal(created_at, ff_identity:created_at(IdentityState)),
        external_id = maybe_marshal(id, ff_identity:external_id(IdentityState)),
        metadata = maybe_marshal(ctx, ff_identity:metadata(IdentityState)),
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
marshal(identity, Identity) ->
    #idnt_Identity{
        id = maybe_marshal(id, ff_identity:id(Identity)),
        name = maybe_marshal(string, ff_identity:name(Identity)),
        party = marshal(id, ff_identity:party(Identity)),
        provider = marshal(id, ff_identity:provider(Identity)),
        contract = maybe_marshal(id, ff_identity:contract(Identity)),
        created_at = maybe_marshal(created_at, ff_identity:created_at(Identity)),
        external_id = maybe_marshal(id, ff_identity:external_id(Identity)),
        metadata = maybe_marshal(ctx, ff_identity:metadata(Identity))
    };
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

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) -> ff_codec:decoded_value().
unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];
unmarshal(timestamped_change, TimestampedChange) ->
    Timestamp = ff_codec:unmarshal(timestamp, TimestampedChange#idnt_TimestampedChange.occured_at),
    Change = unmarshal(change, TimestampedChange#idnt_TimestampedChange.change),
    {ev, Timestamp, Change};
unmarshal(repair_scenario, {add_events, #idnt_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events,
        genlib_map:compact(#{
            events => unmarshal({list, change}, Events),
            action => maybe_unmarshal(complex_action, Action)
        })};
unmarshal(change, {created, Identity}) ->
    {created, unmarshal(identity, Identity)};
% We have to support this unmarshal cause mg contain identety's events with challenge
unmarshal(change, {level_changed, LevelID}) ->
    {level_changed, unmarshal(id, LevelID)};
unmarshal(change, {identity_challenge, #idnt_ChallengeChange{id = ID, payload = Payload}}) ->
    {{challenge, unmarshal(id, ID)}, unmarshal(challenge_payload, Payload)};
unmarshal(change, {effective_challenge_changed, ChallengeID}) ->
    {effective_challenge_changed, unmarshal(id, ChallengeID)};
unmarshal(identity, #idnt_Identity{
    id = ID,
    name = Name,
    party = PartyID,
    provider = ProviderID,
    contract = ContractID,
    external_id = ExternalID,
    created_at = CreatedAt,
    metadata = Metadata
}) ->
    genlib_map:compact(#{
        id => unmarshal(id, ID),
        name => unmarshal(string, Name),
        party => unmarshal(id, PartyID),
        provider => unmarshal(id, ProviderID),
        contract => unmarshal(id, ContractID),
        external_id => maybe_unmarshal(id, ExternalID),
        created_at => maybe_unmarshal(created_at, CreatedAt),
        metadata => maybe_unmarshal(ctx, Metadata),
        version => 2
    });
unmarshal(challenge_payload, {created, Challenge}) ->
    {created, unmarshal(challenge_payload_created, Challenge)};
unmarshal(challenge_payload, {status_changed, ChallengeStatus}) ->
    {status_changed, unmarshal(challenge_payload_status_changed, ChallengeStatus)};
unmarshal(challenge_payload_created, #idnt_Challenge{
    id = ID,
    cls = ChallengeClass,
    provider_id = ProviderID,
    class_id = IdentityClass,
    proofs = Proofs,
    claim_id = ClaimID,
    claimant = Claimant,
    master_id = MasterID
}) ->
    #{
        id => unmarshal(id, ID),
        provider => unmarshal(id, ProviderID),
        identity_class => unmarshal(id, IdentityClass),
        challenge_class => unmarshal(id, ChallengeClass),
        proofs => unmarshal({list, challenge_proofs}, Proofs),
        claim_id => unmarshal(id, ClaimID),
        master_id => unmarshal(id, MasterID),
        claimant => unmarshal(id, Claimant)
    };
unmarshal(challenge_proofs, Proof) ->
    {
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
unmarshal(
    challenge_payload_status_changed,
    {completed, #idnt_ChallengeCompleted{
        resolution = Resolution,
        valid_until = ValidUntil
    }}
) ->
    {completed,
        genlib_map:compact(#{
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
        id => genlib:unique(),
        name => genlib:unique(),
        party => genlib:unique(),
        provider => genlib:unique(),
        contract => genlib:unique(),
        external_id => genlib:unique(),
        version => 2
    },
    IdentityOut = unmarshal(identity, marshal(identity, IdentityIn)),
    ?assertEqual(IdentityOut, IdentityIn).

% -spec challenge_test() -> _.
% challenge_test() ->
%     ChallengeIn = #{
%         id => genlib:unique(),
%         proofs => [{rus_retiree_insurance_cert, <<"Bananazzzz">>}],
%         challenge_class => <<"challenge_class">>,
%         claim_id => <<"claim_id">>,
%         provider => <<"provider">>,
%         identity_class => <<"identity_class">>,
%         master_id => <<"master_id">>,
%         claimant => <<"claimant">>
%     },
%     ChallengeOut = unmarshal(challenge_payload_created, marshal(challenge_payload_created, ChallengeIn)),
%     ?assertEqual(ChallengeIn, ChallengeOut).

-endif.
