-module(ff_identity_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

-export([decode_identity_params/1]).
-export([decode_challenge_params/1]).

-export([marshal_identity_event/1]).
-export([marshal_challenge/1]).

-export([marshal_identity/1]).
-export([unmarshal_identity/1]).

-export([marshal/2]).
-export([unmarshal/2]).


%% API
%% DECODE
-spec decode_identity_params(ff_proto_identity_thrift:'IdentityParams'()) ->
    ff_identity_machine:params().

decode_identity_params(#idnt_IdentityParams{
    name        = Name,
    party       = PartyID,
    provider    = ProviderID,
    cls         = ClassID,
    external_id = ExternalID
}) ->
    genlib_map:compact(#{
        name        => unmarshal(string, Name),
        party       => unmarshal(id, PartyID),
        provider    => unmarshal(id, ProviderID),
        class       => unmarshal(id, ClassID),
        external_id => maybe_unmarshal(id, ExternalID)
    }).

-spec decode_challenge_params(ff_proto_identity_thrift:'ChallengeParams'()) ->
    ff_identity_machine:challenge_params().

decode_challenge_params(#idnt_ChallengeParams{
    id     = ID,
    cls    = ClassID,
    proofs = Proofs
}) ->
    genlib_map:compact(#{
        id     => unmarshal(id, ID),
        class  => unmarshal(id, ClassID),
        proofs => unmarshal(challenge_proofs, Proofs)
    }).

%% Every function marshal_X has got opposite function unmarshal_X.
%% Composition of functions doesn't change x.  x = g(f(x))
-spec marshal_identity_event({integer(), ff_machine:timestamped_event(ff_identity:event())}) ->
    ff_proto_identity_thrift:'IdentityEvent'().

marshal_identity_event({ID, {ev, Timestamp, Ev}}) ->
    #idnt_IdentityEvent{
        sequence   = marshal(event_id, ID),
        occured_at = marshal(timestamp, Timestamp),
        change     = marshal(event, Ev)
    }.

-spec marshal_challenge(ff_identity:challenge()) -> ff_proto_identity:'Challenge'().

marshal_challenge(Challenge) ->
    Proofs = ff_identity_challenge:proofs(Challenge),
    Status = ff_identity_challenge:status(Challenge),
    #idnt_Challenge{
        id     = ff_identity_challenge:id(Challenge),
        cls    = ff_identity_challenge:class(Challenge),
        proofs = marshal({list, challenge_proofs}, Proofs),
        status = marshal(challenge_payload_status_changed, Status)
    }.

-spec marshal_identity(ff_identity:identity()) ->
    ff_proto_identity_thrift:'Identity'().

marshal_identity(Identity) ->
    EffectiveChallengeID = case ff_identity:effective_challenge(Identity) of
        {ok, ID} -> maybe_marshal(id, ID);
        {error, notfound} -> undefined
    end,
    #idnt_Identity{
        id       = maybe_marshal(id, ff_identity:id(Identity)),
        party    = marshal(id, ff_identity:party(Identity)),
        provider = marshal(id, ff_identity:provider(Identity)),
        cls      = marshal(id, ff_identity:class(Identity)),
        contract = maybe_marshal(id, ff_identity:contract(Identity)),
        level    = maybe_marshal(id, ff_identity:level(Identity)),
        blocked  = maybe_marshal(bool, ff_identity:blocked(Identity)),
        external_id = maybe_marshal(id, ff_identity:external_id(Identity)),
        effective_challenge = EffectiveChallengeID
    }.

-spec unmarshal_identity(ff_proto_identity_thrift:'Identity'()) -> ff_identity:identity().

unmarshal_identity(#idnt_Identity{
    id          = ID,
    party       = PartyID,
    provider    = ProviderID,
    cls         = ClassID,
    contract    = ContractID,
    level       = LevelID,
    blocked     = Blocked,
    external_id = ExternalID,
    effective_challenge = EffectiveChallengeID
}) ->
    genlib_map:compact(#{
        id          => unmarshal(id,      ID),
        party       => unmarshal(id,      PartyID),
        provider    => unmarshal(id,      ProviderID),
        class       => unmarshal(id,      ClassID),
        contract    => unmarshal(id,      ContractID),
        level       => maybe_unmarshal(id,   LevelID),
        blocked     => maybe_unmarshal(bool, Blocked),
        external_id => maybe_unmarshal(id,   ExternalID),
        effective   => maybe_unmarshal(id,   EffectiveChallengeID)
    }).

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) -> ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(event, {created, Identity}) ->
    {created, marshal_identity(Identity)};
marshal(event, {level_changed, LevelID}) ->
    {level_changed, marshal(id, LevelID)};
marshal(event, {{challenge, ChallengeID}, ChallengeChange}) ->
    {identity_challenge, marshal(challenge_change, #{
        id => ChallengeID,
        payload => ChallengeChange
    })};
marshal(event, {effective_challenge_changed, ChallengeID}) ->
    {effective_challenge_changed, marshal(id, ChallengeID)};

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
}) ->
    Proofs = maps:get(proofs, Challenge, []),
    #idnt_Challenge{
        cls    = marshal(id, ID),
        proofs = marshal({list, challenge_proofs}, Proofs)
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

marshal(context, Ctx) ->
    maybe_marshal(msgpack, Ctx);

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
    {created, unmarshal_identity(Identity)};
unmarshal(event, {level_changed, LevelID}) ->
    {level_changed, unmarshal(id, LevelID)};
unmarshal(event, {identity_challenge, #idnt_ChallengeChange{id = ID, payload = Payload}}) ->
    {{challenge, unmarshal(id, ID)}, unmarshal(challenge_payload, Payload)};
unmarshal(event, {effective_challenge_changed, ChallengeID}) ->
    {effective_challenge_changed, unmarshal(id, ChallengeID)};

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

unmarshal(challenge_proofs, Proofs) ->
    [{unmarshal(proof_type, P#idnt_ChallengeProof.type),
      unmarshal(id, P#idnt_ChallengeProof.token)} || P <- Proofs];

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
        valid_until => unmarshal(timestamp, ValidUntil)
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

unmarshal(context, Ctx) ->
    maybe_unmarshal(msgpack, Ctx);

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
    ID         = genlib:unique(),
    PartyID    = genlib:unique(),
    ProviderID = genlib:unique(),
    ClassID    = genlib:unique(),
    ContractID = genlib:unique(),
    LevelID    = genlib:unique(),
    Blocked    = true,
    ExternalID = genlib:unique(),
    EffectiveChallengeID = genlib:unique(),
    IdentityIn = #{
        id          => ID,
        party       => PartyID,
        provider    => ProviderID,
        class       => ClassID,
        contract    => ContractID,
        level       => LevelID,
        blocked     => Blocked,
        external_id => ExternalID,
        effective   => EffectiveChallengeID
    },
    IdentityOut = unmarshal_identity(marshal_identity(IdentityIn)),
    ?assertEqual(IdentityOut, IdentityIn).

-endif.