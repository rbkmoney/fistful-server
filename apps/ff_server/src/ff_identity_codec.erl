-module(ff_identity_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

-export([unmarshal_identity_params/1]).
-export([unmarshal_challenge_params/1]).

-export([marshal_identity_event/1]).

-export([marshal_challenge/1]).
-export([unmarshal_challenge/1]).

-export([marshal_identity/1]).
-export([unmarshal_identity/1]).

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
    external_id = ExternalID
}) ->
    genlib_map:compact(#{
        id          => unmarshal(id, ID),
        party       => unmarshal(id, PartyID),
        provider    => unmarshal(id, ProviderID),
        class       => unmarshal(id, ClassID),
        external_id => maybe_unmarshal(id, ExternalID)
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

-spec marshal_challenge(ff_identity_challenge:challenge()) -> ff_proto_identity_thrift:'Challenge'().

marshal_challenge(Challenge) ->
    Proofs = ff_identity_challenge:proofs(Challenge),
    Status = ff_identity_challenge:status(Challenge),
    #idnt_Challenge{
        id     = ff_identity_challenge:id(Challenge),
        cls    = ff_identity_challenge:class(Challenge),
        proofs = marshal({list, challenge_proofs}, Proofs),
        status = marshal(challenge_payload_status_changed, Status)
    }.

-dialyzer([{nowarn_function, [unmarshal_challenge/1]}, no_match]).
-spec unmarshal_challenge(ff_proto_identity_thrift:'Challenge'()) -> ff_identity_challenge:challenge().

unmarshal_challenge(#idnt_Challenge{
        id     = ID,
        cls    = ClassID,
        proofs = Proofs,
        status = Status
    }) -> #{
        id     => unmarshal(id, ID),
        proofs => unmarshal({list, challenge_proofs}, Proofs),
        status => unmarshal(challenge_payload_status_changed, Status),
        challenge_class => unmarshal(id, ClassID)
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
        blocking = maybe_marshal(blocking, ff_identity:blocking(Identity)),
        created_at = maybe_marshal(created_at, ff_identity:created_at(Identity)),
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
    blocking    = Blocking,
    external_id = ExternalID,
    created_at  = CreatedAt,
    effective_challenge = EffectiveChallengeID
}) ->
    genlib_map:compact(#{
        id          => unmarshal(id, ID),
        party       => unmarshal(id, PartyID),
        provider    => unmarshal(id, ProviderID),
        class       => unmarshal(id, ClassID),
        contract    => unmarshal(id, ContractID),
        level       => maybe_unmarshal(id, LevelID),
        blocking    => maybe_unmarshal(blocking, Blocking),
        external_id => maybe_unmarshal(id, ExternalID),
        created_at  => maybe_unmarshal(created_at, CreatedAt),
        effective   => maybe_unmarshal(id, EffectiveChallengeID)
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
    Blocking   = blocked,
    IdentityIn = #{
        id          => genlib:unique(),
        party       => genlib:unique(),
        provider    => genlib:unique(),
        class       => genlib:unique(),
        contract    => genlib:unique(),
        level       => genlib:unique(),
        blocking    => Blocking,
        external_id => genlib:unique(),
        effective   => genlib:unique()
    },
    IdentityOut = unmarshal_identity(marshal_identity(IdentityIn)),
    ?assertEqual(IdentityOut, IdentityIn).

-spec challenge_test() -> _.
challenge_test() ->
    Status = {completed, #{
            resolution => approved,
            valid_until => {calendar:universal_time(), 0}
        }},
    ChallengeIn = #{
        id     => genlib:unique(),
        proofs => [{rus_retiree_insurance_cert, <<"Bananazzzz">>}],
        status => Status,
        challenge_class => genlib:unique()
    },
    ChallengeOut = unmarshal_challenge(marshal_challenge(ChallengeIn)),
    ?assertEqual(ChallengeIn, ChallengeOut).

-endif.
