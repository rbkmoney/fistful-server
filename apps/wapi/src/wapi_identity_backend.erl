-module(wapi_identity_backend).

% -type req_data()        :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data()   :: wapi_handler:response_data().
-type params()          :: map().
-type id()              :: binary().
-type result(T, E)      :: {ok, T} | {error, E}.

-export([create_identity/2]).
-export([get_identity/2]).
-export([get_identities/2]).
-export([create_identity_challenge/3]).
-export([get_identity_challenge/3]).
-export([get_identity_challenges/3]).
-export([get_identity_challenge_events/2]).
-export([get_identity_challenge_event/2]).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

%% Pipeline

-spec get_identity(binary(), handler_context()) ->
    {ok, response_data()}             |
    {error, {identity, notfound}}     |
    {error, {identity, unauthorized}} .

get_identity(IdentityID, WoodyContext) ->
    Request = {fistful_identity, 'Get', [IdentityID]},
    case service_call(Request, WoodyContext) of
        {ok, IdentityThrift} ->
            case wapi_access_backend:check_resource(identity, IdentityThrift, WoodyContext) of
                ok ->
                    {ok, unmarshal(identity, IdentityThrift)};
                {error, unauthorized} ->
                    {error, {identity, unauthorized}}
            end;
        {exception, #fistful_IdentityNotFound{}} ->
            {error, {identity, notfound}}
    end.

-spec create_identity(params(), handler_context()) -> result(map(),
    {provider, notfound}       |
    {identity_class, notfound} |
    {conflict, id()}           |
    inaccessible               |
    _Unexpected
).
create_identity(Params, WoodyContext) ->
    ID = create_id(Params, WoodyContext),
    IdentityParams = marshal(identity_params, {
        Params,
        wapi_handler_utils:get_owner(WoodyContext),
        create_context(Params, WoodyContext)
    }),
    Request = {fistful_identity, 'Create', [ID, IdentityParams]},

    %% FIXME we can`t get #'payproc_PartyNotFound' here, so we can`t create party if need it

    case service_call(Request, WoodyContext) of
        {ok, Identity} ->
            {ok, unmarshal(identity, Identity)};
        {exception, #fistful_ProviderNotFound{}} ->
            {error, {provider, notfound}};
        {exception, #fistful_IdentityClassNotFound{}} ->
            {error, {identity_class, notfound}};
        {exception, #fistful_PartyInaccessible{}} ->
            {error, inaccessible};
        {exception, #fistful_IDExists{}} ->
            {_, Hash} = wapi_backend_utils:create_params_hash(Params),
            get_and_compare_hash(ID, Hash, WoodyContext);
        {exception, Details} ->
            {error, Details}
    end.

-spec get_identities(params(), handler_context()) -> no_return().
get_identities(_Params, _Context) ->
    wapi_handler_utils:throw_not_implemented().

-spec create_identity_challenge(id(), params(), handler_context()) -> result(map(),
    {identity, notfound}               |
    {identity, unauthorized}           |
    {challenge, pending}               |
    {challenge, {class, notfound}}     |
    {challenge, {proof, notfound}}     |
    {challenge, {proof, insufficient}} |
    {challenge, level}                 |
    {challenge, conflict}
).
create_identity_challenge(IdentityID, Params, WoodyContext) ->
    ChallengeID = wapi_backend_utils:make_id(identity_challenge),
    case wapi_access_backend:check_resource(identity, IdentityID, WoodyContext) of
        ok ->
            ChallengeParams = marshal(challenge_params, {ChallengeID, Params}),
            Request = {fistful_identity, 'StartChallenge', [IdentityID, ChallengeParams]},
            case service_call(Request, WoodyContext) of
                {ok, Challenge} ->
                    {ok, unmarshal(challenge, {Challenge, WoodyContext})};
                {exception, #fistful_IdentityNotFound{}} ->
                    {error, {identity, notfound}};
                {exception, #fistful_ChallengePending{}} ->
                    {error, {challenge, pending}};
                {exception, #fistful_ChallengeClassNotFound{}} ->
                    {error, {challenge, {class, notfound}}};
                {exception, #fistful_ProofNotFound{}} ->
                    {error, {challenge, {proof, notfound}}};
                {exception, #fistful_ProofInsufficient{}} ->
                    {error, {challenge, {proof, insufficient}}};
                {exception, #fistful_ChallengeLevelIncorrect{}} ->
                    {error, {challenge, level}};
                {exception, #fistful_ChallengeConflict{}} ->
                    {error, {challenge, conflict}};
                {exception, Details} ->
                    {error, Details}
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}}
    end.

-spec get_identity_challenge(id(), id(), handler_context()) -> result(map(),
    {identity, notfound}     |
    {identity, unauthorized} |
    {challenge, notfound}
).
get_identity_challenge(IdentityID, ChallengeID, WoodyContext) ->
    case wapi_access_backend:check_resource(identity, IdentityID, WoodyContext) of
        ok ->
            Request = {fistful_identity, 'GetChallenges', [IdentityID]},
            case service_call(Request, WoodyContext) of
                {ok, Challenges} ->
                    get_challenge_by_id(ChallengeID, Challenges, WoodyContext);
                {exception, #fistful_IdentityNotFound{}} ->
                    {error, {identity, notfound}};
                {exception, Details} ->
                    {error, Details}
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}}
    end.

-spec get_identity_challenges(id(), binary(), handler_context()) -> result(map(),
    {identity, notfound}     |
    {identity, unauthorized} |
    {challenge, notfound}
).
get_identity_challenges(IdentityID, Status, WoodyContext) ->
    case wapi_access_backend:check_resource(identity, IdentityID, WoodyContext) of
        ok ->
            Request = {fistful_identity, 'GetChallenges', [IdentityID]},
            case service_call(Request, WoodyContext) of
                {ok, Challenges} ->
                    Filtered = filter_challenges_by_status(Status, Challenges, WoodyContext, []),
                    {ok, unmarshal({list, challenge}, Filtered)};
                {exception, #fistful_IdentityNotFound{}} ->
                    {error, {identity, notfound}};
                {exception, Details} ->
                    {error, Details}
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}}
    end.

-spec get_identity_challenge_events(params(), handler_context()) -> result(map(),
    {identity, notfound}     |
    {identity, unauthorized}
).
get_identity_challenge_events(Params = #{
    'identityID'  := IdentityID,
    'challengeID' := ChallengeID,
    'limit'  := Limit
}, WoodyContext) ->
    case wapi_access_backend:check_resource(identity, IdentityID, WoodyContext) of
        ok ->
            Cursor = maps:get('eventCursor', Params, undefined),
            EventRange = marshal(event_range, {Cursor, Limit}),
            Request = {fistful_identity, 'GetEvents', [IdentityID, EventRange]},
            case service_call(Request, WoodyContext) of
                {ok, Events} ->
                    Filtered = filter_events_by_challenge_id(ChallengeID, Events, []),
                    {ok, unmarshal({list, identity_challenge_event}, Filtered)};
                {exception, #fistful_IdentityNotFound{}} ->
                    {error, {identity, notfound}};
                {exception, Details} ->
                    {error, Details}
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}}
    end.

-spec get_identity_challenge_event(params(), handler_context()) -> result(map(),
    {identity, notfound}     |
    {identity, unauthorized} |
    {event, notfound}
).
get_identity_challenge_event(Params = #{
    'identityID'  := IdentityID
}, WoodyContext) ->
    case wapi_access_backend:check_resource(identity, IdentityID, WoodyContext) of
        ok ->
            get_identity_challenge_event_(Params, WoodyContext);
        {error, unauthorized} ->
            {error, {identity, unauthorized}}
    end.

get_identity_challenge_event_(#{
    'identityID'  := IdentityID,
    'challengeID' := ChallengeID,
    'eventID'     := EventId
}, WoodyContext) ->
    EventRange = marshal(event_range, {EventId - 1, 1}),
    Request = {fistful_identity, 'GetEvents', [IdentityID, EventRange]},
    case service_call(Request, WoodyContext) of
        {ok, []} ->
            {error, {event, notfound}};
        {ok, Events} ->
            case filter_events_by_challenge_id(ChallengeID, Events, []) of
                [Event] ->
                    {ok, unmarshal(identity_challenge_event, Event)};
                _ ->
                    {error, {event, notfound}}
            end;
        {exception, #fistful_IdentityNotFound{}} ->
            {error, {identity, notfound}};
        {exception, Details} ->
            {error, Details}
    end.

%%
%% Internal
%%

filter_events_by_challenge_id(_ID, [], Result) ->
    Result;
filter_events_by_challenge_id(
    ID, [
        #idnt_IdentityEvent{
            change = {identity_challenge, #idnt_ChallengeChange{
                id = ID,
                payload = {status_changed, _Status} = Payload
            }},
            occured_at = OccuredAt,
            sequence = EventID
        } |
        Rest
    ],
    Acc
) ->
    filter_events_by_challenge_id(ID, Rest, [{EventID, OccuredAt, Payload} | Acc]);
filter_events_by_challenge_id(ID, [_H | Rest], Acc) ->
    filter_events_by_challenge_id(ID, Rest, Acc).

get_challenge_by_id(_ID, [], _) ->
    {error, {challenge, notfound}};
get_challenge_by_id(ID, [Challenge = #idnt_Challenge{id = ID} | _Rest], WoodyContext) ->
    {ok, unmarshal(challenge, {Challenge, WoodyContext})};
get_challenge_by_id(ID, [_Challenge | Rest], WoodyContext) ->
    get_challenge_by_id(ID, Rest, WoodyContext).

filter_challenges_by_status(undefined, Challenges, WoodyContext, _) ->
    [{Challenge, WoodyContext} || Challenge <- Challenges];
filter_challenges_by_status(_Status, [], _, Result) ->
    Result;
filter_challenges_by_status(
    FilteringStatus,
    [Challenge = #idnt_Challenge{status = Status} | Rest],
    WoodyContext,
    Acc
) ->
    ChallengeStatus = maps:get(<<"status">>, unmarshal(challenge_status, Status), undefined),
    case ChallengeStatus =:= FilteringStatus of
        false ->
            filter_challenges_by_status(FilteringStatus, Rest, WoodyContext, Acc);
        true ->
            filter_challenges_by_status(FilteringStatus, Rest, WoodyContext, [{Challenge, WoodyContext} | Acc])
    end.

enrich_proofs(Proofs, WoodyContext) ->
    [enrich_proof(unmarshal(proof, P), WoodyContext) || P <- Proofs].

enrich_proof(#{<<"token">> := Token}, WoodyContext) ->
    wapi_privdoc_backend:get_proof(Token, WoodyContext).

get_and_compare_hash(ID, Hash, WoodyContext) ->
    Request = {fistful_identity, 'Get', [ID]},
    {ok, Identity} = service_call(Request, WoodyContext),
    case wapi_backend_utils:compare_hash(Hash, get_hash(Identity)) of
        ok ->
            {ok, unmarshal(identity, Identity)};
        {error, conflict_hash} ->
            {error, {conflict, ID}}
    end.

get_hash(#idnt_Identity{context = Ctx}) ->
    wapi_backend_utils:get_hash(unmarshal(context, Ctx)).

create_id(ParamsIn, WoodyContext) ->
    wapi_backend_utils:make_id(
        identity,
        wapi_backend_utils:construct_external_id(ParamsIn, WoodyContext)
    ).

create_context(ParamsIn, WoodyContext) ->
    Hash = wapi_backend_utils:create_params_hash(ParamsIn),
    List = [
        {<<"name">>, maps:get(<<"name">>, ParamsIn, undefined)},
        {<<"owner">>, wapi_handler_utils:get_owner(WoodyContext)},
        {<<"metadata">>, maps:get(<<"metadata">>, ParamsIn, undefined)},
        Hash
    ],
    wapi_backend_utils:extend_ctx_from_list(List, wapi_backend_utils:make_ctx()).

service_call(Params, Ctx) ->
    wapi_handler_utils:service_call(Params, Ctx).

marshal({list, Type}, List) ->
    lists:map(fun(V) -> marshal(Type, V) end, List);

marshal(identity_params, {Params = #{
    <<"provider">>  := Provider,
    <<"class">>     := Class
}, Owner, Context}) ->
    ExternalID = maps:get(<<"externalID">>, Params, undefined),
    #idnt_IdentityParams{
        party = marshal(id, Owner),
        provider = marshal(string, Provider),
        cls = marshal(string, Class),
        external_id = marshal(id, ExternalID),
        context = marshal(context, Context)
    };

marshal(challenge_params, {ID, #{
    <<"type">>     := Class,
    <<"proofs">>    := Proofs
}}) ->
    #idnt_ChallengeParams{
        id = marshal(id, ID),
        cls = marshal(id, Class),
        proofs = marshal({list, proof}, Proofs)
    };

marshal(proof, Params = #{
    <<"token">>     := Token
}) ->
    Type = maps:get(<<"type">>, Params, undefined),
    #idnt_ChallengeProof{
        type = maybe_marshal(string, Type),
        token = marshal(string, Token)
    };

marshal(event_range, {Cursor, Limit}) ->
    #'EventRange'{
        'after' = marshal(integer, Cursor),
        'limit' = marshal(integer, Limit)
    };

marshal(context, Ctx) ->
    ff_context:wrap(Ctx);

marshal(T, V) ->
    ff_codec:marshal(T, V).

%%

unmarshal({list, Type}, List) ->
    lists:map(fun(V) -> unmarshal(Type, V) end, List);

unmarshal(identity, #idnt_Identity{
    id          = IdentityID,
    blocked     = Blocked,
    cls         = Class,
    provider    = Provider,
    level       = Level,
    effective_challenge = EffectiveChallenge,
    external_id = ExternalID,
    context     = Ctx
}) ->
    Context = unmarshal(context, Ctx),
    genlib_map:compact(#{
        <<"id">>                    => unmarshal(id, IdentityID),
        <<"name">>                  => wapi_backend_utils:get_from_ctx(<<"name">>, Context),
        %% TODO add createdAt to proto struct
        % <<"createdAt">>           => unmarshal(timestamp, CreatedAt),
        <<"isBlocked">>             => maybe_unmarshal(blocked, Blocked),
        <<"class">>                 => unmarshal(string, Class),
        <<"provider">>              => unmarshal(id, Provider),
        <<"level">>                 => maybe_unmarshal(id, Level),
        <<"effectiveChallenge">>    => maybe_unmarshal(id, EffectiveChallenge),
        <<"externalID">>            => maybe_unmarshal(id, ExternalID),
        <<"metadata">>              => wapi_backend_utils:get_from_ctx(<<"metadata">>, Context)
    });

unmarshal(challenge, {#idnt_Challenge{
    id          = ID,
    cls         = Class,
    proofs      = Proofs,
    status      = Status
}, WoodyContext}) ->
    genlib_map:compact(maps:merge(#{
        <<"id">>    => unmarshal(id, ID),
        <<"type">>  => unmarshal(id, Class),
        <<"proofs">>  => enrich_proofs(Proofs, WoodyContext)
    }, unmarshal(challenge_status, Status)));

unmarshal(challenge_status, {pending, #idnt_ChallengePending{}}) ->
    #{<<"status">>  => <<"Pending">>};
unmarshal(challenge_status, {cancelled, #idnt_ChallengeCancelled{}}) ->
    #{<<"status">>  => <<"Cancelled">>};
unmarshal(challenge_status, {completed, #idnt_ChallengeCompleted{
    valid_until = Time,
    resolution = approved
}}) ->
    #{
        <<"status">>  => <<"Completed">>,
        <<"validUntil">> => unmarshal(string, Time)
    };
unmarshal(challenge_status, {completed, #idnt_ChallengeCompleted{
    resolution = denied
}}) ->
    %% TODO Add denied reason to proto
    unmarshal(challenge_status, {failed, #idnt_ChallengeFailed{}});
unmarshal(challenge_status, {failed, #idnt_ChallengeFailed{}}) ->
    #{
        <<"status">>  => <<"Failed">>,
        <<"failureReason">>  => <<"Denied">>
    };

unmarshal(proof, #idnt_ChallengeProof{
    type = Type,
    token = Token
}) ->
    genlib_map:compact(#{
        <<"type">>  => unmarshal(proof_type, Type),
        <<"token">>  => unmarshal(string, Token)
    });

unmarshal(proof_type, rus_domestic_passport) ->
    <<"RUSDomesticPassport">>;
unmarshal(proof_type, rus_retiree_insurance_cert) ->
    <<"RUSRetireeInsuranceCertificate">>;

unmarshal(identity_challenge_event, {ID, Ts, V}) ->
    #{
        <<"eventID">>   => unmarshal(integer, ID),
        <<"occuredAt">> => unmarshal(string, Ts),
        <<"changes">>   => [unmarshal(identity_challenge_event_change, V)]
    };

unmarshal(identity_challenge_event_change, {status_changed, S}) ->
    maps:merge(
        #{<<"type">> => <<"IdentityChallengeStatusChanged">>},
        unmarshal(challenge_status, S)
    );

unmarshal(blocked, false) ->
    false;
unmarshal(blocked, true) ->
    true;

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).

maybe_marshal(_, undefined) ->
    undefined;
maybe_marshal(T, V) ->
    marshal(T, V).
