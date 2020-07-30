-module(wapi_identity_backend).

-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().
-type params() :: map().
-type id() :: binary().
-type status() :: binary().
-type result(T, E) :: {ok, T} | {error, E}.

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

-spec get_identity(id(), handler_context()) ->
    {ok, response_data()}             |
    {error, {identity, notfound}}     |
    {error, {identity, unauthorized}} .

get_identity(IdentityID, HandlerContext) ->
    Request = {fistful_identity, 'Get', [IdentityID]},
    case service_call(Request, HandlerContext) of
        {ok, IdentityThrift} ->
            case wapi_access_backend:check_resource(identity, IdentityThrift, HandlerContext) of
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
    {external_id_conflict, id()}           |
    inaccessible               |
    _Unexpected
).
create_identity(Params, HandlerContext) ->
    case create_id(identity, Params, HandlerContext) of
        {ok, ID} ->
            create_identity(ID, Params, HandlerContext);
        {error, {external_id_conflict, _}} = Error ->
            Error
    end.

create_identity(ID, Params, HandlerContext) ->
    IdentityParams = marshal(identity_params, {
        Params#{<<"id">> => ID},
        wapi_handler_utils:get_owner(HandlerContext)
    }),
    Request = {fistful_identity, 'Create', [IdentityParams, marshal(context, create_context(Params, HandlerContext))]},

    case service_call(Request, HandlerContext) of
        {ok, Identity} ->
            {ok, unmarshal(identity, Identity)};
        {exception, #fistful_ProviderNotFound{}} ->
            {error, {provider, notfound}};
        {exception, #fistful_IdentityClassNotFound{}} ->
            {error, {identity_class, notfound}};
        {exception, #fistful_PartyInaccessible{}} ->
            {error, inaccessible};
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
    {challenge, conflict}              |
    {external_id_conflict, id()}
).
create_identity_challenge(IdentityID, Params, HandlerContext) ->
    case create_id(identity_challenge, Params, HandlerContext) of
        {ok, ID} ->
            create_identity_challenge(ID, IdentityID, Params, HandlerContext);
        {error, {external_id_conflict, _}} = Error ->
            Error
    end.

create_identity_challenge(ChallengeID, IdentityID, Params, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            ChallengeParams = marshal(challenge_params, {ChallengeID, Params}),
            Request = {fistful_identity, 'StartChallenge', [IdentityID, ChallengeParams]},
            case service_call(Request, HandlerContext) of
                {ok, Challenge} ->
                    {ok, unmarshal(challenge, {Challenge, HandlerContext})};
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
get_identity_challenge(IdentityID, ChallengeID, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            Request = {fistful_identity, 'GetChallenges', [IdentityID]},
            case service_call(Request, HandlerContext) of
                {ok, Challenges} ->
                    get_challenge_by_id(ChallengeID, Challenges, HandlerContext);
                {exception, #fistful_IdentityNotFound{}} ->
                    {error, {identity, notfound}};
                {exception, Details} ->
                    {error, Details}
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}}
    end.

-spec get_identity_challenges(id(), status(), handler_context()) -> result(map(),
    {identity, notfound}     |
    {identity, unauthorized} |
    {challenge, notfound}
).
get_identity_challenges(IdentityID, Status, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            Request = {fistful_identity, 'GetChallenges', [IdentityID]},
            case service_call(Request, HandlerContext) of
                {ok, Challenges} ->
                    Filtered = filter_challenges_by_status(Status, Challenges, HandlerContext, []),
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
}, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            Cursor = maps:get('eventCursor', Params, undefined),
            EventRange = marshal(event_range, {Cursor, Limit}),
            Request = {fistful_identity, 'GetEvents', [IdentityID, EventRange]},
            case service_call(Request, HandlerContext) of
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
}, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            get_identity_challenge_event_(Params, HandlerContext);
        {error, unauthorized} ->
            {error, {identity, unauthorized}}
    end.

get_identity_challenge_event_(#{
    'identityID'  := IdentityID,
    'challengeID' := ChallengeID,
    'eventID'     := EventId
}, HandlerContext) ->
    EventRange = marshal(event_range, {EventId - 1, 1}),
    Request = {fistful_identity, 'GetEvents', [IdentityID, EventRange]},
    case service_call(Request, HandlerContext) of
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
        #idnt_Event{
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
get_challenge_by_id(ID, [Challenge = #idnt_ChallengeState{id = ID} | _Rest], HandlerContext) ->
    {ok, unmarshal(challenge, {Challenge, HandlerContext})};
get_challenge_by_id(ID, [_Challenge | Rest], HandlerContext) ->
    get_challenge_by_id(ID, Rest, HandlerContext).

filter_challenges_by_status(undefined, Challenges, HandlerContext, _) ->
    [{Challenge, HandlerContext} || Challenge <- Challenges];
filter_challenges_by_status(_Status, [], _, Result) ->
    Result;
filter_challenges_by_status(
    FilteringStatus,
    [Challenge = #idnt_ChallengeState{status = Status} | Rest],
    HandlerContext,
    Acc
) ->
    ChallengeStatus = maps:get(<<"status">>, unmarshal(challenge_status, Status), undefined),
    case ChallengeStatus =:= FilteringStatus of
        false ->
            filter_challenges_by_status(FilteringStatus, Rest, HandlerContext, Acc);
        true ->
            filter_challenges_by_status(FilteringStatus, Rest, HandlerContext, [{Challenge, HandlerContext} | Acc])
    end.

enrich_proofs(Proofs, HandlerContext) ->
    [enrich_proof(unmarshal(proof, P), HandlerContext) || P <- Proofs].

enrich_proof(#{<<"token">> := Token}, HandlerContext) ->
    wapi_privdoc_backend:get_proof(Token, HandlerContext).

create_id(Type, Params, HandlerContext) ->
    wapi_backend_utils:gen_id(
        Type,
        Params,
        HandlerContext
    ).

create_context(Params, HandlerContext) ->
    KV = {<<"name">>, maps:get(<<"name">>, Params, undefined)},
    wapi_backend_utils:add_to_ctx(KV, wapi_backend_utils:make_ctx(Params, HandlerContext)).

service_call(Params, Ctx) ->
    wapi_handler_utils:service_call(Params, Ctx).

%% Marshaling

marshal({list, Type}, List) ->
    lists:map(fun(V) -> marshal(Type, V) end, List);

marshal(identity_params, {Params = #{
    <<"id">>        := ID,
    <<"provider">>  := Provider,
    <<"class">>     := Class
}, Owner}) ->
    ExternalID = maps:get(<<"externalID">>, Params, undefined),
    #idnt_IdentityParams{
        id = marshal(id, ID),
        party = marshal(id, Owner),
        provider = marshal(string, Provider),
        cls = marshal(string, Class),
        external_id = marshal(id, ExternalID)
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

marshal(proof, #{<<"token">> := WapiToken}) ->
    try
        #{<<"type">> := Type, <<"token">> := Token} = wapi_utils:base64url_to_map(WapiToken),
        #idnt_ChallengeProof{
            type = marshal(proof_type, Type),
            token = marshal(string, Token)
        }
    catch
        error:badarg ->
            wapi_handler:throw_result(wapi_handler_utils:reply_error(
                422,
                wapi_handler_utils:get_error_msg(io_lib:format("Invalid proof token: ~p", [WapiToken]))
            ))
    end;

marshal(event_range, {Cursor, Limit}) ->
    #'EventRange'{
        'after' = marshal(integer, Cursor),
        'limit' = marshal(integer, Limit)
    };

marshal(context, Ctx) ->
    ff_codec:marshal(context, Ctx);

marshal(proof_type, <<"RUSDomesticPassport">>) ->
    rus_domestic_passport;
marshal(proof_type, <<"RUSRetireeInsuranceCertificate">>) ->
    rus_retiree_insurance_cert;

marshal(T, V) ->
    ff_codec:marshal(T, V).

%%

unmarshal({list, Type}, List) ->
    lists:map(fun(V) -> unmarshal(Type, V) end, List);

unmarshal(identity, #idnt_IdentityState{
    id = IdentityID,
    blocking = Blocking,
    class_id = Class,
    provider_id = Provider,
    level_id = Level,
    effective_challenge_id = EffectiveChallenge,
    external_id = ExternalID,
    created_at = CreatedAt,
    context = Ctx
}) ->
    Context = unmarshal(context, Ctx),
    genlib_map:compact(#{
        <<"id">>                    => unmarshal(id, IdentityID),
        <<"name">>                  => wapi_backend_utils:get_from_ctx(<<"name">>, Context),
        <<"createdAt">>             => maybe_unmarshal(string, CreatedAt),
        <<"isBlocked">>             => maybe_unmarshal(blocking, Blocking),
        <<"class">>                 => unmarshal(string, Class),
        <<"provider">>              => unmarshal(id, Provider),
        <<"level">>                 => maybe_unmarshal(id, Level),
        <<"effectiveChallenge">>    => maybe_unmarshal(id, EffectiveChallenge),
        <<"externalID">>            => maybe_unmarshal(id, ExternalID),
        <<"metadata">>              => wapi_backend_utils:get_from_ctx(<<"metadata">>, Context)
    });

unmarshal(challenge, {#idnt_ChallengeState{
    id          = ID,
    cls         = Class,
    proofs      = Proofs,
    status      = Status
}, HandlerContext}) ->
    genlib_map:compact(maps:merge(#{
        <<"id">>    => unmarshal(id, ID),
        <<"type">>  => unmarshal(id, Class),
        <<"proofs">>  => enrich_proofs(Proofs, HandlerContext)
    }, unmarshal(challenge_status, Status)));

unmarshal(challenge_status, {pending, #idnt_ChallengePending{}}) ->
    #{<<"status">>  => <<"Pending">>};
unmarshal(challenge_status, {cancelled, #idnt_ChallengeCancelled{}}) ->
    #{<<"status">>  => <<"Cancelled">>};
unmarshal(challenge_status, {completed, #idnt_ChallengeCompleted{
    valid_until = Time,
    resolution = approved
}}) ->
    genlib_map:compact(#{
        <<"status">>  => <<"Completed">>,
        <<"validUntil">> => maybe_unmarshal(string, Time)
    });
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
        <<"type">>  => maybe_unmarshal(proof_type, Type),
        <<"token">>  => maybe_unmarshal(string, Token)
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

unmarshal(blocking, unblocked) ->
    false;
unmarshal(blocking, blocked) ->
    true;

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).
