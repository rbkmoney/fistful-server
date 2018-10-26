-module(ff_identity_eventsink_handler).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export([marshal/2]).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

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

handle_function_('GetEvents', [#'evsink_EventRange'{'after' = After, limit = Limit}],
    _Context, #{schema := Schema}) ->
    {ok, Events} = machinery_eventsink:get_events(ff_identity_machine:get_ns(),
        After, Limit, Schema),
    publish_events(Events);
handle_function_('GetLastEventID', _Params, _Context, _Opts) ->
    case machinery_eventsink:get_last_event_id(ff_identity_machine:get_ns()) of
        {ok, ID} ->
            ID;
        {error, no_last_event} ->
            throw(#'evsink_NoLastEvent'{})
    end.

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

publish_event({ID, _Ns, SourceID, {_EventID, Dt, Payload}}) ->
    #'idnt_SinkEvent'{
        'sequence'      = marshal(event_id, ID),
        'created_at'    = marshal(timestamp, Dt),
        'source'        = marshal(id, SourceID),
        'payload'       = marshal({list, event}, Payload)
    }.

%%

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(id, V) ->
    marshal(string, V);
marshal(event_id, V) ->
    marshal(integer, V);
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
    #'idnt_Identity'{
        'party'     = marshal(id, PartyID),
        'provider'  = marshal(id, ProviderID),
        'cls'       = marshal(id, ClassID),
        'contract'  = marshal(id, ContractID)
};

marshal(challenge_change, #{
        id       := ID,
        payload  := Payload
        }) ->
    #'idnt_ChallengeChange'{
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
    #'idnt_Challenge'{
        cls    = marshal(id, ID),
        proofs = marshal(challenge_proofs, Proofs)
};
marshal(challenge_proofs, _) ->
    #'idnt_ChallengeProof'{};
marshal(challenge_payload_status_changed, pending) ->
    {pending, #'idnt_ChallengePending'{}};
marshal(challenge_payload_status_changed, cancelled) ->
    {cancelled, #'idnt_ChallengeCancelled'{}};
marshal(challenge_payload_status_changed, {completed, Status = #{
        resolution := Resolution
    }}) ->
    ValidUntil = maps:get(valid_until, Status, undefined),
    NewStatus = #'idnt_ChallengeCompleted'{
        resolution = marshal(resolution, Resolution),
        valid_until = marshal(timestamp, ValidUntil)
    },
    {completed, NewStatus};
marshal(challenge_payload_status_changed, {failed, _Status}) ->
    {failed, #'idnt_ChallengeFailed'{}};
marshal(resolution, approved) ->
    approved;
marshal(resolution, denied) ->
    denied;

marshal(timestamp, {{Date, Time}, USec} = V) ->
    case rfc3339:format({Date, Time, USec, 0}) of
        {ok, R} when is_binary(R) ->
            R;
        Error ->
            error(badarg, {timestamp, V, Error})
    end;
marshal(string, V) when is_binary(V) ->
    V;
marshal(integer, V) when is_integer(V) ->
    V;
marshal(_, Other) ->
    Other.
