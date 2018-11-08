-module(ff_identity_eventsink_handler).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-behaviour(ff_eventsink_publisher).

-export([publish_events/1]).

-export([marshal/2]).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

-type event() :: ff_eventsink_publisher:event(ff_identity:event()).
-type sinkevent() :: ff_eventsink_publisher:sinkevent(ff_proto_identity_thrift:'SinkEvent'()).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Context, Opts) ->
    scoper:scope(identity_eventsink, #{function => Func},
        fun() ->
            ok = ff_woody_ctx:set(Context),
            try
                ff_eventsink_handler:handle_function(
                    Func, Args, Context, Opts#{
                        handler => ff_identity_eventsink_handler
                    }
                )
            after
                ff_woody_ctx:unset()
            end
        end
    ).

-spec publish_events(list(event())) ->
    list(sinkevent()).

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

-spec publish_event(event()) ->
    sinkevent().

publish_event(#{
    id          := ID,
    source_id   := SourceID,
    event       := {
        EventID,
        Dt,
        {ev, EventDt, Payload}
    }
}) ->
    #'idnt_SinkEvent'{
        'id'            = ff_eventsink_handler:marshal(event_id, ID),
        'created_at'    = ff_eventsink_handler:marshal(timestamp, Dt),
        'source'        = ff_eventsink_handler:marshal(id, SourceID),
        'payload'       = #'idnt_Event'{
            'sequence'   = ff_eventsink_handler:marshal(event_id, EventID),
            'occured_at' = ff_eventsink_handler:marshal(timestamp, EventDt),
            'changes'    = [marshal(event, Payload)]
        }
    }.

%%
%% Internals
%%

-spec marshal(term(), term()) -> term().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(event, {created, Identity}) ->
    {created, marshal(identity, Identity)};
marshal(event, {level_changed, LevelID}) ->
    {level_changed, ff_eventsink_handler:marshal(id, LevelID)};
marshal(event, {{challenge, ChallengeID}, ChallengeChange}) ->
    {identity_challenge, marshal(challenge_change, #{
        id => ChallengeID,
        payload => ChallengeChange
    })};
marshal(event, {effective_challenge_changed, ChallengeID}) ->
    {effective_challenge_changed, ff_eventsink_handler:marshal(id, ChallengeID)};

marshal(identity, Identity = #{
        party       := PartyID,
        provider    := ProviderID,
        class       := ClassID
}) ->
    ContractID = maps:get(contract, Identity, undefined),
    #'idnt_Identity'{
        'party'     = ff_eventsink_handler:marshal(id, PartyID),
        'provider'  = ff_eventsink_handler:marshal(id, ProviderID),
        'cls'       = ff_eventsink_handler:marshal(id, ClassID),
        'contract'  = ff_eventsink_handler:marshal(id, ContractID)
    };

marshal(challenge_change, #{
        id       := ID,
        payload  := Payload
    }) ->
    #'idnt_ChallengeChange'{
        id      = ff_eventsink_handler:marshal(id, ID),
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
        cls    = ff_eventsink_handler:marshal(id, ID),
        proofs = marshal({list, challenge_proofs}, Proofs)
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
        valid_until = ff_eventsink_handler:marshal(timestamp, ValidUntil)
    },
    {completed, NewStatus};
marshal(challenge_payload_status_changed, {failed, _Status}) ->
    {failed, #'idnt_ChallengeFailed'{}};
marshal(resolution, approved) ->
    approved;
marshal(resolution, denied) ->
    denied;

% Catch this up in thrift validation
marshal(_, Other) ->
    Other.
