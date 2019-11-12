-module(ff_p2p_transfer_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

-define(PREFIX, #{
    transfer => <<"p2p_transfer">>,
    adjustment => <<"p2p_adj">>,
    status => <<"p2p_status">>
}).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal(event, {created, P2PTransfer}) ->
    {created, marshal(p2p_transfer, P2PTransfer)};
marshal(event, {status_changed, Status}) ->
    {status_changed, #p2p_transfer_StatusChange{status = marshal(status, Status)}};
marshal(event, {resource_got, Sender, Receiver}) ->
    {resource, marshal(resource_got, {Sender, Receiver})};
marshal(event, {risk_score_changed, RiskScore}) ->
    {risk_score, marshal(risk_score, RiskScore)};
marshal(event, {route_changed, Route}) ->
    {route, marshal(route, Route)};
marshal(event, {p_transfer, TransferChange}) ->
    {transfer, #p2p_transfer_TransferChange{payload = ff_p_transfer_codec:marshal(event, TransferChange)}};
marshal(event, {session, Session}) ->
    {session, marshal(session, Session)};
marshal(event, {adjustment, #{id := ID, payload := Payload}}) ->
    {adjustment, #p2p_transfer_AdjustmentChange{
        id = ff_adjustment_codec:marshal(?PREFIX, id, ID),
        payload = ff_adjustment_codec:marshal(?PREFIX, event, Payload)
    }};

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(repair_scenario, {add_events, #p2p_transfer_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events, genlib_map:compact(#{
        events => unmarshal({list, event}, Events),
        actions => maybe_unmarshal(complex_action, Action)
    })};

unmarshal(event, {created, Source}) ->
    {created, unmarshal(source, Source)};
unmarshal(event, {account, AccountChange}) ->
    {account, unmarshal(account_change, AccountChange)};
unmarshal(event, {status, #p2p_transfer_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

% maybe_marshal(_Type, undefined) ->
%     undefined;
% maybe_marshal(Type, Value) ->
%     marshal(Type, Value).
