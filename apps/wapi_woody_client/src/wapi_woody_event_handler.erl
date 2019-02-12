-module(wapi_woody_event_handler).
-behaviour(woody_event_handler).

-include_lib("woody/src/woody_defs.hrl").
-export([handle_event/4]).

-spec handle_event(Event, RpcID, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcID :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(EventType, RpcID, EventMeta, _Opts) ->
    Msg = woody_event_handler:format_event(EventType, EventMeta, RpcID),
    format_event(EventType, RpcID, EventMeta, Msg).

%% common

-define(CLIENT, 'rpc.client').

format_event(EventType, RpcID, _EventMeta, Msg) when
    EventType == ?EV_INTERNAL_ERROR;
    EventType == ?EV_TRACE
->
    log(RpcID, Msg, collect(?CLIENT, #{
        event => EventType
    }));

%% client

format_event(EventType = ?EV_CALL_SERVICE, RpcID, #{
    service  := Service,
    function := Function,
    type     := Type,
    metadata := Metadata
}, Msg) ->
    ok = enter(?CLIENT, maps:merge(Metadata, #{
        service  => Service,
        function => Function,
        type     => Type
    })),
    log(RpcID, Msg, collect(?CLIENT, #{
        event => EventType
    }));

format_event(EventType = ?EV_CLIENT_SEND, RpcID, #{}, Msg) ->
    log(RpcID, Msg, collect(?CLIENT, #{
        event => EventType
    }));

format_event(EventType = ?EV_CLIENT_RECEIVE, RpcID, #{status := Status}, Msg) ->
    log(RpcID, Msg, collect(?CLIENT, #{
        event => EventType,
        status => Status
    }));

format_event(EventType = ?EV_SERVICE_RESULT, RpcID, #{status := Status}, Msg) ->
    _ = log(RpcID, Msg, collect(?CLIENT, #{
        event => EventType,
        status => Status
    })),
    leave(?CLIENT);

%% server

format_event(_EventType, _RpcID, _EventMeta, _Msg) ->
    % skip safely, there's no woody servers around
    ok.

%%

log(RpcID, {Level, {Format, Args}}, MD) ->
    lager:log(Level, [{pid, self()}] ++ rpc_id_to_md(RpcID) ++ orddict:to_list(MD), Format, Args).

rpc_id_to_md(undefined) ->
    [];
rpc_id_to_md(RpcID = #{}) ->
    maps:to_list(RpcID).

%%

enter(Name, Meta) ->
    lager:md(collect(Name, Meta)).

leave(Name) ->
    lager:md(orddict:erase(Name, lager:md())).

collect(Name, Meta) ->
    orddict:store(Name, maps:merge(find_scope(Name), Meta), lager:md()).

find_scope(Name) ->
    case orddict:find(Name, lager:md()) of
        {ok, V = #{}} -> V;
        error         -> #{}
    end.
