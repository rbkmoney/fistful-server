-module(ff_p2p_session_handler).
-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_p2p_session_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function(Func, Args, Opts) ->
    scoper:scope(p2p_session, #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%

handle_function_('Get', [ID, EventRange], _Opts) ->
    case p2p_session_machine:get(ID, ff_codec:unmarshal(event_range, EventRange)) of
        {ok, Machine} ->
            State = p2p_session_machine:session(Machine),
            Ctx = p2p_session_machine:ctx(Machine),
            Response = ff_p2p_session_codec:marshal_state(State, Ctx),
            {ok, Response};
        {error, {unknown_p2p_session, _Ref}} ->
            woody_error:raise(business, #fistful_P2PSessionNotFound{})
    end;

handle_function_('GetContext', [ID], _Opts) ->
    case p2p_session_machine:get(ID, {undefined, 0}) of
        {ok, Machine} ->
            Ctx = p2p_session_machine:ctx(Machine),
            Response = ff_p2p_session_codec:marshal(ctx, Ctx),
            {ok, Response};
        {error, {unknown_p2p_session, _Ref}} ->
            woody_error:raise(business, #fistful_P2PSessionNotFound{})
    end.
