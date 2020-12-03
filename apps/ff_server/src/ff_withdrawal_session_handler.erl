-module(ff_withdrawal_session_handler).

-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_withdrawal_session_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) -> {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(
        withdrawal_session,
        #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%

handle_function_('Get', [ID, EventRange], _Opts) ->
    case ff_withdrawal_session_machine:get(ID, ff_codec:unmarshal(event_range, EventRange)) of
        {ok, Machine} ->
            State = ff_withdrawal_session_machine:session(Machine),
            Ctx = ff_withdrawal_session_machine:ctx(Machine),
            Response = ff_withdrawal_session_codec:marshal_state(State, ID, Ctx),
            {ok, Response};
        {error, notfound} ->
            woody_error:raise(business, #fistful_WithdrawalSessionNotFound{})
    end;
handle_function_('GetContext', [ID], _Opts) ->
    case ff_withdrawal_session_machine:get(ID, {undefined, 0}) of
        {ok, Machine} ->
            Ctx = ff_withdrawal_session_machine:ctx(Machine),
            Response = ff_withdrawal_session_codec:marshal(ctx, Ctx),
            {ok, Response};
        {error, notfound} ->
            woody_error:raise(business, #fistful_WithdrawalSessionNotFound{})
    end.
