%% P2P adapter host

-module(ff_p2p_adapter_host).

-behaviour(ff_woody_wrapper).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").

%% Exports

-export([handle_function/3]).

%% Types

-type p2p_process_callback_result() :: dmsl_p2p_adapter_thrift:'ProcessCallbackResult'().

%% Handler

-spec handle_function(woody:func(), woody:args(), woody:options()) -> {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(ff_p2p_adapter_host, #{}, fun() -> handle_function_(Func, Args, Opts) end).

%% Implementation

-spec handle_function_('ProcessCallback', woody:args(), woody:options()) ->
    {ok, p2p_process_callback_result()} | no_return().
handle_function_('ProcessCallback', {Callback}, _Opts) ->
    DecodedCallback = p2p_adapter_codec:unmarshal(callback, Callback),
    case p2p_session_machine:process_callback(DecodedCallback) of
        {ok, CallbackResponse} ->
            {ok, p2p_adapter_codec:marshal(process_callback_result, {succeeded, CallbackResponse})};
        {error, {session_already_finished, Context}} ->
            {ok, p2p_adapter_codec:marshal(process_callback_result, {finished, Context})};
        {error, {unknown_p2p_session, _Ref}} ->
            woody_error:raise(business, #p2p_adapter_SessionNotFound{})
    end.
