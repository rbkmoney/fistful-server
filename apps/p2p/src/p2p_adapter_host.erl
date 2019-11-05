%% P2P adapter host

-module(p2p_adapter_host).
-behaviour(ff_woody_wrapper).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").

%% Exports

-export([handle_function/3]).
-export([process_callback/2]).

-define(SERVICE, {dmsl_p2p_adapter_thrift, 'P2PAdapterHost'}).

%% Types

-type adapter()                     :: ff_adapter:adapter().
-type callback_tag()                :: binary().
-type callback_payload()            :: binary().
-type callback()                    :: p2p_adapter:callback().

-type p2p_process_callback_result() :: dmsl_p2p_adapter_thrift:'ProcessCallbackResult'().

-type process_callback_result()     :: {ok, process_callback_finished() | process_callback_succeeded()}.

-type process_callback_finished()   :: {finished, p2p_adapter:context()}.
-type process_callback_succeeded()  :: {succeeded, callback_response_payload()}.

-type callback_response_payload()   :: binary().

%% Handler

-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(wallet, #{}, fun() -> handle_function_(Func, Args, Opts) end).

%% Implementation

-spec handle_function_('ProcessCallback', woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().
% placeholder
handle_function_('ProcessCallback', [Callback], _Opts) ->
    #{tag := Tag, payload := Payload} = p2p_adapter_codec:decode_callback(Callback),
    case p2p_session_machine:process_callback(Tag, Payload) of
      {ok, _result} -> Result;
        {error, notfound} ->
            woody_error:raise(business, #p2p_adapter_SessionNotFound{})
    end.
