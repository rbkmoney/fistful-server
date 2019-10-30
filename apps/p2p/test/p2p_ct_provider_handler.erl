-module(p2p_ct_provider_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function('Process', [_Context], _Ctx, _Opts) ->
    {ok, #p2p_adapter_ProcessResult{
        intent = {finish, #p2p_adapter_FinishIntent{
            status = {success, #p2p_adapter_Success{}}
        }}
    }};
handle_function('HandleCallback', [_Callback, _Context], _Ctx, _Opts) ->
    {ok, #p2p_adapter_CallbackResult{
        response = #p2p_adapter_CallbackResponse{payload = <<"payload">>},
        intent = {finish, #p2p_adapter_FinishIntent{
            status = {success, #p2p_adapter_Success{}}
        }}
    }}.

%%
%% Internals
%%

