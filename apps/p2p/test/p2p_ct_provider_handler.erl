-module(p2p_ct_provider_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function(Func, Args, Ctx, Opts) ->
    scoper:scope(p2p_ct_provider, #{},
        fun() ->
            handle_function_(Func, Args, Ctx, Opts)
        end
    ).

handle_function_('Process', [
    #p2p_adapter_Context{
        operation = {process, #p2p_adapter_ProcessOperationInfo{body = #p2p_adapter_Cash{amount = 101}}},
        session = #p2p_adapter_Session{state = State}
}], _Ctx, _Opts) ->
    case State of
        undefined ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {sleep, #p2p_adapter_SleepIntent{
                    timer = {timeout, 1},
                    callback_tag = <<"user_tag">>,
                    user_interaction = #p2p_adapter_UserInteraction{
                        id = <<"test_user_interaction">>,
                        intent = {create, #p2p_adapter_UserInteractionCreate{
                            user_interaction = {qr_code_show_request, #'QrCodeShowRequest'{
                                qr_code = #'QrCode'{
                                    payload = <<"data">>
                                }
                            }}
                        }}
                    }
                }},
                next_state = <<"user_sleep">>
            }};
        <<"user_sleep">> ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {sleep, #p2p_adapter_SleepIntent{
                    timer = {timeout, 1},
                    callback_tag = <<"user_tag">>,
                    user_interaction = #p2p_adapter_UserInteraction{
                        id = <<"test_user_interaction">>,
                        intent = {finish, #p2p_adapter_UserInteractionFinish{}}
                    }
                }},
                next_state = <<"user_finish_sleep">>
            }};
        <<"user_finish_sleep">> ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {finish, #p2p_adapter_FinishIntent{
                    status = {success, #p2p_adapter_Success{}}
                }},
                next_state = <<"user_sleep_finished">>
            }}
    end;
handle_function_('Process', [
    #p2p_adapter_Context{
        operation = {process, #p2p_adapter_ProcessOperationInfo{body = #p2p_adapter_Cash{amount = 999}}},
        session = #p2p_adapter_Session{state = State}
}], _Ctx, _Opts) ->
    case State of
        undefined ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {sleep, #p2p_adapter_SleepIntent{
                    timer = {timeout, 1},
                    callback_tag = <<"simple_tag">>
                }},
                next_state = <<"simple_sleep">>
            }};
        <<"simple_sleep">> ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {finish, #p2p_adapter_FinishIntent{
                    status = {success, #p2p_adapter_Success{}}
                }},
                next_state = <<"sleep_finished">>
            }}
    end;
handle_function_('Process', [
    #p2p_adapter_Context{
        operation = {process, #p2p_adapter_ProcessOperationInfo{body = #p2p_adapter_Cash{amount = 1001}}}
}], _Ctx, _Opts) ->
    {ok, #p2p_adapter_ProcessResult{
        intent = {finish, #p2p_adapter_FinishIntent{
            status = {failure, #domain_Failure{code = <<"test_failure">>}}
        }}
    }};
handle_function_('Process', [_Context], _Ctx, _Opts) ->
    % error({test, _Context}),
    {ok, #p2p_adapter_ProcessResult{
        intent = {finish, #p2p_adapter_FinishIntent{
            status = {success, #p2p_adapter_Success{}}
        }}
    }};
handle_function_('HandleCallback', [_Callback, _Context], _Ctx, _Opts) ->
    {ok, #p2p_adapter_CallbackResult{
        response = #p2p_adapter_CallbackResponse{payload = <<"payload">>},
        intent = {finish, #p2p_adapter_FinishIntent{
            status = {success, #p2p_adapter_Success{}}
        }}
    }}.

%%
%% Internals
%%

