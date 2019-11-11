-module(p2p_ct_provider_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").

-define(ADAPTER_CALLBACK(Tag), #p2p_adapter_Callback{tag = Tag}).

-define(ADAPTER_CONTEXT(Amount), #p2p_adapter_Context{
    operation = {process, #p2p_adapter_ProcessOperationInfo{
        body = #p2p_adapter_Cash{
            amount = Amount
        }
    }}
}).
-define(ADAPTER_CONTEXT(Amount, Token, State), #p2p_adapter_Context{
    operation = {process, #p2p_adapter_ProcessOperationInfo{
        body = #p2p_adapter_Cash{amount = Amount},
        sender = {disposable, #domain_DisposablePaymentResource{
            payment_tool = {bank_card, #domain_BankCard{
                token = Token
            }}
        }}
    }},
    session = #p2p_adapter_Session{state = State}
}).

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

handle_function_('Process', [?ADAPTER_CONTEXT(101, Token, State)], _Ctx, _Opts) ->
    case State of
        undefined ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {sleep, #p2p_adapter_SleepIntent{
                    timer = {timeout, 1},
                    callback_tag = Token,
                    user_interaction = #p2p_adapter_UserInteraction{
                        id = <<"test_user_interaction">>,
                        intent = {create, #p2p_adapter_UserInteractionCreate{
                            user_interaction = {redirect,
                                {get_request,
                                    #'BrowserGetRequest'{uri = <<"uri">>}
                                }
                            }
                        }}
                    }
                }},
                next_state = <<"user_sleep">>
            }};
        <<"user_sleep">> ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {sleep, #p2p_adapter_SleepIntent{
                    timer = {timeout, 1},
                    callback_tag = Token,
                    user_interaction = #p2p_adapter_UserInteraction{
                        id = <<"test_user_interaction">>,
                        intent = {finish, #p2p_adapter_UserInteractionFinish{}}
                    }
                }}
            }};
        <<"user_callback">> ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {finish, #p2p_adapter_FinishIntent{
                    status = {success, #p2p_adapter_Success{}}
                }},
                next_state = <<"user_sleep_finished">>
            }}
    end;
handle_function_('Process', [?ADAPTER_CONTEXT(99, Token, State)], _Ctx, _Opts) ->
    case State of
        undefined ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {sleep, #p2p_adapter_SleepIntent{
                    timer = {timeout, 1},
                    callback_tag = Token
                }},
                next_state = <<"wrong">>
            }};
        <<"wrong">> ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {finish, #p2p_adapter_FinishIntent{
                    status = {success, #p2p_adapter_Success{}}
                }},
                next_state = <<"wrong_finished">>
            }}
    end;
handle_function_('Process', [?ADAPTER_CONTEXT(999, Token, State)], _Ctx, _Opts) ->
    case State of
        undefined ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {sleep, #p2p_adapter_SleepIntent{
                    timer = {timeout, 1},
                    callback_tag = Token
                }},
                next_state = <<"simple_sleep">>
            }};
        <<"simple_sleep">> ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {sleep, #p2p_adapter_SleepIntent{
                    timer = {timeout, 1},
                    callback_tag = Token
                }}
            }};
        <<"simple_callback">> ->
            {ok, #p2p_adapter_ProcessResult{
                intent = {finish, #p2p_adapter_FinishIntent{
                    status = {success, #p2p_adapter_Success{}}
                }},
                next_state = <<"sleep_finished">>
            }}
    end;
handle_function_('Process', [?ADAPTER_CONTEXT(1001)], _Ctx, _Opts) ->
    {ok, #p2p_adapter_ProcessResult{
        intent = {finish, #p2p_adapter_FinishIntent{
            status = {failure, #domain_Failure{code = <<"test_failure">>}}
        }}
    }};
handle_function_('Process', [_Context], _Ctx, _Opts) ->
    {ok, #p2p_adapter_ProcessResult{
        intent = {finish, #p2p_adapter_FinishIntent{
            status = {success, #p2p_adapter_Success{}}
        }}
    }};

handle_function_('HandleCallback', [?ADAPTER_CALLBACK(Token), ?ADAPTER_CONTEXT(_, Token, State)], _Ctx, _Opts) ->
    case State of
        <<"user_sleep">> ->
            {ok, #p2p_adapter_CallbackResult{
                response = #p2p_adapter_CallbackResponse{payload = <<"user_payload">>},
                intent = {finish, #p2p_adapter_FinishIntent{
                    status = {success, #p2p_adapter_Success{}}
                }},
                next_state = <<"user_callback">>
            }};
        <<"simple_sleep">> ->
            {ok, #p2p_adapter_CallbackResult{
                response = #p2p_adapter_CallbackResponse{payload = <<"simple_payload">>},
                intent = {finish, #p2p_adapter_FinishIntent{
                    status = {success, #p2p_adapter_Success{}}
                }},
                next_state = <<"simple_callback">>
            }}
    end;
handle_function_('HandleCallback', [_Callback, _Context], _Ctx, _Opts) ->
    {ok, #p2p_adapter_CallbackResult{
        response = #p2p_adapter_CallbackResponse{payload = <<"handle_payload">>},
        intent = {finish, #p2p_adapter_FinishIntent{
            status = {success, #p2p_adapter_Success{}}
        }}
    }}.
