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

-define(ADAPTER_PROCESS_RESULT(Intent, NextState), #p2p_adapter_ProcessResult{
    intent = Intent,
    next_state = NextState
}).

-define(ADAPTER_SLEEP_INTENT(Timeout, CallbackTag, UI), {sleep, #p2p_adapter_SleepIntent{
    timer = {timeout, Timeout},
    callback_tag = CallbackTag,
    user_interaction = UI
}}).

-define(ADAPTER_FINISH_INTENT(Result), {finish, #p2p_adapter_FinishIntent{
    status = Result
}}).

-define(ADAPTER_UI(ID, Intent), #p2p_adapter_UserInteraction{
    id = ID,
    intent = Intent
}).

-define(ADAPTER_UI_CREATED, {create, #p2p_adapter_UserInteractionCreate{
    user_interaction = {redirect,
        {get_request,
            #'BrowserGetRequest'{uri = <<"uri">>}
        }
    }
}}).

-define(ADAPTER_UI_FINISH, {finish, #p2p_adapter_UserInteractionFinish{}}).

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

handle_function_('Process', [?ADAPTER_CONTEXT(101, _Token, State)], _Ctx, _Opts) ->
    case State of
        undefined ->
            {ok, ?ADAPTER_PROCESS_RESULT(
                ?ADAPTER_SLEEP_INTENT(2, undefined, ?ADAPTER_UI(
                    <<"test_user_interaction">>,
                    ?ADAPTER_UI_CREATED
                )),
                <<"user_sleep">>
            )};
        <<"user_sleep">> ->
            {ok, ?ADAPTER_PROCESS_RESULT(
                ?ADAPTER_SLEEP_INTENT(1, undefined, ?ADAPTER_UI(
                    <<"test_user_interaction">>,
                    ?ADAPTER_UI_FINISH
                )),
                <<"user_ui_finished">>
            )};
        <<"user_ui_finished">> ->
            {ok, ?ADAPTER_PROCESS_RESULT(
                ?ADAPTER_FINISH_INTENT({success, #p2p_adapter_Success{}}),
                <<"user_sleep_finished">>
            )}
    end;
handle_function_('Process', [?ADAPTER_CONTEXT(99, Token, State)], _Ctx, _Opts) ->
    case State of
        undefined ->
            {ok, ?ADAPTER_PROCESS_RESULT(
                ?ADAPTER_SLEEP_INTENT(1, Token, undefined),
                <<"wrong">>
            )};
        <<"wrong">> ->
            {ok, ?ADAPTER_PROCESS_RESULT(
                ?ADAPTER_FINISH_INTENT({success, #p2p_adapter_Success{}}),
                <<"wrong_finished">>
            )}
    end;
handle_function_('Process', [?ADAPTER_CONTEXT(999, Token, State)], _Ctx, _Opts) ->
    case State of
        undefined ->
            {ok, ?ADAPTER_PROCESS_RESULT(
                ?ADAPTER_SLEEP_INTENT(1, Token, undefined),
                <<"simple_sleep">>
            )};
        <<"simple_sleep">> ->
            {ok, ?ADAPTER_PROCESS_RESULT(
                ?ADAPTER_SLEEP_INTENT(1, undefined, undefined),
                undefined
            )};
        <<"simple_callback">> ->
            {ok, ?ADAPTER_PROCESS_RESULT(
                ?ADAPTER_FINISH_INTENT({success, #p2p_adapter_Success{}}),
                <<"sleep_finished">>
            )}
    end;
handle_function_('Process', [?ADAPTER_CONTEXT(1001)], _Ctx, _Opts) ->
    {ok, ?ADAPTER_PROCESS_RESULT(
        ?ADAPTER_FINISH_INTENT({failure, #domain_Failure{code = <<"test_failure">>}}),
        undefined
    )};
handle_function_('Process', [_Context], _Ctx, _Opts) ->
    {ok, ?ADAPTER_PROCESS_RESULT(
        ?ADAPTER_FINISH_INTENT({success, #p2p_adapter_Success{}}),
       undefined
    )};

handle_function_('HandleCallback', [?ADAPTER_CALLBACK(Token), ?ADAPTER_CONTEXT(_, Token, State)], _Ctx, _Opts) ->
    case State of
        <<"simple_sleep">> ->
            {ok, #p2p_adapter_CallbackResult{
                response = #p2p_adapter_CallbackResponse{payload = <<"simple_payload">>},
                intent = {sleep, #p2p_adapter_SleepIntent{
                    timer = {timeout, 1}
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
