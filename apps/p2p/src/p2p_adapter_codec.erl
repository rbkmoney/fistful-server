%% P2P adapter codec

-module(p2p_adapter_codec).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").

%% Exports

-export([marshal/2]).
-export([unmarshal/2]).

-type type_name() :: atom() | {list, atom()}.
-type codec()     :: module().

-type encoded_value()  :: encoded_value(any()).
-type encoded_value(T) :: T.

-type decoded_value()  :: decoded_value(any()).
-type decoded_value(T) :: T.

-export_type([codec/0]).
-export_type([type_name/0]).
-export_type([encoded_value/0]).
-export_type([encoded_value/1]).
-export_type([decoded_value/0]).
-export_type([decoded_value/1]).

-type process_result()              :: p2p_adapter:process_result().
-type p2p_process_result()          :: dmsl_p2p_adapter_thrift:'ProcessResult'().

-type handle_callback_result()      :: p2p_adapter:handle_callback_result().
-type p2p_callback_result()         :: dmsl_p2p_adapter_thrift:'CallbackResult'().

-type process_callback_result()     :: p2p_session_machine:process_callback_result().
-type p2p_process_callback_result() :: dmsl_p2p_adapter_thrift:'ProcessCallbackResult'().

-type callback()                    :: p2p_adapter:callback().
-type p2p_callback()                :: dmsl_p2p_adapter_thrift:'Callback'().

-type context()                     :: p2p_adapter:context().
-type p2p_context()                 :: dmsl_p2p_adapter_thrift:'Context'().

-type operation_info()              :: p2p_adapter:operation_info().
-type p2p_operation_info()          :: dmsl_p2p_adapter_thrift:'OperationInfo'().

-type adapter_state()               :: p2p_adapter:adapter_state().
-type p2p_session()                 :: dmsl_p2p_adapter_thrift:'Session'().

-type cash()                        :: p2p_adapter:cash().
-type p2p_cash()                    :: dmsl_p2p_adapter_thrift:'Cash'().

-type intent()                      :: p2p_adapter:intent().
-type p2p_intent()                  :: dmsl_p2p_adapter_thrift:'Intent'().

-type user_interaction()            :: p2p_adapter:user_interaction().
-type p2p_user_interaction()        :: dmsl_p2p_adapter_thrift:'UserInteraction'().

-type user_interaction_intent()     :: p2p_user_interaction:intent().
-type p2p_user_interaction_intent() :: dmsl_p2p_adapter_thrift:'UserInteractionIntent'().

-type callback_response()           :: p2p_callback:response().
-type p2p_callback_response()       :: dmsl_p2p_adapter_thrift:'CallbackResponse'().

-type resource()                    :: ff_resource:resource().
-type disposable_resource()         :: dmsl_p2p_adapter_thrift:'PaymentResource'().

-type deadline()                    :: p2p_adapter:deadline().

-type fees()                        :: p2p_adapter:fees().
-type p2p_fees()                    :: dmsl_p2p_adapter_thrift:'Fees'().

%% API

-spec marshal(process_callback_result, process_callback_result()) -> p2p_process_callback_result();
             (callback_response,       callback_response())       -> p2p_callback_response();
             (callback,                callback())                -> p2p_callback();
             (context,                 context())                 -> p2p_context();
             (session,                 adapter_state())           -> p2p_session();
             (operation_info,          operation_info())          -> p2p_operation_info();
             (resource,                resource())                -> disposable_resource();
             (cash,                    cash())                    -> p2p_cash();
             (deadline,                deadline())                -> binary();
             (p2p_fees,                fees())                    -> p2p_fees().
marshal(process_callback_result, {succeeded, Response}) ->
    {succeeded, #p2p_adapter_ProcessCallbackSucceeded{
        response = marshal(callback_response, Response)
    }};
marshal(process_callback_result, {finished, Context}) ->
    {finished, #p2p_adapter_ProcessCallbackFinished{
        response = marshal(context, Context)
    }};

marshal(callback_response, #{payload := Payload}) ->
    #p2p_adapter_CallbackResponse{payload = Payload};

marshal(callback, #{tag := Tag, payload := Payload}) ->
    #p2p_adapter_Callback{
        tag     = Tag,
        payload = Payload
    };

marshal(context, #{
        session   := AdapterState,
        operation := OperationInfo,
        options   := AdapterOpts
}) ->
    #p2p_adapter_Context{
        session   = marshal(session, AdapterState),
        operation = marshal(operation_info, OperationInfo),
        options   = AdapterOpts
    };

marshal(session, AdapterState) ->
    #p2p_adapter_Session{state = AdapterState};

marshal(operation_info, OperationInfo = #{
        body     := Cash,
        sender   := Sender,
        receiver := Receiver
}) ->
    {process, #p2p_adapter_ProcessOperationInfo{
        body          = marshal(cash,     Cash),
        sender        = marshal(resource, Sender),
        receiver      = marshal(resource, Receiver),
        deadline      = maybe_marshal(deadline, maps:get(deadline, OperationInfo, undefined)),
        merchant_fees = maybe_marshal(p2p_fees, maps:get(merchant_fees, OperationInfo, undefined)),
        provider_fees = maybe_marshal(p2p_fees, maps:get(provider_fees, OperationInfo, undefined))
    }};

marshal(resource, Resource) ->
    {disposable, #domain_DisposablePaymentResource{
        payment_tool = ff_dmsl_codec:marshal(resource, Resource)
    }};

marshal(cash, {Amount, Currency}) ->
    #p2p_adapter_Cash{
        amount   = Amount,
        currency = ff_dmsl_codec:marshal(currency, Currency)
    };

marshal(deadline, Deadline) ->
    ff_time:to_rfc3339(Deadline);

marshal(p2p_fees, #{fees := Fees}) ->
    #p2p_adapter_Fees{
        fees = maps:map(
            fun(_CashFlowConstant, Cash) ->
                marshal(cash, Cash)
            end,
            Fees
        )
    }.

maybe_marshal(_T, undefined) ->
    undefined;
maybe_marshal(T, V) ->
    marshal(T, V).

-spec unmarshal(process_result,           p2p_process_result())          -> process_result();
               (handle_callback_result,   p2p_callback_result())         -> handle_callback_result();
               (intent,                   p2p_intent())                  -> intent();
               (callback_response,        p2p_callback_response())       -> callback_response();
               (user_interaction,         p2p_user_interaction())        -> user_interaction();
               (user_interaction_intent,  p2p_user_interaction_intent()) -> user_interaction_intent();
               (callback,                 p2p_callback())                -> callback();
               (context,                  p2p_context())                 -> context();
               (session,                  p2p_session())                 -> adapter_state();
               (operation_info,           p2p_operation_info())          -> operation_info();
               (cash,                     p2p_cash())                    -> cash();
               (deadline,                 binary())                      -> deadline().
unmarshal(process_result, #p2p_adapter_ProcessResult{
    intent     = Intent,
    next_state = NextState,
    trx        = TransactionInfo
}) ->
    genlib_map:compact(#{
        intent           => unmarshal(intent, Intent),
        next_state       => NextState,
        transaction_info => maybe_unmarshal(transaction_info, TransactionInfo)
    });

unmarshal(handle_callback_result, #p2p_adapter_CallbackResult{
    intent     = Intent,
    next_state = NextState,
    trx        = TransactionInfo,
    response   = Response
}) ->
    genlib_map:compact(#{
        intent           => unmarshal(intent, Intent),
        response         => unmarshal(callback_response, Response),
        next_state       => NextState,
        transaction_info => maybe_unmarshal(transaction_info, TransactionInfo)
    });

unmarshal(intent, {finish, #p2p_adapter_FinishIntent{status = {success, #p2p_adapter_Success{}}}}) ->
    {finish, success};
unmarshal(intent, {finish, #p2p_adapter_FinishIntent{status = {failure, Failure}}}) ->
    {finish, {failure, ff_dmsl_codec:unmarshal(failure, Failure)}};
unmarshal(intent, {sleep,  #p2p_adapter_SleepIntent{
    timer            = Timer,
    user_interaction = UserInteraction,
    callback_tag     = CallbackTag
}}) ->
    {sleep, genlib_map:compact(#{
        timer            => Timer,
        callback_tag     => CallbackTag,
        user_interaction => maybe_unmarshal(user_interaction, UserInteraction)
    })};


unmarshal(callback_response, #p2p_adapter_CallbackResponse{payload = Payload}) ->
    #{payload => Payload};

unmarshal(user_interaction, #p2p_adapter_UserInteraction{id = ID, intent = UIIntent}) ->
    {ID, unmarshal(user_interaction_intent, UIIntent)};

unmarshal(user_interaction_intent, {finish, #p2p_adapter_UserInteractionFinish{}}) ->
    finish;
unmarshal(user_interaction_intent, {create, #p2p_adapter_UserInteractionCreate{
    user_interaction = UserInteraction
}}) ->
    {create, ff_dmsl_codec:unmarshal(user_interaction, UserInteraction)};

unmarshal(callback, #p2p_adapter_Callback{
    tag     = Tag,
    payload = Payload
}) ->
    #{tag => Tag, payload => Payload};

unmarshal(context, #p2p_adapter_Context{
    session   = Session,
    operation = OperationInfo,
    options   = AdapterOpts
}) ->
    genlib_map:compact(#{
        session   => unmarshal(session, Session),
        operation => unmarshal(operation_info, OperationInfo),
        options   => AdapterOpts
    });

unmarshal(session, #p2p_adapter_Session{state = AdapterState}) ->
    AdapterState;

unmarshal(operation_info, {process, #p2p_adapter_ProcessOperationInfo{
    body     = Body,
    sender   = Sender,
    receiver = Receiver,
    deadline = Deadline
}}) ->
    genlib_map:compact(#{
        body     => unmarshal(cash, Body),
        sender   => ff_dmsl_codec:unmarshal(resource, Sender),
        receiver => ff_dmsl_codec:unmarshal(resource, Receiver),
        deadline => maybe_unmarshal(deadline, Deadline)
    });

unmarshal(cash, #p2p_adapter_Cash{amount = Amount, currency = Currency}) ->
    {Amount, ff_dmsl_codec:unmarshal(currency, Currency)};

unmarshal(deadline, Deadline) ->
    ff_time:from_rfc3339(Deadline).

% Internal

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(transaction_info, TransactionInfo) ->
    ff_dmsl_codec:unmarshal(transaction_info, TransactionInfo);
maybe_unmarshal(user_interaction, UserInteraction) ->
    unmarshal(user_interaction, UserInteraction);
maybe_unmarshal(deadline, Deadline) ->
    unmarshal(deadline, Deadline).
