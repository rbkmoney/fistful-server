%% P2P adapter codec

-module(p2p_adapter_codec).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").

%% Exports

-export([marshal/2]).
-export([unmarshal/2]).

-type process_result()              :: p2p_adapter:process_result().
-type p2p_process_result()          :: dmsl_p2p_adapter_thrift:'ProcessResult'().

-type handle_callback_result()      :: p2p_adapter:handle_callback_result().
-type p2p_callback_result()         :: dmsl_p2p_adapter_thrift:'CallbackResult'().

-type process_callback_result()     :: p2p_adapter_host:process_callback_result().
-type p2p_process_callback_result() :: dmsl_p2p_adapter_thrift:'ProcessCallbackResult'().

-type callback()                    :: p2p_adapter:callback().
-type p2p_callback()                :: dmsl_p2p_adapter_thrift:'Callback'().

-type context()                     :: p2p_adapter:context().
-type p2p_context()                 :: dmsl_p2p_adapter_thrift:'Context'().

-type operation_info()              :: p2p_adapter:operation_info().
-type p2p_operation_info()          :: dmsl_p2p_adapter_thrift:'OperationInfo'().

-type adapter_state()               :: p2p_adapter:adapter_state().
-type p2p_session()                 :: dmsl_p2p_adapter_thrift:'Session'().

-type resource()                    :: p2p_adapter:resource().
-type p2p_payment_resource()        :: dmsl_p2p_adapter_thrift:'PaymentResource'().

-type cash()                        :: p2p_adapter:cash().
-type p2p_cash()                    :: dmsl_p2p_adapter_thrift:'Cash'().

-type currency()                    :: p2p_adapter:currency().
-type domain_currency()             :: dmsl_domain_thrift:'Currency'().

-type intent()                      :: p2p_adapter:intent().
-type p2p_intent()                  :: dmsl_p2p_adapter_thrift:'Intent'().

-type user_interaction()            :: p2p_adapter:user_interaction().
-type p2p_user_interaction()        :: dmsl_p2p_adapter_thrift:'UserInteraction'().

-type user_interaction_intent()     :: p2p_user_interaction:intent().
-type p2p_user_interaction_intent() :: dmsl_p2p_adapter_thrift:'UserInteractionIntent'().

-type user_interaction_content()    :: p2p_user_interaction:content().
-type domain_user_interaction()     :: dmsl_user_interaction_thrift:'UserInteraction'().

-type callback_response()           :: p2p_callback:response().
-type p2p_callback_response()       :: dmsl_p2p_adapter_thrift:'CallbackResponse'().

-type transaction_info()            :: ff_adapter:trx_info().
-type domain_transaction_info()     :: dmsl_domain_thrift:'TransactionInfo'().

%% API

-spec marshal(process_callback_result, process_callback_result()) -> p2p_process_callback_result();
             (callback_response,       callback_response())       -> p2p_callback_response();
             (callback,                callback())                -> p2p_callback();
             (context,                 context())                 -> p2p_context();
             (session,                 adapter_state())           -> p2p_session();
             (operation_info,          operation_info())          -> p2p_operation_info();
             (body,                    cash())                    -> p2p_cash();
             (currency,                currency())                -> domain_currency();
             (resource,                resource())                -> p2p_payment_resource().
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
        body     = marshal(body,     Cash),
        sender   = marshal(resource, Sender),
        receiver = marshal(resource, Receiver),
        deadline = maps:get(deadline, OperationInfo, undefined)
    }};
marshal(body, {Amount, Currency}) ->
    #p2p_adapter_Cash{amount = Amount, currency = marshal(currency, Currency)};
marshal(currency, #{
    name     := Name,
    symcode  := Symcode,
    numcode  := Numcode,
    exponent := Exponent
}) ->
    #domain_Currency{
        name          = Name,
        symbolic_code = Symcode,
        numeric_code  = Numcode,
        exponent      = Exponent
    };
marshal(resource, {bank_card, #{
    token           := Token,
    payment_system  := PaymentSystem,
    bin             := Bin,
    masked_pan      := MaskedPan
}}) ->
    {disposable, #domain_DisposablePaymentResource{
        payment_tool = {bank_card, #domain_BankCard{
            token          = Token,
            payment_system = PaymentSystem,
            bin            = Bin,
            masked_pan     = MaskedPan
        }}
    }}.

-spec unmarshal(process_result,           p2p_process_result())          -> process_result();
               (handle_callback_result,   p2p_callback_result())         -> handle_callback_result();
               (intent,                   p2p_intent())                  -> intent();
               (callback_response,        p2p_callback_response())       -> callback_response();
               (user_interaction_intent,  p2p_user_interaction_intent()) -> user_interaction_intent();
               (user_interaction_content, domain_user_interaction())     -> user_interaction_content();
               (callback,                 p2p_callback())                -> callback();
               (context,                  p2p_context())                 -> context();
               (session,                  p2p_session())                 -> adapter_state();
               (operation_info,           p2p_operation_info())          -> operation_info();
               (body,                     p2p_cash())                    -> cash();
               (currency,                 domain_currency())             -> currency();
               (resource,                 p2p_payment_resource())        -> resource().
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
unmarshal(user_interaction_intent, {finish, #p2p_adapter_UserInteractionFinish{}}) ->
    finish;
unmarshal(user_interaction_intent, {create, #p2p_adapter_UserInteractionCreate{
    user_interaction = UserInteractionType
}}) ->
    {create, unmarshal(user_interaction_content, UserInteractionType)};
unmarshal(user_interaction_content, {redirect, {get_request,
    #'BrowserGetRequest'{uri = URI}
}}) ->
    #{type => redirect, content => {get, URI}};
unmarshal(user_interaction_content, {redirect, {post_request,
    #'BrowserPostRequest'{uri = URI, form = Form}
}}) ->
    #{type => redirect, content => {post, URI, Form}};
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
        body     => unmarshal(body,     Body),
        sender   => unmarshal(resource, Sender),
        receiver => unmarshal(resource, Receiver),
        deadline => Deadline
    });
unmarshal(body, #p2p_adapter_Cash{amount = Amount, currency = Currency}) ->
    {Amount, unmarshal(currency, Currency)};
unmarshal(currency, #domain_Currency{
    name          = Name,
    symbolic_code = Symcode,
    numeric_code  = Numcode,
    exponent      = Exponent
}) ->
    #{
        name     => Name,
        symcode  => Symcode,
        numcode  => Numcode,
        exponent => Exponent
    };
unmarshal(resource, {disposable, #domain_DisposablePaymentResource{
    payment_tool = {bank_card, #domain_BankCard{
        token          = Token,
        payment_system = PaymentSystem,
        bin            = Bin,
        masked_pan     = MaskedPan
    }}
}}) ->
    {bank_card, #{
        token           => Token,
        payment_system  => PaymentSystem,
        bin             => Bin,
        masked_pan      => MaskedPan
    }}.

-spec maybe_unmarshal(atom(),           undefined)                 -> undefined;
                     (transaction_info, domain_transaction_info()) -> transaction_info();
                     (user_interaction, p2p_user_interaction())    -> user_interaction().
maybe_unmarshal(transaction_info, TransactionInfo = #domain_TransactionInfo{}) ->
    ff_dmsl_codec:unmarshal(transaction_info, TransactionInfo);
maybe_unmarshal(transaction_info, undefined) ->
    undefined;
maybe_unmarshal(user_interaction, #p2p_adapter_UserInteraction{id = ID, intent = UIIntent}) ->
    {ID, unmarshal(user_interaction_intent, UIIntent)};
maybe_unmarshal(user_interaction, undefined) ->
    undefined.
