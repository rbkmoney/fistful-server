%% P2P adapter codec

-module(p2p_adapter_codec).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").

%% Exports

-export([encode_callback/1]).
-export([encode_context/1]).

-export([decode_callback/1]).
-export([decode_process_result/1]).
-export([decode_handle_callback_result/1]).

-type callback()                    :: p2p_adapter:callback().
-type p2p_callback()                :: dmsl_p2p_adapter_thrift:'Callback'().

-type context()                     :: p2p_adapter:context().
-type operation_info()              :: p2p_adapter:operation_info().
-type adapter_state()               :: p2p_adapter:adapter_state().
-type p2p_context()                 :: dmsl_p2p_adapter_thrift:'Context'().

-type p2p_session()                 :: dmsl_p2p_adapter_thrift:'Session'().
-type p2p_operation_info()          :: dmsl_p2p_adapter_thrift:'OperationInfo'().

-type resource()                    :: p2p_adapter:resource().

-type p2p_payment_resource()        :: dmsl_p2p_adapter_thrift:'PaymentResource'().
-type p2p_cash()                    :: dmsl_p2p_adapter_thrift:'Cash'().

-type cash()                        :: p2p_adapter:cash().

-type domain_currency()             :: dmsl_domain_thrift:'Currency'().
-type currency()                    :: ff_currency:currency().

-type p2p_process_result()          :: dmsl_p2p_adapter_thrift:'ProcessResult'().
-type p2p_callback_result()         :: dmsl_p2p_adapter_thrift:'CallbackResult'().

-type p2p_intent()                  :: dmsl_p2p_adapter_thrift:'Intent'().

-type p2p_user_interaction()        :: dmsl_p2p_adapter_thrift:'UserInteraction'().
-type p2p_user_interaction_intent() :: dmsl_p2p_adapter_thrift:'UserInteractionIntent'().
-type p2p_callback_response()       :: dmsl_p2p_adapter_thrift:'CallbackResponse'().

-type domain_transaction_info()     :: dmsl_domain_thrift:'TransactionInfo'().
-type transaction_info()            :: ff_adapter:trx_info().

-type user_interaction_type()       :: dmsl_user_interaction_thrift:'UserInteraction'().

%% API

% Encoders

-spec encode_callback(callback()) ->
    p2p_callback().
encode_callback(#{tag := Tag, payload := Payload}) ->
    #p2p_adapter_Callback{
        tag     = Tag,
        payload = Payload
    }.

-spec encode_context(context()) ->
    p2p_context().
encode_context(Context) ->
    #{
        session   := AdapterState,
        operation := OperationInfo,
        options   := AdapterOpts

    } = Context,
    #p2p_adapter_Context{
        session   = encode_session(AdapterState),
        operation = encode_operation_info(OperationInfo),
        options   = AdapterOpts
    }.

% Decoders

-spec decode_process_result(p2p_process_result()) ->
    p2p_adapter:process_result().
decode_process_result(Result = #p2p_adapter_ProcessResult{intent = Intent}) ->
    {decode_intent(Intent), decode_process_result_data(Result)}.

-spec decode_handle_callback_result(p2p_callback_result()) ->
    p2p_adapter:handle_callback_result().
decode_handle_callback_result(Result = #p2p_adapter_CallbackResult{intent = Intent, response = Response}) ->
    {decode_intent(Intent), decode_callback_response(Response), decode_callback_result_data(Result)}.

-spec decode_callback(p2p_callback()) ->
    callback().
decode_callback(#p2p_adapter_Callback{tag = Tag, payload = Payload}) ->
    #{tag => Tag, payload => Payload}.

%% Internal

% Encoders

-spec encode_session(adapter_state()) ->
    p2p_session().
encode_session(AdapterState) ->
    #p2p_adapter_Session{state = AdapterState}.

-spec encode_operation_info(operation_info()) ->
    p2p_operation_info().
encode_operation_info(OperationInfo) ->
    #{
        body     := Cash,
        sender   := Sender,
        receiver := Receiver
    } = OperationInfo,
    {process, #p2p_adapter_ProcessOperationInfo{
        body     = encode_cash(Cash),
        sender   = encode_resource(Sender),
        receiver = encode_resource(Receiver),
        deadline = maps:get(deadline, OperationInfo, undefined)
    }}.

-spec encode_cash(cash()) ->
    p2p_cash().
encode_cash({Amount, Currency}) ->
    #p2p_adapter_Cash{amount = Amount, currency = encode_currency(Currency)}.

-spec encode_currency(currency()) ->
    domain_currency().
encode_currency(#{
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
    }.

-spec encode_resource(resource()) ->
    p2p_payment_resource().
encode_resource(Resource) ->
    {disposable, #domain_DisposablePaymentResource{
        payment_tool = {bank_card, #domain_BankCard{
            token          = maps:get(token, Resource),
            payment_system = maps:get(payment_system, Resource),
            bin            = maps:get(bin, Resource),
            masked_pan     = maps:get(masked_pan, Resource)
        }}
    }}.

% Decoders

-spec decode_process_result_data(p2p_process_result()) ->
    p2p_adapter:result_data().
decode_process_result_data(#p2p_adapter_ProcessResult{next_state = NextState, trx = TransactionInfo}) ->
    genlib_map:compact(#{next_state => NextState, transaction_info => decode_transaction_info(TransactionInfo)}).

-spec decode_callback_result_data(p2p_callback_result()) ->
    p2p_adapter:result_data().
decode_callback_result_data(#p2p_adapter_CallbackResult{next_state = NextState, trx = TransactionInfo}) ->
    genlib_map:compact(#{next_state => NextState, transaction_info => decode_transaction_info(TransactionInfo)}).

-spec decode_intent(p2p_intent()) ->
    p2p_adapter:intent().
decode_intent({finish, #p2p_adapter_FinishIntent{status = {success, #p2p_adapter_Success{}}}}) ->
    {finish, success};
decode_intent({finish, #p2p_adapter_FinishIntent{status = {failure, Failure}}}) ->
    {finish, {failure, ff_dmsl_codec:unmarshal(failure, Failure)}};
decode_intent({sleep,  #p2p_adapter_SleepIntent{
    timer            = Timer,
    user_interaction = UserInteraction,
    callback_tag     = CallbackTag
}}) ->
    {sleep, genlib_map:compact(#{
        timer            => Timer,
        callback_tag     => CallbackTag,
        user_interaction => decode_user_interaction(UserInteraction)
    })}.

-spec decode_callback_response(p2p_callback_response()) ->
    p2p_adapter:callback_response_payload().
decode_callback_response(#p2p_adapter_CallbackResponse{payload = Payload}) ->
    #{payload => Payload}.

-spec decode_user_interaction(p2p_user_interaction()) -> p2p_adapter:user_interaction();
                             (undefined)              -> undefined.
decode_user_interaction(#p2p_adapter_UserInteraction{id = ID, intent = UIIntent}) ->
    {ID, decode_user_interaction_intent(UIIntent)};
decode_user_interaction(undefined) ->
    undefined.

-spec decode_user_interaction_intent(p2p_user_interaction_intent()) ->
    p2p_user_interaction:intent().
decode_user_interaction_intent({finish, #p2p_adapter_UserInteractionFinish{}}) ->
    finish;
decode_user_interaction_intent({create, #p2p_adapter_UserInteractionCreate{
    user_interaction = UserInteractionType
}}) ->
    {create, decode_user_interaction_type(UserInteractionType)}.

-spec decode_user_interaction_type(user_interaction_type()) ->
    p2p_user_interaction:content().
decode_user_interaction_type({redirect, {get_request,
    #'BrowserGetRequest'{uri = URI}
}}) ->
    #{type => redirect, content => {get, URI}};
decode_user_interaction_type({redirect, {post_request,
    #'BrowserPostRequest'{uri = URI, form = Form}
}}) ->
    #{type => redirect, content => {post, URI, Form}}.

-spec decode_transaction_info(domain_transaction_info()) -> transaction_info();
                             (undefined)                 -> undefined.
decode_transaction_info(TransactionInfo = #domain_TransactionInfo{}) ->
    ff_dmsl_codec:unmarshal(transaction_info, TransactionInfo);
decode_transaction_info(undefined) ->
    undefined.
