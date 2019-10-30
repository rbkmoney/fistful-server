%% P2P adapter client

-module(p2p_adapter).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").

%% Exports

-export([process/4]).
-export([handle_callback/5]).

-define(SERVICE, {dmsl_p2p_adapter_thrift, 'P2PAdapter'}).

%% Types

-type id()                          :: binary().
-type deadline()                    :: binary().

-type callback_tag()                :: binary().
-type callback_payload()            :: binary().
-type callback()                    :: #{
    tag     := callback_tag(),
    payload := callback_payload()
}.

-type adapter()                     :: ff_adapter:adapter().
-type adapter_opts()                :: ff_adapter:opts().
-type adapter_state()               :: dmsl_p2p_adapter_thrift:'AdapterState'().

-type transaction_info()            :: ff_adapter:trx_info().

-type resource()                    :: any(). % FIXME p2p_transfer:resource_full(),

-type cash()                        :: ff_cash:cash().
-type currency()                    :: ff_currency:currency().
-type failure()                     :: ff_failure:failure().

-type timer()                       :: dmsl_base_thrift:'Timer'().

-type p2p_operation_info()          :: dmsl_p2p_adapter_thrift:'OperationInfo'().
-type p2p_process_result()          :: dmsl_p2p_adapter_thrift:'ProcessResult'().
-type p2p_callback_result()         :: dmsl_p2p_adapter_thrift:'CallbackResult'().
-type p2p_callback_response()       :: dmsl_p2p_adapter_thrift:'CallbackResponse'().
-type p2p_intent()                  :: dmsl_p2p_adapter_thrift:'Intent'().
-type p2p_cash()                    :: dmsl_p2p_adapter_thrift:'Cash'().
-type p2p_session()                 :: dmsl_p2p_adapter_thrift:'Session'().
-type p2p_context()                 :: dmsl_p2p_adapter_thrift:'Context'().
-type p2p_callback()                :: dmsl_p2p_adapter_thrift:'Callback'().
-type p2p_user_interaction()        :: dmsl_p2p_adapter_thrift:'UserInteraction'().
-type p2p_user_interaction_intent() :: dmsl_p2p_adapter_thrift:'UserInteractionIntent'().
-type p2p_payment_resource()        :: dmsl_p2p_adapter_thrift:'PaymentResource'().

-type user_interaction_type()       :: dmsl_user_interaction_thrift:'UserInteraction'().

-type domain_currency()             :: dmsl_domain_thrift:'Currency'().
-type domain_transaction_info()     :: dmsl_domain_thrift:'TransactionInfo'().

-type process_result()              :: {ok, {intent(), result_data()}}.
-type handle_callback_result()      :: {ok, {intent(), callback_response_payload(), result_data()}}.

-type callback_response_payload()   :: binary().

-type intent()                      :: {finish, finish_status()}
                                     | {sleep , sleep_status()}.

-type finish_status()               :: success | {failure, failure()}.

-type sleep_status()                :: #{
    timer            := timer(),
    callback_tag     := callback_tag(),
    user_interaction => user_interaction()
}.

-type result_data()                 :: #{
    next_state       => adapter_state(),
    transaction_info => transaction_info()
}.

-type user_interaction()            :: {id(), user_interaction_intent()}.
-type user_interaction_intent()     :: finish | {create, user_interaction_content()}.
-type user_interaction_content()    :: user_interaction_redirect()
                                     | user_interaction_receipt()
                                     | user_interaction_crypto()
                                     | user_interaction_qr_code().

-type user_interaction_redirect()   :: #{
    type    := redirect,
    content := redirect()
}.

-type user_interaction_receipt()    :: #{
    type       := payment_terminal_receipt,
    payment_id := payment_id(),
    timestamp  := timestamp()
}.

-type user_interaction_crypto()     :: #{
    type           := crypto_currency_transfer_request,
    crypto_address := crypto_address(),
    crypto_cash    := crypto_cash()
}.

-type user_interaction_qr_code()    :: #{
    type    := qr_code_show_request,
    payload := qr_code_payload()
}.

-type redirect()                    :: redirect_get() | redirect_post().
-type redirect_get()                :: {get, uri()}.
-type redirect_post()               :: {post, uri(), form()}.
-type uri()                         :: binary().
-type form()                        :: #{binary() => template()}.
-type template()                    :: binary().

-type payment_id()                  :: binary().
-type timestamp()                   :: binary().

-type crypto_address()              :: binary().
-type crypto_cash()                 :: {crypto_amount(), crypto_symbolic_code()}.
-type crypto_amount()               :: genlib_rational:t().
-type crypto_symbolic_code()        :: binary().

-type qr_code_payload()             :: binary().

-type transfer_params()             :: #{
    id       := id(),
    cash     := cash(),
    sender   := resource(),
    receiver := resource(),
    deadline => deadline()
}.

%% API

-spec process(adapter(), transfer_params(), adapter_state(), adapter_opts()) ->
    process_result().
process(Adapter, TransferParams, AdapterState, AdapterOpts) ->
    do_process(Adapter, TransferParams, AdapterState, AdapterOpts).

-spec handle_callback(adapter(), callback(), transfer_params(), adapter_state(), adapter_opts()) ->
    handle_callback_result().
handle_callback(Adapter, Callback, TransferParams, AdapterState, AdapterOpts) ->
    do_handle_callback(Adapter, Callback, TransferParams, AdapterState, AdapterOpts).

%% Implementation

-spec do_process(adapter(), transfer_params(), adapter_state(), adapter_opts()) ->
    process_result().
do_process(Adapter, TransferParams, AdapterState, AdapterOpts) ->
    Context      = encode_context(AdapterState, TransferParams, AdapterOpts),
    {ok, Result} = call(Adapter, 'Process', [Context]),
    decode_process_result(Result).

-spec do_handle_callback(adapter(), callback(), transfer_params(), adapter_state(), adapter_opts()) ->
    handle_callback_result().
do_handle_callback(Adapter, Callback, TransferParams, AdapterState, AdapterOpts) ->
    EncodedCallback = encode_callback(Callback),
    Context         = encode_context(AdapterState, TransferParams, AdapterOpts),
    {ok, Result}    = call(Adapter, 'HandleCallback', [EncodedCallback, Context]),
    decode_handle_callback_result(Result).

-spec call(adapter(), 'Process',        [any()]) -> {ok, p2p_process_result()}  | no_return();
          (adapter(), 'HandleCallback', [any()]) -> {ok, p2p_callback_result()} | no_return().
call(Adapter, Function, Args) ->
    Request = {?SERVICE, Function, Args},
    ff_woody_client:call(Adapter, Request).

%% Encoders

-spec encode_callback(callback()) ->
    p2p_callback().
encode_callback(#{tag := Tag, payload := Payload}) ->
    #p2p_adapter_Callback{
        tag     = Tag,
        payload = Payload
    }.

-spec encode_context(adapter_state(), transfer_params(), adapter_opts()) ->
    p2p_context().
encode_context(AdapterState, TransferParams, AdapterOpts) ->
    #p2p_adapter_Context{
        session   = encode_session(AdapterState),
        operation = encode_operation_info(TransferParams),
        options   = AdapterOpts
    }.

-spec encode_session(adapter_state()) ->
    p2p_session().
encode_session(AdapterState) ->
    #p2p_adapter_Session{state = AdapterState}.

-spec encode_operation_info(transfer_params()) ->
    p2p_operation_info().
encode_operation_info(TransferParams) ->
    #{
        cash     := Cash,
        sender   := Sender,
        receiver := Receiver
    } = TransferParams,
    {process, #p2p_adapter_ProcessOperationInfo{
        body     = encode_body(Cash),
        sender   = encode_resource(Sender),
        receiver = encode_resource(Receiver),
        deadline = maps:get(deadline, TransferParams, undefined)
    }}.

-spec encode_body(cash()) -> p2p_cash().
encode_body({Amount, CurrencyID}) ->
    {ok, Currency} = ff_currency:get(CurrencyID),
    DomainCurrency = encode_currency(Currency),
    #p2p_adapter_Cash{amount = Amount, currency = DomainCurrency}.

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
encode_resource({raw_full, #{
    token          := Token,
    payment_system := PaymentSystem,
    bin            := BIN,
    masked_pan     := MaskedPan
}}) ->
    {disposable, #domain_DisposablePaymentResource{
        payment_tool = {bank_card, #domain_BankCard{
            token          = Token,
            payment_system = PaymentSystem,
            bin            = BIN,
            masked_pan     = MaskedPan
        }}
    }}.

%% Decoders

-spec decode_process_result(p2p_process_result()) ->
    process_result().
decode_process_result(Result = #p2p_adapter_ProcessResult{intent = Intent}) ->
    {ok, {decode_intent(Intent), decode_process_result_data(Result)}}.

-spec decode_handle_callback_result(p2p_callback_result()) ->
    handle_callback_result().
decode_handle_callback_result(Result = #p2p_adapter_CallbackResult{intent = Intent, response = Response}) ->
    {ok, {decode_intent(Intent), decode_callback_response(Response), decode_callback_result_data(Result)}}.

-spec decode_process_result_data(p2p_process_result()) ->
    result_data().
decode_process_result_data(#p2p_adapter_ProcessResult{next_state = NextState, trx = TransactionInfo}) ->
    genlib_map:compact(#{next_state => NextState, transaction_info => decode_transaction_info(TransactionInfo)}).

-spec decode_callback_result_data(p2p_callback_result()) ->
    result_data().
decode_callback_result_data(#p2p_adapter_CallbackResult{next_state = NextState, trx = TransactionInfo}) ->
    genlib_map:compact(#{next_state => NextState, transaction_info => decode_transaction_info(TransactionInfo)}).

-spec decode_intent(p2p_intent()) ->
    intent().
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
    callback_response_payload().
decode_callback_response(#p2p_adapter_CallbackResponse{payload = Payload}) ->
    Payload.

-spec decode_user_interaction(p2p_user_interaction()) -> user_interaction();
                             (undefined)              -> undefined.
decode_user_interaction(#p2p_adapter_UserInteraction{id = ID, intent = UIIntent}) ->
    {ID, decode_user_interaction_intent(UIIntent)};
decode_user_interaction(undefined) ->
    undefined.

-spec decode_user_interaction_intent(p2p_user_interaction_intent()) ->
    user_interaction_intent().
decode_user_interaction_intent({finish, #p2p_adapter_UserInteractionFinish{}}) ->
    finish;
decode_user_interaction_intent({create, #p2p_adapter_UserInteractionCreate{user_interaction = UserInteractionType}}) ->
    {create, decode_user_interaction_type(UserInteractionType)}.

-spec decode_user_interaction_type(user_interaction_type()) ->
    user_interaction_content().
decode_user_interaction_type({redirect, {get_request, #'BrowserGetRequest'{uri = URI}}}) ->
    #{type => redirect, content => {get, URI}};
decode_user_interaction_type({redirect, {post_request, #'BrowserPostRequest'{uri = URI, form = Form}}}) ->
    #{type => redirect, content => {post, URI, Form}};
% TYPO IN PROTOCOL: reciept instead of receipt
decode_user_interaction_type({payment_terminal_reciept, Receipt}) ->
    ID        = Receipt#'PaymentTerminalReceipt'.short_payment_id,
    Timestamp = Receipt#'PaymentTerminalReceipt'.due,
    #{type => payment_terminal_receipt, payment_id => ID, timestamp => Timestamp};
decode_user_interaction_type({crypto_currency_transfer_request, Crypto}) ->
    Address    = Crypto#'CryptoCurrencyTransferRequest'.crypto_address,
    Cash       = Crypto#'CryptoCurrencyTransferRequest'.crypto_cash,
    Amount     = Cash#'CryptoCash'.crypto_amount,
    SymCode    = Cash#'CryptoCash'.crypto_symbolic_code,
    Quantity   = Amount#'Rational'.q,
    Part       = Amount#'Rational'.p,
    CryptoCash = {{Quantity, Part}, SymCode},
    #{type => crypto_currency_transfer_request, crypto_address => Address, crypto_cash => CryptoCash};
decode_user_interaction_type({qr_code_show_request, Payload}) ->
    #{type => qr_code_show_request, payload => Payload}.

-spec decode_transaction_info(domain_transaction_info()) -> transaction_info();
                             (undefined)                 -> undefined.
decode_transaction_info(TransactionInfo = #domain_TransactionInfo{}) ->
    ff_dmsl_codec:unmarshal(transaction_info, TransactionInfo);
decode_transaction_info(undefined) ->
    undefined.
