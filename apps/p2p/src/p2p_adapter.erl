%% P2P adapter client

-module(p2p_adapter).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").
%% Exports

-export([process/2]).
-export([handle_callback/3]).

-export_type([callback/0]).
-export_type([context/0]).

-export_type([intent/0]).
-export_type([user_interaction/0]).
-export_type([user_interaction_intent/0]).
-export_type([user_interaction_content/0]).
-export_type([callback_response_payload/0]).

-export_type([process_result/0]).
-export_type([handle_callback_result/0]).

-define(SERVICE, {dmsl_p2p_adapter_thrift, 'P2PAdapter'}).

%% Types

-type adapter()                     :: ff_adapter:adapter().

-type context()                     :: #{
    id            := id(),
    cash          := cash(),
    sender        := resource(),
    receiver      := resource(),
    deadline      => deadline(),
    adapter_state => adapter_state(),
    adapter_opts  => adapter_opts()
}.

-type id()                          :: binary().
-type cash()                        :: ff_cash:cash().
-type resource()                    :: any(). % FIXME p2p_transfer:resource_full(),
-type deadline()                    :: binary().
-type adapter_state()               :: dmsl_p2p_adapter_thrift:'AdapterState'().
-type adapter_opts()                :: ff_adapter:opts().

-type callback()                    :: #{
    tag     := callback_tag(),
    payload := callback_payload()
}.

-type callback_tag()                :: binary().
-type callback_payload()            :: binary().

-type p2p_process_result()          :: dmsl_p2p_adapter_thrift:'ProcessResult'().
-type p2p_callback_result()         :: dmsl_p2p_adapter_thrift:'CallbackResult'().

-type process_result()              :: {ok, {intent(), result_data()}}.
-type handle_callback_result()      :: {ok, {intent(), callback_response_payload(), result_data()}}.

-type callback_response_payload()   :: binary().

-type intent()                      :: {finish, finish_status()}
                                     | {sleep , sleep_status()}.

-type result_data()                 :: #{
    next_state       => adapter_state(),
    transaction_info => transaction_info()
}.

-type transaction_info()            :: ff_adapter:transaction_info().

-type finish_status()               :: success | {failure, failure()}.
-type failure()                     :: ff_failure:failure().

-type sleep_status()                :: #{
    timer            := timer(),
    callback_tag     := callback_tag(),
    user_interaction => user_interaction()
}.

-type timer()                       :: dmsl_base_thrift:'Timer'().

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

-type user_interaction_qr_code() :: #{
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

%% API

-spec process(adapter(), context()) ->
    process_result().
process(Adapter, Context) ->
    do_process(Adapter, Context).

-spec handle_callback(adapter(), callback(), context()) ->
    handle_callback_result().
handle_callback(Adapter, Callback, Context) ->
    do_handle_callback(Adapter, Callback, Context).

%% Implementation

-spec do_process(adapter(), context()) ->
    process_result().
do_process(Adapter, Context) ->
    EncodedContext = p2p_adapter_codec:encode_context(Context),
    {ok, Result}   = call(Adapter, 'Process', [EncodedContext]),
    p2p_adapter_codec:decode_process_result(Result).

-spec do_handle_callback(adapter(), callback(), context()) ->
    handle_callback_result().
do_handle_callback(Adapter, Callback, Context) ->
    EncodedCallback = p2p_adapter_codec:encode_callback(Callback),
    EncodedContext  = p2p_adapter_codec:encode_context(Context),
    {ok, Result}    = call(Adapter, 'HandleCallback', [EncodedCallback, EncodedContext]),
    p2p_adapter_codec:decode_handle_callback_result(Result).

-spec call(adapter(), 'Process',        [any()]) -> {ok, p2p_process_result()}  | no_return();
          (adapter(), 'HandleCallback', [any()]) -> {ok, p2p_callback_result()} | no_return().
call(Adapter, Function, Args) ->
    Request = {?SERVICE, Function, Args},
    ff_woody_client:call(Adapter, Request).
