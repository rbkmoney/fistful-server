%% P2P adapter client

-module(p2p_adapter).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").

-define(SERVICE, {dmsl_p2p_adapter_thrift, 'P2PAdapter'}).

%% Exports

-export([process/4]).
-export([handle_callback/5]).

-export_type([callback/0]).

-export_type([intent/0]).
-export_type([user_interaction/0]).
-export_type([callback_response_payload/0]).

-export_type([process_result/0]).
-export_type([handle_callback_result/0]).

-export_type([result_data/0]).

-export_type([transfer_params/0]).
-export_type([adapter_state/0]).
-export_type([adapter_opts/0]).
-export_type([finish_status/0]).

%% Types

-type adapter()                     :: ff_adapter:adapter().

-type transfer_params()             :: #{
    id       := id(),
    cash     := cash(),
    sender   := resource(),
    receiver := resource(),
    deadline => deadline()
}.

-type id()                          :: binary().
-type cash()                        :: ff_cash:cash().
-type resource()                    :: p2p_transfer:resource_full().
-type deadline()                    :: binary().

-type adapter_state()               :: dmsl_p2p_adapter_thrift:'AdapterState'() | undefined.
-type adapter_opts()                :: ff_adapter:opts().

-type transaction_info()            :: ff_adapter:trx_info().

-type callback()                    :: p2p_callback:process_params().

-type p2p_process_result()          :: dmsl_p2p_adapter_thrift:'ProcessResult'().
-type p2p_callback_result()         :: dmsl_p2p_adapter_thrift:'CallbackResult'().

-type process_result()              :: {intent(), result_data()}.
-type handle_callback_result()      :: {intent(), callback_response_payload(), result_data()}.

-type callback_response_payload()   :: p2p_callback:response().

-type result_data()                 :: #{
    next_state       => adapter_state(),
    transaction_info => transaction_info()
}.

-type intent()                      :: {finish, finish_status()}
                                     | {sleep , sleep_status()}.

-type finish_status()               :: success | {failure, failure()}.
-type failure()                     :: ff_failure:failure().

-type sleep_status()                :: #{
    timer            := timer(),
    callback_tag     := p2p_callback:tag(),
    user_interaction => user_interaction()
}.

-type timer()                       :: dmsl_base_thrift:'Timer'().

-type user_interaction()            :: {id(), user_interaction_intent()}.
-type user_interaction_intent()     :: p2p_user_interaction:intent().

%% API

-spec process(adapter(), transfer_params(), adapter_state(), adapter_opts()) ->
    {ok, process_result()}.
process(Adapter, TransferParams, AdapterState, AdapterOpts) ->
    do_process(Adapter, TransferParams, AdapterState, AdapterOpts).

-spec handle_callback(adapter(), callback(), transfer_params(), adapter_state(), adapter_opts()) ->
    {ok, handle_callback_result()}.
handle_callback(Adapter, Callback, TransferParams, AdapterState, AdapterOpts) ->
    do_handle_callback(Adapter, Callback, TransferParams, AdapterState, AdapterOpts).

%% Implementation

-spec do_process(adapter(), transfer_params(), adapter_state(), adapter_opts()) ->
    {ok, process_result()}.
do_process(Adapter, TransferParams, AdapterState, AdapterOpts) ->
    Context = p2p_adapter_codec:encode_context(TransferParams, AdapterState, AdapterOpts),
    {ok, Result} = call(Adapter, 'Process', [Context]),
    p2p_adapter_codec:decode_process_result(Result).

-spec do_handle_callback(adapter(), callback(), transfer_params(), adapter_state(), adapter_opts()) ->
    {ok, handle_callback_result()}.
do_handle_callback(Adapter, Callback, TransferParams, AdapterState, AdapterOpts) ->
    EncodedCallback = p2p_adapter_codec:encode_callback(Callback),
    Context         = p2p_adapter_codec:encode_context(TransferParams, AdapterState, AdapterOpts),
    {ok, Result}    = call(Adapter, 'HandleCallback', [EncodedCallback, Context]),
    p2p_adapter_codec:decode_handle_callback_result(Result).

-spec call(adapter(), 'Process',        [any()]) -> {ok, p2p_process_result()}  | no_return();
          (adapter(), 'HandleCallback', [any()]) -> {ok, p2p_callback_result()} | no_return().
call(Adapter, Function, Args) ->
    Request = {?SERVICE, Function, Args},
    ff_woody_client:call(Adapter, Request).
