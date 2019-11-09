%% P2P adapter client

-module(p2p_adapter).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").

-define(SERVICE, {dmsl_p2p_adapter_thrift, 'P2PAdapter'}).

%% Exports

-export([process/2]).
-export([handle_callback/3]).

-export_type([callback/0]).
-export_type([context/0]).

-export_type([adapter_state/0]).
-export_type([adapter_opts/0]).

-export_type([operation_info/0]).
-export_type([cash/0]).
-export_type([currency/0]).
-export_type([resource/0]).

-export_type([process_result/0]).
-export_type([handle_callback_result/0]).

-export_type([intent/0]).
-export_type([finish_status/0]).
-export_type([user_interaction/0]).
-export_type([callback_response_payload/0]).

%% Types

-type adapter()                     :: ff_adapter:adapter().

-type context()                     :: #{
    session   := adapter_state(),
    operation := operation_info(),
    options   := #{binary() => binary()}
}.

-type operation_info()              :: #{
    body     := cash(),
    sender   := resource(),
    receiver := resource(),
    deadline => deadline()
}.

-type id()                          :: binary().
-type cash()                        :: {integer(), currency()}.
-type currency()                    :: #{
    name     := binary(),
    symcode  := symcode(),
    numcode  := integer(),
    exponent := non_neg_integer()
}.

-type symcode()                     :: binary().

-type resource()                    :: #{
    token          := binary(),
    bin            := binary(),
    payment_system := atom(),
    masked_pan     := binary()
}.

-type deadline()                    :: binary().

-type adapter_state()               :: dmsl_p2p_adapter_thrift:'AdapterState'() | undefined.
-type adapter_opts()                :: ff_adapter:opts().

-type transaction_info()            :: ff_adapter:trx_info().

-type callback()                    :: p2p_callback:process_params().

-type p2p_process_result()          :: dmsl_p2p_adapter_thrift:'ProcessResult'().
-type p2p_callback_result()         :: dmsl_p2p_adapter_thrift:'CallbackResult'().

-type process_result()              :: #{
    intent           := intent(),
    next_state       => adapter_state(),
    transaction_info => transaction_info()
}.

-type handle_callback_result()      :: #{
    intent           := intent(),
    response         := callback_response_payload(),
    next_state       => adapter_state(),
    transaction_info => transaction_info()
}.

-type callback_response_payload()   :: p2p_callback:response().

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

-spec process(adapter(), context()) ->
    {ok, process_result()}.
process(Adapter, Context) ->
    do_process(Adapter, Context).

-spec handle_callback(adapter(), callback(), context()) ->
    {ok, handle_callback_result()}.
handle_callback(Adapter, Callback, Context) ->
    do_handle_callback(Adapter, Callback, Context).

%% Implementation

-spec do_process(adapter(), context()) ->
    {ok, process_result()}.
do_process(Adapter, Context) ->
    EncodedContext = p2p_adapter_codec:encode_context(Context),
    {ok, Result} = call(Adapter, 'Process', [EncodedContext]),
    {ok, p2p_adapter_codec:decode_process_result(Result)}.

-spec do_handle_callback(adapter(), callback(), context()) ->
    {ok, handle_callback_result()}.
do_handle_callback(Adapter, Callback, Context) ->
    EncodedCallback = p2p_adapter_codec:encode_callback(Callback),
    EncodedContext  = p2p_adapter_codec:encode_context(Context),
    {ok, Result}    = call(Adapter, 'HandleCallback', [EncodedCallback, EncodedContext]),
    {ok, p2p_adapter_codec:decode_handle_callback_result(Result)}.

-spec call(adapter(), 'Process',        [any()]) -> {ok, p2p_process_result()}  | no_return();
          (adapter(), 'HandleCallback', [any()]) -> {ok, p2p_callback_result()} | no_return().
call(Adapter, Function, Args) ->
    Request = {?SERVICE, Function, Args},
    ff_woody_client:call(Adapter, Request).
