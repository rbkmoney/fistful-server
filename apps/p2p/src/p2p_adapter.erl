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

-export([build_context/3]).

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
-export_type([callback_response/0]).

%% Types

-type adapter()                 :: ff_adapter:adapter().

-type context()                 :: #{
    session   := adapter_state(),
    operation := operation_info(),
    options   := #{binary() => binary()}
}.

-type operation_info()          :: #{
    body     := cash(),
    sender   := resource(),
    receiver := resource(),
    deadline => deadline()
}.

-type id()                      :: binary().
-type cash()                    :: {integer(), currency()}.
-type currency()                :: #{
    name     := binary(),
    symcode  := symcode(),
    numcode  := integer(),
    exponent := non_neg_integer()
}.

-type symcode()                 :: binary().

-type resource()                :: {bank_card, #{
    token          := binary(),
    bin            := binary(),
    payment_system := atom(),
    masked_pan     := binary()
}}.

-type deadline()                :: binary().

-type adapter_state()           :: dmsl_p2p_adapter_thrift:'AdapterState'() | undefined.
-type adapter_opts()            :: ff_adapter:opts().

-type transaction_info()        :: ff_adapter:transaction_info().

-type callback()                :: p2p_callback:process_params().

-type p2p_process_result()      :: dmsl_p2p_adapter_thrift:'ProcessResult'().
-type p2p_callback_result()     :: dmsl_p2p_adapter_thrift:'CallbackResult'().

-type process_result()          :: #{
    intent           := intent(),
    next_state       => adapter_state(),
    transaction_info => transaction_info()
}.

-type handle_callback_result()  :: #{
    intent           := intent(),
    response         := callback_response(),
    next_state       => adapter_state(),
    transaction_info => transaction_info()
}.

-type callback_response()       :: p2p_callback:response().

-type intent()                  :: {finish, finish_status()}
                                 | {sleep , sleep_status()}.

-type finish_status()           :: success | {failure, failure()}.
-type failure()                 :: ff_failure:failure().

-type sleep_status()            :: #{
    timer            := timer(),
    callback_tag     := p2p_callback:tag(),
    user_interaction => user_interaction()
}.

-type transfer_params()         :: p2p_session:transfer_params().

-type timer()                   :: dmsl_base_thrift:'Timer'().

-type user_interaction()        :: {id(), user_interaction_intent()}.
-type user_interaction_intent() :: p2p_user_interaction:intent().

%% API

-spec process(adapter(), context()) ->
    {ok, process_result()}.
process(Adapter, Context) ->
    do_process(Adapter, Context).

-spec handle_callback(adapter(), callback(), context()) ->
    {ok, handle_callback_result()}.
handle_callback(Adapter, Callback, Context) ->
    do_handle_callback(Adapter, Callback, Context).

-spec build_context(adapter_state(), transfer_params(), adapter_opts()) ->
    context().
build_context(AdapterState, TransferParams, AdapterOpts) ->
    do_build_context(AdapterState, TransferParams, AdapterOpts).

%% Implementation

-spec do_process(adapter(), context()) ->
    {ok, process_result()}.
do_process(Adapter, Context) ->
    EncodedContext = p2p_adapter_codec:marshal(context, Context),
    {ok, Result} = call(Adapter, 'Process', [EncodedContext]),
    {ok, p2p_adapter_codec:unmarshal(process_result, Result)}.

-spec do_handle_callback(adapter(), callback(), context()) ->
    {ok, handle_callback_result()}.
do_handle_callback(Adapter, Callback, Context) ->
    EncodedCallback = p2p_adapter_codec:marshal(callback, Callback),
    EncodedContext  = p2p_adapter_codec:marshal(context, Context),
    {ok, Result}    = call(Adapter, 'HandleCallback', [EncodedCallback, EncodedContext]),
    {ok, p2p_adapter_codec:unmarshal(handle_callback_result, Result)}.

-spec call(adapter(), 'Process',        [any()]) -> {ok, p2p_process_result()}  | no_return();
          (adapter(), 'HandleCallback', [any()]) -> {ok, p2p_callback_result()} | no_return().
call(Adapter, Function, Args) ->
    Request = {?SERVICE, Function, Args},
    ff_woody_client:call(Adapter, Request).

-spec do_build_context(adapter_state(), transfer_params(), adapter_opts()) ->
    context().
do_build_context(AdapterState, TransferParams, AdapterOpts) ->
    #{
        session   => AdapterState,
        operation => build_operation_info(TransferParams),
        options   => AdapterOpts
    }.

-spec build_operation_info(transfer_params()) ->
    operation_info().
build_operation_info(TransferParams) ->
    Body     = build_operation_info_body(TransferParams),
    Sender   = build_operation_info_resource(maps:get(sender, TransferParams)),
    Receiver = build_operation_info_resource(maps:get(receiver, TransferParams)),
    Deadline = maps:get(deadline, TransferParams, undefined),
    genlib_map:compact(#{
        body     => Body,
        sender   => Sender,
        receiver => Receiver,
        deadline => Deadline
    }).

-spec build_operation_info_body(transfer_params()) ->
    cash().
build_operation_info_body(TransferParams) ->
    {Amount, CurrencyID} = maps:get(body, TransferParams),
    {ok, Currency}       = ff_currency:get(CurrencyID),
    {Amount, #{
        name      => maps:get(name, Currency),
        symcode   => maps:get(symcode, Currency),
        numcode   => maps:get(numcode, Currency),
        exponent  => maps:get(exponent, Currency)
    }}.

-spec build_operation_info_resource(p2p_transfer:resource_full()) ->
    resource().
build_operation_info_resource({raw_full, Resource}) ->
    {bank_card, #{
        token          => maps:get(token, Resource),
        bin            => maps:get(bin, Resource),
        payment_system => maps:get(payment_system, Resource),
        masked_pan     => maps:get(masked_pan, Resource)
    }}.
