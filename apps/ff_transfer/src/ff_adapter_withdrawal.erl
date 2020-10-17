%%% Client for adapter for withdrawal provider
-module(ff_adapter_withdrawal).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

%% API

-export([process_withdrawal/4]).
-export([handle_callback/5]).
-export([get_quote/3]).

%%
%% Internal types
%%

-type resource()    :: ff_destination:resource_full().

-type identity()    :: #{
    id := binary(),
    effective_challenge => challenge()
}.

-type challenge()    :: #{
    id => binary(),
    proofs => [proof()]
}.

-type proof() ::
    {proof_type(), identdoc_token()}.

-type proof_type() ::
    rus_domestic_passport |
    rus_retiree_insurance_cert.

-type identdoc_token() ::
    binary().

-type cash()        :: ff_transaction:body().

-type withdrawal() :: #{
    id          => binary(),
    session_id  => binary(),
    resource    => resource(),
    cash        => cash(),
    sender      => identity() | undefined,
    receiver    => identity() | undefined,
    quote       => quote()
}.

-type quote_params() :: #{
    external_id => binary(),
    currency_from := ff_currency:id(),
    currency_to := ff_currency:id(),
    body := cash()
}.

-type quote() :: quote(quote_data()).

-type quote(T) :: #{
    cash_from   := cash(),
    cash_to     := cash(),
    created_at  := binary(),
    expires_on  := binary(),
    quote_data  := T
}.

-type quote_data()     :: %% as stolen from `machinery_msgpack`
    nil                |
    boolean()          |
    integer()          |
    float()            |
    binary()           | %% string
    {binary, binary()} | %% binary
    [quote_data()]     |
    #{quote_data() => quote_data()}.

-type adapter()               :: ff_adapter:adapter().
-type intent()                :: {finish, finish_status()} | {sleep, sleep_intent()}.
-type sleep_intent()          :: #{
    timer := timer(),
    tag => ff_withdrawal_callback:tag()
}.
-type finish_status()         :: success | {success, transaction_info()} | {failure, failure()}.
-type timer()                 :: dmsl_base_thrift:'Timer'().
-type transaction_info()      :: ff_adapter:transaction_info().
-type failure()               :: ff_adapter:failure().

-type adapter_state()         :: ff_adapter:state().
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

-type callback()          :: ff_withdrawal_callback:process_params().
-type callback_response() :: ff_withdrawal_callback:response().

-export_type([withdrawal/0]).
-export_type([failure/0]).
-export_type([transaction_info/0]).
-export_type([finish_status/0]).
-export_type([quote/0]).
-export_type([quote/1]).
-export_type([quote_params/0]).
-export_type([quote_data/0]).
-export_type([identity/0]).

%%
%% API
%%

-spec process_withdrawal(Adapter, Withdrawal, ASt, AOpt) ->
    {ok, process_result()} when
        Adapter :: adapter(),
        Withdrawal :: withdrawal(),
        ASt :: adapter_state(),
        AOpt :: map().

process_withdrawal(Adapter, Withdrawal, ASt, AOpt) ->
    DomainWithdrawal = marshal(withdrawal, Withdrawal),
    {ok, Result} = call(Adapter, 'ProcessWithdrawal', [DomainWithdrawal, marshal(adapter_state, ASt), AOpt]),
    % rebind trx field
    RebindedResult = rebind_transaction_info(Result),
    decode_result(RebindedResult).

-spec handle_callback(Adapter, Callback, Withdrawal, ASt, AOpt) ->
    {ok, handle_callback_result()} when
        Adapter :: adapter(),
        Callback :: callback(),
        Withdrawal :: withdrawal(),
        ASt :: adapter_state(),
        AOpt :: map().

handle_callback(Adapter, Callback, Withdrawal, ASt, AOpt) ->
    DWithdrawal = marshal(withdrawal, Withdrawal),
    DCallback= marshal(callback, Callback),
    DASt = marshal(adapter_state, ASt),
    {ok, Result} = call(Adapter, 'HandleCallback', [DCallback, DWithdrawal, DASt, AOpt]),
    % rebind trx field
    RebindedResult = rebind_transaction_info(Result),
    decode_result(RebindedResult).

-spec get_quote(adapter(), quote_params(), map()) ->
    {ok, quote()}.

get_quote(Adapter, Params, AOpt) ->
    QuoteParams = marshal(quote_params, Params),
    {ok, Result} = call(Adapter, 'GetQuote', [QuoteParams, AOpt]),
    decode_result(Result).

%%
%% Internals
%%

call(Adapter, Function, Args) ->
    Request = {{dmsl_withdrawals_provider_adapter_thrift, 'Adapter'}, Function, Args},
    ff_woody_client:call(Adapter, Request).

-spec decode_result
    (dmsl_withdrawals_provider_adapter_thrift:'ProcessResult'()) -> {ok, process_result()};
    (dmsl_withdrawals_provider_adapter_thrift:'Quote'()) -> {ok, quote()};
    (dmsl_withdrawals_provider_adapter_thrift:'CallbackResult'()) -> {ok, handle_callback_result()}.

decode_result(#wthadpt_ProcessResult{} = ProcessResult) ->
    {ok, unmarshal(process_result, ProcessResult)};
decode_result(#wthadpt_Quote{} = Quote) ->
    {ok, unmarshal(quote, Quote)};
decode_result(#wthadpt_CallbackResult{} = CallbackResult) ->
    {ok, unmarshal(callback_result, CallbackResult)}.

%% @doc
%% The field Intent.FinishIntent.FinishStatus.Success.trx_info is ignored further in the code (#FF-207).
%% If TransactionInfo is set on this field, then rebind its value to the (ProcessResult|CallbackResult).trx field.
%%
%% @see ff_withdrawal_session:process_intent/2
%% @see ff_withdrawal_session:apply_event/2
%% @see ff_withdrawal_session_codec:marshal/2
%% @see ff_withdrawal_session_codec:unmarshal/2
%% @see ff_withdrawal_codec:marshal/2
%% @see ff_withdrawal_codec:unmarshal/2
%%
%% @todo Remove this code when adapter stops set TransactionInfo to field Success.trx_info

rebind_transaction_info(#wthadpt_ProcessResult{intent = Intent} = Result) ->
    {NewIntent, TransactionInfo} = extract_transaction_info(Intent, Result#wthadpt_ProcessResult.trx),
    Result#wthadpt_ProcessResult{intent = NewIntent, trx = TransactionInfo};
rebind_transaction_info(#wthadpt_CallbackResult{intent = Intent} = Result) ->
    {NewIntent, TransactionInfo} = extract_transaction_info(Intent, Result#wthadpt_CallbackResult.trx),
    Result#wthadpt_CallbackResult{intent = NewIntent, trx = TransactionInfo}.

extract_transaction_info({finish, #wthadpt_FinishIntent{status = {success, Success}}}, TransactionInfo) ->
    {
        {finish, #wthadpt_FinishIntent{status = {success, #wthadpt_Success{trx_info = undefined}}}},
        case Success of
            #wthadpt_Success{trx_info = undefined} -> TransactionInfo;
            #wthadpt_Success{trx_info = LegacyTransactionInfo} -> LegacyTransactionInfo
        end
    };
extract_transaction_info(Intent, TransactionInfo) ->
    {Intent, TransactionInfo}.

%%

marshal(Type, Value) ->
    ff_adapter_withdrawal_codec:marshal(Type, Value).

unmarshal(Type, Value) ->
    ff_adapter_withdrawal_codec:unmarshal(Type, Value).
