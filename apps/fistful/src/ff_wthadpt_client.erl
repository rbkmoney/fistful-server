%%% Client for adapter for withdrawal provider
-module(ff_wthadpt_client).

-include_lib("dmsl/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

%% API
-export([process_withdrawal/4]).

%%
%% Internal types
%%

-type withdrawal() :: ff_wthadpt:withdrawal().
-type adapter() :: ff_wthadpt:adapter().
-type intent() :: {finish, status()} | {sleep, timer()}.
-type status() :: {success, trx_info()} | {failure, ff_wthadpt:failure()}.
-type timer() :: dmsl_base_thrift:'Timer'().
-type trx_info() :: dmsl_domain_thrift:'TransactionInfo'().
-type adapter_state() :: ff_wthadpt:adapter_state().
-type process_result() :: {ok, intent(), adapter_state()} | {ok, intent()}.

%%
%% API
%%

-spec process_withdrawal(adapter(), withdrawal(), adapter_state(), map()) -> process_result().
process_withdrawal(Adapter, Withdrawal, State, Options) ->
    {ok, Result} = call(Adapter, 'ProcessWithdrawal', [encode_withdrawal(Withdrawal), State, Options]),
    decode_result(Result).

%%
%% Internals
%%

call(Adapter, Function, Args) ->
    Request = {{dmsl_withdrawals_provider_adapter_thrift, 'Adapter'}, Function, Args},
    ff_woody_client:call(Adapter, Request).

-spec encode_withdrawal(withdrawal()) -> dmsl_withdrawals_provider_adapter_thrift:'Withdrawal'().
encode_withdrawal(#{
    id := Id,
    body := Body,
    destination := Destination,
    sender := Sender,
    receiver := Receiver
}) ->
    #wthadpt_Withdrawal{
        id = Id,
        body = Body,
        destination = Destination,
        sender = Sender,
        receiver = Receiver
    }.

-spec decode_result(dmsl_withdrawals_provider_adapter_thrift:'ProcessResult'()) -> process_result().
decode_result(#wthadpt_ProcessResult{intent = Intent, next_state = undefined}) ->
    {ok, decode_intent(Intent)};
decode_result(#wthadpt_ProcessResult{intent = Intent, next_state = NextState}) ->
    {ok, decode_intent(Intent), NextState}.

-spec decode_intent(dmsl_withdrawals_provider_adapter_thrift:'Intent'()) -> intent().
decode_intent({finish, #wthadpt_FinishIntent{status = {success, #wthadpt_Success{trx_info = TrxInfo}}}}) ->
    {finish, {success, TrxInfo}};
decode_intent({finish, #wthadpt_FinishIntent{status = {failure, Failure}}}) ->
    {finish, {failure, Failure}};
decode_intent({sleep, #wthadpt_SleepIntent{timer = Timer}}) ->
    {sleep, Timer}.
