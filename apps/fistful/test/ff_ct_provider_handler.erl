-module(ff_ct_provider_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("dmsl/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function('ProcessWithdrawal', [Withdrawal, InternalState, Options], _Context, _Opts) ->
    DWithdrawal = decode_withdrawal(Withdrawal),
    DState = decode_state(InternalState),
    DOptions = decode_options(Options),
    {ok, Intent, NewState} = ff_ct_provider:process_withdrawal(DWithdrawal, DState, DOptions),
    {ok, encode_intent(Intent), encode_state(NewState)}.

%%
%% Internals
%%

decode_withdrawal(#wthadpt_Withdrawal{
    id = Id,
    body = Body,
    destination = Destination,
    sender = Sender,
    receiver = Receiver
}) ->
    #{
        id => Id,
        body => Body,
        destination => Destination,
        sender => Sender,
        receiver => Receiver
    }.

decode_options(Options) ->
    Options.

decode_state({string, EncodedState}) ->
    erlang:binary_to_term(EncodedState, [safe]).

encode_state(State) ->
    {string, erlang:term_to_binary(State)}.

encode_intent({finish, {success, TrxInfo}}) ->
    {finish, #wthadpt_FinishIntent{status = {success, #wthadpt_Success{trx_info = encode_trx(TrxInfo)}}}};
encode_intent({finish, {failure, Failure}}) ->
    {finish, #wthadpt_FinishIntent{status = {failure, encode_failure(Failure)}}};
encode_intent({sleep, Timer}) ->
    {sleep, #wthadpt_SleepIntent{timer = encode_timer(Timer)}}.

encode_trx(#{id := Id} = TrxInfo) ->
    Timestamp = maps:get(timestamp, TrxInfo, undefined),
    Extra = maps:get(extra, TrxInfo, #{}),
    #domain_TransactionInfo{id = Id, timestamp = Timestamp, extra = Extra}.

encode_failure(Failure) ->
    Failure.

encode_timer(Timer) ->
    Timer.
