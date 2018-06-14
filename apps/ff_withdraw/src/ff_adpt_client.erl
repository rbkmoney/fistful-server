%%% Client for adapter for withdrawal provider
-module(ff_adpt_client).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

%% API
-export([process_withdrawal/5]).

%%
%% Internal types
%%

-type id() :: machinery:id().
-type identity_id() :: id().

-type withdrawal() :: ff_adpt_withdrawal:withdrawal().
-type destination() :: ff_adpt_withdrawal:destination().
-type cash() :: ff_adpt_withdrawal:cash().

-type adapter() :: ff_wthadpt:adapter().
-type intent() :: {finish, status()} | {sleep, timer()}.
-type status() :: {success, trx_info()} | {failure, ff_adpt:failure()}.
-type timer() :: dmsl_base_thrift:'Timer'().
-type trx_info() :: dmsl_domain_thrift:'TransactionInfo'().
-type adapter_state() :: ff_adpt:adapter_state().
-type process_result() :: {ok, intent(), adapter_state()} | {ok, intent()}.

-type domain_withdrawal() :: dmsl_withdrawals_provider_adapter_thrift:'Withdrawal'().
-type domain_cash() :: dmsl_withdrawals_provider_adapter_thrift:'Cash'().
-type domain_currency() :: dmsl_domain_thrift:'Currency'().
-type domain_destination() :: dmsl_withdrawals_provider_adapter_thrift:'Destination'().
-type domain_identity() :: dmsl_withdrawals_provider_adapter_thrift:'Identity'().

-type backend() :: machinery:backend(_).

%%
%% API
%%

-spec process_withdrawal(Adapter, Withdrawal, ASt, AOpt, Be) -> process_result() when
    Adapter :: adapter(),
    Withdrawal :: withdrawal(),
    ASt :: adapter_state(),
    AOpt :: map(),
    Be :: backend().
process_withdrawal(Adapter, Withdrawal, ASt, AOpt, Be) ->
    DomainWithdrawal = build_and_encode_withdrawal(Withdrawal, Be),
    {ok, Result} = call(Adapter, 'ProcessWithdrawal', [DomainWithdrawal, ASt, AOpt]),
    decode_result(Result).

%%
%% Internals
%%

call(Adapter, Function, Args) ->
    Request = {{dmsl_withdrawals_provider_adapter_thrift, 'Adapter'}, Function, Args},
    ff_woody_client:call(Adapter, Request).

%% Encoders

-spec build_and_encode_withdrawal(Withdrawal, Be) -> domain_withdrawal() when
    Withdrawal :: withdrawal(),
    Be :: backend().
build_and_encode_withdrawal(Withdrawal, Be) ->
    #{
        id := WId,
        cash := Cash,
        destination := Dest,
        sender := Sender,
        receiver := Receiver
    } = Withdrawal,
    #wthadpt_Withdrawal{
        id = WId,
        body = encode_body(Cash),
        destination = encode_destination(Dest),
        sender = fetch_and_encode_identity(Sender, Be),
        receiver = fetch_and_encode_identity(Receiver, Be)
    }.

-spec encode_body(cash()) -> domain_cash().
encode_body({Amount, CurrencyId}) ->
    Currency = ff_currency:get(CurrencyId),
    DomainCurrency = encode_currency(Currency),
    #wthadpt_Cash{amount = Amount, currency = DomainCurrency}.

-spec encode_currency(ff_currency:currency()) -> domain_currency().
encode_currency(#{
    name := Name,
    symcode := Symcode,
    numcode := Numcode,
    exponent := Exponent
}) ->
    #domain_Currency{
        name = Name,
        symbolic_code = Symcode,
        numeric_code = Numcode,
        exponent = Exponent
    }.

-spec encode_destination(destination()) -> domain_destination().
encode_destination(Destination) ->
    #{resource := Resource} = Destination,
    #{
        token := Token,
        payment_system := PaymentSystem,
        bin := Bin,
        masked_pan := MaskedPan
    } = Resource,
    {bank_card, #domain_BankCard{
        token = Token,
        payment_system = PaymentSystem,
        bin = Bin,
        masked_pan = MaskedPan
    }}.

-spec fetch_and_encode_identity
    (identity_id(), backend()) -> domain_identity();
    (undefined, backend()) -> undefined.
fetch_and_encode_identity(undefined, _Be) ->
    undefined;
fetch_and_encode_identity(IdentityId, _Be) ->
    % {ok, Identity} = ff_identity:get(IdentityId, Be),
    % TODO: Add documents and contract fields
    #wthdm_Identity{
        id = IdentityId
    }.

-spec decode_result(dmsl_withdrawals_provider_adapter_thrift:'ProcessResult'()) -> process_result().
decode_result(#wthadpt_ProcessResult{intent = Intent, next_state = undefined}) ->
    {ok, decode_intent(Intent)};
decode_result(#wthadpt_ProcessResult{intent = Intent, next_state = NextState}) ->
    {ok, decode_intent(Intent), NextState}.

%% Decoders

-spec decode_intent(dmsl_withdrawals_provider_adapter_thrift:'Intent'()) -> intent().
decode_intent({finish, #wthadpt_FinishIntent{status = {success, #wthadpt_Success{trx_info = TrxInfo}}}}) ->
    {finish, {success, TrxInfo}};
decode_intent({finish, #wthadpt_FinishIntent{status = {failure, Failure}}}) ->
    {finish, {failure, Failure}};
decode_intent({sleep, #wthadpt_SleepIntent{timer = Timer}}) ->
    {sleep, Timer}.
