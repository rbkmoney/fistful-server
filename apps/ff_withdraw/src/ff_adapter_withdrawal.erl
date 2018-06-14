%%% Client for adapter for withdrawal provider
-module(ff_adpt_client).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

%% API

-export([process_withdrawal/4]).

%%
%% Internal types
%%

-type id() :: machinery:id().
-type identity_id() :: id().

-type destination() :: ff_destination:destination().
-type identity() :: ff_identity:identity().
-type cash() :: ff_transfer:body().

-type withdrawal() :: #{
    id => binary(),
    destination => destination(),
    cash => cash(),
    sender => identity() | undefined,
    receiver => identity() | undefined
}.

-export_type([withdrawal/0]).

-type adapter() :: ff_adapter:adapter().
-type intent() :: {finish, status()} | {sleep, timer()}.
-type status() :: {success, trx_info()} | {failure, failure()}.
-type timer() :: dmsl_base_thrift:'Timer'().
-type trx_info() :: dmsl_domain_thrift:'TransactionInfo'().
-type failure() :: dmsl_domain_thrift:'Failure'().
-type adapter_state() :: ff_adapter:state().
-type process_result() :: {ok, intent(), adapter_state()} | {ok, intent()}.

-type domain_withdrawal() :: dmsl_withdrawals_provider_adapter_thrift:'Withdrawal'().
-type domain_cash() :: dmsl_withdrawals_provider_adapter_thrift:'Cash'().
-type domain_currency() :: dmsl_domain_thrift:'Currency'().
-type domain_destination() :: dmsl_withdrawals_provider_adapter_thrift:'Destination'().
-type domain_identity() :: dmsl_withdrawals_provider_adapter_thrift:'Identity'().
-type domain_internal_state() :: dmsl_withdrawals_provider_adapter_thrift:'InternalState'().

%%
%% API
%%

-spec process_withdrawal(Adapter, Withdrawal, ASt, AOpt) ->
    process_result() when
        Adapter :: adapter(),
        Withdrawal :: withdrawal(),
        ASt :: adapter_state(),
        AOpt :: map().

process_withdrawal(Adapter, Withdrawal, ASt, AOpt) ->
    DomainWithdrawal = encode_withdrawal(Withdrawal),
    {ok, Result} = call(Adapter, 'ProcessWithdrawal', [DomainWithdrawal, encode_adapter_state(ASt), AOpt]),
    decode_result(Result).

%%
%% Internals
%%

call(Adapter, Function, Args) ->
    Request = {{dmsl_withdrawals_provider_adapter_thrift, 'Adapter'}, Function, Args},
    ff_woody_client:call(Adapter, Request).

%% Encoders

-spec encode_withdrawal(Withdrawal) -> domain_withdrawal() when
    Withdrawal :: withdrawal().
encode_withdrawal(Withdrawal) ->
    #{
        id := ID,
        cash := Cash,
        destination := Dest,
        sender := Sender,
        receiver := Receiver
    } = Withdrawal,
    #wthadpt_Withdrawal{
        id = ID,
        body = encode_body(Cash),
        destination = encode_destination(Dest),
        sender = encode_identity(Sender),
        receiver = encode_identity(Receiver)
    }.

-spec encode_body(cash()) -> domain_cash().
encode_body({Amount, CurrencyID}) ->
    Currency = ff_currency:get(CurrencyID),
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

-spec encode_identity
    (identity_id()) -> domain_identity();
    (undefined) -> undefined.
encode_identity(undefined) ->
    undefined;
encode_identity(IdentityID) ->
    % TODO: Add documents and contract fields
    #wthdm_Identity{
        id = IdentityID
    }.

-spec encode_adapter_state(adapter_state()) -> domain_internal_state().
encode_adapter_state(undefined) ->
    {nl, #msgpack_Nil{}};
encode_adapter_state(ASt) ->
    ASt.

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
