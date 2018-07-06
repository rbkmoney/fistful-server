%%% Client for adapter for withdrawal provider
-module(ff_adapter_withdrawal).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

%% API

-export([process_withdrawal/4]).

%%
%% Internal types
%%

-type id()          :: machinery:id().
-type identity_id() :: id().

-type destination() :: ff_destination:destination().
-type resource()    :: ff_destination:resource().
-type identity()    :: ff_identity:identity().
-type cash()        :: ff_transaction:body().

-type withdrawal() :: #{
    id          => binary(),
    destination => destination(),
    cash        => cash(),
    sender      => identity() | undefined,
    receiver    => identity() | undefined
}.

-type adapter()               :: ff_adapter:adapter().
-type intent()                :: {finish, status()} | {sleep, timer()}.
-type status()                :: {success, trx_info()} | {failure, failure()}.
-type timer()                 :: dmsl_base_thrift:'Timer'().
-type trx_info()              :: dmsl_domain_thrift:'TransactionInfo'().
-type failure()               :: dmsl_domain_thrift:'Failure'().
-type adapter_state()         :: ff_adapter:state().
-type process_result()        :: {ok, intent(), adapter_state()} | {ok, intent()}.

-type domain_withdrawal()     :: dmsl_withdrawals_provider_adapter_thrift:'Withdrawal'().
-type domain_cash()           :: dmsl_withdrawals_provider_adapter_thrift:'Cash'().
-type domain_currency()       :: dmsl_domain_thrift:'Currency'().
-type domain_destination()    :: dmsl_withdrawals_provider_adapter_thrift:'Destination'().
-type domain_identity()       :: dmsl_withdrawals_provider_adapter_thrift:'Identity'().
-type domain_internal_state() :: dmsl_withdrawals_provider_adapter_thrift:'InternalState'().

-export_type([withdrawal/0]).
-export_type([failure/0]).

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
    {ok, Currency} = ff_currency:get(CurrencyID),
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
    Resource = ff_destination:resource(Destination),
    encode_destination_resource(Resource).

-spec encode_destination_resource(resource()) -> domain_destination().
encode_destination_resource(
    {bank_card, #{
        token          := Token,
        payment_system := PaymentSystem,
        bin            := BIN,
        masked_pan     := MaskedPan
    }}
) ->
    {bank_card, #domain_BankCard{
        token           = Token,
        payment_system  = PaymentSystem,
        bin             = BIN,
        masked_pan      = MaskedPan
    }}.

-spec encode_identity
    (identity_id()) -> domain_identity();
    (undefined) -> undefined.
encode_identity(undefined) ->
    undefined;
encode_identity(Identity) ->
    % TODO: Add real contact fields
    #wthdm_Identity{
        id        = ff_identity:id(Identity),
        documents = encode_identity_documents(Identity),
        contact   = [{phone_number, <<"9876543210">>}]
    }.

encode_identity_documents(Identity) ->
    case ff_identity:effective_challenge(Identity) of
        {ok, ChallengeID} ->
            {ok, Challenge} = ff_identity:challenge(ChallengeID, Identity),
            encode_challenge_documents(Challenge);
        {error, notfound} ->
            []
    end.

encode_challenge_documents(Challenge) ->
    lists:foldl(fun try_encode_proof_document/2, [], ff_identity_challenge:proofs(Challenge)).

try_encode_proof_document({rus_domestic_passport, Token}, Acc) ->
    [{rus_domestic_passport, #wthdm_RUSDomesticPassport{token = Token}} | Acc];
try_encode_proof_document(_, Acc) ->
    Acc.

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
