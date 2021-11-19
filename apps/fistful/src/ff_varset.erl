-module(ff_varset).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export_type([varset/0]).
-export_type([encoded_varset/0]).

-export([encode/1]).
-export([encode_contract_terms_varset/1]).

-type varset() :: #{
    category => dmsl_domain_thrift:'CategoryRef'(),
    currency => dmsl_domain_thrift:'CurrencyRef'(),
    cost => dmsl_domain_thrift:'Cash'(),
    payment_tool => dmsl_domain_thrift:'PaymentTool'(),
    party_id => dmsl_domain_thrift:'PartyID'(),
    shop_id => dmsl_domain_thrift:'ShopID'(),
    risk_score => dmsl_domain_thrift:'RiskScore'(),
    flow => instant | {hold, dmsl_domain_thrift:'HoldLifetime'()},
    payout_method => dmsl_domain_thrift:'PayoutMethodRef'(),
    wallet_id => dmsl_domain_thrift:'WalletID'(),
    identification_level => dmsl_domain_thrift:'ContractorIdentificationLevel'(),
    bin_data => dmsl_domain_thrift:'BinData'()
}.

-type encoded_varset() :: dmsl_payment_processing_thrift:'Varset'().

-spec encode(varset()) -> encoded_varset().
encode(Varset) ->
    PaymentTool = genlib_map:get(payment_tool, Varset),
    #payproc_Varset{
        currency = genlib_map:get(currency, Varset),
        amount = genlib_map:get(cost, Varset),
        wallet_id = genlib_map:get(wallet_id, Varset),
        payment_tool = PaymentTool,
        payment_method = encode_payment_method(PaymentTool),
        identification_level = genlib_map:get(identification_level, Varset),
        party_id = genlib_map:get(party_id, Varset),
        bin_data = genlib_map:get(bin_data, Varset)
    }.

-spec encode_contract_terms_varset(varset()) -> dmsl_payment_processing_thrift:'ComputeContractTermsVarset'().
encode_contract_terms_varset(Varset) ->
    #payproc_ComputeContractTermsVarset{
        currency = genlib_map:get(currency, Varset),
        amount = genlib_map:get(cost, Varset),
        shop_id = genlib_map:get(shop_id, Varset),
        payout_method = genlib_map:get(payout_method, Varset),
        payment_tool = genlib_map:get(payment_tool, Varset),
        wallet_id = genlib_map:get(wallet_id, Varset),
        bin_data = genlib_map:get(bin_data, Varset)
    }.

-spec encode_payment_method(ff_destination:resource_params() | undefined) ->
    dmsl_domain_thrift:'PaymentMethodRef'() | undefined.
encode_payment_method(undefined) ->
    undefined;
encode_payment_method({bank_card, #domain_BankCard{payment_system_deprecated = PaymentSystem}}) ->
    #domain_PaymentMethodRef{
        id = {bank_card_deprecated, PaymentSystem}
    };
encode_payment_method({crypto_currency_deprecated, CryptoCurrency}) ->
    #domain_PaymentMethodRef{
        id = {crypto_currency_deprecated, CryptoCurrency}
    };
encode_payment_method({digital_wallet, #domain_DigitalWallet{provider_deprecated = DigitalWalletType}}) ->
    #domain_PaymentMethodRef{
        id = {digital_wallet_deprecated, DigitalWalletType}
    }.
