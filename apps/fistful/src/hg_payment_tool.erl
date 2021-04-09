%%% Payment tools

-module(hg_payment_tool).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%%
-export([test_condition/2]).

%%

-type t() :: dmsl_domain_thrift:'PaymentTool'().
-type condition() :: dmsl_domain_thrift:'PaymentToolCondition'().

%%

-spec test_condition(condition(), t()) -> boolean() | undefined.
test_condition({bank_card, C}, {bank_card, V = #domain_BankCard{}}) ->
    test_bank_card_condition(C, V);
test_condition({payment_terminal, C}, {payment_terminal, V = #domain_PaymentTerminal{}}) ->
    test_payment_terminal_condition(C, V);
test_condition({digital_wallet, C}, {digital_wallet, V = #domain_DigitalWallet{}}) ->
    test_digital_wallet_condition(C, V);
test_condition({crypto_currency, C}, {crypto_currency, V}) ->
    test_crypto_currency_condition(C, V);
test_condition(_PaymentTool, _Condition) ->
    false.

test_bank_card_condition(#domain_BankCardCondition{definition = Def}, V) when Def /= undefined ->
    test_bank_card_condition_def(Def, V);
test_bank_card_condition(#domain_BankCardCondition{}, _) ->
    true.

% legacy
test_bank_card_condition_def(
    {payment_system_is, Ps},
    #domain_BankCard{payment_system_deprecated = Ps, token_provider_deprecated = undefined}
) ->
    true;
test_bank_card_condition_def({payment_system_is, _Ps}, #domain_BankCard{}) ->
    false;
test_bank_card_condition_def({payment_system, PaymentSystem}, V) ->
    test_payment_system_condition(PaymentSystem, V);
test_bank_card_condition_def({issuer_country_is, IssuerCountry}, V) ->
    test_issuer_country_condition(IssuerCountry, V);
test_bank_card_condition_def({issuer_bank_is, BankRef}, V) ->
    test_issuer_bank_condition(BankRef, V).

test_payment_system_condition(
    #domain_PaymentSystemCondition{payment_system_is_deprecated = Ps, token_provider_is_deprecated = Tp},
    #domain_BankCard{payment_system_deprecated = Ps, token_provider_deprecated = Tp}
) ->
    true;
test_payment_system_condition(#domain_PaymentSystemCondition{}, #domain_BankCard{}) ->
    false.

test_issuer_country_condition(_Country, #domain_BankCard{issuer_country = undefined}) ->
    undefined;
test_issuer_country_condition(Country, #domain_BankCard{issuer_country = TargetCountry}) ->
    Country == TargetCountry.

test_issuer_bank_condition(BankRef, #domain_BankCard{bank_name = BankName, bin = BIN}) ->
    %% TODO this is complete bullshitery. Rework this check so we don't need to go to domain_config.
    Bank = ff_pipeline:unwrap(ff_domain_config:object({bank, BankRef})),
    #domain_Bank{binbase_id_patterns = Patterns, bins = BINs} = Bank,
    case {Patterns, BankName} of
        {P, B} when is_list(P) and is_binary(B) ->
            test_bank_card_patterns(Patterns, BankName);
        % TODO т.к. BinBase не обладает полным объемом данных, при их отсутствии мы возвращаемся к проверкам по бинам.
        %      B будущем стоит избавиться от этого.
        {_, _} ->
            test_bank_card_bins(BIN, BINs)
    end.

test_bank_card_bins(BIN, BINs) ->
    ordsets:is_element(BIN, BINs).

test_bank_card_patterns(Patterns, BankName) ->
    Matches = ordsets:filter(fun(E) -> genlib_wildcard:match(BankName, E) end, Patterns),
    ordsets:size(Matches) > 0.

test_payment_terminal_condition(#domain_PaymentTerminalCondition{definition = Def}, V) ->
    Def =:= undefined orelse test_payment_terminal_condition_def(Def, V).

test_payment_terminal_condition_def({provider_is_deprecated, V1}, #domain_PaymentTerminal{terminal_type_deprecated = V2}) ->
    V1 =:= V2.

test_digital_wallet_condition(#domain_DigitalWalletCondition{definition = Def}, V) ->
    Def =:= undefined orelse test_digital_wallet_condition_def(Def, V).

test_digital_wallet_condition_def({provider_is_deprecated, V1}, #domain_DigitalWallet{provider_deprecated = V2}) ->
    V1 =:= V2.

test_crypto_currency_condition(#domain_CryptoCurrencyCondition{definition = Def}, V) ->
    Def =:= undefined orelse test_crypto_currency_condition_def(Def, V).

test_crypto_currency_condition_def({crypto_currency_is_deprecated, C1}, C2) ->
    C1 =:= C2.
