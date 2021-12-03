%%%
%%% Domain object helpers
%%%

-module(ct_domain).

-export([currency/1]).
-export([category/3]).
-export([payment_method/1]).
-export([payment_system/2]).
-export([contract_template/2]).
-export([inspector/3]).
-export([inspector/4]).
-export([proxy/2]).
-export([proxy/3]).
-export([proxy/4]).
-export([system_account_set/4]).
-export([external_account_set/4]).
-export([term_set_hierarchy/1]).
-export([term_set_hierarchy/2]).
-export([term_set_hierarchy/3]).
-export([timed_term_set/1]).
-export([globals/3]).
-export([withdrawal_provider/4]).
-export([withdrawal_terminal/1]).

%%

-include_lib("ff_cth/include/ct_domain.hrl").
-include_lib("shumpune_proto/include/shumpune_shumpune_thrift.hrl").

-define(dtp(Type), dmsl_domain_thrift:Type()).

-type object() ::
    dmsl_domain_thrift:'DomainObject'().

-spec withdrawal_provider(?dtp('ProviderRef'), ?dtp('ProxyRef'), binary(), ct_helper:config()) -> object().
withdrawal_provider(?prv(16) = Ref, ProxyRef, IdentityID, C) ->
    AccountID = account(<<"RUB">>, C),
    {provider, #domain_ProviderObject{
        ref = Ref,
        data = #domain_Provider{
            name = <<"WithdrawalProvider">>,
            description = <<"Withdrawal provider">>,
            proxy = #domain_Proxy{ref = ProxyRef, additional = #{}},
            identity = IdentityID,
            terms = undefined,
            accounts = #{
                ?cur(<<"RUB">>) => #domain_ProviderAccount{settlement = AccountID}
            },
            terminal =
                {decisions, [
                    #domain_TerminalDecision{
                        if_ = {constant, true},
                        then_ = {value, [?prv_trm(6)]}
                    }
                ]}
        }
    }};
withdrawal_provider(Ref, ProxyRef, IdentityID, C) ->
    AccountID = account(<<"RUB">>, C),
    {provider, #domain_ProviderObject{
        ref = Ref,
        data = #domain_Provider{
            name = <<"WithdrawalProvider">>,
            description = <<"Withdrawal provider">>,
            proxy = #domain_Proxy{ref = ProxyRef, additional = #{}},
            identity = IdentityID,
            terms = #domain_ProvisionTermSet{
                wallet = #domain_WalletProvisionTerms{
                    withdrawals = #domain_WithdrawalProvisionTerms{
                        currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                        payout_methods = {value, ?ordset([])},
                        cash_limit =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(0, <<"RUB">>)},
                                    {exclusive, ?cash(10000000, <<"RUB">>)}
                                )},
                        cash_flow =
                            {decisions, [
                                #domain_CashFlowDecision{
                                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                                    then_ =
                                        {value, [
                                            ?cfpost(
                                                {system, settlement},
                                                {provider, settlement},
                                                {product,
                                                    {min_of,
                                                        ?ordset([
                                                            ?fixed(10, <<"RUB">>),
                                                            ?share(5, 100, operation_amount, round_half_towards_zero)
                                                        ])}}
                                            )
                                        ]}
                                }
                            ]}
                    }
                }
            },
            accounts = #{
                ?cur(<<"RUB">>) => #domain_ProviderAccount{settlement = AccountID}
            },
            terminal =
                case Ref of
                    ?prv(9) ->
                        {decisions, [
                            #domain_TerminalDecision{
                                if_ = {constant, true},
                                then_ = {value, [?prv_trm(1, 500)]}
                            }
                        ]};
                    ?prv(10) ->
                        {decisions, [
                            #domain_TerminalDecision{
                                if_ = {constant, true},
                                then_ = {value, [?prv_trm(1)]}
                            }
                        ]};
                    ?prv(11) ->
                        {decisions, [
                            #domain_TerminalDecision{
                                if_ = {constant, true},
                                then_ = {value, [?prv_trm(1)]}
                            }
                        ]};
                    ?prv(17) ->
                        {decisions, [
                            #domain_TerminalDecision{
                                if_ =
                                    {condition,
                                        {cost_in,
                                            ?cashrng(
                                                {inclusive, ?cash(300, <<"RUB">>)},
                                                {inclusive, ?cash(300, <<"RUB">>)}
                                            )}},
                                then_ = {value, [?prv_trm(1)]}
                            },
                            #domain_TerminalDecision{
                                if_ =
                                    {condition,
                                        {cost_in,
                                            ?cashrng(
                                                {inclusive, ?cash(301, <<"RUB">>)},
                                                {inclusive, ?cash(301, <<"RUB">>)}
                                            )}},
                                then_ = {value, [?prv_trm(8)]}
                            }
                        ]};
                    _ ->
                        {decisions, [
                            #domain_TerminalDecision{
                                if_ =
                                    {condition,
                                        {cost_in,
                                            ?cashrng(
                                                {inclusive, ?cash(0, <<"RUB">>)},
                                                {exclusive, ?cash(1000000, <<"RUB">>)}
                                            )}},
                                then_ = {value, [?prv_trm(1)]}
                            },
                            #domain_TerminalDecision{
                                if_ =
                                    {condition,
                                        {cost_in,
                                            ?cashrng(
                                                {inclusive, ?cash(3000000, <<"RUB">>)},
                                                {exclusive, ?cash(10000000, <<"RUB">>)}
                                            )}},
                                then_ = {value, [?prv_trm(7)]}
                            }
                        ]}
                end
        }
    }}.

-spec withdrawal_terminal(?dtp('TerminalRef')) -> object().
withdrawal_terminal(?trm(N) = Ref) when N > 0, N < 6 ->
    {terminal, #domain_TerminalObject{
        ref = Ref,
        data = #domain_Terminal{
            name = <<"WithdrawalTerminal">>,
            description = <<"Withdrawal terminal">>,
            provider_ref = ?prv(1)
        }
    }};
withdrawal_terminal(?trm(6) = Ref) ->
    {terminal, #domain_TerminalObject{
        ref = Ref,
        data = #domain_Terminal{
            name = <<"WithdrawalTerminal">>,
            description = <<"Withdrawal terminal">>
        }
    }};
withdrawal_terminal(?trm(7) = Ref) ->
    {terminal, #domain_TerminalObject{
        ref = Ref,
        data = #domain_Terminal{
            name = <<"Terminal7">>,
            description = <<"Withdrawal terminal">>,
            terms = #domain_ProvisionTermSet{
                wallet = #domain_WalletProvisionTerms{
                    withdrawals = #domain_WithdrawalProvisionTerms{
                        currencies = {value, ?ordset([?cur(<<"BTC">>)])},
                        payout_methods = {value, ?ordset([])},
                        cash_limit =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(1000000, <<"BTC">>)},
                                    {exclusive, ?cash(10000000, <<"BTC">>)}
                                )},
                        cash_flow = {value, ?ordset([])}
                    }
                }
            }
        }
    }};
withdrawal_terminal(?trm(8) = Ref) ->
    {terminal, #domain_TerminalObject{
        ref = Ref,
        data = #domain_Terminal{
            name = <<"Terminal8">>,
            description = <<"Override provider cashflow">>,
            terms = #domain_ProvisionTermSet{
                wallet = #domain_WalletProvisionTerms{
                    withdrawals = #domain_WithdrawalProvisionTerms{
                        cash_flow =
                            {decisions, [
                                #domain_CashFlowDecision{
                                    if_ = {constant, true},
                                    then_ =
                                        {value, [
                                            ?cfpost(
                                                {system, settlement},
                                                {provider, settlement},
                                                ?fixed(16, <<"RUB">>)
                                            )
                                        ]}
                                }
                            ]}
                    }
                }
            }
        }
    }}.

-spec currency(?dtp('CurrencyRef')) -> object().
currency(?cur(<<"EUR">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name = <<"Europe"/utf8>>,
            numeric_code = 978,
            symbolic_code = SymCode,
            exponent = 2
        }
    }};
currency(?cur(<<"RUB">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name = <<"Яussian Яuble"/utf8>>,
            numeric_code = 643,
            symbolic_code = SymCode,
            exponent = 2
        }
    }};
currency(?cur(<<"USD">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name = <<"U$ Dollar">>,
            numeric_code = 840,
            symbolic_code = SymCode,
            exponent = 2
        }
    }};
currency(?cur(<<"BTC">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name = <<"Bitcoin">>,
            numeric_code = 999,
            symbolic_code = SymCode,
            exponent = 10
        }
    }}.

-spec category(?dtp('CategoryRef'), binary(), ?dtp('CategoryType')) -> object().
category(Ref, Name, Type) ->
    {category, #domain_CategoryObject{
        ref = Ref,
        data = #domain_Category{
            name = Name,
            description = <<>>,
            type = Type
        }
    }}.

-spec payment_method(?dtp('PaymentMethodRef')) -> object().
payment_method(?pmt(_Type, Name) = Ref) when is_atom(Name) ->
    payment_method(Name, Ref);
payment_method(?pmt(_Type, #domain_BankCardPaymentMethod{} = PM) = Ref) ->
    payment_method(PM#domain_BankCardPaymentMethod.payment_system, Ref).

payment_method(Name, Ref) ->
    {payment_method, #domain_PaymentMethodObject{
        ref = Ref,
        data = #domain_PaymentMethodDefinition{
            name = erlang:atom_to_binary(Name, unicode),
            description = <<>>
        }
    }}.

-spec payment_system(?dtp('PaymentSystemRef'), binary()) -> object().
payment_system(Ref, Name) ->
    {payment_system, #domain_PaymentSystemObject{
        ref = Ref,
        data = #domain_PaymentSystem{
            name = Name
        }
    }}.

-spec contract_template(?dtp('ContractTemplateRef'), ?dtp('TermSetHierarchyRef')) -> object().
contract_template(Ref, TermsRef) ->
    contract_template(Ref, TermsRef, undefined, undefined).

contract_template(Ref, TermsRef, ValidSince, ValidUntil) ->
    {contract_template, #domain_ContractTemplateObject{
        ref = Ref,
        data = #domain_ContractTemplate{
            valid_since = ValidSince,
            valid_until = ValidUntil,
            terms = TermsRef
        }
    }}.

-spec inspector(?dtp('InspectorRef'), binary(), ?dtp('ProxyRef')) -> object().
inspector(Ref, Name, ProxyRef) ->
    inspector(Ref, Name, ProxyRef, #{}).

-spec inspector(?dtp('InspectorRef'), binary(), ?dtp('ProxyRef'), ?dtp('ProxyOptions')) -> object().
inspector(Ref, Name, ProxyRef, Additional) ->
    {inspector, #domain_InspectorObject{
        ref = Ref,
        data = #domain_Inspector{
            name = Name,
            description = <<>>,
            proxy = #domain_Proxy{
                ref = ProxyRef,
                additional = Additional
            }
        }
    }}.

-spec proxy(?dtp('ProxyRef'), Name :: binary()) -> object().
proxy(Ref, Name) ->
    proxy(Ref, Name, <<>>).

-spec proxy(?dtp('ProxyRef'), Name :: binary(), URL :: binary()) -> object().
proxy(Ref, Name, URL) ->
    proxy(Ref, Name, URL, #{}).

-spec proxy(?dtp('ProxyRef'), Name :: binary(), URL :: binary(), ?dtp('ProxyOptions')) -> object().
proxy(Ref, Name, URL, Opts) ->
    {proxy, #domain_ProxyObject{
        ref = Ref,
        data = #domain_ProxyDefinition{
            name = Name,
            description = <<>>,
            url = URL,
            options = Opts
        }
    }}.

-spec system_account_set(?dtp('SystemAccountSetRef'), binary(), ?dtp('CurrencyRef'), ct_helper:config()) -> object().
system_account_set(Ref, Name, ?cur(SymCode), C) ->
    AccountID1 = account(SymCode, C),
    AccountID2 = account(SymCode, C),
    {system_account_set, #domain_SystemAccountSetObject{
        ref = Ref,
        data = #domain_SystemAccountSet{
            name = Name,
            description = <<>>,
            accounts = #{
                ?cur(SymCode) => #domain_SystemAccount{
                    settlement = AccountID1,
                    subagent = AccountID2
                }
            }
        }
    }}.

-spec external_account_set(?dtp('ExternalAccountSetRef'), binary(), ?dtp('CurrencyRef'), ct_helper:config()) ->
    object().
external_account_set(Ref, Name, ?cur(SymCode), C) ->
    AccountID1 = account(SymCode, C),
    AccountID2 = account(SymCode, C),
    {external_account_set, #domain_ExternalAccountSetObject{
        ref = Ref,
        data = #domain_ExternalAccountSet{
            name = Name,
            description = <<>>,
            accounts = #{
                ?cur(SymCode) => #domain_ExternalAccount{
                    income = AccountID1,
                    outcome = AccountID2
                }
            }
        }
    }}.

-spec term_set_hierarchy(?dtp('TermSetHierarchyRef')) -> object().
term_set_hierarchy(Ref) ->
    term_set_hierarchy(Ref, []).

-spec term_set_hierarchy(?dtp('TermSetHierarchyRef'), [?dtp('TimedTermSet')]) -> object().
term_set_hierarchy(Ref, TermSets) ->
    term_set_hierarchy(Ref, undefined, TermSets).

-spec term_set_hierarchy(Ref, ff_maybe:maybe(Ref), [?dtp('TimedTermSet')]) -> object() when
    Ref :: ?dtp('TermSetHierarchyRef').
term_set_hierarchy(Ref, ParentRef, TermSets) ->
    {term_set_hierarchy, #domain_TermSetHierarchyObject{
        ref = Ref,
        data = #domain_TermSetHierarchy{
            parent_terms = ParentRef,
            term_sets = TermSets
        }
    }}.

-spec timed_term_set(?dtp('TermSet')) -> ?dtp('TimedTermSet').
timed_term_set(TermSet) ->
    #domain_TimedTermSet{
        action_time = #'TimestampInterval'{},
        terms = TermSet
    }.

-spec globals(?dtp('ExternalAccountSetRef'), [?dtp('PaymentInstitutionRef')], [?dtp('IdentityProviderRef')]) ->
    object().
globals(EASRef, PIRefs, IPRefs) ->
    {globals, #domain_GlobalsObject{
        ref = ?glob(),
        data = #domain_Globals{
            external_account_set = {value, EASRef},
            payment_institutions = ?ordset(PIRefs),
            identity_providers = ?ordset(IPRefs)
        }
    }}.

-spec account(binary(), ct_helper:config()) -> shumpune_shumpune_thrift:'AccountID'().
account(SymCode, C) ->
    Client = ff_woody_client:new(maps:get('accounter', ct_helper:cfg(services, C))),
    WoodyCtx = ct_helper:get_woody_ctx(C),
    Prototype = #shumpune_AccountPrototype{
        currency_sym_code = SymCode,
        description = <<>>,
        creation_time = timestamp()
    },
    Request = {{shumpune_shumpune_thrift, 'Accounter'}, 'CreateAccount', {Prototype}},
    case woody_client:call(Request, Client, WoodyCtx) of
        {ok, ID} ->
            ID
    end.

timestamp() ->
    genlib_rfc3339:format(genlib_time:daytime_to_unixtime(calendar:universal_time()), second).
