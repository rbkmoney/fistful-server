%%%
%%% Domain object helpers
%%%

-module(ct_domain).

-export([currency/1]).
-export([category/3]).
-export([payment_method/1]).
-export([contract_template/2]).
-export([inspector/3]).
-export([inspector/4]).
-export([p2p_inspector/4]).
-export([proxy/2]).
-export([proxy/3]).
-export([proxy/4]).
-export([system_account_set/4]).
-export([external_account_set/4]).
-export([term_set_hierarchy/1]).
-export([term_set_hierarchy/2]).
-export([term_set_hierarchy/3]).
-export([timed_term_set/1]).
-export([globals/2]).
-export([withdrawal_provider/4]).
-export([withdrawal_terminal/1]).
-export([p2p_provider/4]).

%%

-include_lib("ff_cth/include/ct_domain.hrl").
-include_lib("shumpune_proto/include/shumpune_shumpune_thrift.hrl").

-define(dtp(Type), dmsl_domain_thrift:Type()).

-type object() ::
    dmsl_domain_thrift:'DomainObject'().

-spec p2p_provider(?dtp('P2PProviderRef'), ?dtp('ProxyRef'), binary(), ct_helper:config()) ->
    object().

p2p_provider(Ref, ProxyRef, IdentityID, C) ->
    AccountID = account(<<"RUB">>, C),
    {p2p_provider, #domain_P2PProviderObject{
        ref = Ref,
        data = #domain_P2PProvider{
            name = <<"P2PProvider">>,
            proxy = #domain_Proxy{ref = ProxyRef, additional = #{}},
            identity = IdentityID,
            p2p_terms = #domain_P2PProvisionTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                cash_limit = {value, ?cashrng(
                    {inclusive, ?cash(       0, <<"RUB">>)},
                    {exclusive, ?cash(10000000, <<"RUB">>)}
                )},
                cash_flow = {decisions, [
                    #domain_CashFlowDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, [
                            ?cfpost(
                                {system, settlement},
                                {provider, settlement},
                                {product, {min_of, ?ordset([
                                    ?fixed(10, <<"RUB">>),
                                    ?share(5, 100, operation_amount, round_half_towards_zero)
                                ])}}
                            )
                        ]}
                    }
                ]},
                fees = {decisions, [
                    #domain_FeeDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, #domain_Fees{
                            fees = #{surplus => ?share(1, 1, operation_amount)}
                        }}
                    },
                    #domain_FeeDecision{
                        if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                        then_ = {value, #domain_Fees{
                            fees = #{surplus => ?share(1, 1, operation_amount)}
                        }}
                    }#domain_FeeDecision{
                        if_ = {condition, {currency_is, ?cur(<<"EUR">>)}},
                        then_ = {value, #domain_Fees{
                            fees = #{surplus => ?share(1, 1, operation_amount)}
                        }}
                    }
                ]}
            },
            accounts = #{
                ?cur(<<"RUB">>) => #domain_ProviderAccount{settlement = AccountID}
            }
        }
    }}.

-spec withdrawal_provider(?dtp('WithdrawalProviderRef'), ?dtp('ProxyRef'), binary(), ct_helper:config()) ->
    object().
withdrawal_provider(?wthdr_prv(6) = Ref, ProxyRef, IdentityID, C) ->
    AccountID = account(<<"RUB">>, C),
    {withdrawal_provider, #domain_WithdrawalProviderObject{
        ref = Ref,
        data = #domain_WithdrawalProvider{
            name = <<"WithdrawalProvider">>,
            proxy = #domain_Proxy{ref = ProxyRef, additional = #{}},
            identity = IdentityID,
            withdrawal_terms = undefined,
            accounts = #{
                ?cur(<<"RUB">>) => #domain_ProviderAccount{settlement = AccountID}
            },
            terminal = {decisions, [
                #domain_WithdrawalTerminalDecision{
                    if_   = {constant, true},
                    then_ = {value, [?wthdr_trm(6)]}
                }
            ]}
        }
    }};

withdrawal_provider(Ref, ProxyRef, IdentityID, C) ->
    AccountID = account(<<"RUB">>, C),
    {withdrawal_provider, #domain_WithdrawalProviderObject{
        ref = Ref,
        data = #domain_WithdrawalProvider{
            name = <<"WithdrawalProvider">>,
            proxy = #domain_Proxy{ref = ProxyRef, additional = #{}},
            identity = IdentityID,
            withdrawal_terms = #domain_WithdrawalProvisionTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                payout_methods = {value, ?ordset([])},
                cash_limit = {value, ?cashrng(
                    {inclusive, ?cash(       0, <<"RUB">>)},
                    {exclusive, ?cash(10000000, <<"RUB">>)}
                )},
                cash_flow = {decisions, [
                    #domain_CashFlowDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, [
                            ?cfpost(
                                {system, settlement},
                                {provider, settlement},
                                {product, {min_of, ?ordset([
                                    ?fixed(10, <<"RUB">>),
                                    ?share(5, 100, operation_amount, round_half_towards_zero)
                                ])}}
                            )
                        ]}
                    }
                ]}
            },
            accounts = #{
                ?cur(<<"RUB">>) => #domain_ProviderAccount{settlement = AccountID}
            },
            terminal = {decisions, [
                #domain_WithdrawalTerminalDecision{
                    if_   = {condition, {cost_in, ?cashrng(
                        {inclusive, ?cash(      0, <<"RUB">>)},
                        {exclusive, ?cash(1000000, <<"RUB">>)}
                    )}},
                    then_ = {value, [?wthdr_trm(1)]}
                },
                #domain_WithdrawalTerminalDecision{
                    if_   = {condition, {cost_in, ?cashrng(
                        {inclusive, ?cash( 3000000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )}},
                    then_ = {value, [?wthdr_trm(7)]}
                }
            ]}
        }
    }}.

-spec withdrawal_terminal(?dtp('WithdrawalTerminalRef')) ->
    object().
withdrawal_terminal(?wthdr_trm(1) = Ref) ->
    {withdrawal_terminal, #domain_WithdrawalTerminalObject{
        ref = Ref,
        data = #domain_WithdrawalTerminal{
            name = <<"WithdrawalTerminal">>,
            terms = #domain_WithdrawalProvisionTerms{}
        }
    }};
withdrawal_terminal(?wthdr_trm(6) = Ref) ->
    {withdrawal_terminal, #domain_WithdrawalTerminalObject{
        ref = Ref,
        data = #domain_WithdrawalTerminal{
            name = <<"WithdrawalTerminal">>,
            terms = undefined
        }
    }};
withdrawal_terminal(?wthdr_trm(7) = Ref) ->
    {withdrawal_terminal, #domain_WithdrawalTerminalObject{
        ref = Ref,
        data = #domain_WithdrawalTerminal{
            name = <<"Terminal7">>,
            terms = #domain_WithdrawalProvisionTerms{
                currencies = {value, ?ordset([?cur(<<"BTC">>)])},
                payout_methods = {value, ?ordset([])},
                cash_limit = {value, ?cashrng(
                    {inclusive, ?cash( 1000000, <<"BTC">>)},
                    {exclusive, ?cash(10000000, <<"BTC">>)}
                )},
                cash_flow = {value, ?ordset([])}
            }
        }
    }}.

-spec currency(?dtp('CurrencyRef')) ->
    object().

currency(?cur(<<"EUR">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name          = <<"Europe"/utf8>>,
            numeric_code  = 978,
            symbolic_code = SymCode,
            exponent      = 2
        }
    }};
currency(?cur(<<"RUB">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name          = <<"Яussian Яuble"/utf8>>,
            numeric_code  = 643,
            symbolic_code = SymCode,
            exponent      = 2
        }
    }};
currency(?cur(<<"USD">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name          = <<"U$ Dollar">>,
            numeric_code  = 840,
            symbolic_code = SymCode,
            exponent      = 2
        }
    }};
currency(?cur(<<"BTC">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name          = <<"Bitcoin">>,
            numeric_code  = 999,
            symbolic_code = SymCode,
            exponent      = 10
        }
    }}.

-spec category(?dtp('CategoryRef'), binary(), ?dtp('CategoryType')) ->
    object().

category(Ref, Name, Type) ->
    {category, #domain_CategoryObject{
        ref = Ref,
        data = #domain_Category{
            name        = Name,
            description = <<>>,
            type        = Type
        }
    }}.

-spec payment_method(?dtp('PaymentMethodRef')) ->
    object().

payment_method(?pmt(_Type, Name) = Ref) when is_atom(Name) ->
    payment_method(Name, Ref).

payment_method(Name, Ref) ->
    {payment_method, #domain_PaymentMethodObject{
        ref = Ref,
        data = #domain_PaymentMethodDefinition{
            name        = erlang:atom_to_binary(Name, unicode),
            description = <<>>
        }
    }}.

-spec contract_template(?dtp('ContractTemplateRef'), ?dtp('TermSetHierarchyRef')) ->
    object().

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

-spec inspector(?dtp('InspectorRef'), binary(), ?dtp('ProxyRef')) ->
    object().

inspector(Ref, Name, ProxyRef) ->
    inspector(Ref, Name, ProxyRef, #{}).

-spec inspector(?dtp('InspectorRef'), binary(), ?dtp('ProxyRef'), ?dtp('ProxyOptions')) ->
    object().

inspector(Ref, Name, ProxyRef, Additional) ->
    {inspector, #domain_InspectorObject{
        ref  = Ref,
        data = #domain_Inspector{
            name        = Name,
            description = <<>>,
            proxy = #domain_Proxy{
                ref        = ProxyRef,
                additional = Additional
            }
        }
    }}.

-spec p2p_inspector(?dtp('P2PInspectorRef'), binary(), ?dtp('ProxyRef'), ?dtp('ProxyOptions')) ->
    object().

p2p_inspector(Ref, Name, ProxyRef, Additional) ->
    {p2p_inspector, #domain_P2PInspectorObject{
        ref  = Ref,
        data = #domain_P2PInspector{
            name        = Name,
            description = <<>>,
            fallback_risk_score = #{<<"fraud">> => high},
            proxy = #domain_Proxy{
                ref        = ProxyRef,
                additional = Additional
            }
        }
    }}.

-spec proxy(?dtp('ProxyRef'), Name :: binary()) ->
    object().

proxy(Ref, Name) ->
    proxy(Ref, Name, <<>>).

-spec proxy(?dtp('ProxyRef'), Name :: binary(), URL :: binary()) ->
    object().

proxy(Ref, Name, URL) ->
    proxy(Ref, Name, URL, #{}).


-spec proxy(?dtp('ProxyRef'), Name :: binary(), URL :: binary(), ?dtp('ProxyOptions')) ->
    object().

proxy(Ref, Name, URL, Opts) ->
    {proxy, #domain_ProxyObject{
        ref  = Ref,
        data = #domain_ProxyDefinition{
            name        = Name,
            description = <<>>,
            url         = URL,
            options     = Opts
        }
    }}.

-spec system_account_set(?dtp('SystemAccountSetRef'), binary(), ?dtp('CurrencyRef'), ct_helper:config()) ->
    object().

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
                    income  = AccountID1,
                    outcome = AccountID2
                }
            }
        }
    }}.

-spec term_set_hierarchy(?dtp('TermSetHierarchyRef')) ->
    object().

term_set_hierarchy(Ref) ->
    term_set_hierarchy(Ref, []).

-spec term_set_hierarchy(?dtp('TermSetHierarchyRef'), [?dtp('TimedTermSet')]) ->
    object().

term_set_hierarchy(Ref, TermSets) ->
    term_set_hierarchy(Ref, undefined, TermSets).

-spec term_set_hierarchy(Ref, ff_maybe:maybe(Ref), [?dtp('TimedTermSet')]) ->
    object() when
        Ref :: ?dtp('TermSetHierarchyRef').

term_set_hierarchy(Ref, ParentRef, TermSets) ->
    {term_set_hierarchy, #domain_TermSetHierarchyObject{
        ref = Ref,
        data = #domain_TermSetHierarchy{
            parent_terms = ParentRef,
            term_sets = TermSets
        }
    }}.

-spec timed_term_set(?dtp('TermSet')) ->
    ?dtp('TimedTermSet').

timed_term_set(TermSet) ->
    #domain_TimedTermSet{
        action_time = #'TimestampInterval'{},
        terms = TermSet
    }.

-spec globals(?dtp('ExternalAccountSetRef'), [?dtp('PaymentInstitutionRef')]) ->
    object().

globals(EASRef, PIRefs) ->
    {globals, #domain_GlobalsObject{
        ref = ?glob(),
        data = #domain_Globals{
            external_account_set = {value, EASRef},
            payment_institutions = ?ordset(PIRefs)
        }
    }}.

-spec account(binary(), ct_helper:config()) ->
    shumpune_shumpune_thrift:'AccountID'().

account(SymCode, C) ->
    Client = ff_woody_client:new(maps:get('accounter', ct_helper:cfg(services, C))),
    WoodyCtx = ct_helper:get_woody_ctx(C),
    Prototype = #shumpune_AccountPrototype{
        currency_sym_code = SymCode,
        description       = <<>>,
        creation_time     = timestamp()
    },
    Request = {{shumpune_shumpune_thrift, 'Accounter'}, 'CreateAccount', [Prototype]},
    case woody_client:call(Request, Client, WoodyCtx) of
        {ok, ID} ->
            ID
    end.

timestamp() ->
    genlib_rfc3339:format(genlib_time:daytime_to_unixtime(calendar:universal_time()), second).
