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
-export([proxy/2]).
-export([proxy/3]).
-export([system_account_set/4]).
-export([external_account_set/4]).

%%

-include_lib("ff_cth/include/ct_domain.hrl").

-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").

currency(?cur(<<"RUB">> = SymCode) = Ref) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name          = <<"Яussian Яuble">>,
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
    }}.

category(Ref, Name, Type) ->
    {category, #domain_CategoryObject{
        ref = Ref,
        data = #domain_Category{
            name        = Name,
            description = <<>>,
            type        = Type
        }
    }}.

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

inspector(Ref, Name, ProxyRef) ->
    inspector(Ref, Name, ProxyRef, #{}).

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

proxy(Ref, Name) ->
    proxy(Ref, Name, #{}).

proxy(Ref, Name, Opts) ->
    {proxy, #domain_ProxyObject{
        ref  = Ref,
        data = #domain_ProxyDefinition{
            name        = Name,
            description = <<>>,
            url         = <<>>,
            options     = Opts
        }
    }}.

system_account_set(Ref, Name, ?cur(SymCode), C) ->
    AccountID = account(SymCode, C),
    {system_account_set, #domain_SystemAccountSetObject{
        ref = Ref,
        data = #domain_SystemAccountSet{
            name = Name,
            description = <<>>,
            accounts = #{
                ?cur(SymCode) => #domain_SystemAccount{
                    settlement = AccountID
                }
            }
        }
    }}.

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

account(SymCode, C) ->
    Client = maps:get('accounter', ct_helper:cfg(services, C)),
    WoodyCtx = ct_helper:get_woody_ctx(C),
    Prototype = #accounter_AccountPrototype{
        currency_sym_code = SymCode,
        description       = <<>>,
        creation_time     = timestamp()
    },
    Request = {{dmsl_accounter_thrift, 'Accounter'}, 'CreateAccount', [Prototype]},
    case woody_client:call(Request, Client, WoodyCtx) of
        {ok, ID} ->
            ID
    end.

timestamp() ->
    {ok, Now} = rfc3339:format(calendar:universal_time()),
    Now.
