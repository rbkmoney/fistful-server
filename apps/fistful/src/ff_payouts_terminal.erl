-module(ff_payouts_terminal).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type withdrawal_terminal() :: #{
    id := id(),
    provider_ref => ff_payouts_provider:withdrawal_provider_ref(),
    withdrawal_terms => dmsl_domain_thrift:'WithdrawalProvisionTerms'(),
    adapter_opts => map()
}.

-type id() :: dmsl_domain_thrift:'ObjectID'().

-type withdrawal_terminal_ref() :: dmsl_domain_thrift:'WithdrawalTerminalRef'().
-type withdrawal_provision_terms() :: dmsl_domain_thrift:'WithdrawalProvisionTerms'().

-export_type([id/0]).
-export_type([withdrawal_terminal/0]).
-export_type([withdrawal_terminal_ref/0]).

-export([adapter_opts/1]).
-export([terms/1]).

-export([ref/1]).
-export([get/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec adapter_opts(withdrawal_terminal()) ->
    map().

adapter_opts(WithdrawalTerminal) ->
    maps:get(adapter_opts, WithdrawalTerminal, #{}).

-spec terms(withdrawal_terminal()) ->
    withdrawal_provision_terms() | undefined.

terms(WithdrawalTerminal) ->
    maps:get(withdrawal_terms, WithdrawalTerminal, undefined).

%%

-spec ref(id()) -> withdrawal_terminal_ref().

ref(ID) ->
    #domain_WithdrawalTerminalRef{id = ID}.

-spec get(id()) ->
    {ok, withdrawal_terminal()} |
    {error, notfound}.

get(ID) ->
    do(fun () ->
        WithdrawalTerminal = unwrap(ff_domain_config:object({withdrawal_terminal, ref(ID)})),
        decode(ID, WithdrawalTerminal)
    end).

%%

decode(ID, #domain_WithdrawalTerminal{
    options = ProxyOptions,
    terms = WithdrawalTerms,
    provider_ref = ProviderRef
}) ->
    genlib_map:compact(#{
        id => ID,
        provider_ref => ProviderRef,
        withdrawal_terms => WithdrawalTerms,
        adapter_opts => ProxyOptions
    }).
