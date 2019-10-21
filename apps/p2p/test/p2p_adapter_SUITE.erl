-module(p2p_adapter_SUITE).

% -include("hg_ct_domain.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([process/1]).
-export([handle_callback/1]).

-import(ct_helper, [cfg/2]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: ok | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() -> [
        process,
        handle_callback
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    SuiteSup     = ct_sup:start(),
    _            = ct_helper:start_apps([dmt_client]),
    DomainConfig = [construct_proxy(), construct_terminal(), construct_provider()],
    ok           = ct_domain_config:insert(DomainConfig),
    % TODO: add dummy adapter
    % Adapter  = dummy_p2p_adapter:start(),
    [{suite_sup, SuiteSup}, {started_apps, []} | C].

-spec end_per_suite(config()) -> ok.
end_per_suite(C) ->
    ok = ct_domain_config:cleanup(),
    ok = ct_sup:stop(cfg(suite_sup, C)),
    ok = ct_helper:stop_apps(cfg(started_apps, C)),
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

-spec process(config()) -> test_return().
process(_C) ->
    Context     = construct_context(),
    ProviderRef = #domain_ProviderRef{id = 111},
    TerminalRef = #domain_TerminalRef{id = 111},
    Route       = #domain_PaymentRoute{provider = ProviderRef, terminal = TerminalRef},
    Result      = p2p_adapter:process(Context, Route),
    ?assertMatch({ok, #p2p_adapter_ProcessResult{}}, Result),
    ok.

-spec handle_callback(config()) -> test_return().
handle_callback(_C) ->
    Context     = construct_context(),
    Callback    = #p2p_adapter_Callback{tag = <<"p2p">>, payload = <<>>},
    ProviderRef = #domain_ProviderRef{id = 111},
    TerminalRef = #domain_TerminalRef{id = 111},
    Route       = #domain_PaymentRoute{provider = ProviderRef, terminal = TerminalRef},
    Result      = p2p_adapter:handle_callback(Callback, Context, Route),
    ?assertMatch({ok, #p2p_adapter_CallbackResult{}}, Result),
    ok.

construct_context() ->
    #p2p_adapter_Context{
        session   = #p2p_adapter_Session{},
        operation = {process, #p2p_adapter_ProcessOperationInfo{
            body     = #p2p_adapter_Cash{
                amount   = 1,
                currency = #domain_Currency{
                    name          = <<"US Dollar">>,
                    symbolic_code = <<"USD">>,
                    numeric_code  = 840,
                    exponent      = 2
                }
            },
            sender   = {disposable, #domain_DisposablePaymentResource{
                payment_tool = {bank_card, #domain_BankCard{
                    token          = <<>>,
                    payment_system = visa,
                    bin            = <<>>,
                    masked_pan     = <<>>
                }}
            }},
            receiver = {disposable, #domain_DisposablePaymentResource{
                payment_tool = {bank_card, #domain_BankCard{
                    token          = <<>>,
                    payment_system = visa,
                    bin            = <<>>,
                    masked_pan     = <<>>
                }}
            }}
        }}
    }.

construct_provider() ->
    {provider, #domain_ProviderObject{
        ref  = #domain_ProviderRef{id = 111},
        data = #domain_Provider{
            name        = <<"Provider">>,
            description = <<"Provider">>,
            abs_account = <<"abs">>,
            terminal    = {value, [#domain_ProviderTerminalRef{id = 111}]},
            proxy       = #domain_Proxy{
                ref        = #domain_ProxyRef{id = 111},
                additional = #{}
            }
        }
    }}.

construct_terminal() ->
    {terminal, #domain_TerminalObject{
        ref  = #domain_TerminalRef{id = 111},
        data = #domain_Terminal{
            name          = <<"Terminal">>,
            description   = <<"Terminal">>,
            risk_coverage = low
        }
    }}.

construct_proxy() ->
    {proxy, #domain_ProxyObject{
        ref  = #domain_ProxyRef{id = 111},
        data = #domain_ProxyDefinition{
            name        = <<"Proxy">>,
            description = <<"Proxy">>,
            url         = <<"127.0.0.1:10001/dummy">>,
            options     = #{}
        }
    }}.
