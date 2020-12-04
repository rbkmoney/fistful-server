-module(ff_ct_provider_sup).

-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).

%%
%% Supervisor callbacks
%%

-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Opts) ->
    Path = proplists:get_value(path, Opts, "/v1/adapter"),
    Service = woody_server:child_spec(
        ff_ct_provider_thrift_service_sup,
        #{
            handlers => [
                {Path, {{dmsl_withdrawals_provider_adapter_thrift, 'Adapter'}, {ff_ct_provider_thrift, []}}}
            ],
            event_handler => scoper_woody_event_handler,
            ip => proplists:get_value(ip, Opts, "::"),
            port => proplists:get_value(port, Opts, 8022),
            net_opts => proplists:get_value(net_opts, Opts, [])
        }
    ),
    {ok, {{one_for_one, 1, 5}, [Service]}}.
