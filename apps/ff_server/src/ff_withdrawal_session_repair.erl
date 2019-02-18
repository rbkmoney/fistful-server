-module(ff_withdrawal_session_repair).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-include_lib("fistful_proto/include/ff_proto_fistful_thrift.hrl").

-type options() :: #{}.

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Context, Opts) ->
    ok = ff_woody_ctx:set(Context),
    try
        do_handle_function(Func, Args, Context, Opts)
    after
        ff_woody_ctx:unset()
    end.

do_handle_function('Repair', [ID, Scenario], _Context, _Opts) ->
    DecodedScenario = ff_codec:unmarshal(ff_withdrawal_session_codec, repair_scenario, Scenario),
    case ff_withdrawal_session_machine:repair(ID, DecodedScenario) of
        ok ->
            {ok, ok};
        {error, notfound} ->
            woody_error:raise(business, #fistful_WithdrawalSessionNotFound{});
        {error, working} ->
            woody_error:raise(business, #fistful_MachineAlreadyWorking{})
    end.
