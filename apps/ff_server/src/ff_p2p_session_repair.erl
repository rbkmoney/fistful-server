-module(ff_p2p_session_repair).

-behaviour(ff_woody_wrapper).

-export([handle_function/3]).

-include_lib("fistful_proto/include/ff_proto_fistful_thrift.hrl").

-type options() :: undefined.

%%
%% ff_woody_wrapper callbacks
%%

-spec handle_function(woody:func(), woody:args(), options()) ->
    {ok, woody:result()} | no_return().
handle_function('Repair', [ID, Scenario], _Opts) ->
    DecodedScenario = ff_codec:unmarshal(ff_p2p_session_codec, repair_scenario, Scenario),
    case p2p_session_machine:repair(ID, DecodedScenario) of
        ok ->
            {ok, ok};
        {error, notfound} ->
            woody_error:raise(business, #fistful_P2PSessionNotFound{});
        {error, working} ->
            woody_error:raise(business, #fistful_MachineAlreadyWorking{})
    end.
