-module(p2p_ct_inspector_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("damsel/include/dmsl_proxy_inspector_p2p_thrift.hrl").

-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(
    'InspectTransfer',
    [
        #p2p_insp_Context{info = #p2p_insp_TransferInfo{
            transfer = #p2p_insp_Transfer{cost = #domain_Cash{amount = 199}}
        }},
        _RiskTypes
    ],
     _Context,
      _Opts
) ->
    erlang:error({test, inspector_failed});
handle_function('InspectTransfer', [_Params, _RiskTypes], _Context, _Opts) ->
    {ok, encode_result()}.

encode_result() ->
    #p2p_insp_InspectResult{
        scores = #{<<"fraud">> => low}
    }.
