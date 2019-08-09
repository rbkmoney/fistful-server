-module(ff_ct_binbase_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function('GetByCardToken', [_Token], _Context, _Opts) ->
   {ok, #binbase_ResponseData{
        bin_data = #binbase_BinData{
            payment_system = <<"visa">>,
            bank_name = <<"sber">>,
            iso_country_code = <<"RUS">>,
            bin_data_id = {i, 123}
        },
        version = 1
    }}.

