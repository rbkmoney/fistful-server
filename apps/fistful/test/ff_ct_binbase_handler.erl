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
handle_function('GetByCardToken', {<<"TEST_NOTFOUND">>}, _Context, _Opts) ->
    woody_error:raise(business, #binbase_BinNotFound{});
handle_function('GetByCardToken', {<<"TEST_NOTFOUND_SENDER">>}, _Context, _Opts) ->
    woody_error:raise(business, #binbase_BinNotFound{});
handle_function('GetByCardToken', {<<"TEST_NOTFOUND_RECEIVER">>}, _Context, _Opts) ->
    woody_error:raise(business, #binbase_BinNotFound{});
handle_function('GetByCardToken', {<<"USD_COUNTRY">>}, _Context, _Opts) ->
    {ok, #binbase_ResponseData{
        bin_data = #binbase_BinData{
            payment_system = <<"VISA">>,
            bank_name = <<"uber">>,
            iso_country_code = <<"USA">>,
            bin_data_id = {i, 123}
        },
        version = 1
    }};
handle_function('GetByCardToken', {<<"NSPK MIR">>}, _Context, _Opts) ->
    {ok, #binbase_ResponseData{
        bin_data = #binbase_BinData{
            payment_system = <<"NSPK MIR">>,
            bank_name = <<"poopa">>,
            iso_country_code = <<"RUS">>,
            bin_data_id = {i, 123}
        },
        version = 1
    }};
handle_function('GetByCardToken', {_Token}, _Context, _Opts) ->
    {ok, #binbase_ResponseData{
        bin_data = #binbase_BinData{
            payment_system = <<"VISA">>,
            bank_name = <<"sber">>,
            iso_country_code = <<"RUS">>,
            bin_data_id = {i, 123}
        },
        version = 1
    }};
handle_function('GetByBinDataId', {ID}, _Context, _Opts) ->
    {ok, #binbase_ResponseData{
        bin_data = #binbase_BinData{
            payment_system = <<"VISA">>,
            bank_name = <<"sber">>,
            iso_country_code = <<"RUS">>,
            bin_data_id = ID
        },
        version = 1
    }}.
