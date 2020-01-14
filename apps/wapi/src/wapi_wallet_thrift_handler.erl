-module(wapi_wallet_thrift_handler).

-behaviour(swag_server_wallet_logic_handler).
-behaviour(wapi_handler).

%% swag_server_wallet_logic_handler callbacks
-export([authorize_api_key/3]).
-export([handle_request/4]).

%% wapi_handler callbacks
-export([process_request/4]).

%% Types

-type req_data()        :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type request_result()  :: wapi_handler:request_result().
-type operation_id()    :: swag_server_wallet:operation_id().
-type api_key()         :: swag_server_wallet:api_key().
-type request_context() :: swag_server_wallet:request_context().
-type handler_opts()    :: swag_server_wallet:handler_opts(_).

%% API

-spec authorize_api_key(operation_id(), api_key(), handler_opts()) ->
    false | {true, wapi_auth:context()}.
authorize_api_key(OperationID, ApiKey, Opts) ->
    ok = scoper:add_meta(#{api => wallet, operation_id => OperationID}),
    wapi_auth:authorize_api_key(OperationID, ApiKey, Opts).

-spec handle_request(swag_server_wallet:operation_id(), req_data(), request_context(), handler_opts()) ->
    request_result().
handle_request(OperationID, Req, SwagContext, Opts) ->
    wapi_handler:handle_request(wallet, OperationID, Req, SwagContext, Opts).

-spec process_request(operation_id(), req_data(), handler_context(), handler_opts()) ->
    request_result().

%% Wallets

process_request('GetWallet', #{'walletID' := WalletId}, Context, _Opts) ->
    case wapi_wallet_backend:get(WalletId, Context) of
        {ok, Wallet}                    -> wapi_handler_utils:reply_ok(200, Wallet);
        {error, {wallet, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {wallet, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateWallet', #{'Wallet' := Params}, Context, Opts) ->
    case wapi_wallet_backend:create(Params, Context) of
        {ok, Wallet = #{<<"id">> := WalletId}} ->
            wapi_handler_utils:reply_ok(201, Wallet, get_location('GetWallet', [WalletId], Opts));
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {currency, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Currency not supported">>));
        {error, inaccessible} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Identity inaccessible">>));
        {error, {conflict, ID}} ->
            wapi_handler_utils:reply_error(409, #{<<"id">> => ID})
    end;
process_request(OperationID, Params, Context, Opts) ->
    wapi_wallet_handler:process_request(OperationID, Params, Context, Opts).

%% Internal functions

get_location(OperationId, Params, Opts) ->
    #{path := PathSpec} = swag_server_wallet_router:get_operation(OperationId),
    wapi_handler_utils:get_location(PathSpec, Params, Opts).
