-module(wapi_privdoc_handler).

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").

-behaviour(swag_server_privdoc_logic_handler).
-behaviour(wapi_handler).

%% swag_server_privdoc_logic_handler callbacks
-export([authorize_api_key/2]).
-export([handle_request/3]).

%% wapi_handler callbacks
-export([process_request/3]).

%% Types

-type req_data()        :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:handler_context().
-type request_result()  :: wapi_handler:request_result().

%% API

-spec authorize_api_key(swag_server_privdoc:operation_id(), swag_server_privdoc:api_key()) ->
    false | {true, wapi_auth:context()}.
authorize_api_key(OperationID, ApiKey) ->
    ok = scoper:add_meta(#{api => privdoc, operation_id => OperationID}),
    wapi_auth:authorize_api_key(OperationID, ApiKey).

-spec handle_request(swag_server_privdoc:operation_id(), req_data(), swag_server_privdoc:request_context()) ->
    request_result().
handle_request(OperationID, Req, SwagContext) ->
    wapi_handler:handle_request(OperationID, Req, SwagContext, ?MODULE).

-spec process_request(swag_server_privdoc:operation_id(), req_data(), handler_context()) ->
    request_result().
process_request(_OperationID, _Req, _Context) ->
    {ok, {200, [], #{}}}.

%% Internal functions

%% service_call({ServiceName, Function, Args}, #{woody_context := WoodyContext}) ->
%%     wapi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).
