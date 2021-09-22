-module(swag_server_wallet_logic_handler).

-export([handle_request/4]).
-export([map_error/2]).

-type operation_id()    :: swag_server_wallet:operation_id().
-type api_key()         :: swag_server_wallet:api_key().
-type auth_context()    :: swag_server_wallet:auth_context().
-type object()          :: swag_server_wallet:object().
-type request_context() :: swag_server_wallet:request_context().
-type handler_opts(T)   :: swag_server_wallet:handler_opts(T).
-type logic_handler(T)  :: swag_server_wallet:logic_handler(T).
-type response()        :: swag_server_wallet:response().

-type validation_error() :: swag_server_wallet_validation:error().
-type error_type()      :: validation_error.
-type error_message()   :: swag_server_wallet:error_reason().

-export_type([error_type/0]).

%% Behaviour definition

-export([authorize_api_key/3]).

-callback authorize_api_key(operation_id(), api_key(), handler_opts(_)) ->
    boolean() | {boolean(), auth_context()}.

-callback handle_request(operation_id(), object(), request_context(), handler_opts(_)) ->
    {ok | error, response()}.

-callback map_error(error_type(), validation_error()) ->
    error_message().

%% API

-spec handle_request(logic_handler(_), operation_id(), object(), request_context()) ->
    {ok | error, response()}.

handle_request(Handler, OperationID, Request, Context) ->
    {Module, Opts} = get_mod_opts(Handler),
    Module:handle_request(OperationID, Request, Context, Opts).

-spec map_error(module(), {error_type(), validation_error()}) ->
    error_message().

map_error(Handler, {Type, Error}) ->
    {Module, _Opts} = get_mod_opts(Handler),
    Module:map_error(Type, Error).

-spec authorize_api_key(logic_handler(_), operation_id(), api_key()) ->
    false | {true, auth_context()}.
authorize_api_key(Handler, OperationID, ApiKey) ->
    {Module, Opts} = get_mod_opts(Handler),
    Module:authorize_api_key(OperationID, ApiKey, Opts).

%% Internal functions

get_mod_opts(ModOpts= {_, _}) ->
    ModOpts;
get_mod_opts(Module) ->
    {Module, undefined}.
