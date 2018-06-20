-module(wapi_handler).

-export([handle_request/4]).
-export([throw_result/1]).

-export([logic_error/2]).
-export([reply_5xx/1]).

-define(APP, wapi).
-define(request_result, wapi_req_result).

%% Behaviour definition
-type req_data() :: #{atom() | binary() => term()}.

-type handler_context() :: #{
    woody_context   := woody_context:ctx(),
    swagger_context := swag_server_payres:request_context()
}.

-type operation_id() ::
    swag_server_payres:operation_id() |
    swag_server_wallet:operation_id() |
    swag_server_privdoc:operation_id().

-type request_result() :: {ok | error, swag_server_payres_logic_handler:response()}.

-export_type([req_data/0]).
-export_type([handler_context/0]).
-export_type([operation_id/0]).
-export_type([request_result/0]).

-callback process_request(operation_id(), req_data(), handler_context()) ->
    request_result() | no_return().

-type error_code()    :: atom().
-type error_message() :: binary().
-type logic_error() :: #{
    binary() := error_code(),
    binary() := error_message()
}.

%% API

-spec handle_request(swag_server_payres:operation_id(), req_data(), swag_server_payres:request_context(), module()) ->
    request_result().
handle_request(OperationID, Req, SwagContext = #{auth_context := AuthContext}, Handler) ->
    _ = lager:info("Processing request ~p", [OperationID]),
    try
        case wapi_auth:authorize_operation(OperationID, Req, AuthContext) of
            ok ->
                WoodyContext = create_woody_context(Req, AuthContext),
                Context = create_handler_context(SwagContext, WoodyContext),
                Handler:process_request(OperationID, Req, Context);
            {error, _} = Error ->
                _ = lager:info("Operation ~p authorization failed due to ~p", [OperationID, Error]),
                {error, {401, [], general_error(<<"Unauthorized operation">>)}}
        end
    catch
        throw:{?request_result, Result} ->
            Result;
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details)
    end.

-spec throw_result(request_result()) ->
    no_return().
throw_result(Res) ->
    erlang:throw({?request_result, Res}).

-spec logic_error(error_code(), error_message()) ->
    logic_error().
logic_error(Code, Message) ->
    #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)}.

general_error(Message) ->
    #{<<"message">> => genlib:to_binary(Message)}.

process_woody_error(_Source, result_unexpected   , _Details) -> {error, reply_5xx(500)};
process_woody_error(_Source, resource_unavailable, _Details) -> {error, reply_5xx(503)};
process_woody_error(_Source, result_unknown      , _Details) -> {error, reply_5xx(504)}.

-spec reply_5xx(Code) ->
    {Code, [], <<>>} when Code :: 500..600.
reply_5xx(Code) when Code >= 500 andalso Code < 600 ->
    {Code, [], <<>>}.

-spec create_woody_context(req_data(), wapi_auth:context()) ->
    woody_context:ctx().
create_woody_context(#{'X-Request-ID' := RequestID}, AuthContext) ->
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    ok = scoper:add_meta(#{request_id => RequestID}),
    _ = lager:debug("Created TraceID:~p for RequestID:~p", [TraceID , RequestID]),
    woody_user_identity:put(collect_user_identity(AuthContext), woody_context:new(RpcID)).

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id       => wapi_auth:get_subject_id(AuthContext),
        realm    => genlib_app:env(?APP, realm),
        email    => wapi_auth:get_claim(<<"email">>, AuthContext, undefined),
        username => wapi_auth:get_claim(<<"name">> , AuthContext, undefined)
    }).

-spec create_handler_context(swag_server_payres:request_context(), woody_context:ctx()) ->
    handler_context().
create_handler_context(SwagContext, WoodyContext) ->
    #{
        woody_context   => WoodyContext,
        swagger_context => SwagContext
    }.
