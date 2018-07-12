-module(wapi_handler).

%% API
-export([handle_request/5]).
-export([throw_result/1]).

%% Behaviour definition

-type operation_id() ::
    swag_server_payres:operation_id() |
    swag_server_wallet:operation_id() |
    swag_server_privdoc:operation_id().

-type swagger_context() ::
    swag_server_payres:request_context() |
    swag_server_wallet:request_context() |
    swag_server_privdoc:request_context().

-type context() :: #{
    woody_context   := woody_context:ctx(),
    swagger_context := swagger_context()
}.

-type opts() ::
    swag_server_wallet:handler_opts() |
    swag_server_payres:handler_opts() |
    swag_server_privdoc:handler_opts().

-type req_data()         :: #{atom() | binary() => term()}.
-type status_code()      :: 200..599.
-type headers()          :: cowboy:http_headers().
-type response_data()    :: map() | [map()] | undefined.
-type request_result()   :: {ok | error, {status_code(), headers(), response_data()}}.

-callback process_request(operation_id(), req_data(), context(), opts()) ->
    request_result() | no_return().

-export_type([operation_id/0]).
-export_type([swagger_context/0]).
-export_type([context/0]).
-export_type([opts/0]).
-export_type([req_data/0]).
-export_type([status_code/0]).
-export_type([response_data/0]).
-export_type([headers/0]).
-export_type([request_result/0]).

%% API

-define(request_result, wapi_req_result).

-spec handle_request(operation_id(), req_data(), swagger_context(), module(), opts()) ->
    request_result().
handle_request(OperationID, Req, SwagContext = #{auth_context := AuthContext}, Handler, Opts) ->
    _ = lager:info("Processing request ~p", [OperationID]),
    try
        case wapi_auth:authorize_operation(OperationID, Req, AuthContext) of
            ok ->
                WoodyContext = create_woody_context(Req, AuthContext, Opts),
                Context = create_handler_context(SwagContext, WoodyContext),
                Handler:process_request(OperationID, Req, Context, Opts)
            %% ToDo: return back as soon, as authorization is implemented
            %% {error, _} = Error ->
            %%     _ = lager:info("Operation ~p authorization failed due to ~p", [OperationID, Error]),
            %%     wapi_handler_utils:reply_error(401, wapi_handler_utils:get_error_msg(<<"Unauthorized operation">>))
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

-spec create_woody_context(req_data(), wapi_auth:context(), opts()) ->
    woody_context:ctx().
create_woody_context(#{'X-Request-ID' := RequestID}, AuthContext, Opts) ->
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    ok = scoper:add_meta(#{request_id => RequestID, trace_id => TraceID}),
    _ = lager:debug("Created TraceID for the request"),
    Ctx = woody_user_identity:put(collect_user_identity(AuthContext, Opts), woody_context:new(RpcID)),
    %% TODO remove this fistful specific step, when separating the wapi service.
    ok = ff_woody_ctx:set(Ctx),
    Ctx.

-define(APP, wapi).

collect_user_identity(AuthContext, _Opts) ->
    genlib_map:compact(#{
        id       => wapi_auth:get_subject_id(AuthContext),
        %% TODO pass realm via Opts
        realm    => genlib_app:env(?APP, realm),
        email    => wapi_auth:get_claim(<<"email">>, AuthContext, undefined),
        username => wapi_auth:get_claim(<<"name">> , AuthContext, undefined)
    }).

-spec create_handler_context(swagger_context(), woody_context:ctx()) ->
    context().
create_handler_context(SwagContext, WoodyContext) ->
    #{
        woody_context   => WoodyContext,
        swagger_context => SwagContext
    }.

process_woody_error(_Source, result_unexpected   , _Details) -> wapi_handler_utils:reply_error(500);
process_woody_error(_Source, resource_unavailable, _Details) -> wapi_handler_utils:reply_error(503);
process_woody_error(_Source, result_unknown      , _Details) -> wapi_handler_utils:reply_error(504).
