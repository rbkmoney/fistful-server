-module(wapi_handler).

%% API
-export([handle_request/5]).
-export([throw_result/1]).

%% Behaviour definition

-type tag() :: wallet | payres.

-type operation_id() ::
    swag_client_payres:operation_id() |
    swag_server_wallet:operation_id().

-type swagger_context() ::
    swag_client_payres:request_context() |
    swag_server_wallet:request_context().

-type context() :: #{
    woody_context   := woody_context:ctx(),
    swagger_context := swagger_context()
}.

-type opts() ::
    swag_server_wallet:handler_opts(_).

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
-define(APP, wapi).

-spec handle_request(tag(), operation_id(), req_data(), swagger_context(), opts()) ->
    request_result().
handle_request(Tag, OperationID, Req, SwagContext = #{auth_context := AuthContext}, Opts) ->
    #{'X-Request-Deadline' := Header} = Req,
    case wapi_utils:parse_deadline(Header) of
        {ok, Deadline} ->
            WoodyContext = attach_deadline(Deadline, create_woody_context(Tag, Req, AuthContext, Opts)),
            process_request(Tag, OperationID, Req, SwagContext, Opts, WoodyContext);
        _ ->
            _ = logger:warning("Operation ~p failed due to invalid deadline header ~p", [OperationID, Header]),
            wapi_handler_utils:reply_ok(400, #{
                <<"errorType">>   => <<"SchemaViolated">>,
                <<"name">>        => <<"X-Request-Deadline">>,
                <<"description">> => <<"Invalid data in X-Request-Deadline header">>
            })
    end.

process_request(Tag, OperationID, Req, SwagContext, Opts, WoodyContext) ->
    _ = logger:info("Processing request ~p", [OperationID]),
    try
        %% TODO remove this fistful specific step, when separating the wapi service.
        ok = ff_context:save(create_ff_context(WoodyContext, Opts)),

        Context      = create_handler_context(SwagContext, WoodyContext),
        Handler      = get_handler(Tag, genlib_app:env(?APP, transport)),
        case wapi_auth:authorize_operation(OperationID, Req, Context) of
            {ok, AuthDetails} ->
                ok = logger:info("Operation ~p authorized via ~p", [OperationID, AuthDetails]),
                Handler:process_request(OperationID, Req, Context, Opts);
            {error, Error} ->
                ok = logger:info("Operation ~p authorization failed due to ~p", [OperationID, Error]),
                wapi_handler_utils:reply_ok(401, wapi_handler_utils:get_error_msg(<<"Unauthorized operation">>))
        end
    catch
        throw:{?request_result, Result} ->
            Result;
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details)
    after
        ff_context:cleanup()
    end.

-spec throw_result(request_result()) ->
    no_return().
throw_result(Res) ->
    erlang:throw({?request_result, Res}).

get_handler(wallet, thrift)  -> wapi_wallet_thrift_handler;
get_handler(wallet, _)  -> wapi_wallet_handler;
get_handler(payres, _)  -> wapi_payres_handler.

-spec create_woody_context(tag(), req_data(), wapi_auth:context(), opts()) ->
    woody_context:ctx().
create_woody_context(Tag, #{'X-Request-ID' := RequestID}, AuthContext, Opts) ->
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    ok = scoper:add_meta(#{request_id => RequestID, trace_id => TraceID}),
    _ = logger:debug("Created TraceID for the request"),
    woody_user_identity:put(
        collect_user_identity(AuthContext, Opts),
        woody_context:new(RpcID, undefined, wapi_woody_client:get_service_deadline(Tag))
    ).

attach_deadline(undefined, Context) ->
    Context;
attach_deadline(Deadline, Context) ->
    woody_context:set_deadline(Deadline, Context).

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

process_woody_error(_Source, result_unexpected, _Details) ->
    wapi_handler_utils:reply_error(500);
process_woody_error(_Source, resource_unavailable, _Details) ->
    % Return an 504 since it is unknown if state of the system has been altered
    % @TODO Implement some sort of tagging for operations that mutate the state,
    % so we can still return 503s for those that don't
    wapi_handler_utils:reply_error(504);
process_woody_error(_Source, result_unknown, _Details) ->
    wapi_handler_utils:reply_error(504).

-spec create_ff_context(woody_context:ctx(), opts()) ->
    ff_context:context().
create_ff_context(WoodyContext, Opts) ->
    ContextOptions = #{
        woody_context => WoodyContext,
        party_client => maps:get(party_client, Opts)
    },
    ff_context:create(ContextOptions).
