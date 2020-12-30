-module(wapi_woody_client).

-export([call_service/4]).
-export([call_service/5]).

-export([get_service_modname/1]).
-export([get_service_deadline/1]).

%%
-define(APP, wapi_woody_client).

-type service_name() :: atom().

-export_type([service_name/0]).

-spec call_service(service_name(), woody:func(), woody:args(), woody_context:ctx()) -> woody:result().
call_service(ServiceName, Function, Args, Context) ->
    call_service(ServiceName, Function, Args, Context, scoper_woody_event_handler).

-spec call_service(service_name(), woody:func(), woody:args(), woody_context:ctx(), woody:ev_handler()) ->
    woody:result().
call_service(ServiceName, Function, Args, Context0, EventHandler) ->
    Deadline = get_service_deadline(ServiceName),
    Context1 = set_deadline(Deadline, Context0),
    Retry = get_service_retry(ServiceName, Function),
    call_service(ServiceName, Function, Args, Context1, EventHandler, Retry).

call_service(ServiceName, Function, Args, Context, EventHandler, Retry) ->
    Url = get_service_url(ServiceName),
    Service = get_service_modname(ServiceName),
    Request = {Service, Function, Args},
    try
        woody_client:call(
            Request,
            #{url => Url, event_handler => EventHandler},
            Context
        )
    catch
        error:{woody_error, {_Source, Class, _Details}} = Error when
            Class =:= resource_unavailable orelse Class =:= result_unknown
        ->
            NextRetry = apply_retry_strategy(Retry, Error, Context),
            call_service(ServiceName, Function, Args, Context, EventHandler, NextRetry)
    end.

apply_retry_strategy(Retry, Error, Context) ->
    apply_retry_step(genlib_retry:next_step(Retry), woody_context:get_deadline(Context), Error).

apply_retry_step(finish, _, Error) ->
    erlang:error(Error);
apply_retry_step({wait, Timeout, Retry}, undefined, _) ->
    ok = timer:sleep(Timeout),
    Retry;
apply_retry_step({wait, Timeout, Retry}, Deadline0, Error) ->
    Deadline1 = woody_deadline:from_unixtime_ms(
        woody_deadline:to_unixtime_ms(Deadline0) - Timeout
    ),
    case woody_deadline:is_reached(Deadline1) of
        true ->
            % no more time for retries
            erlang:error(Error);
        false ->
            ok = timer:sleep(Timeout),
            Retry
    end.

get_service_url(ServiceName) ->
    maps:get(ServiceName, genlib_app:env(?APP, service_urls)).

-spec get_service_modname(service_name()) -> woody:service().
get_service_modname(cds_storage) ->
    {dmsl_cds_thrift, 'Storage'};
get_service_modname(identdoc_storage) ->
    {identdocstore_identity_document_storage_thrift, 'IdentityDocumentStorage'};
get_service_modname(fistful_stat) ->
    {ff_proto_fistful_stat_thrift, 'FistfulStatistics'};
get_service_modname(fistful_report) ->
    {ff_reporter_reports_thrift, 'Reporting'};
get_service_modname(file_storage) ->
    {fs_file_storage_thrift, 'FileStorage'};
get_service_modname(fistful_provider) ->
    {ff_proto_provider_thrift, 'Management'};
get_service_modname(fistful_identity) ->
    {ff_proto_identity_thrift, 'Management'};
get_service_modname(fistful_wallet) ->
    {ff_proto_wallet_thrift, 'Management'};
get_service_modname(fistful_destination) ->
    {ff_proto_destination_thrift, 'Management'};
get_service_modname(fistful_withdrawal) ->
    {ff_proto_withdrawal_thrift, 'Management'};
get_service_modname(fistful_p2p_template) ->
    {ff_proto_p2p_template_thrift, 'Management'};
get_service_modname(webhook_manager) ->
    {ff_proto_webhooker_thrift, 'WebhookManager'};
get_service_modname(fistful_p2p_transfer) ->
    {ff_proto_p2p_transfer_thrift, 'Management'};
get_service_modname(fistful_p2p_session) ->
    {ff_proto_p2p_session_thrift, 'Management'};
get_service_modname(fistful_w2w_transfer) ->
    {ff_proto_w2w_transfer_thrift, 'Management'}.

-spec get_service_deadline(service_name()) -> undefined | woody_deadline:deadline().
get_service_deadline(ServiceName) ->
    ServiceDeadlines = genlib_app:env(?APP, api_deadlines, #{}),
    case maps:get(ServiceName, ServiceDeadlines, undefined) of
        Timeout when is_integer(Timeout) andalso Timeout >= 0 ->
            woody_deadline:from_timeout(Timeout);
        undefined ->
            undefined
    end.

set_deadline(Deadline, Context) ->
    case woody_context:get_deadline(Context) of
        undefined ->
            woody_context:set_deadline(Deadline, Context);
        _AlreadySet ->
            Context
    end.

get_service_retry(ServiceName, Function) ->
    ServiceRetries = genlib_app:env(?APP, service_retries, #{}),
    FunctionReties = maps:get(ServiceName, ServiceRetries, #{}),
    DefaultRetry = maps:get('_', FunctionReties, finish),
    maps:get(Function, FunctionReties, DefaultRetry).
