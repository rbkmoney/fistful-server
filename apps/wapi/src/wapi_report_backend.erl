-module(wapi_report_backend).

-include_lib("fistful_reporter_proto/include/ff_reporter_reports_thrift.hrl").
-include_lib("file_storage_proto/include/fs_file_storage_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

-export([create_report/2]).
-export([get_report/3]).
-export([get_reports/2]).
-export([download_file/3]).

-type id() :: binary().
-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().

-spec create_report(req_data(), handler_context()) -> {ok, response_data()} | {error, Error} when
    Error ::
        {identity, unauthorized}
        | {identity, notfound}
        | invalid_request
        | invalid_contract.
create_report(
    #{
        identityID := IdentityID,
        'ReportParams' := ReportParams
    },
    HandlerContext
) ->
    case get_contract_id_from_identity(IdentityID, HandlerContext) of
        {ok, ContractID} ->
            Req = create_report_request(#{
                party_id => wapi_handler_utils:get_owner(HandlerContext),
                contract_id => ContractID,
                from_time => get_time(<<"fromTime">>, ReportParams),
                to_time => get_time(<<"toTime">>, ReportParams)
            }),
            Call = {fistful_report, 'GenerateReport', {Req, maps:get(<<"reportType">>, ReportParams)}},
            case wapi_handler_utils:service_call(Call, HandlerContext) of
                {ok, ReportID} ->
                    get_report(contractID, ReportID, ContractID, HandlerContext);
                {exception, #ff_reports_InvalidRequest{}} ->
                    {error, invalid_request};
                {exception, #ff_reports_ContractNotFound{}} ->
                    {error, invalid_contract}
            end;
        {error, _} = Error ->
            Error
    end.

-spec get_report(integer(), binary(), handler_context()) -> {ok, response_data()} | {error, Error} when
    Error ::
        {identity, unauthorized}
        | {identity, notfound}
        | notfound.
get_report(ReportID, IdentityID, HandlerContext) ->
    get_report(identityID, ReportID, IdentityID, HandlerContext).

get_report(identityID, ReportID, IdentityID, HandlerContext) ->
    case get_contract_id_from_identity(IdentityID, HandlerContext) of
        {ok, ContractID} ->
            get_report(contractID, ReportID, ContractID, HandlerContext);
        {error, _} = Error ->
            Error
    end;
get_report(contractID, ReportID, ContractID, HandlerContext) ->
    PartyID = wapi_handler_utils:get_owner(HandlerContext),
    Call = {fistful_report, 'GetReport', {PartyID, ContractID, ReportID}},
    case wapi_handler_utils:service_call(Call, HandlerContext) of
        {ok, Report} ->
            {ok, unmarshal_report(Report)};
        {exception, #ff_reports_ReportNotFound{}} ->
            {error, notfound}
    end.

-spec get_reports(req_data(), handler_context()) -> {ok, response_data()} | {error, Error} when
    Error ::
        {identity, unauthorized}
        | {identity, notfound}
        | invalid_request
        | {dataset_too_big, integer()}.
get_reports(#{identityID := IdentityID} = Params, HandlerContext) ->
    case get_contract_id_from_identity(IdentityID, HandlerContext) of
        {ok, ContractID} ->
            Req = create_report_request(#{
                party_id => wapi_handler_utils:get_owner(HandlerContext),
                contract_id => ContractID,
                from_time => get_time(fromTime, Params),
                to_time => get_time(toTime, Params)
            }),
            Call = {fistful_report, 'GetReports', {Req, [genlib:to_binary(maps:get(type, Params))]}},
            case wapi_handler_utils:service_call(Call, HandlerContext) of
                {ok, ReportList} ->
                    {ok, unmarshal_reports(ReportList)};
                {exception, #ff_reports_InvalidRequest{}} ->
                    {error, invalid_request};
                {exception, #ff_reports_DatasetTooBig{limit = Limit}} ->
                    {error, {dataset_too_big, Limit}}
            end;
        {error, _} = Error ->
            Error
    end.

-spec download_file(binary(), binary(), handler_context()) -> {ok, response_data()} | {error, Error} when
    Error ::
        notfound.
download_file(FileID, ExpiresAt, HandlerContext) ->
    Timestamp = wapi_utils:to_universal_time(ExpiresAt),
    Call = {file_storage, 'GenerateDownloadUrl', {FileID, Timestamp}},
    case wapi_handler_utils:service_call(Call, HandlerContext) of
        {exception, #file_storage_FileNotFound{}} ->
            {error, notfound};
        Result ->
            Result
    end.

%% Internal

-spec get_contract_id_from_identity(id(), handler_context()) -> {ok, id()} | {error, Error} when
    Error ::
        {identity, unauthorized}
        | {identity, notfound}.
get_contract_id_from_identity(IdentityID, HandlerContext) ->
    case wapi_identity_backend:get_thrift_identity(IdentityID, HandlerContext) of
        {ok, #idnt_IdentityState{contract_id = ContractID}} ->
            {ok, ContractID};
        {error, _} = Error ->
            Error
    end.

create_report_request(#{
    party_id := PartyID,
    contract_id := ContractID,
    from_time := FromTime,
    to_time := ToTime
}) ->
    #'ff_reports_ReportRequest'{
        party_id = PartyID,
        contract_id = ContractID,
        time_range = #'ff_reports_ReportTimeRange'{
            from_time = FromTime,
            to_time = ToTime
        }
    }.

get_time(Key, Req) ->
    case genlib_map:get(Key, Req) of
        Timestamp when is_binary(Timestamp) ->
            wapi_utils:to_universal_time(Timestamp);
        undefined ->
            undefined
    end.

%% Marshaling

unmarshal_reports(List) ->
    lists:map(fun(Report) -> unmarshal_report(Report) end, List).

unmarshal_report(#ff_reports_Report{
    report_id = ReportID,
    time_range = TimeRange,
    created_at = CreatedAt,
    report_type = Type,
    status = Status,
    file_data_ids = Files
}) ->
    genlib_map:compact(#{
        <<"id">> => ReportID,
        <<"fromTime">> => TimeRange#ff_reports_ReportTimeRange.from_time,
        <<"toTime">> => TimeRange#ff_reports_ReportTimeRange.to_time,
        <<"createdAt">> => CreatedAt,
        <<"status">> => unmarshal_report_status(Status),
        <<"type">> => Type,
        <<"files">> => unmarshal_report_files(Files)
    }).

unmarshal_report_status(pending) ->
    <<"pending">>;
unmarshal_report_status(created) ->
    <<"created">>;
unmarshal_report_status(canceled) ->
    <<"canceled">>.

unmarshal_report_files(undefined) ->
    [];
unmarshal_report_files(Files) ->
    lists:map(fun(File) -> unmarshal_report_file(File) end, Files).

unmarshal_report_file(File) ->
    #{<<"id">> => File}.
