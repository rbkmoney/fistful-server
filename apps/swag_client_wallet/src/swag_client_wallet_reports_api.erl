%% -*- mode: erlang -*-
-module(swag_client_wallet_reports_api).

%% generated methods

-export([create_report/2]).
-export([create_report/3]).

-export([get_report/2]).
-export([get_report/3]).

-export([get_reports/2]).
-export([get_reports/3]).


-spec create_report(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_report(Endpoint, Params) ->
    create_report(Endpoint, Params, []).

-spec create_report(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_report(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities/:identityID/reports"),
        Params,
        get_request_spec(create_report),
        Opts
    ), create_report).

-spec get_report(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_report(Endpoint, Params) ->
    get_report(Endpoint, Params, []).

-spec get_report(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_report(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities/:identityID/reports/:reportID"),
        Params,
        get_request_spec(get_report),
        Opts
    ), get_report).

-spec get_reports(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_reports(Endpoint, Params) ->
    get_reports(Endpoint, Params, []).

-spec get_reports(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_reports(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities/:identityID/reports"),
        Params,
        get_request_spec(get_reports),
        Opts
    ), get_reports).

process_response({ok, Code, Headers, RespBody}, OperationID) ->
    try swag_client_wallet_procession:process_response(
        get_response_spec(OperationID, Code),
        RespBody
    ) of
        {ok, Resp} ->
            {ok, Code, Headers, Resp};
        Error ->
            Error
    catch
        error:invalid_response_code ->
            {error, {invalid_response_code, Code}}
    end;
process_response(Error, _) ->
    Error.


-spec get_request_spec(OperationID :: swag_client_wallet:operation_id()) ->
    Spec :: swag_client_wallet_procession:request_spec().

get_request_spec('create_report') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'identityID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'ReportParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_report') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'identityID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'reportID', #{
            source => binding,
            rules  => [{type, 'int64'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_reports') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'identityID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'datetime'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'datetime'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'type', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['withdrawalRegistry']}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client_wallet:operation_id(), Code :: swag_client_wallet_procession:code()) ->
    Spec :: swag_client_wallet_procession:response_spec() | no_return().


get_response_spec('create_report', 201) ->
    {'Report', 'Report'};

get_response_spec('create_report', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('create_report', 401) ->
    undefined;

get_response_spec('get_report', 200) ->
    {'Report', 'Report'};

get_response_spec('get_report', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_report', 401) ->
    undefined;

get_response_spec('get_report', 404) ->
    undefined;

get_response_spec('get_reports', 200) ->
    {'list', 'Report'};

get_response_spec('get_reports', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_reports', 401) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
