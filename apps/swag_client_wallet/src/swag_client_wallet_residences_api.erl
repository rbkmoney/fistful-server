%% -*- mode: erlang -*-
-module(swag_client_wallet_residences_api).

%% generated methods

-export([get_residence/2]).
-export([get_residence/3]).


-spec get_residence(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_residence(Endpoint, Params) ->
    get_residence(Endpoint, Params, []).

-spec get_residence(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_residence(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/residences/:residence"),
        Params,
        get_request_spec(get_residence),
        Opts
    ), get_residence).

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

get_request_spec('get_residence') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'residence', #{
            source => binding,
            rules  => [{type, 'binary'}, {pattern, "^[A-Za-z]{3}$"}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client_wallet:operation_id(), Code :: swag_client_wallet_procession:code()) ->
    Spec :: swag_client_wallet_procession:response_spec() | no_return().


get_response_spec('get_residence', 200) ->
    {'Residence', 'Residence'};

get_response_spec('get_residence', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_residence', 401) ->
    undefined;

get_response_spec('get_residence', 404) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
