%% -*- mode: erlang -*-
-module(swag_client_wallet_deposits_api).

%% generated methods

-export([list_deposits/2]).
-export([list_deposits/3]).


-spec list_deposits(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_deposits(Endpoint, Params) ->
    list_deposits(Endpoint, Params, []).

-spec list_deposits(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_deposits(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/deposits"),
        Params,
        get_request_spec(list_deposits),
        Opts
    ), list_deposits).

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

get_request_spec('list_deposits') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive},true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'walletID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'identityID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'depositID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 50}, {min_length, 1}, true
, {required, false}]
        }},
        {'sourceID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'status', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['Pending', 'Succeeded', 'Failed']}, true
, {required, false}]
        }},
        {'createdAtFrom', #{
            source => qs_val,
            rules  => [{type, 'datetime'}, true
, {required, false}]
        }},
        {'createdAtTo', #{
            source => qs_val,
            rules  => [{type, 'datetime'}, true
, {required, false}]
        }},
        {'amountFrom', #{
            source => qs_val,
            rules  => [{type, 'int64'}, true
, {required, false}]
        }},
        {'amountTo', #{
            source => qs_val,
            rules  => [{type, 'int64'}, true
, {required, false}]
        }},
        {'currencyID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[A-Z]{3}$"}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client_wallet:operation_id(), Code :: swag_client_wallet_procession:code()) ->
    Spec :: swag_client_wallet_procession:response_spec() | no_return().


get_response_spec('list_deposits', 200) ->
    {'inline_response_200', 'inline_response_200'};

get_response_spec('list_deposits', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('list_deposits', 401) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
