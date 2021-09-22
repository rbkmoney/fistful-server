%% -*- mode: erlang -*-
-module(swag_client_wallet_w2_w_api).

%% generated methods

-export([create_w2_w_transfer/2]).
-export([create_w2_w_transfer/3]).

-export([get_w2_w_transfer/2]).
-export([get_w2_w_transfer/3]).


-spec create_w2_w_transfer(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_w2_w_transfer(Endpoint, Params) ->
    create_w2_w_transfer(Endpoint, Params, []).

-spec create_w2_w_transfer(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_w2_w_transfer(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/w2w/transfers"),
        Params,
        get_request_spec(create_w2_w_transfer),
        Opts
    ), create_w2_w_transfer).

-spec get_w2_w_transfer(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_w2_w_transfer(Endpoint, Params) ->
    get_w2_w_transfer(Endpoint, Params, []).

-spec get_w2_w_transfer(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_w2_w_transfer(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/w2w/transfers/:w2wTransferID"),
        Params,
        get_request_spec(get_w2_w_transfer),
        Opts
    ), get_w2_w_transfer).

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

get_request_spec('create_w2_w_transfer') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'W2WTransferParameters', #{
            source => body,
            rules  => [schema, {required, false}]
        }}
    ];
get_request_spec('get_w2_w_transfer') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'w2wTransferID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
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


get_response_spec('create_w2_w_transfer', 202) ->
    {'W2WTransfer', 'W2WTransfer'};

get_response_spec('create_w2_w_transfer', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('create_w2_w_transfer', 401) ->
    undefined;

get_response_spec('create_w2_w_transfer', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('create_w2_w_transfer', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('get_w2_w_transfer', 200) ->
    {'W2WTransfer', 'W2WTransfer'};

get_response_spec('get_w2_w_transfer', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_w2_w_transfer', 401) ->
    undefined;

get_response_spec('get_w2_w_transfer', 404) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
