%% -*- mode: erlang -*-
-module(swag_client_wallet_p2_p_api).

%% generated methods

-export([create_p2_p_transfer/2]).
-export([create_p2_p_transfer/3]).

-export([get_p2_p_transfer/2]).
-export([get_p2_p_transfer/3]).

-export([get_p2_p_transfer_events/2]).
-export([get_p2_p_transfer_events/3]).

-export([quote_p2_p_transfer/2]).
-export([quote_p2_p_transfer/3]).


-spec create_p2_p_transfer(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_p2_p_transfer(Endpoint, Params) ->
    create_p2_p_transfer(Endpoint, Params, []).

-spec create_p2_p_transfer(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_p2_p_transfer(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/p2p/transfers"),
        Params,
        get_request_spec(create_p2_p_transfer),
        Opts
    ), create_p2_p_transfer).

-spec get_p2_p_transfer(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_p2_p_transfer(Endpoint, Params) ->
    get_p2_p_transfer(Endpoint, Params, []).

-spec get_p2_p_transfer(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_p2_p_transfer(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/p2p/transfers/:p2pTransferID"),
        Params,
        get_request_spec(get_p2_p_transfer),
        Opts
    ), get_p2_p_transfer).

-spec get_p2_p_transfer_events(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_p2_p_transfer_events(Endpoint, Params) ->
    get_p2_p_transfer_events(Endpoint, Params, []).

-spec get_p2_p_transfer_events(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_p2_p_transfer_events(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/p2p/transfers/:p2pTransferID/events"),
        Params,
        get_request_spec(get_p2_p_transfer_events),
        Opts
    ), get_p2_p_transfer_events).

-spec quote_p2_p_transfer(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
quote_p2_p_transfer(Endpoint, Params) ->
    quote_p2_p_transfer(Endpoint, Params, []).

-spec quote_p2_p_transfer(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
quote_p2_p_transfer(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/p2p/quotes"),
        Params,
        get_request_spec(quote_p2_p_transfer),
        Opts
    ), quote_p2_p_transfer).

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

get_request_spec('create_p2_p_transfer') ->
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
        {'P2PTransferParameters', #{
            source => body,
            rules  => [schema, {required, false}]
        }}
    ];
get_request_spec('get_p2_p_transfer') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'p2pTransferID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_p2_p_transfer_events') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'p2pTransferID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ];
get_request_spec('quote_p2_p_transfer') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'QuoteParameters', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client_wallet:operation_id(), Code :: swag_client_wallet_procession:code()) ->
    Spec :: swag_client_wallet_procession:response_spec() | no_return().


get_response_spec('create_p2_p_transfer', 202) ->
    {'P2PTransfer', 'P2PTransfer'};

get_response_spec('create_p2_p_transfer', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('create_p2_p_transfer', 401) ->
    undefined;

get_response_spec('create_p2_p_transfer', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('create_p2_p_transfer', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('get_p2_p_transfer', 200) ->
    {'P2PTransfer', 'P2PTransfer'};

get_response_spec('get_p2_p_transfer', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_p2_p_transfer', 401) ->
    undefined;

get_response_spec('get_p2_p_transfer', 404) ->
    undefined;

get_response_spec('get_p2_p_transfer_events', 200) ->
    {'inline_response_200_3', 'inline_response_200_3'};

get_response_spec('get_p2_p_transfer_events', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_p2_p_transfer_events', 401) ->
    undefined;

get_response_spec('get_p2_p_transfer_events', 404) ->
    undefined;

get_response_spec('quote_p2_p_transfer', 201) ->
    {'P2PTransferQuote', 'P2PTransferQuote'};

get_response_spec('quote_p2_p_transfer', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('quote_p2_p_transfer', 401) ->
    undefined;

get_response_spec('quote_p2_p_transfer', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec(_, _) ->
    error(invalid_response_code).
