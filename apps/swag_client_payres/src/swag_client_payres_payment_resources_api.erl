%% -*- mode: erlang -*-
-module(swag_client_payres_payment_resources_api).

%% generated methods

-export([get_bank_card/2]).
-export([get_bank_card/3]).

-export([store_bank_card/2]).
-export([store_bank_card/3]).


-spec get_bank_card(Endpoint :: swag_client_payres:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_bank_card(Endpoint, Params) ->
    get_bank_card(Endpoint, Params, []).

-spec get_bank_card(Endpoint :: swag_client_payres:endpoint(), Params :: map(), Opts :: swag_client_payres:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_bank_card(Endpoint, Params, Opts) ->
    process_response(swag_client_payres_procession:process_request(
        get,
        swag_client_payres_utils:get_url(Endpoint, "/payres/v0/bank-cards/:token"),
        Params,
        get_request_spec(get_bank_card),
        Opts
    ), get_bank_card).

-spec store_bank_card(Endpoint :: swag_client_payres:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
store_bank_card(Endpoint, Params) ->
    store_bank_card(Endpoint, Params, []).

-spec store_bank_card(Endpoint :: swag_client_payres:endpoint(), Params :: map(), Opts :: swag_client_payres:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
store_bank_card(Endpoint, Params, Opts) ->
    process_response(swag_client_payres_procession:process_request(
        post,
        swag_client_payres_utils:get_url(Endpoint, "/payres/v0/bank-cards"),
        Params,
        get_request_spec(store_bank_card),
        Opts
    ), store_bank_card).

process_response({ok, Code, Headers, RespBody}, OperationID) ->
    try swag_client_payres_procession:process_response(
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


-spec get_request_spec(OperationID :: swag_client_payres:operation_id()) ->
    Spec :: swag_client_payres_procession:request_spec().

get_request_spec('get_bank_card') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'token', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 1000}, {min_length, 1}, true
, {required, true}]
        }}
    ];
get_request_spec('store_bank_card') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'BankCard', #{
            source => body,
            rules  => [schema, {required, true}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client_payres:operation_id(), Code :: swag_client_payres_procession:code()) ->
    Spec :: swag_client_payres_procession:response_spec() | no_return().


get_response_spec('get_bank_card', 200) ->
    {'SecuredBankCard', 'SecuredBankCard'};

get_response_spec('get_bank_card', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_bank_card', 404) ->
    undefined;

get_response_spec('store_bank_card', 201) ->
    {'StoreBankCardResponse', 'StoreBankCardResponse'};

get_response_spec('store_bank_card', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('store_bank_card', 422) ->
    {'InvalidBankCard', 'InvalidBankCard'};

get_response_spec(_, _) ->
    error(invalid_response_code).
