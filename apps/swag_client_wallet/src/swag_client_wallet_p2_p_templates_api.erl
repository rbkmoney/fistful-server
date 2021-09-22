%% -*- mode: erlang -*-
-module(swag_client_wallet_p2_p_templates_api).

%% generated methods

-export([block_p2_p_transfer_template/2]).
-export([block_p2_p_transfer_template/3]).

-export([create_p2_p_transfer_template/2]).
-export([create_p2_p_transfer_template/3]).

-export([create_p2_p_transfer_with_template/2]).
-export([create_p2_p_transfer_with_template/3]).

-export([get_p2_p_transfer_template_by_id/2]).
-export([get_p2_p_transfer_template_by_id/3]).

-export([issue_p2_p_transfer_template_access_token/2]).
-export([issue_p2_p_transfer_template_access_token/3]).

-export([issue_p2_p_transfer_ticket/2]).
-export([issue_p2_p_transfer_ticket/3]).

-export([quote_p2_p_transfer_with_template/2]).
-export([quote_p2_p_transfer_with_template/3]).


-spec block_p2_p_transfer_template(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
block_p2_p_transfer_template(Endpoint, Params) ->
    block_p2_p_transfer_template(Endpoint, Params, []).

-spec block_p2_p_transfer_template(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
block_p2_p_transfer_template(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/p2p/transfer/templates/:p2pTransferTemplateID/block"),
        Params,
        get_request_spec(block_p2_p_transfer_template),
        Opts
    ), block_p2_p_transfer_template).

-spec create_p2_p_transfer_template(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_p2_p_transfer_template(Endpoint, Params) ->
    create_p2_p_transfer_template(Endpoint, Params, []).

-spec create_p2_p_transfer_template(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_p2_p_transfer_template(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/p2p/transfer/templates"),
        Params,
        get_request_spec(create_p2_p_transfer_template),
        Opts
    ), create_p2_p_transfer_template).

-spec create_p2_p_transfer_with_template(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_p2_p_transfer_with_template(Endpoint, Params) ->
    create_p2_p_transfer_with_template(Endpoint, Params, []).

-spec create_p2_p_transfer_with_template(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_p2_p_transfer_with_template(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/p2p/transfer/templates/:p2pTransferTemplateID/transfers"),
        Params,
        get_request_spec(create_p2_p_transfer_with_template),
        Opts
    ), create_p2_p_transfer_with_template).

-spec get_p2_p_transfer_template_by_id(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_p2_p_transfer_template_by_id(Endpoint, Params) ->
    get_p2_p_transfer_template_by_id(Endpoint, Params, []).

-spec get_p2_p_transfer_template_by_id(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_p2_p_transfer_template_by_id(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/p2p/transfer/templates/:p2pTransferTemplateID"),
        Params,
        get_request_spec(get_p2_p_transfer_template_by_id),
        Opts
    ), get_p2_p_transfer_template_by_id).

-spec issue_p2_p_transfer_template_access_token(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
issue_p2_p_transfer_template_access_token(Endpoint, Params) ->
    issue_p2_p_transfer_template_access_token(Endpoint, Params, []).

-spec issue_p2_p_transfer_template_access_token(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
issue_p2_p_transfer_template_access_token(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/p2p/transfer/templates/:p2pTransferTemplateID/access-tokens"),
        Params,
        get_request_spec(issue_p2_p_transfer_template_access_token),
        Opts
    ), issue_p2_p_transfer_template_access_token).

-spec issue_p2_p_transfer_ticket(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
issue_p2_p_transfer_ticket(Endpoint, Params) ->
    issue_p2_p_transfer_ticket(Endpoint, Params, []).

-spec issue_p2_p_transfer_ticket(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
issue_p2_p_transfer_ticket(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/p2p/transfer/templates/:p2pTransferTemplateID/tickets"),
        Params,
        get_request_spec(issue_p2_p_transfer_ticket),
        Opts
    ), issue_p2_p_transfer_ticket).

-spec quote_p2_p_transfer_with_template(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
quote_p2_p_transfer_with_template(Endpoint, Params) ->
    quote_p2_p_transfer_with_template(Endpoint, Params, []).

-spec quote_p2_p_transfer_with_template(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
quote_p2_p_transfer_with_template(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/p2p/transfer/templates/:p2pTransferTemplateID/quotes"),
        Params,
        get_request_spec(quote_p2_p_transfer_with_template),
        Opts
    ), quote_p2_p_transfer_with_template).

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

get_request_spec('block_p2_p_transfer_template') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'p2pTransferTemplateID', #{
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
get_request_spec('create_p2_p_transfer_template') ->
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
        {'P2PTransferTemplateParameters', #{
            source => body,
            rules  => [schema, {required, false}]
        }}
    ];
get_request_spec('create_p2_p_transfer_with_template') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'p2pTransferTemplateID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'P2PTransferWithTemplateParameters', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_p2_p_transfer_template_by_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'p2pTransferTemplateID', #{
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
get_request_spec('issue_p2_p_transfer_template_access_token') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'p2pTransferTemplateID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'P2PTransferTemplateTokenRequest', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('issue_p2_p_transfer_ticket') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'p2pTransferTemplateID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'P2PTransferTemplateTicketRequest', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('quote_p2_p_transfer_with_template') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'p2pTransferTemplateID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'P2PTransferTemplateQuoteParameters', #{
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


get_response_spec('block_p2_p_transfer_template', 204) ->
    undefined;

get_response_spec('block_p2_p_transfer_template', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('block_p2_p_transfer_template', 401) ->
    undefined;

get_response_spec('block_p2_p_transfer_template', 404) ->
    undefined;

get_response_spec('block_p2_p_transfer_template', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('create_p2_p_transfer_template', 201) ->
    {'P2PTransferTemplate', 'P2PTransferTemplate'};

get_response_spec('create_p2_p_transfer_template', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('create_p2_p_transfer_template', 401) ->
    undefined;

get_response_spec('create_p2_p_transfer_template', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('create_p2_p_transfer_template', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('create_p2_p_transfer_with_template', 202) ->
    {'P2PTransfer', 'P2PTransfer'};

get_response_spec('create_p2_p_transfer_with_template', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('create_p2_p_transfer_with_template', 401) ->
    undefined;

get_response_spec('create_p2_p_transfer_with_template', 404) ->
    undefined;

get_response_spec('create_p2_p_transfer_with_template', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('create_p2_p_transfer_with_template', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('get_p2_p_transfer_template_by_id', 200) ->
    {'P2PTransferTemplate', 'P2PTransferTemplate'};

get_response_spec('get_p2_p_transfer_template_by_id', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_p2_p_transfer_template_by_id', 401) ->
    undefined;

get_response_spec('get_p2_p_transfer_template_by_id', 404) ->
    undefined;

get_response_spec('issue_p2_p_transfer_template_access_token', 201) ->
    {'P2PTransferTemplateTokenRequest', 'P2PTransferTemplateTokenRequest'};

get_response_spec('issue_p2_p_transfer_template_access_token', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('issue_p2_p_transfer_template_access_token', 401) ->
    undefined;

get_response_spec('issue_p2_p_transfer_template_access_token', 404) ->
    undefined;

get_response_spec('issue_p2_p_transfer_template_access_token', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('issue_p2_p_transfer_ticket', 201) ->
    {'P2PTransferTemplateTokenRequest', 'P2PTransferTemplateTokenRequest'};

get_response_spec('issue_p2_p_transfer_ticket', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('issue_p2_p_transfer_ticket', 401) ->
    undefined;

get_response_spec('issue_p2_p_transfer_ticket', 404) ->
    undefined;

get_response_spec('issue_p2_p_transfer_ticket', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('quote_p2_p_transfer_with_template', 201) ->
    {'P2PTransferQuote', 'P2PTransferQuote'};

get_response_spec('quote_p2_p_transfer_with_template', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('quote_p2_p_transfer_with_template', 401) ->
    undefined;

get_response_spec('quote_p2_p_transfer_with_template', 404) ->
    undefined;

get_response_spec('quote_p2_p_transfer_with_template', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec(_, _) ->
    error(invalid_response_code).
