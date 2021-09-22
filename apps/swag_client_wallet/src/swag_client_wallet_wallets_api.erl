%% -*- mode: erlang -*-
-module(swag_client_wallet_wallets_api).

%% generated methods

-export([create_wallet/2]).
-export([create_wallet/3]).

-export([get_wallet/2]).
-export([get_wallet/3]).

-export([get_wallet_account/2]).
-export([get_wallet_account/3]).

-export([get_wallet_by_external_id/2]).
-export([get_wallet_by_external_id/3]).

-export([issue_wallet_grant/2]).
-export([issue_wallet_grant/3]).

-export([list_wallets/2]).
-export([list_wallets/3]).


-spec create_wallet(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_wallet(Endpoint, Params) ->
    create_wallet(Endpoint, Params, []).

-spec create_wallet(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_wallet(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/wallets"),
        Params,
        get_request_spec(create_wallet),
        Opts
    ), create_wallet).

-spec get_wallet(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_wallet(Endpoint, Params) ->
    get_wallet(Endpoint, Params, []).

-spec get_wallet(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_wallet(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/wallets/:walletID"),
        Params,
        get_request_spec(get_wallet),
        Opts
    ), get_wallet).

-spec get_wallet_account(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_wallet_account(Endpoint, Params) ->
    get_wallet_account(Endpoint, Params, []).

-spec get_wallet_account(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_wallet_account(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/wallets/:walletID/account"),
        Params,
        get_request_spec(get_wallet_account),
        Opts
    ), get_wallet_account).

-spec get_wallet_by_external_id(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_wallet_by_external_id(Endpoint, Params) ->
    get_wallet_by_external_id(Endpoint, Params, []).

-spec get_wallet_by_external_id(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_wallet_by_external_id(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/external/wallets"),
        Params,
        get_request_spec(get_wallet_by_external_id),
        Opts
    ), get_wallet_by_external_id).

-spec issue_wallet_grant(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
issue_wallet_grant(Endpoint, Params) ->
    issue_wallet_grant(Endpoint, Params, []).

-spec issue_wallet_grant(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
issue_wallet_grant(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/wallets/:walletID/grants"),
        Params,
        get_request_spec(issue_wallet_grant),
        Opts
    ), issue_wallet_grant).

-spec list_wallets(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_wallets(Endpoint, Params) ->
    list_wallets(Endpoint, Params, []).

-spec list_wallets(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_wallets(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/wallets"),
        Params,
        get_request_spec(list_wallets),
        Opts
    ), list_wallets).

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

get_request_spec('create_wallet') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'Wallet', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_wallet') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'walletID', #{
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
get_request_spec('get_wallet_account') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'walletID', #{
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
get_request_spec('get_wallet_by_external_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'externalID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('issue_wallet_grant') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'walletID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'WalletGrantRequest', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('list_wallets') ->
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
        {'identityID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
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


get_response_spec('create_wallet', 201) ->
    {'Wallet', 'Wallet'};

get_response_spec('create_wallet', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('create_wallet', 401) ->
    undefined;

get_response_spec('create_wallet', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('create_wallet', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('get_wallet', 200) ->
    {'Wallet', 'Wallet'};

get_response_spec('get_wallet', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_wallet', 401) ->
    undefined;

get_response_spec('get_wallet', 404) ->
    undefined;

get_response_spec('get_wallet_account', 200) ->
    {'WalletAccount', 'WalletAccount'};

get_response_spec('get_wallet_account', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_wallet_account', 401) ->
    undefined;

get_response_spec('get_wallet_account', 404) ->
    undefined;

get_response_spec('get_wallet_by_external_id', 200) ->
    {'Wallet', 'Wallet'};

get_response_spec('get_wallet_by_external_id', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_wallet_by_external_id', 401) ->
    undefined;

get_response_spec('get_wallet_by_external_id', 404) ->
    undefined;

get_response_spec('issue_wallet_grant', 201) ->
    {'WalletGrantRequest', 'WalletGrantRequest'};

get_response_spec('issue_wallet_grant', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('issue_wallet_grant', 401) ->
    undefined;

get_response_spec('issue_wallet_grant', 404) ->
    undefined;

get_response_spec('issue_wallet_grant', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('list_wallets', 200) ->
    {'inline_response_200_4', 'inline_response_200_4'};

get_response_spec('list_wallets', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('list_wallets', 401) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
