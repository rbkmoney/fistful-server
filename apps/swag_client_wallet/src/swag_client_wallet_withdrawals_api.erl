%% -*- mode: erlang -*-
-module(swag_client_wallet_withdrawals_api).

%% generated methods

-export([create_destination/2]).
-export([create_destination/3]).

-export([create_quote/2]).
-export([create_quote/3]).

-export([create_withdrawal/2]).
-export([create_withdrawal/3]).

-export([get_destination/2]).
-export([get_destination/3]).

-export([get_destination_by_external_id/2]).
-export([get_destination_by_external_id/3]).

-export([get_withdrawal/2]).
-export([get_withdrawal/3]).

-export([get_withdrawal_by_external_id/2]).
-export([get_withdrawal_by_external_id/3]).

-export([get_withdrawal_events/2]).
-export([get_withdrawal_events/3]).

-export([issue_destination_grant/2]).
-export([issue_destination_grant/3]).

-export([list_destinations/2]).
-export([list_destinations/3]).

-export([list_withdrawals/2]).
-export([list_withdrawals/3]).

-export([poll_withdrawal_events/2]).
-export([poll_withdrawal_events/3]).


-spec create_destination(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_destination(Endpoint, Params) ->
    create_destination(Endpoint, Params, []).

-spec create_destination(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_destination(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/destinations"),
        Params,
        get_request_spec(create_destination),
        Opts
    ), create_destination).

-spec create_quote(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_quote(Endpoint, Params) ->
    create_quote(Endpoint, Params, []).

-spec create_quote(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_quote(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/withdrawal-quotes"),
        Params,
        get_request_spec(create_quote),
        Opts
    ), create_quote).

-spec create_withdrawal(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_withdrawal(Endpoint, Params) ->
    create_withdrawal(Endpoint, Params, []).

-spec create_withdrawal(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_withdrawal(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/withdrawals"),
        Params,
        get_request_spec(create_withdrawal),
        Opts
    ), create_withdrawal).

-spec get_destination(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_destination(Endpoint, Params) ->
    get_destination(Endpoint, Params, []).

-spec get_destination(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_destination(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/destinations/:destinationID"),
        Params,
        get_request_spec(get_destination),
        Opts
    ), get_destination).

-spec get_destination_by_external_id(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_destination_by_external_id(Endpoint, Params) ->
    get_destination_by_external_id(Endpoint, Params, []).

-spec get_destination_by_external_id(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_destination_by_external_id(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/external-ids/destinations/:externalID"),
        Params,
        get_request_spec(get_destination_by_external_id),
        Opts
    ), get_destination_by_external_id).

-spec get_withdrawal(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_withdrawal(Endpoint, Params) ->
    get_withdrawal(Endpoint, Params, []).

-spec get_withdrawal(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_withdrawal(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/withdrawals/:withdrawalID"),
        Params,
        get_request_spec(get_withdrawal),
        Opts
    ), get_withdrawal).

-spec get_withdrawal_by_external_id(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_withdrawal_by_external_id(Endpoint, Params) ->
    get_withdrawal_by_external_id(Endpoint, Params, []).

-spec get_withdrawal_by_external_id(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_withdrawal_by_external_id(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/external-ids/withdrawals/:externalID"),
        Params,
        get_request_spec(get_withdrawal_by_external_id),
        Opts
    ), get_withdrawal_by_external_id).

-spec get_withdrawal_events(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_withdrawal_events(Endpoint, Params) ->
    get_withdrawal_events(Endpoint, Params, []).

-spec get_withdrawal_events(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_withdrawal_events(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/withdrawals/:withdrawalID/events/:eventID"),
        Params,
        get_request_spec(get_withdrawal_events),
        Opts
    ), get_withdrawal_events).

-spec issue_destination_grant(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
issue_destination_grant(Endpoint, Params) ->
    issue_destination_grant(Endpoint, Params, []).

-spec issue_destination_grant(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
issue_destination_grant(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/destinations/:destinationID/grants"),
        Params,
        get_request_spec(issue_destination_grant),
        Opts
    ), issue_destination_grant).

-spec list_destinations(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_destinations(Endpoint, Params) ->
    list_destinations(Endpoint, Params, []).

-spec list_destinations(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_destinations(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/destinations"),
        Params,
        get_request_spec(list_destinations),
        Opts
    ), list_destinations).

-spec list_withdrawals(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_withdrawals(Endpoint, Params) ->
    list_withdrawals(Endpoint, Params, []).

-spec list_withdrawals(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_withdrawals(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/withdrawals"),
        Params,
        get_request_spec(list_withdrawals),
        Opts
    ), list_withdrawals).

-spec poll_withdrawal_events(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
poll_withdrawal_events(Endpoint, Params) ->
    poll_withdrawal_events(Endpoint, Params, []).

-spec poll_withdrawal_events(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
poll_withdrawal_events(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/withdrawals/:withdrawalID/events"),
        Params,
        get_request_spec(poll_withdrawal_events),
        Opts
    ), poll_withdrawal_events).

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

get_request_spec('create_destination') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'Destination', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('create_quote') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'WithdrawalQuoteParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('create_withdrawal') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'WithdrawalParameters', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_destination') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'destinationID', #{
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
get_request_spec('get_destination_by_external_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'externalID', #{
            source => binding,
            rules  => [{type, 'binary'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_withdrawal') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'withdrawalID', #{
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
get_request_spec('get_withdrawal_by_external_id') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'externalID', #{
            source => binding,
            rules  => [{type, 'binary'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_withdrawal_events') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'withdrawalID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'eventID', #{
            source => binding,
            rules  => [{type, 'int32'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('issue_destination_grant') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'destinationID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'DestinationGrantRequest', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('list_destinations') ->
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
    ];
get_request_spec('list_withdrawals') ->
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
        {'withdrawalID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'destinationID', #{
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
    ];
get_request_spec('poll_withdrawal_events') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'withdrawalID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
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
        {'eventCursor', #{
            source => qs_val,
            rules  => [{type, 'int32'}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client_wallet:operation_id(), Code :: swag_client_wallet_procession:code()) ->
    Spec :: swag_client_wallet_procession:response_spec() | no_return().


get_response_spec('create_destination', 201) ->
    {'Destination', 'Destination'};

get_response_spec('create_destination', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('create_destination', 401) ->
    undefined;

get_response_spec('create_destination', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('create_destination', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('create_quote', 202) ->
    {'WithdrawalQuote', 'WithdrawalQuote'};

get_response_spec('create_quote', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('create_quote', 401) ->
    undefined;

get_response_spec('create_quote', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('create_quote', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('create_withdrawal', 202) ->
    {'Withdrawal', 'Withdrawal'};

get_response_spec('create_withdrawal', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('create_withdrawal', 401) ->
    undefined;

get_response_spec('create_withdrawal', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('create_withdrawal', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('get_destination', 200) ->
    {'Destination', 'Destination'};

get_response_spec('get_destination', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_destination', 401) ->
    undefined;

get_response_spec('get_destination', 404) ->
    undefined;

get_response_spec('get_destination_by_external_id', 200) ->
    {'Destination', 'Destination'};

get_response_spec('get_destination_by_external_id', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_destination_by_external_id', 401) ->
    undefined;

get_response_spec('get_destination_by_external_id', 404) ->
    undefined;

get_response_spec('get_withdrawal', 200) ->
    {'Withdrawal', 'Withdrawal'};

get_response_spec('get_withdrawal', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_withdrawal', 401) ->
    undefined;

get_response_spec('get_withdrawal', 404) ->
    undefined;

get_response_spec('get_withdrawal_by_external_id', 200) ->
    {'Withdrawal', 'Withdrawal'};

get_response_spec('get_withdrawal_by_external_id', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_withdrawal_by_external_id', 401) ->
    undefined;

get_response_spec('get_withdrawal_by_external_id', 404) ->
    undefined;

get_response_spec('get_withdrawal_events', 200) ->
    {'WithdrawalEvent', 'WithdrawalEvent'};

get_response_spec('get_withdrawal_events', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_withdrawal_events', 401) ->
    undefined;

get_response_spec('get_withdrawal_events', 404) ->
    undefined;

get_response_spec('issue_destination_grant', 201) ->
    {'DestinationGrantRequest', 'DestinationGrantRequest'};

get_response_spec('issue_destination_grant', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('issue_destination_grant', 401) ->
    undefined;

get_response_spec('issue_destination_grant', 404) ->
    undefined;

get_response_spec('issue_destination_grant', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('list_destinations', 200) ->
    {'inline_response_200_1', 'inline_response_200_1'};

get_response_spec('list_destinations', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('list_destinations', 401) ->
    undefined;

get_response_spec('list_withdrawals', 200) ->
    {'inline_response_200_5', 'inline_response_200_5'};

get_response_spec('list_withdrawals', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('list_withdrawals', 401) ->
    undefined;

get_response_spec('poll_withdrawal_events', 200) ->
    {'list', 'WithdrawalEvent'};

get_response_spec('poll_withdrawal_events', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('poll_withdrawal_events', 401) ->
    undefined;

get_response_spec('poll_withdrawal_events', 404) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
