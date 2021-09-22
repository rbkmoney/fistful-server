%% -*- mode: erlang -*-
-module(swag_client_wallet_identities_api).

%% generated methods

-export([create_identity/2]).
-export([create_identity/3]).

-export([get_identity/2]).
-export([get_identity/3]).

-export([get_identity_challenge/2]).
-export([get_identity_challenge/3]).

-export([get_identity_challenge_event/2]).
-export([get_identity_challenge_event/3]).

-export([list_identities/2]).
-export([list_identities/3]).

-export([list_identity_challenges/2]).
-export([list_identity_challenges/3]).

-export([poll_identity_challenge_events/2]).
-export([poll_identity_challenge_events/3]).

-export([start_identity_challenge/2]).
-export([start_identity_challenge/3]).


-spec create_identity(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_identity(Endpoint, Params) ->
    create_identity(Endpoint, Params, []).

-spec create_identity(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_identity(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities"),
        Params,
        get_request_spec(create_identity),
        Opts
    ), create_identity).

-spec get_identity(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_identity(Endpoint, Params) ->
    get_identity(Endpoint, Params, []).

-spec get_identity(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_identity(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities/:identityID"),
        Params,
        get_request_spec(get_identity),
        Opts
    ), get_identity).

-spec get_identity_challenge(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_identity_challenge(Endpoint, Params) ->
    get_identity_challenge(Endpoint, Params, []).

-spec get_identity_challenge(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_identity_challenge(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities/:identityID/challenges/:challengeID"),
        Params,
        get_request_spec(get_identity_challenge),
        Opts
    ), get_identity_challenge).

-spec get_identity_challenge_event(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_identity_challenge_event(Endpoint, Params) ->
    get_identity_challenge_event(Endpoint, Params, []).

-spec get_identity_challenge_event(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_identity_challenge_event(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities/:identityID/challenges/:challengeID/events/:eventID"),
        Params,
        get_request_spec(get_identity_challenge_event),
        Opts
    ), get_identity_challenge_event).

-spec list_identities(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_identities(Endpoint, Params) ->
    list_identities(Endpoint, Params, []).

-spec list_identities(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_identities(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities"),
        Params,
        get_request_spec(list_identities),
        Opts
    ), list_identities).

-spec list_identity_challenges(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_identity_challenges(Endpoint, Params) ->
    list_identity_challenges(Endpoint, Params, []).

-spec list_identity_challenges(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_identity_challenges(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities/:identityID/challenges"),
        Params,
        get_request_spec(list_identity_challenges),
        Opts
    ), list_identity_challenges).

-spec poll_identity_challenge_events(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
poll_identity_challenge_events(Endpoint, Params) ->
    poll_identity_challenge_events(Endpoint, Params, []).

-spec poll_identity_challenge_events(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
poll_identity_challenge_events(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        get,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities/:identityID/challenges/:challengeID/events"),
        Params,
        get_request_spec(poll_identity_challenge_events),
        Opts
    ), poll_identity_challenge_events).

-spec start_identity_challenge(Endpoint :: swag_client_wallet:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
start_identity_challenge(Endpoint, Params) ->
    start_identity_challenge(Endpoint, Params, []).

-spec start_identity_challenge(Endpoint :: swag_client_wallet:endpoint(), Params :: map(), Opts :: swag_client_wallet:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
start_identity_challenge(Endpoint, Params, Opts) ->
    process_response(swag_client_wallet_procession:process_request(
        post,
        swag_client_wallet_utils:get_url(Endpoint, "/wallet/v0/identities/:identityID/challenges"),
        Params,
        get_request_spec(start_identity_challenge),
        Opts
    ), start_identity_challenge).

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

get_request_spec('create_identity') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'Identity', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_identity') ->
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
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_identity_challenge') ->
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
        {'challengeID', #{
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
get_request_spec('get_identity_challenge_event') ->
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
        {'challengeID', #{
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
get_request_spec('list_identities') ->
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
        {'providerID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'class', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'level', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ];
get_request_spec('list_identity_challenges') ->
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
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'status', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['Pending', 'Completed', 'Failed', 'Cancelled']}, true
, {required, false}]
        }}
    ];
get_request_spec('poll_identity_challenge_events') ->
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
        {'challengeID', #{
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
    ];
get_request_spec('start_identity_challenge') ->
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
        {'IdentityChallenge', #{
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


get_response_spec('create_identity', 201) ->
    {'Identity', 'Identity'};

get_response_spec('create_identity', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('create_identity', 401) ->
    undefined;

get_response_spec('create_identity', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('create_identity', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('get_identity', 200) ->
    {'Identity', 'Identity'};

get_response_spec('get_identity', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_identity', 401) ->
    undefined;

get_response_spec('get_identity', 404) ->
    undefined;

get_response_spec('get_identity_challenge', 200) ->
    {'IdentityChallenge', 'IdentityChallenge'};

get_response_spec('get_identity_challenge', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_identity_challenge', 401) ->
    undefined;

get_response_spec('get_identity_challenge', 404) ->
    undefined;

get_response_spec('get_identity_challenge_event', 200) ->
    {'IdentityChallengeEvent', 'IdentityChallengeEvent'};

get_response_spec('get_identity_challenge_event', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_identity_challenge_event', 401) ->
    undefined;

get_response_spec('get_identity_challenge_event', 404) ->
    undefined;

get_response_spec('list_identities', 200) ->
    {'inline_response_200_2', 'inline_response_200_2'};

get_response_spec('list_identities', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('list_identities', 401) ->
    undefined;

get_response_spec('list_identity_challenges', 200) ->
    {'list', 'IdentityChallenge'};

get_response_spec('list_identity_challenges', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('list_identity_challenges', 401) ->
    undefined;

get_response_spec('list_identity_challenges', 404) ->
    undefined;

get_response_spec('poll_identity_challenge_events', 200) ->
    {'list', 'IdentityChallengeEvent'};

get_response_spec('poll_identity_challenge_events', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('poll_identity_challenge_events', 401) ->
    undefined;

get_response_spec('poll_identity_challenge_events', 404) ->
    undefined;

get_response_spec('start_identity_challenge', 202) ->
    {'IdentityChallenge', 'IdentityChallenge'};

get_response_spec('start_identity_challenge', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('start_identity_challenge', 401) ->
    undefined;

get_response_spec('start_identity_challenge', 404) ->
    undefined;

get_response_spec('start_identity_challenge', 409) ->
    undefined;

get_response_spec('start_identity_challenge', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec(_, _) ->
    error(invalid_response_code).
