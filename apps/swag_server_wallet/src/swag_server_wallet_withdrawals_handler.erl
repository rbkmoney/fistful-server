%% -*- mode: erlang -*-

%% basic handler
-module(swag_server_wallet_withdrawals_handler).

%% Cowboy REST callbacks
-export([allowed_methods/2]).
-export([init/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([known_content_type/2]).
-export([malformed_request/2]).
-export([valid_content_headers/2]).
-export([valid_entity_length/2]).

%% Handlers
-export([handle_request_json/2]).

-record(state, {
    operation_id  :: swag_server_wallet:operation_id(),
    logic_handler :: module(),
    swagger_handler_opts :: swag_server_wallet_router:swagger_handler_opts(),
    context       :: swag_server_wallet:request_context()
}).

-type state()              :: state().
-type content_type()       :: {binary(), binary(), '*' | [{binary(), binary()}]}.
-type processed_response() :: {stop, cowboy_req:req(), state()}.

%% Cowboy REST callbacks

-spec init(Req :: cowboy_req:req(), Opts :: swag_server_wallet_router:init_opts()) ->
    {cowboy_rest, Req :: cowboy_req:req(), State :: state()}.

init(Req, {_Operations, LogicHandler, SwaggerHandlerOpts} = InitOpts) ->
    OperationID    = swag_server_wallet_utils:get_operation_id(Req, InitOpts),

    error_logger:info_msg("Attempt to process operation: ~p", [OperationID]),

    State = #state{
        operation_id  = OperationID,
        logic_handler = LogicHandler,
        swagger_handler_opts = SwaggerHandlerOpts,
        context       = #{cowboy_req => Req}
    },
    {cowboy_rest, Req, State}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: [binary()], Req :: cowboy_req:req(), State :: state()}.


allowed_methods(
    Req,
    State = #state{
        operation_id = 'CreateDestination'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'CreateQuote'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'CreateWithdrawal'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetDestination'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetDestinationByExternalID'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetWithdrawal'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetWithdrawalByExternalID'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetWithdrawalEvents'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'IssueDestinationGrant'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'ListDestinations'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'ListWithdrawals'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'PollWithdrawalEvents'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: true | {false, AuthHeader :: iodata()},
        Req   :: cowboy_req:req(),
        State :: state()
    }.

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'CreateDestination' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_wallet_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'CreateQuote' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_wallet_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'CreateWithdrawal' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_wallet_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'GetDestination' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_wallet_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'GetDestinationByExternalID' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_wallet_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'GetWithdrawal' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_wallet_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'GetWithdrawalByExternalID' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_wallet_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'GetWithdrawalEvents' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_wallet_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'IssueDestinationGrant' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_wallet_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'ListDestinations' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_wallet_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'ListWithdrawals' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_wallet_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'PollWithdrawalEvents' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_wallet_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(Req, State) ->
    {{false, <<"">>}, Req, State}.

-spec content_types_accepted(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{content_type(), AcceptResource :: atom()}],
        Req   :: cowboy_req:req(),
        State :: state()
    }.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, [{<<"charset">>, <<"utf-8">>}]}, handle_request_json}
    ], Req, State}.

-spec valid_content_headers(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'CreateDestination'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'CreateQuote'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'CreateWithdrawal'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetDestination'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetDestinationByExternalID'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetWithdrawal'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetWithdrawalByExternalID'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetWithdrawalEvents'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'IssueDestinationGrant'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'ListDestinations'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'ListWithdrawals'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'PollWithdrawalEvents'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{content_type(), ProvideResource :: atom()}],
        Req   :: cowboy_req:req(),
        State :: state()
    }.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_request_json}
    ], Req, State}.

-spec charsets_provided(Req :: cowboy_req:req(), State :: state()) ->
    {Charsets :: [binary()], Req :: cowboy_req:req(), State :: state()}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

-spec malformed_request(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

malformed_request(Req, State = #state{context = Context}) ->
    PeerResult = swag_server_wallet_handler_api:determine_peer(Req),
    case PeerResult of
        {ok, Peer} ->
            Context1 = Context#{peer => Peer},
            State1   = State#state{context = Context1},
            {false, Req, State1};
        {error, Reason} ->
            error_logger:error_msg("Unable to determine client peer: ~p", [Reason]),
            {true, Req, State}
    end.

-spec allow_missing_post(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: false, Req :: cowboy_req:req(), State :: state()}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

-spec delete_resource(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

delete_resource(Req, State) ->
    handle_request_json(Req, State).

-spec known_content_type(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

known_content_type(Req, State) ->
    {true, Req, State}.

-spec valid_entity_length(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

valid_entity_length(Req, State) ->
    %% @TODO check the length
    {true, Req, State}.


%% Handlers

-spec handle_request_json(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

handle_request_json(
    Req0,
    State = #state{
        operation_id  = OperationID,
        logic_handler = LogicHandler,
        swagger_handler_opts = SwaggerHandlerOpts,
        context       = Context
    }
) ->
    ValidationOpts = maps:get(validation_opts, SwaggerHandlerOpts, #{}),
    case populate_request(LogicHandler, OperationID, Req0, ValidationOpts) of
        {ok, Populated, Req1} ->
            {Status, Resp} = handle_request(LogicHandler, OperationID, Populated, Context),
            ok = validate_response(Status, Resp, OperationID, ValidationOpts),
            process_response(ok, encode_response(Resp), Req1, State);
        {error, Reason, Req1} ->
            process_response(error, Reason, Req1, State)
    end.


%% Internal

populate_request(LogicHandler, OperationID, Req, ValidationOpts) ->
    Spec = get_request_spec(OperationID),
    swag_server_wallet_handler_api:populate_request(LogicHandler, OperationID, Spec, Req, ValidationOpts).

handle_request(LogicHandler, OperationID, Populated, Context) ->
    swag_server_wallet_logic_handler:handle_request(LogicHandler, OperationID, Populated, Context).

validate_response(error, _, _, _) ->
    ok;
validate_response(ok, {Code, _Headers, Body}, OperationID, ValidationOpts) ->
    Spec = get_response_spec(OperationID, Code),
    swag_server_wallet_handler_api:validate_response(OperationID, Spec, Body, ValidationOpts).

encode_response(Resp) ->
    swag_server_wallet_handler_api:encode_response(Resp).

process_response(Status, Result, Req0, State = #state{operation_id = OperationID}) ->
    Req = swag_server_wallet_handler_api:process_response(Status, Result, Req0, OperationID),
    {stop, Req, State}.

validate_headers(_, Req) ->
    {true, Req}.

-spec get_request_spec(OperationID :: swag_server_wallet:operation_id()) ->
    Spec :: swag_server_wallet_handler_api:request_spec() | no_return().


get_request_spec('CreateDestination') ->
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

get_request_spec('CreateQuote') ->
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

get_request_spec('CreateWithdrawal') ->
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

get_request_spec('GetDestination') ->
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

get_request_spec('GetDestinationByExternalID') ->
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

get_request_spec('GetWithdrawal') ->
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

get_request_spec('GetWithdrawalByExternalID') ->
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

get_request_spec('GetWithdrawalEvents') ->
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
            rules  => [{type, 'integer'}, {format, 'int32'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];

get_request_spec('IssueDestinationGrant') ->
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

get_request_spec('ListDestinations') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
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

get_request_spec('ListWithdrawals') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
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
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, false}]
        }},
        {'createdAtTo', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, false}]
        }},
        {'amountFrom', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, false}]
        }},
        {'amountTo', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
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

get_request_spec('PollWithdrawalEvents') ->
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
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'eventCursor', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, true
, {required, false}]
        }}
    ];

get_request_spec(OperationID) ->
    error({invalid_operation_id, OperationID}).

-spec get_response_spec(OperationID :: swag_server_wallet:operation_id(), Code :: cowboy:http_status()) ->
    Spec :: swag_server_wallet_handler_api:response_spec() | no_return().


get_response_spec('CreateDestination', 201) ->
    {'Destination', 'Destination'};

get_response_spec('CreateDestination', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('CreateDestination', 401) ->
    undefined;

get_response_spec('CreateDestination', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('CreateDestination', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('CreateQuote', 202) ->
    {'WithdrawalQuote', 'WithdrawalQuote'};

get_response_spec('CreateQuote', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('CreateQuote', 401) ->
    undefined;

get_response_spec('CreateQuote', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('CreateQuote', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('CreateWithdrawal', 202) ->
    {'Withdrawal', 'Withdrawal'};

get_response_spec('CreateWithdrawal', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('CreateWithdrawal', 401) ->
    undefined;

get_response_spec('CreateWithdrawal', 409) ->
    {'ConflictRequest', 'ConflictRequest'};

get_response_spec('CreateWithdrawal', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('GetDestination', 200) ->
    {'Destination', 'Destination'};

get_response_spec('GetDestination', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('GetDestination', 401) ->
    undefined;

get_response_spec('GetDestination', 404) ->
    undefined;

get_response_spec('GetDestinationByExternalID', 200) ->
    {'Destination', 'Destination'};

get_response_spec('GetDestinationByExternalID', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('GetDestinationByExternalID', 401) ->
    undefined;

get_response_spec('GetDestinationByExternalID', 404) ->
    undefined;

get_response_spec('GetWithdrawal', 200) ->
    {'Withdrawal', 'Withdrawal'};

get_response_spec('GetWithdrawal', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('GetWithdrawal', 401) ->
    undefined;

get_response_spec('GetWithdrawal', 404) ->
    undefined;

get_response_spec('GetWithdrawalByExternalID', 200) ->
    {'Withdrawal', 'Withdrawal'};

get_response_spec('GetWithdrawalByExternalID', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('GetWithdrawalByExternalID', 401) ->
    undefined;

get_response_spec('GetWithdrawalByExternalID', 404) ->
    undefined;

get_response_spec('GetWithdrawalEvents', 200) ->
    {'WithdrawalEvent', 'WithdrawalEvent'};

get_response_spec('GetWithdrawalEvents', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('GetWithdrawalEvents', 401) ->
    undefined;

get_response_spec('GetWithdrawalEvents', 404) ->
    undefined;

get_response_spec('IssueDestinationGrant', 201) ->
    {'DestinationGrantRequest', 'DestinationGrantRequest'};

get_response_spec('IssueDestinationGrant', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('IssueDestinationGrant', 401) ->
    undefined;

get_response_spec('IssueDestinationGrant', 404) ->
    undefined;

get_response_spec('IssueDestinationGrant', 422) ->
    {'InvalidOperationParameters', 'InvalidOperationParameters'};

get_response_spec('ListDestinations', 200) ->
    {'inline_response_200_1', 'inline_response_200_1'};

get_response_spec('ListDestinations', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('ListDestinations', 401) ->
    undefined;

get_response_spec('ListWithdrawals', 200) ->
    {'inline_response_200_5', 'inline_response_200_5'};

get_response_spec('ListWithdrawals', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('ListWithdrawals', 401) ->
    undefined;

get_response_spec('PollWithdrawalEvents', 200) ->
    {'list', 'WithdrawalEvent'};

get_response_spec('PollWithdrawalEvents', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('PollWithdrawalEvents', 401) ->
    undefined;

get_response_spec('PollWithdrawalEvents', 404) ->
    undefined;

get_response_spec(OperationID, Code) ->
    error({invalid_response_code, OperationID, Code}).
