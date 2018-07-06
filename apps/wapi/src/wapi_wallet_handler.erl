-module(wapi_wallet_handler).

-behaviour(swag_server_wallet_logic_handler).
-behaviour(wapi_handler).

%% swag_server_wallet_logic_handler callbacks
-export([authorize_api_key/3]).
-export([handle_request/4]).

%% wapi_handler callbacks
-export([process_request/4]).

%% Types

-type req_data()        :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:handler_context().
-type request_result()  :: wapi_handler:request_result().
-type operation_id()    :: swag_server_wallet:operation_id().
-type api_key()         :: swag_server_wallet:api_key().
-type request_context() :: swag_server_wallet:request_context().
-type handler_opts()    :: swag_server_wallet:handler_opts().

%% API

-spec authorize_api_key(operation_id(), api_key(), handler_opts()) ->
    false | {true, wapi_auth:context()}.
authorize_api_key(OperationID, ApiKey, Opts) ->
    ok = scoper:add_meta(#{api => wallet, operation_id => OperationID}),
    wapi_auth:authorize_api_key(OperationID, ApiKey, Opts).

-spec handle_request(swag_server_wallet:operation_id(), req_data(), request_context(), handler_opts()) ->
    request_result().
handle_request(OperationID, Req, SwagContext, Opts) ->
    wapi_handler:handle_request(OperationID, Req, SwagContext, ?MODULE, Opts).


%% Providers
-spec process_request(operation_id(), req_data(), handler_context(), handler_opts()) ->
    request_result().
process_request('ListProviders', Req, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_providers(maps:with(['residence'], Req), Context) of
        {ok, Providers}   -> wapi_handler_utils:reply_ok(200, Providers);
        {error, notfound} -> wapi_handler_utils:reply_ok(200, [])
    end;
process_request('GetProvider', #{'providerID' := Id}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_provider(Id, Context) of
        {ok, Provider}    -> wapi_handler_utils:reply_ok(200, Provider);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('ListProviderIdentityClasses', #{'providerID' := Id}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_provider_identity_classes(Id, Context) of
        {ok, Classes}     -> wapi_handler_utils:reply_ok(200, Classes);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetProviderIdentityClass', #{
    'providerID'      := ProviderId,
    'identityClassID' := ClassId
}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_provider_identity_class(ProviderId, ClassId, Context) of
        {ok, Class}       -> wapi_handler_utils:reply_ok(200, Class);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('ListProviderIdentityLevels', #{
    'providerID'      := ProviderId,
    'identityClassID' := ClassId
}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_provider_identity_class_levels(ProviderId, ClassId, Context) of
        {ok, Levels}      -> wapi_handler_utils:reply_ok(200, Levels);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetProviderIdentityLevel', #{
    'providerID'      := ProviderId,
    'identityClassID' := ClassId,
    'identityLevelID' := LevelId
}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_provider_identity_class_level(ProviderId, ClassId, LevelId, Context) of
        {ok, Level}       -> wapi_handler_utils:reply_ok(200, Level);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;

%% Identities
process_request('ListIdentities', Req, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_identities(maps:with(['provider', 'class', 'level'], Req), Context) of
        {ok, Identities}  -> wapi_handler_utils:reply_ok(200, Identities);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetIdentity', #{'identityID' := IdentityId}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_identity(IdentityId, Context) of
        {ok, Identity}    -> wapi_handler_utils:reply_ok(200, Identity);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request(O = 'CreateIdentity', #{'Identity' := Params}, C = #{woody_context := Context}, Opts) ->
    case wapi_wallet_ff_backend:create_identity(Params#{<<"party">> => wapi_handler_utils:get_party_id(C)}, Context) of
        {ok, Identity = #{<<"id">> := IdentityId}} ->
            wapi_handler_utils:reply_ok(201, Identity, get_location(O, [IdentityId], Opts));
        {error, {provider, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such provider">>));
        {error, {identity_class, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity class">>))
    end;
process_request('ListIdentityChallenges', Req, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challengies(maps:with(['status', 'identityID'], Req), Context) of
        {ok, Challengies}  -> wapi_handler_utils:reply_ok(200, Challengies);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request(O = 'StartIdentityChallenge', #{
    'identityID'        := IdentityId,
    'IdentityChallenge' := Params
}, #{woody_context := Context}, Opts) ->
    case wapi_wallet_ff_backend:create_identity_challenge(IdentityId, Params, Context) of
        {ok, Challenge = #{<<"id">> := ChallengeId}} ->
            wapi_handler_utils:reply_ok(202, Challenge, get_location(O, [ChallengeId], Opts));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {challenge, {pending, _}}} ->
            wapi_handler_utils:reply_ok(409);
        {error, {challenge, {class, notfound}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity class">>));
        {error, {challenge, {proof, notfound}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Proof not found">>));
        {error, {challenge, {proof, insufficient}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Insufficient proof">>))
        %% TODO any other possible errors here?
    end;
process_request('GetIdentityChallenge', #{
    'identityID'  := IdentityId,
    'challengeID' := ChallengeId
}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challenge(IdentityId, ChallengeId, Context) of
        {ok, Challenge}   -> wapi_handler_utils:reply_ok(200, Challenge);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('CancelIdentityChallenge', #{
    'identityID'  := IdentityId,
    'challengeID' := ChallengeId
}, #{woody_context := Context}, _Opts) ->
    wapi_wallet_ff_backend:cancel_identity_challenge(IdentityId, ChallengeId, Context);
process_request('PollIdentityChallengeEvents', Params, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challenge_events(Params, Context) of
        {ok, Events}      -> wapi_handler_utils:reply_ok(200, Events);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetIdentityChallengeEvent', #{
    'identityID'  := IdentityId,
    'challengeID' := ChallengeId,
    'eventID'      := EventId
}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challenge_event(IdentityId, ChallengeId, EventId, Context) of
        {ok, Event}      -> wapi_handler_utils:reply_ok(200, Event);
        {error, notfound} -> wapi_handler_utils:reply_ok(404);
        {error, {event, notfound}} -> wapi_handler_utils:reply_ok(404)
    end;

%% Wallets
process_request(O, _Req, _Context, _Opts) when
    O =:= 'ListWallets'      orelse
    O =:= 'CreateWallet'     orelse
    O =:= 'GetWallet'        orelse
    O =:= 'GetWalletAccount' orelse
    O =:= 'IssueWalletGrant'
->
    wapi_wallet_ff_backend:not_implemented();

%% Deposits
process_request(O, _Req, _Context, _Opts) when
    O =:= 'CreateDeposit'     orelse
    O =:= 'GetDeposit'        orelse
    O =:= 'PollDepositEvents' orelse
    O =:= 'GetDepositEvents'
->
    wapi_wallet_ff_backend:not_implemented();

%% Withdrawals
process_request('ListDestinations', Req, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_destinations(maps:with(['identity', 'currency'], Req), Context) of
        {ok, Destinations} -> wapi_handler_utils:reply_ok(200, Destinations);
        {error, notfound}  -> wapi_handler_utils:reply_ok(200, [])
    end;
process_request('GetDestination', #{'destinationID' := DestinationId}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_destination(DestinationId, Context) of
        {ok, Destination} -> wapi_handler_utils:reply_ok(200, Destination);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request(O = 'CreateDestination', #{'Destination' := Params}, C = #{woody_context := Context}, Opts) ->
    case wapi_wallet_ff_backend:create_destination(Params#{party => wapi_handler_utils:get_party_id(C)}, Context) of
        {ok, Destination = #{<<"id">> := DestinationId}} ->
            wapi_handler_utils:reply_ok(201, Destination, get_location(O, [DestinationId], Opts));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {currency, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Currency not supported">>))
    end;
process_request('IssueDestinationGrant', #{
    'destinationID'           := DestinationId,
    'DestinationGrantRequest' := #{<<"validUntil">> := Expiration}
}, #{woody_context := Context}, _Opts) ->
    ExpirationUTC = wapi_utils:to_universal_time(Expiration),
    ok = check_expiration(ExpirationUTC),
    case wapi_wallet_ff_backend:get_destination(DestinationId, Context) of
        {ok, _Destination} ->
            {ok, {Date, Time, Usec, _Tz}} = rfc3339:parse(ExpirationUTC),
            wapi_handler_utils:reply_ok(201, #{
                <<"token">> => wapi_auth:issue_access_token(
                    wapi_handler_utils:get_party_id(Context),
                    {destinations, DestinationId},
                    {deadline, {{Date, Time}, Usec}}
                ),
                <<"validUntil">> => Expiration
            });
        {error, notfound} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request(O = 'CreateWithdrawal', #{'WithdrawalParameters' := Params}, #{woody_context := Context}, Opts) ->
    %% TODO: check authorization crap here (or on the backend)
    case wapi_wallet_ff_backend:create_withdrawal(Params, Context) of
        {ok, Withdrawal = #{<<"id">> := WithdrawalId}} ->
            wapi_handler_utils:reply_ok(201, Withdrawal, get_location(O, [WithdrawalId], Opts));
        {error, {source, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such wallet">>));
        {error, {destination, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such wallet">>))
    end;
process_request('GetWithdrawal', #{'withdrawalID' := WithdrawalId}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_withdrawal(WithdrawalId, Context) of
        {ok, Withdrawal}  -> wapi_handler_utils:reply_ok(200, Withdrawal);
        {error, notfound} -> wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such withdrawal">>))
    end;
process_request('PollWithdrawalEvents', Params, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_withdrawal_events(Params, Context) of
        {ok, Events}      -> wapi_handler_utils:reply_ok(200, Events);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetWithdrawalEvents', #{
    'withdrawalID' := WithdrawalId,
    'eventID'      := EventId
}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_withdrawal_event(WithdrawalId, EventId, Context) of
        {ok, Event}      -> wapi_handler_utils:reply_ok(200, Event);
        {error, notfound} -> wapi_handler_utils:reply_ok(404);
        {error, {event, notfound}} -> wapi_handler_utils:reply_ok(404)
    end;

%% Residences
process_request('GetResidence', _Req, _Context, _Opts) ->
    wapi_wallet_ff_backend:not_implemented();

%% Currencies
process_request('GetCurrency', _Req, _Context, _Opts) ->
    wapi_wallet_ff_backend:not_implemented().

%% Internal functions

get_location(OperationId, Params, Opts) ->
    #{path := PathSpec} = swag_server_wallet_router:get_operation(OperationId),
    wapi_handler_utils:get_location(PathSpec, Params, Opts).

check_expiration(Expiration) ->
    {ok, ExpirationSec} = rfc3339:to_time(Expiration, second),
    case (genlib_time:unow() -  ExpirationSec) >= 0 of
        true ->
            wapi_handler:throw_result(wapi_handler_utils:reply_ok(
                422,
                wapi_handler_utils:get_error_msg(<<"Already expired">>)
            ));
        false ->
            ok
    end.
