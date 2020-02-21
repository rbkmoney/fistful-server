-module(wapi_wallet_thrift_handler).

-behaviour(swag_server_wallet_logic_handler).
-behaviour(wapi_handler).

%% swag_server_wallet_logic_handler callbacks
-export([authorize_api_key/3]).
-export([handle_request/4]).

%% wapi_handler callbacks
-export([process_request/4]).

%% Types

-type req_data()        :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type request_result()  :: wapi_handler:request_result().
-type operation_id()    :: swag_server_wallet:operation_id().
-type api_key()         :: swag_server_wallet:api_key().
-type request_context() :: swag_server_wallet:request_context().
-type handler_opts()    :: swag_server_wallet:handler_opts(_).

%% API

-spec authorize_api_key(operation_id(), api_key(), handler_opts()) ->
    false | {true, wapi_auth:context()}.
authorize_api_key(OperationID, ApiKey, Opts) ->
    ok = scoper:add_meta(#{api => wallet, operation_id => OperationID}),
    wapi_auth:authorize_api_key(OperationID, ApiKey, Opts).

-spec handle_request(swag_server_wallet:operation_id(), req_data(), request_context(), handler_opts()) ->
    request_result().
handle_request(OperationID, Req, SwagContext, Opts) ->
    wapi_handler:handle_request(wallet, OperationID, Req, SwagContext, Opts).

-spec process_request(operation_id(), req_data(), handler_context(), handler_opts()) ->
    request_result().

%% Identities
process_request('GetIdentity', #{'identityID' := IdentityId}, Context, _Opts) ->
    case wapi_identity_backend:get_identity(IdentityId, Context) of
        {ok, Identity}                    -> wapi_handler_utils:reply_ok(200, Identity);
        {error, {identity, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateIdentity', #{'Identity' := Params}, Context, Opts) ->
    case wapi_identity_backend:create_identity(Params, Context) of
        {ok, Identity = #{<<"id">> := IdentityId}} ->
            wapi_handler_utils:reply_ok(201, Identity, get_location('GetIdentity', [IdentityId], Opts));
        {error, {provider, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such provider">>));
        {error, {identity_class, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity class">>));
        {error, inaccessible} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Identity inaccessible">>));
        {error, {external_id_conflict, ID}} ->
            wapi_handler_utils:reply_error(409, #{<<"id">> => ID})
    end;
process_request('ListIdentityChallenges', #{'identityID' := Id, 'status' := Status}, Context, _Opts) ->
    case wapi_identity_backend:get_identity_challenges(Id, Status, Context) of
        {ok, Challenges}                  -> wapi_handler_utils:reply_ok(200, Challenges);
        {error, {identity, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('StartIdentityChallenge', #{
    'identityID'        := IdentityId,
    'IdentityChallenge' := Params
}, Context, Opts) ->
    case wapi_identity_backend:create_identity_challenge(IdentityId, Params, Context) of
        {ok, Challenge = #{<<"id">> := ChallengeId}} ->
            wapi_handler_utils:reply_ok(202, Challenge, get_location('GetIdentityChallenge', [ChallengeId], Opts));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {challenge, conflict}} ->
            wapi_handler_utils:reply_ok(409);
        {error, {external_id_conflict, ID}} ->
            wapi_handler_utils:reply_ok(409, #{<<"id">> => ID});
        {error, {challenge, pending}} ->
            wapi_handler_utils:reply_ok(409);
        {error, {challenge, {class, notfound}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such challenge type">>));
        {error, {challenge, {proof, notfound}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Proof not found">>));
        {error, {challenge, {proof, insufficient}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Insufficient proof">>));
        {error, {challenge, level}} ->
            wapi_handler_utils:reply_ok(422,
                wapi_handler_utils:get_error_msg(<<"Illegal identification type for current identity level">>)
            )
        %% TODO any other possible errors here?
    end;
process_request('GetIdentityChallenge', #{
    'identityID'  := IdentityId,
    'challengeID' := ChallengeId
}, Context, _Opts) ->
    case wapi_identity_backend:get_identity_challenge(IdentityId, ChallengeId, Context) of
        {ok, Challenge}                   -> wapi_handler_utils:reply_ok(200, Challenge);
        {error, {identity, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404);
        {error, {challenge, notfound}}    -> wapi_handler_utils:reply_ok(404)
    end;
process_request('PollIdentityChallengeEvents', Params, Context, _Opts) ->
    case wapi_identity_backend:get_identity_challenge_events(Params, Context) of
        {ok, Events}                      -> wapi_handler_utils:reply_ok(200, Events);
        {error, {identity, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetIdentityChallengeEvent', Params, Context, _Opts) ->
    case wapi_identity_backend:get_identity_challenge_event(Params, Context) of
        {ok, Event}                       -> wapi_handler_utils:reply_ok(200, Event);
        {error, {identity, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404);
        {error, {event, notfound}}        -> wapi_handler_utils:reply_ok(404)
    end;

%% Wallets

process_request('GetWallet', #{'walletID' := WalletId}, Context, _Opts) ->
    case wapi_wallet_backend:get(WalletId, Context) of
        {ok, Wallet}                    -> wapi_handler_utils:reply_ok(200, Wallet);
        {error, {wallet, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {wallet, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateWallet', #{'Wallet' := Params}, Context, Opts) ->
    case wapi_wallet_backend:create(Params, Context) of
        {ok, Wallet = #{<<"id">> := WalletId}} ->
            wapi_handler_utils:reply_ok(201, Wallet, get_location('GetWallet', [WalletId], Opts));
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {currency, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Currency not supported">>));
        {error, inaccessible} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Identity inaccessible">>));
        {error, {external_id_conflict, ID}} ->
            wapi_handler_utils:reply_error(409, #{<<"id">> => ID})
    end;
process_request(OperationID, Params, Context, Opts) ->
    wapi_wallet_handler:process_request(OperationID, Params, Context, Opts).

%% Internal functions

get_location(OperationId, Params, Opts) ->
    #{path := PathSpec} = swag_server_wallet_router:get_operation(OperationId),
    wapi_handler_utils:get_location(PathSpec, Params, Opts).
