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
process_request('ListProviders', _Req, #{woody_context := _Context}, _Opts) ->
    %% case wapi_wallet_ff_backend:get_providers(maps:with(['residence'], Req), Context) of
    %%     {ok, Providers}   -> wapi_handler_utils:reply_ok(200, Providers);
    %%     {error, notfound} -> wapi_handler_utils:reply_ok(200, [])
    %% end;
    not_implemented();
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
    'providerID'      := _ProviderId,
    'identityClassID' := _ClassId
}, #{woody_context := _Context}, _Opts) ->
    %% case wapi_wallet_ff_backend:get_provider_identity_class_levels(ProviderId, ClassId, Context) of
    %%     {ok, Levels}      -> wapi_handler_utils:reply_ok(200, Levels);
    %%     {error, notfound} -> wapi_handler_utils:reply_ok(404)
    %% end;
    not_implemented();
process_request('GetProviderIdentityLevel', #{
    'providerID'      := _ProviderId,
    'identityClassID' := _ClassId,
    'identityLevelID' := _LevelId
}, #{woody_context := _Context}, _Opts) ->
    %% case wapi_wallet_ff_backend:get_provider_identity_class_level(ProviderId, ClassId, LevelId, Context) of
    %%     {ok, Level}       -> wapi_handler_utils:reply_ok(200, Level);
    %%     {error, notfound} -> wapi_handler_utils:reply_ok(404)
    %% end;
    not_implemented();

%% Identities
process_request('ListIdentities', _Req, #{woody_context := _Context}, _Opts) ->
    %% case wapi_wallet_ff_backend:get_identities(maps:with(['provider', 'class', 'level'], Req), Context) of
    %%     {ok, Identities}  -> wapi_handler_utils:reply_ok(200, Identities);
    %%     {error, notfound} -> wapi_handler_utils:reply_ok(404)
    %% end;
    not_implemented();
process_request('GetIdentity', #{'identityID' := IdentityId}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_identity(IdentityId, Context) of
        {ok, Identity}    -> wapi_handler_utils:reply_ok(200, Identity);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateIdentity', #{'Identity' := Params}, C = #{woody_context := Context}, Opts) ->
    case wapi_wallet_ff_backend:create_identity(Params#{<<"party">> => wapi_handler_utils:get_party_id(C)}, Context) of
        {ok, Identity = #{<<"id">> := IdentityId}} ->
            wapi_handler_utils:reply_ok(201, Identity, get_location('GetIdentity', [IdentityId], Opts));
        {error, {provider, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such provider">>));
        {error, {identity_class, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity class">>))
    end;
process_request('ListIdentityChallenges', _Req, #{woody_context := _Context}, _Opts) ->
    %% case wapi_wallet_ff_backend:get_identity_challengies(maps:with(['status', 'identityID'], Req), Context) of
    %%     {ok, Challengies}  -> wapi_handler_utils:reply_ok(200, Challengies);
    %%     {error, notfound} -> wapi_handler_utils:reply_ok(404)
    %% end;
    not_implemented();
process_request('StartIdentityChallenge', #{
    'identityID'        := IdentityId,
    'IdentityChallenge' := Params
}, #{woody_context := Context}, Opts) ->
    case wapi_wallet_ff_backend:create_identity_challenge(IdentityId, Params, Context) of
        {ok, Challenge = #{<<"id">> := ChallengeId}} ->
            wapi_handler_utils:reply_ok(202, Challenge, get_location('GetIdentityChallenge', [ChallengeId], Opts));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {challenge, {pending, _}}} ->
            wapi_handler_utils:reply_ok(409);
        {error, {challenge, {class, notfound}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such challenge type">>));
        {error, {challenge, {proof, notfound}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Proof not found">>));
        {error, {challenge, {proof, insufficient}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Insufficient proof">>));
        {error,{challenge, {level, _}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Illegal identification type for current identity level">>));
        {error, {challenge, conflict}} ->
            wapi_handler_utils:reply_ok(409)
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
    'identityID'  := _IdentityId,
    'challengeID' := _ChallengeId
}, #{woody_context := _Context}, _Opts) ->
    not_implemented();
process_request('PollIdentityChallengeEvents', Params, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challenge_events(Params, Context) of
        {ok, Events}      -> wapi_handler_utils:reply_ok(200, Events);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetIdentityChallengeEvent', Params, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challenge_event(Params, Context) of
        {ok, Event}       -> wapi_handler_utils:reply_ok(200, Event);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;

%% Wallets
process_request('ListWallets', _Req, _Context, _Opts) ->
    not_implemented();
process_request('GetWallet', #{'walletID' := WalletId}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_wallet(WalletId, Context) of
        {ok, Wallet}      -> wapi_handler_utils:reply_ok(200, Wallet);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateWallet', #{'Wallet' := Params}, #{woody_context := Context}, Opts) ->
    case wapi_wallet_ff_backend:create_wallet(Params, Context) of
        {ok, Wallet = #{<<"id">> := WalletId}} ->
            wapi_handler_utils:reply_ok(201, Wallet, get_location('GetWallet', [WalletId], Opts));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {currency, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Currency not supported">>));
        {error, {inaccessible, _}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Identity inaccessible">>));
        {error, invalid} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Invalid currency">>))
    end;
process_request('GetWalletAccount', #{'walletID' := WalletId}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_wallet_account(WalletId, Context) of
        {ok, WalletAccount} -> wapi_handler_utils:reply_ok(200, WalletAccount);
        {error, notfound}   -> wapi_handler_utils:reply_ok(404)
    end;

process_request('IssueWalletGrant', #{
    'walletID'           := WalletId,
    'WalletGrantRequest' := #{<<"validUntil">> := Expiration, <<"asset">> := Asset}
}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_wallet(WalletId, Context) of
        {ok, _} ->
            %% TODO issue token properly
            wapi_handler_utils:reply_ok(201, #{
                <<"token">>      => issue_grant_token(wallets, WalletId, Expiration, #{<<"asset">> => Asset}),
                <<"validUntil">> => Expiration,
                <<"asset">>      => Asset
            });
        {error, notfound} ->
            wapi_handler_utils:reply_ok(404)
    end;

%% Deposits
process_request(O, _Req, _Context, _Opts) when
    O =:= 'CreateDeposit'     orelse
    O =:= 'GetDeposit'        orelse
    O =:= 'PollDepositEvents' orelse
    O =:= 'GetDepositEvents'
->
    not_implemented();

%% Withdrawals
process_request('ListDestinations', _Req, #{woody_context := _Context}, _Opts) ->
    %% case wapi_wallet_ff_backend:get_destinations(maps:with(['identity', 'currency'], Req), Context) of
    %%     {ok, Destinations} -> wapi_handler_utils:reply_ok(200, Destinations);
    %%     {error, notfound}  -> wapi_handler_utils:reply_ok(200, [])
    %% end;
    not_implemented();
process_request('GetDestination', #{'destinationID' := DestinationId}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_destination(DestinationId, Context) of
        {ok, Destination} -> wapi_handler_utils:reply_ok(200, Destination);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateDestination', #{'Destination' := Params}, #{woody_context := Context}, Opts) ->
    case wapi_wallet_ff_backend:create_destination(Params, Context) of
        {ok, Destination = #{<<"id">> := DestinationId}} ->
            wapi_handler_utils:reply_ok(201, Destination, get_location('GetDestination', [DestinationId], Opts));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {currency, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Currency not supported">>));
        {error, {inaccessible, _}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Identity inaccessible">>));
        {error, invalid} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Invalid currency">>))
    end;
process_request('IssueDestinationGrant', #{
    'destinationID'           := DestinationId,
    'DestinationGrantRequest' := #{<<"validUntil">> := Expiration}
}, #{woody_context := Context}, _Opts) ->
    %% TODO issue token properly
    case wapi_wallet_ff_backend:get_destination(DestinationId, Context) of
        {ok, _} ->
            wapi_handler_utils:reply_ok(201, #{
                <<"token">> => issue_grant_token(destinations, DestinationId, Expiration, #{}),
                <<"validUntil">> => Expiration
            });
        {error, notfound} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateWithdrawal', #{'WithdrawalParameters' := Params}, #{woody_context := Context}, Opts) ->
    %% TODO: properly check authorization tokens here
    case wapi_wallet_ff_backend:create_withdrawal(Params, Context) of
        {ok, Withdrawal = #{<<"id">> := WithdrawalId}} ->
            wapi_handler_utils:reply_ok(202, Withdrawal, get_location('GetWithdrawal', [WithdrawalId], Opts));
        {error, {source, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such wallet">>));
        {error, {destination, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such destination">>));
        {error, {destination, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Destination unauthorized">>));
        {error, {provider, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such provider">>));
        {error, {wallet, {inaccessible, _}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Inaccessible source or destination">>));
        {error, {wallet, {currency, invalid}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Invalid currency for source or destination">>));
        {error, {wallet, {provider, invalid}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Invalid provider for source or destination">>))
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
        {ok, Event}       -> wapi_handler_utils:reply_ok(200, Event);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;

%% Residences
process_request('GetResidence', #{'residence' := Residence}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_residence(Residence, Context) of
        {ok, Currency}    -> wapi_handler_utils:reply_ok(200, Currency);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;

%% Currencies
process_request('GetCurrency', #{'currencyID' := CurrencyId}, #{woody_context := Context}, _Opts) ->
    case wapi_wallet_ff_backend:get_currency(CurrencyId, Context) of
        {ok, Currency}    -> wapi_handler_utils:reply_ok(200, Currency);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end.

%% Internal functions

get_location(OperationId, Params, Opts) ->
    #{path := PathSpec} = swag_server_wallet_router:get_operation(OperationId),
    wapi_handler_utils:get_location(PathSpec, Params, Opts).

-spec not_implemented() -> no_return().
not_implemented() ->
    wapi_handler_utils:throw_not_implemented().


issue_grant_token(Type, Id, Expiration, Meta) when is_map(Meta) ->
    wapi_utils:map_to_base64url(#{
        <<"resourceType">> => Type,
        <<"resourceID">>   => Id,
        <<"validUntil">>   => Expiration,
        <<"metadata">>     => Meta
    }).

%% TODO issue token properly
%%
%% issue_grant_token(destinations, Id, Expiration, _Meta, Context) ->
%%     {ok, {Date, Time, Usec, _Tz}} = rfc3339:parse(Expiration),
%%     wapi_auth:issue_access_token(
%%         wapi_handler_utils:get_party_id(Context),
%%         {destinations, Id},
%%         {deadline, {{Date, Time}, Usec}}
%%     ).
%%
%% is_expired(Expiration) ->
%%     {ok, ExpirationSec} = rfc3339:to_time(Expiration, second),
%%     (genlib_time:unow() -  ExpirationSec) >= 0.
