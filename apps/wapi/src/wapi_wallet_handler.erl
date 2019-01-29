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
-type handler_context() :: wapi_handler:context().
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
    wapi_handler:handle_request(wallet, OperationID, Req, SwagContext, Opts).


%% Providers
-spec process_request(operation_id(), req_data(), handler_context(), handler_opts()) ->
    request_result().
process_request('ListProviders', #{'residence' := Residence}, Context, _Opts) ->
    Providers = wapi_wallet_ff_backend:get_providers(ff_maybe:to_list(Residence), Context),
    wapi_handler_utils:reply_ok(200, Providers);
process_request('GetProvider', #{'providerID' := Id}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_provider(Id, Context) of
        {ok, Provider}    -> wapi_handler_utils:reply_ok(200, Provider);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('ListProviderIdentityClasses', #{'providerID' := Id}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_provider_identity_classes(Id, Context) of
        {ok, Classes}     -> wapi_handler_utils:reply_ok(200, Classes);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetProviderIdentityClass', #{
    'providerID'      := ProviderId,
    'identityClassID' := ClassId
}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_provider_identity_class(ProviderId, ClassId, Context) of
        {ok, Class}       -> wapi_handler_utils:reply_ok(200, Class);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('ListProviderIdentityLevels', #{
    'providerID'      := _ProviderId,
    'identityClassID' := _ClassId
}, _Context, _Opts) ->
    %% case wapi_wallet_ff_backend:get_provider_identity_class_levels(ProviderId, ClassId, Context) of
    %%     {ok, Levels}      -> wapi_handler_utils:reply_ok(200, Levels);
    %%     {error, notfound} -> wapi_handler_utils:reply_ok(404)
    %% end;
    not_implemented();
process_request('GetProviderIdentityLevel', #{
    'providerID'      := _ProviderId,
    'identityClassID' := _ClassId,
    'identityLevelID' := _LevelId
}, _Context, _Opts) ->
    %% case wapi_wallet_ff_backend:get_provider_identity_class_level(ProviderId, ClassId, LevelId, Context) of
    %%     {ok, Level}       -> wapi_handler_utils:reply_ok(200, Level);
    %%     {error, notfound} -> wapi_handler_utils:reply_ok(404)
    %% end;
    not_implemented();

%% Identities
process_request('ListIdentities', _Req, _Context, _Opts) ->
    %% case wapi_wallet_ff_backend:get_identities(maps:with(['provider', 'class', 'level'], Req), Context) of
    %%     {ok, Identities}  -> wapi_handler_utils:reply_ok(200, Identities);
    %%     {error, notfound} -> wapi_handler_utils:reply_ok(404)
    %% end;
    not_implemented();
process_request('GetIdentity', #{'identityID' := IdentityId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_identity(IdentityId, Context) of
        {ok, Identity}                    -> wapi_handler_utils:reply_ok(200, Identity);
        {error, {identity, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateIdentity', #{'Identity' := Params}, Context, Opts) ->
    case wapi_wallet_ff_backend:create_identity(Params, Context) of
        {ok, Identity = #{<<"id">> := IdentityId}} ->
            wapi_handler_utils:reply_ok(201, Identity, get_location('GetIdentity', [IdentityId], Opts));
        {error, {provider, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such provider">>));
        {error, {identity_class, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity class">>));
        {error, {conflict, ID}} ->
            wapi_handler_utils:reply_error(409, #{<<"id">> => ID});
        {error, {email, notfound}} ->
            wapi_handler_utils:reply_error(400, #{
                <<"errorType">>   => <<"NotFound">>,
                <<"name">>        => <<"email">>,
                <<"description">> => <<"No email in JWT">>
            })
    end;
process_request('ListIdentityChallenges', #{'identityID' := Id, 'status' := Status}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challenges(Id, ff_maybe:to_list(Status), Context) of
        {ok, Challenges}                  -> wapi_handler_utils:reply_ok(200, Challenges);
        {error, {identity, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('StartIdentityChallenge', #{
    'identityID'        := IdentityId,
    'IdentityChallenge' := Params
}, Context, Opts) ->
    case wapi_wallet_ff_backend:create_identity_challenge(IdentityId, Params, Context) of
        {ok, Challenge = #{<<"id">> := ChallengeId}} ->
            wapi_handler_utils:reply_ok(202, Challenge, get_location('GetIdentityChallenge', [ChallengeId], Opts));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {challenge, conflict}} ->
            wapi_handler_utils:reply_ok(409);
        {error, {challenge, {pending, _}}} ->
            wapi_handler_utils:reply_ok(409);
        {error, {challenge, {class, notfound}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such challenge type">>));
        {error, {challenge, {proof, notfound}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Proof not found">>));
        {error, {challenge, {proof, insufficient}}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Insufficient proof">>));
        {error, {challenge, {level, _}}} ->
            wapi_handler_utils:reply_ok(422,
                wapi_handler_utils:get_error_msg(<<"Illegal identification type for current identity level">>)
            )
        %% TODO any other possible errors here?
    end;
process_request('GetIdentityChallenge', #{
    'identityID'  := IdentityId,
    'challengeID' := ChallengeId
}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challenge(IdentityId, ChallengeId, Context) of
        {ok, Challenge}                   -> wapi_handler_utils:reply_ok(200, Challenge);
        {error, {identity, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404);
        {error, {challenge, notfound}}    -> wapi_handler_utils:reply_ok(404)
    end;
process_request('PollIdentityChallengeEvents', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challenge_events(Params, Context) of
        {ok, Events}                      -> wapi_handler_utils:reply_ok(200, Events);
        {error, {identity, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetIdentityChallengeEvent', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_identity_challenge_event(Params, Context) of
        {ok, Event}                       -> wapi_handler_utils:reply_ok(200, Event);
        {error, {identity, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {identity, unauthorized}} -> wapi_handler_utils:reply_ok(404);
        {error, {event, notfound}}        -> wapi_handler_utils:reply_ok(404)
    end;

%% Wallets
process_request('ListWallets', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:list_wallets(Params, Context) of
        {ok, {200, _, List}}       -> wapi_handler_utils:reply_ok(200, List);
        {error, {Code, _, Error}}  -> wapi_handler_utils:reply_error(Code, Error)
    end;
process_request('GetWallet', #{'walletID' := WalletId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_wallet(WalletId, Context) of
        {ok, Wallet}                    -> wapi_handler_utils:reply_ok(200, Wallet);
        {error, {wallet, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {wallet, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateWallet', #{'Wallet' := Params}, Context, Opts) ->
    case wapi_wallet_ff_backend:create_wallet(Params, Context) of
        {ok, Wallet = #{<<"id">> := WalletId}} ->
            wapi_handler_utils:reply_ok(201, Wallet, get_location('GetWallet', [WalletId], Opts));
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {currency, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Currency not supported">>));
        {error, {inaccessible, _}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Identity inaccessible">>));
        {error, {conflict, ID}} ->
            wapi_handler_utils:reply_error(409, #{<<"id">> => ID});
        {error, invalid} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Invalid currency">>))
    end;
process_request('GetWalletAccount', #{'walletID' := WalletId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_wallet_account(WalletId, Context) of
        {ok, WalletAccount}             -> wapi_handler_utils:reply_ok(200, WalletAccount);
        {error, {wallet, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {wallet, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('IssueWalletGrant', #{
    'walletID'           := WalletId,
    'WalletGrantRequest' := #{<<"validUntil">> := Expiration, <<"asset">> := Asset}
}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_wallet(WalletId, Context) of
        {ok, _} ->
            case issue_grant_token({wallets, WalletId, Asset}, Expiration, Context) of
                {ok, Token} ->
                    wapi_handler_utils:reply_ok(201, #{
                        <<"token">>      => Token,
                        <<"validUntil">> => Expiration,
                        <<"asset">>      => Asset
                    });
                {error, expired} ->
                    wapi_handler_utils:reply_ok(422,
                        wapi_handler_utils:get_error_msg(<<"Invalid expiration: already expired">>)
                    )
            end;
        {error, {wallet, notfound}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {wallet, unauthorized}} ->
            wapi_handler_utils:reply_ok(404)
    end;

%% Withdrawals
process_request('ListDestinations', _Req, _Context, _Opts) ->
    %% case wapi_wallet_ff_backend:get_destinations(maps:with(['identity', 'currency'], Req), Context) of
    %%     {ok, Destinations} -> wapi_handler_utils:reply_ok(200, Destinations);
    %%     {error, notfound}  -> wapi_handler_utils:reply_ok(200, [])
    %% end;
    not_implemented();
process_request('GetDestination', #{'destinationID' := DestinationId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_destination(DestinationId, Context) of
        {ok, Destination}                    -> wapi_handler_utils:reply_ok(200, Destination);
        {error, {destination, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {destination, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateDestination', #{'Destination' := Params}, Context, Opts) ->
    case wapi_wallet_ff_backend:create_destination(Params, Context) of
        {ok, Destination = #{<<"id">> := DestinationId}} ->
            wapi_handler_utils:reply_ok(201, Destination, get_location('GetDestination', [DestinationId], Opts));
        {error, {identity, unauthorized}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {identity, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"No such identity">>));
        {error, {currency, notfound}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Currency not supported">>));
        {error, {inaccessible, _}} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Identity inaccessible">>));
        {error, {conflict, ID}} ->
            wapi_handler_utils:reply_error(409, #{<<"id">> => ID});
        {error, invalid} ->
            wapi_handler_utils:reply_ok(422, wapi_handler_utils:get_error_msg(<<"Invalid currency">>))
    end;
process_request('IssueDestinationGrant', #{
    'destinationID'           := DestinationId,
    'DestinationGrantRequest' := #{<<"validUntil">> := Expiration}
}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_destination(DestinationId, Context) of
        {ok, _} ->
            case issue_grant_token({destinations, DestinationId}, Expiration, Context) of
                {ok, Token} ->
                    wapi_handler_utils:reply_ok(201, #{
                        <<"token">>      => Token,
                        <<"validUntil">> => Expiration
                    });
                {error, expired} ->
                    wapi_handler_utils:reply_ok(422,
                        wapi_handler_utils:get_error_msg(<<"Invalid expiration: already expired">>)
                    )
            end;
        {error, {destination, notfound}} ->
            wapi_handler_utils:reply_ok(404);
        {error, {destination, unauthorized}} ->
            wapi_handler_utils:reply_ok(404)
    end;
process_request('CreateWithdrawal', #{'WithdrawalParameters' := Params}, Context, Opts) ->
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
        {error, {conflict, ID}} ->
            wapi_handler_utils:reply_error(409, #{<<"id">> => ID});
        {error, {wallet, {inaccessible, _}}} ->
            wapi_handler_utils:reply_ok(422,
                wapi_handler_utils:get_error_msg(<<"Inaccessible source or destination">>)
            );
        {error, {wallet, {currency, invalid}}} ->
            wapi_handler_utils:reply_ok(422,
                wapi_handler_utils:get_error_msg(<<"Invalid currency for source or destination">>)
            );
        {error, {wallet, {provider, invalid}}} ->
            wapi_handler_utils:reply_ok(422,
                wapi_handler_utils:get_error_msg(<<"Invalid provider for source or destination">>)
            )
    end;
process_request('GetWithdrawal', #{'withdrawalID' := WithdrawalId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_withdrawal(WithdrawalId, Context) of
        {ok, Withdrawal}                    -> wapi_handler_utils:reply_ok(200, Withdrawal);
        {error, {withdrawal, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {withdrawal, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('PollWithdrawalEvents', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_withdrawal_events(Params, Context) of
        {ok, Events}                        -> wapi_handler_utils:reply_ok(200, Events);
        {error, {withdrawal, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {withdrawal, unauthorized}} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetWithdrawalEvents', #{
    'withdrawalID' := WithdrawalId,
    'eventID'      := EventId
}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_withdrawal_event(WithdrawalId, EventId, Context) of
        {ok, Event}           -> wapi_handler_utils:reply_ok(200, Event);
        {error, {withdrawal, notfound}}     -> wapi_handler_utils:reply_ok(404);
        {error, {withdrawal, unauthorized}} -> wapi_handler_utils:reply_ok(404);
        {error, {event, notfound}}          -> wapi_handler_utils:reply_ok(404)
    end;
process_request('ListWithdrawals', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:list_withdrawals(Params, Context) of
        {ok, {200, _, List}}       -> wapi_handler_utils:reply_ok(200, List);
        {error, {Code, _, Error}}  -> wapi_handler_utils:reply_error(Code, Error)
    end;

%% Residences
process_request('GetResidence', #{'residence' := ResidenceId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_residence(ResidenceId, Context) of
        {ok, Residence}   -> wapi_handler_utils:reply_ok(200, Residence);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;

%% Currencies
process_request('GetCurrency', #{'currencyID' := CurrencyId}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_currency(CurrencyId, Context) of
        {ok, Currency}    -> wapi_handler_utils:reply_ok(200, Currency);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;

%% Reports
process_request('CreateReport', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:create_report(Params, Context) of
        {ok, Report}              -> wapi_handler_utils:reply_ok(201, Report);
        {error, invalid_request}  -> wapi_handler_utils:reply_ok(400, #{
                <<"errorType">>   => <<"NoMatch">>,
                <<"name">>        => <<"timestamps">>,
                <<"description">> => <<"invalid time range">>
            });
        {error, invalid_contract} -> wapi_handler_utils:reply_ok(400, #{
                <<"errorType">>   => <<"NotFound">>,
                <<"name">>        => <<"contractID">>,
                <<"description">> => <<"contract not found">>
            })
    end;
process_request('GetReport', #{
    contractID := ContractId,
    reportID   := ReportId
}, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_report(ReportId, ContractId, Context) of
        {ok, Report}      -> wapi_handler_utils:reply_ok(200, Report);
        {error, notfound} -> wapi_handler_utils:reply_ok(404)
    end;
process_request('GetReports', Params, Context, _Opts) ->
    case wapi_wallet_ff_backend:get_reports(Params, Context) of
        {ok, ReportList}          -> wapi_handler_utils:reply_ok(200, ReportList);
        {error, invalid_request}  -> wapi_handler_utils:reply_ok(400, #{
                <<"errorType">>   => <<"NoMatch">>,
                <<"name">>        => <<"timestamps">>,
                <<"description">> => <<"invalid time range">>
            });
        {error, {dataset_too_big, Limit}} -> wapi_handler_utils:reply_ok(400, #{
                <<"errorType">>   => <<"WrongLength">>,
                <<"name">>        => <<"limitExceeded">>,
                <<"description">> => io_lib:format("Max limit: ~p", [Limit])
            })
    end;
process_request('DownloadFile', #{fileID := FileId, expiresAt := ExpiresAt}, Context, _Opts) ->
    case wapi_wallet_ff_backend:download_file(FileId, ExpiresAt, Context) of
        {ok, URL}         ->
            wapi_handler_utils:reply_ok(201, #{<<"url">> => URL, <<"expiresAt">> => ExpiresAt});
        {error, notfound} ->
            wapi_handler_utils:reply_ok(404)
    end.

%% Internal functions

get_location(OperationId, Params, Opts) ->
    #{path := PathSpec} = swag_server_wallet_router:get_operation(OperationId),
    wapi_handler_utils:get_location(PathSpec, Params, Opts).

-spec not_implemented() -> no_return().
not_implemented() ->
    wapi_handler_utils:throw_not_implemented().

issue_grant_token(TokenSpec, Expiration, Context) ->
    case get_expiration_deadline(Expiration) of
        {ok, Deadline} ->
            {ok, wapi_auth:issue_access_token(wapi_handler_utils:get_owner(Context), TokenSpec, {deadline, Deadline})};
        Error = {error, _} ->
            Error
    end.

get_expiration_deadline(Expiration) ->
    {DateTime, MilliSec} = woody_deadline:from_binary(wapi_utils:to_universal_time(Expiration)),
    Deadline = genlib_time:daytime_to_unixtime(DateTime) + MilliSec div 1000,
    case genlib_time:unow() - Deadline < 0 of
        true ->
            {ok, Deadline};
        false ->
            {error, expired}
    end.
