-module(swag_server_wallet_router).

-export([get_paths/1]).
-export([get_paths/2]).
-export([get_operation/1]).
-export([get_operations/0]).

-type operations() :: #{
    Method :: binary() => OperationID :: swag_server_wallet:operation_id()
}.

-type logic_handler(T) :: swag_server_wallet:logic_handler(T).

-type swagger_handler_opts() :: #{
    validation_opts => swag_server_wallet_validation:validation_opts()
}.

-type init_opts() :: {
    Operations      :: operations(),
    LogicHandler    :: logic_handler(_),
    SwaggerHandlerOpts :: swagger_handler_opts()
}.

-type operation_spec() :: #{
    path    := '_' | iodata(),
    method  := binary(),
    handler := module()
}.

-export_type([swagger_handler_opts/0]).
-export_type([init_opts/0]).
-export_type([operation_spec/0]).

-spec get_paths(LogicHandler :: atom()) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler) ->
    get_paths(LogicHandler, #{}).

-spec get_paths(LogicHandler :: atom(), SwaggerHandlerOpts :: swagger_handler_opts()) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler, SwaggerHandlerOpts) ->
    PreparedPaths = maps:fold(
        fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
            [{Path, Handler, Operations} | Acc]
        end,
        [],
        group_paths()
    ),
    [
        {'_',
            [{P, H, {O, LogicHandler, SwaggerHandlerOpts}} || {P, H, O} <- PreparedPaths]
        }
    ].

group_paths() ->
    maps:fold(
        fun(OperationID, #{path := Path, method := Method, handler := Handler}, Acc) ->
            case maps:find(Path, Acc) of
                {ok, PathInfo0 = #{operations := Operations0}} ->
                    Operations = Operations0#{Method => OperationID},
                    PathInfo = PathInfo0#{operations => Operations},
                    Acc#{Path => PathInfo};
                error ->
                    Operations = #{Method => OperationID},
                    PathInfo = #{handler => Handler, operations => Operations},
                    Acc#{Path => PathInfo}
            end
        end,
        #{},
        get_operations()
    ).

-spec get_operation(swag_server_wallet:operation_id()) ->
   operation_spec().

get_operation(OperationId) ->
    maps:get(OperationId, get_operations()).

-spec get_operations() -> #{
    swag_server_wallet:operation_id() := operation_spec()
}.

get_operations() ->
    #{ 
        'GetCurrency' => #{
            path => "/wallet/v0/currencies/:currencyID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_currencies_handler'
        },
        'ListDeposits' => #{
            path => "/wallet/v0/deposits",
            method => <<"GET">>,
            handler => 'swag_server_wallet_deposits_handler'
        },
        'DownloadFile' => #{
            path => "/wallet/v0/files/:fileID/download",
            method => <<"POST">>,
            handler => 'swag_server_wallet_downloads_handler'
        },
        'CreateIdentity' => #{
            path => "/wallet/v0/identities",
            method => <<"POST">>,
            handler => 'swag_server_wallet_identities_handler'
        },
        'GetIdentity' => #{
            path => "/wallet/v0/identities/:identityID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_identities_handler'
        },
        'GetIdentityChallenge' => #{
            path => "/wallet/v0/identities/:identityID/challenges/:challengeID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_identities_handler'
        },
        'GetIdentityChallengeEvent' => #{
            path => "/wallet/v0/identities/:identityID/challenges/:challengeID/events/:eventID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_identities_handler'
        },
        'ListIdentities' => #{
            path => "/wallet/v0/identities",
            method => <<"GET">>,
            handler => 'swag_server_wallet_identities_handler'
        },
        'ListIdentityChallenges' => #{
            path => "/wallet/v0/identities/:identityID/challenges",
            method => <<"GET">>,
            handler => 'swag_server_wallet_identities_handler'
        },
        'PollIdentityChallengeEvents' => #{
            path => "/wallet/v0/identities/:identityID/challenges/:challengeID/events",
            method => <<"GET">>,
            handler => 'swag_server_wallet_identities_handler'
        },
        'StartIdentityChallenge' => #{
            path => "/wallet/v0/identities/:identityID/challenges",
            method => <<"POST">>,
            handler => 'swag_server_wallet_identities_handler'
        },
        'CreateP2PTransfer' => #{
            path => "/wallet/v0/p2p/transfers",
            method => <<"POST">>,
            handler => 'swag_server_wallet_p2_p_handler'
        },
        'GetP2PTransfer' => #{
            path => "/wallet/v0/p2p/transfers/:p2pTransferID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_p2_p_handler'
        },
        'GetP2PTransferEvents' => #{
            path => "/wallet/v0/p2p/transfers/:p2pTransferID/events",
            method => <<"GET">>,
            handler => 'swag_server_wallet_p2_p_handler'
        },
        'QuoteP2PTransfer' => #{
            path => "/wallet/v0/p2p/quotes",
            method => <<"POST">>,
            handler => 'swag_server_wallet_p2_p_handler'
        },
        'BlockP2PTransferTemplate' => #{
            path => "/wallet/v0/p2p/transfer/templates/:p2pTransferTemplateID/block",
            method => <<"POST">>,
            handler => 'swag_server_wallet_p2_p_templates_handler'
        },
        'CreateP2PTransferTemplate' => #{
            path => "/wallet/v0/p2p/transfer/templates",
            method => <<"POST">>,
            handler => 'swag_server_wallet_p2_p_templates_handler'
        },
        'CreateP2PTransferWithTemplate' => #{
            path => "/wallet/v0/p2p/transfer/templates/:p2pTransferTemplateID/transfers",
            method => <<"POST">>,
            handler => 'swag_server_wallet_p2_p_templates_handler'
        },
        'GetP2PTransferTemplateByID' => #{
            path => "/wallet/v0/p2p/transfer/templates/:p2pTransferTemplateID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_p2_p_templates_handler'
        },
        'IssueP2PTransferTemplateAccessToken' => #{
            path => "/wallet/v0/p2p/transfer/templates/:p2pTransferTemplateID/access-tokens",
            method => <<"POST">>,
            handler => 'swag_server_wallet_p2_p_templates_handler'
        },
        'IssueP2PTransferTicket' => #{
            path => "/wallet/v0/p2p/transfer/templates/:p2pTransferTemplateID/tickets",
            method => <<"POST">>,
            handler => 'swag_server_wallet_p2_p_templates_handler'
        },
        'QuoteP2PTransferWithTemplate' => #{
            path => "/wallet/v0/p2p/transfer/templates/:p2pTransferTemplateID/quotes",
            method => <<"POST">>,
            handler => 'swag_server_wallet_p2_p_templates_handler'
        },
        'GetProvider' => #{
            path => "/wallet/v0/providers/:providerID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_providers_handler'
        },
        'GetProviderIdentityClass' => #{
            path => "/wallet/v0/providers/:providerID/identity-classes/:identityClassID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_providers_handler'
        },
        'GetProviderIdentityLevel' => #{
            path => "/wallet/v0/providers/:providerID/identity-classes/:identityClassID/levels/:identityLevelID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_providers_handler'
        },
        'ListProviderIdentityClasses' => #{
            path => "/wallet/v0/providers/:providerID/identity-classes",
            method => <<"GET">>,
            handler => 'swag_server_wallet_providers_handler'
        },
        'ListProviderIdentityLevels' => #{
            path => "/wallet/v0/providers/:providerID/identity-classes/:identityClassID/levels",
            method => <<"GET">>,
            handler => 'swag_server_wallet_providers_handler'
        },
        'ListProviders' => #{
            path => "/wallet/v0/providers",
            method => <<"GET">>,
            handler => 'swag_server_wallet_providers_handler'
        },
        'CreateReport' => #{
            path => "/wallet/v0/identities/:identityID/reports",
            method => <<"POST">>,
            handler => 'swag_server_wallet_reports_handler'
        },
        'GetReport' => #{
            path => "/wallet/v0/identities/:identityID/reports/:reportID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_reports_handler'
        },
        'GetReports' => #{
            path => "/wallet/v0/identities/:identityID/reports",
            method => <<"GET">>,
            handler => 'swag_server_wallet_reports_handler'
        },
        'GetResidence' => #{
            path => "/wallet/v0/residences/:residence",
            method => <<"GET">>,
            handler => 'swag_server_wallet_residences_handler'
        },
        'CreateW2WTransfer' => #{
            path => "/wallet/v0/w2w/transfers",
            method => <<"POST">>,
            handler => 'swag_server_wallet_w2_w_handler'
        },
        'GetW2WTransfer' => #{
            path => "/wallet/v0/w2w/transfers/:w2wTransferID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_w2_w_handler'
        },
        'CreateWallet' => #{
            path => "/wallet/v0/wallets",
            method => <<"POST">>,
            handler => 'swag_server_wallet_wallets_handler'
        },
        'GetWallet' => #{
            path => "/wallet/v0/wallets/:walletID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_wallets_handler'
        },
        'GetWalletAccount' => #{
            path => "/wallet/v0/wallets/:walletID/account",
            method => <<"GET">>,
            handler => 'swag_server_wallet_wallets_handler'
        },
        'GetWalletByExternalID' => #{
            path => "/wallet/v0/external/wallets",
            method => <<"GET">>,
            handler => 'swag_server_wallet_wallets_handler'
        },
        'IssueWalletGrant' => #{
            path => "/wallet/v0/wallets/:walletID/grants",
            method => <<"POST">>,
            handler => 'swag_server_wallet_wallets_handler'
        },
        'ListWallets' => #{
            path => "/wallet/v0/wallets",
            method => <<"GET">>,
            handler => 'swag_server_wallet_wallets_handler'
        },
        'CreateWebhook' => #{
            path => "/wallet/v0/webhooks",
            method => <<"POST">>,
            handler => 'swag_server_wallet_webhooks_handler'
        },
        'DeleteWebhookByID' => #{
            path => "/wallet/v0/webhooks/:webhookID",
            method => <<"DELETE">>,
            handler => 'swag_server_wallet_webhooks_handler'
        },
        'GetWebhookByID' => #{
            path => "/wallet/v0/webhooks/:webhookID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_webhooks_handler'
        },
        'GetWebhooks' => #{
            path => "/wallet/v0/webhooks",
            method => <<"GET">>,
            handler => 'swag_server_wallet_webhooks_handler'
        },
        'CreateDestination' => #{
            path => "/wallet/v0/destinations",
            method => <<"POST">>,
            handler => 'swag_server_wallet_withdrawals_handler'
        },
        'CreateQuote' => #{
            path => "/wallet/v0/withdrawal-quotes",
            method => <<"POST">>,
            handler => 'swag_server_wallet_withdrawals_handler'
        },
        'CreateWithdrawal' => #{
            path => "/wallet/v0/withdrawals",
            method => <<"POST">>,
            handler => 'swag_server_wallet_withdrawals_handler'
        },
        'GetDestination' => #{
            path => "/wallet/v0/destinations/:destinationID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_withdrawals_handler'
        },
        'GetDestinationByExternalID' => #{
            path => "/wallet/v0/external-ids/destinations/:externalID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_withdrawals_handler'
        },
        'GetWithdrawal' => #{
            path => "/wallet/v0/withdrawals/:withdrawalID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_withdrawals_handler'
        },
        'GetWithdrawalByExternalID' => #{
            path => "/wallet/v0/external-ids/withdrawals/:externalID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_withdrawals_handler'
        },
        'GetWithdrawalEvents' => #{
            path => "/wallet/v0/withdrawals/:withdrawalID/events/:eventID",
            method => <<"GET">>,
            handler => 'swag_server_wallet_withdrawals_handler'
        },
        'IssueDestinationGrant' => #{
            path => "/wallet/v0/destinations/:destinationID/grants",
            method => <<"POST">>,
            handler => 'swag_server_wallet_withdrawals_handler'
        },
        'ListDestinations' => #{
            path => "/wallet/v0/destinations",
            method => <<"GET">>,
            handler => 'swag_server_wallet_withdrawals_handler'
        },
        'ListWithdrawals' => #{
            path => "/wallet/v0/withdrawals",
            method => <<"GET">>,
            handler => 'swag_server_wallet_withdrawals_handler'
        },
        'PollWithdrawalEvents' => #{
            path => "/wallet/v0/withdrawals/:withdrawalID/events",
            method => <<"GET">>,
            handler => 'swag_server_wallet_withdrawals_handler'
        }
    }.
