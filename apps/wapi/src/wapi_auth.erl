-module(wapi_auth).

-export([authorize_operation/3]).
-export([issue_access_token/2]).
-export([issue_access_token/3]).

-export([get_resource_hierarchy/0]).

-export([get_verification_options/0]).

-export([get_access_config/0]).

-export([get_signee/0]).

-export([create_wapi_context/1]).

-type context() :: uac_authorizer_jwt:t().
-type claims() :: uac_authorizer_jwt:claims().
-type consumer() :: client | merchant | provider.

-export_type([context/0]).
-export_type([claims/0]).
-export_type([consumer/0]).

-type operation_id() :: wapi_handler:operation_id().

%%

% TODO
% We need shared type here, exported somewhere in swagger app
-type request_data() :: #{atom() | binary() => term()}.
-type auth_method() :: bearer_token | grant.
-type resource() :: wallet | destination.
-type auth_details() :: auth_method() | [{resource(), auth_details()}].

-define(DOMAIN, <<"wallet-api">>).

-spec authorize_operation(operation_id(), request_data(), wapi_handler:context()) -> ok | {error, unauthorized}.
authorize_operation(OperationID, Req, #{swagger_context := #{auth_context := AuthContext}}) ->
    OperationACL = get_operation_access(OperationID, Req),
    uac:authorize_operation(OperationACL, AuthContext).

-type token_spec() ::
    {p2p_templates, P2PTemplateID :: binary(), Data :: map()}
    | {p2p_template_transfers, P2PTemplateID :: binary(), Data :: map()}
    | {destinations, DestinationID :: binary()}
    | {wallets, WalletID :: binary(), Asset :: map()}.

-spec issue_access_token(wapi_handler_utils:owner(), token_spec()) -> uac_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec) ->
    issue_access_token(PartyID, TokenSpec, unlimited).

-spec issue_access_token(wapi_handler_utils:owner(), token_spec(), uac_authorizer_jwt:expiration()) ->
    uac_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec, Expiration) ->
    Claims0 = resolve_token_spec(TokenSpec),
    Claims = Claims0#{<<"exp">> => Expiration},
    wapi_utils:unwrap(
        uac_authorizer_jwt:issue(
            wapi_utils:get_unique_id(),
            PartyID,
            Claims,
            get_signee()
        )
    ).

-spec resolve_token_spec(token_spec()) -> claims().
resolve_token_spec({p2p_templates, P2PTemplateID, #{<<"expiration">> := Expiration}}) ->
    #{
        <<"data">> => #{<<"expiration">> => Expiration},
        <<"resource_access">> => #{
            ?DOMAIN => uac_acl:from_list(
                [
                    {[{p2p_templates, P2PTemplateID}, p2p_template_tickets], write},
                    {[{p2p_templates, P2PTemplateID}], read}
                ]
            )
        }
    };
resolve_token_spec({p2p_template_transfers, P2PTemplateID, #{<<"transferID">> := TransferID}}) ->
    #{
        <<"data">> => #{<<"transferID">> => TransferID},
        <<"resource_access">> => #{
            ?DOMAIN => uac_acl:from_list(
                [
                    {[{p2p_templates, P2PTemplateID}, p2p_template_transfers], write},
                    {[{p2p_templates, P2PTemplateID}, p2p_template_quotes], write},
                    {[{p2p, TransferID}], read}
                ]
            )
        }
    };
resolve_token_spec({destinations, DestinationId}) ->
    #{
        <<"resource_access">> => #{
            ?DOMAIN => uac_acl:from_list(
                [{[party, {destinations, DestinationId}], write}]
            )
        }
    };
resolve_token_spec({wallets, WalletId, #{<<"amount">> := Amount, <<"currency">> := Currency}}) ->
    #{
        <<"amount">> => Amount,
        <<"currency">> => Currency,
        <<"resource_access">> => #{
            ?DOMAIN => uac_acl:from_list(
                [{[party, {wallets, WalletId}], write}]
            )
        }
    }.

%%

get_operation_access('GetCurrency', _) ->
    [{[party], read}];
get_operation_access('ListDeposits', _) ->
    [{[party], read}];
get_operation_access('ListDepositReverts', _) ->
    [{[party], read}];
get_operation_access('ListDepositAdjustments', _) ->
    [{[party], read}];
get_operation_access('ListDestinations', _) ->
    [{[party, destinations], read}];
get_operation_access('CreateDestination', _) ->
    [{[party, destinations], write}];
get_operation_access('GetDestination', #{destinationID := ID}) ->
    [{[party, {destinations, ID}], read}];
get_operation_access('GetDestinationByExternalID', _) ->
    [{[party, destinations], read}];
get_operation_access('IssueDestinationGrant', #{destinationID := ID}) ->
    [{[party, {destinations, ID}], write}];
get_operation_access('DownloadFile', _) ->
    [{[party], write}];
get_operation_access('ListIdentities', _) ->
    [{[party], read}];
get_operation_access('CreateIdentity', _) ->
    [{[party], write}];
get_operation_access('GetIdentity', _) ->
    [{[party], read}];
get_operation_access('ListIdentityChallenges', _) ->
    [{[party], read}];
get_operation_access('StartIdentityChallenge', _) ->
    [{[party], write}];
get_operation_access('GetIdentityChallenge', _) ->
    [{[party], read}];
get_operation_access('PollIdentityChallengeEvents', _) ->
    [{[party], read}];
get_operation_access('GetIdentityChallengeEvent', _) ->
    [{[party], read}];
get_operation_access('CreateReport', _) ->
    [{[party], write}];
get_operation_access('GetReports', _) ->
    [{[party], read}];
get_operation_access('GetReport', _) ->
    [{[party], read}];
get_operation_access('ListProviders', _) ->
    [{[party], read}];
get_operation_access('GetProvider', _) ->
    [{[party], read}];
get_operation_access('ListProviderIdentityClasses', _) ->
    [{[party], read}];
get_operation_access('GetProviderIdentityClass', _) ->
    [{[party], read}];
get_operation_access('ListProviderIdentityLevels', _) ->
    [{[party], read}];
get_operation_access('GetProviderIdentityLevel', _) ->
    [{[party], read}];
get_operation_access('GetResidence', _) ->
    [{[party], read}];
get_operation_access('ListWallets', _) ->
    [{[party, wallets], read}];
get_operation_access('CreateWallet', _) ->
    [{[party, wallets], write}];
get_operation_access('GetWallet', #{walletID := ID}) ->
    [{[party, {wallets, ID}], read}];
get_operation_access('GetWalletByExternalID', _) ->
    [{[party], read}];
get_operation_access('GetWalletAccount', #{walletID := ID}) ->
    [{[party, {wallets, ID}], read}];
get_operation_access('IssueWalletGrant', #{walletID := ID}) ->
    [{[party, {wallets, ID}], write}];
get_operation_access('CreateWebhook', _) ->
    [{[webhooks], write}];
get_operation_access('GetWebhooks', _) ->
    [{[webhooks], read}];
get_operation_access('GetWebhookByID', _) ->
    [{[webhooks], read}];
get_operation_access('DeleteWebhookByID', _) ->
    [{[webhooks], write}];
get_operation_access('CreateQuote', _) ->
    [{[withdrawals, withdrawal_quotes], write}];
get_operation_access('ListWithdrawals', _) ->
    [{[withdrawals], read}];
get_operation_access('CreateWithdrawal', _) ->
    [{[withdrawals], write}];
get_operation_access('GetWithdrawal', _) ->
    [{[withdrawals], read}];
get_operation_access('GetWithdrawalByExternalID', _) ->
    [{[withdrawals], read}];
get_operation_access('PollWithdrawalEvents', _) ->
    [{[withdrawals], read}];
get_operation_access('GetWithdrawalEvents', _) ->
    [{[withdrawals], read}];
get_operation_access('CreateP2PTransfer', _) ->
    [{[p2p], write}];
get_operation_access('QuoteP2PTransfer', _) ->
    [{[p2p, p2p_quotes], write}];
get_operation_access('GetP2PTransfer', #{'p2pTransferID' := ID}) ->
    [{[{p2p, ID}], read}];
get_operation_access('GetP2PTransferEvents', _) ->
    [{[p2p], read}];
get_operation_access('CreateP2PTransferTemplate', _) ->
    [{[p2p_templates], write}];
get_operation_access('GetP2PTransferTemplateByID', #{'p2pTransferTemplateID' := ID}) ->
    [{[{p2p_templates, ID}], read}];
get_operation_access('BlockP2PTransferTemplate', _) ->
    [{[p2p_templates], write}];
get_operation_access('IssueP2PTransferTemplateAccessToken', _) ->
    [{[p2p_templates], write}];
get_operation_access('IssueP2PTransferTicket', #{'p2pTransferTemplateID' := ID}) ->
    [{[{p2p_templates, ID}, p2p_template_tickets], write}];
get_operation_access('CreateP2PTransferWithTemplate', #{'p2pTransferTemplateID' := ID}) ->
    [{[{p2p_templates, ID}, p2p_template_transfers], write}];
get_operation_access('QuoteP2PTransferWithTemplate', #{'p2pTransferTemplateID' := ID}) ->
    [{[{p2p_templates, ID}, p2p_template_quotes], write}];
get_operation_access('CreateW2WTransfer', _) ->
    [{[w2w], write}];
get_operation_access('GetW2WTransfer', _) ->
    [{[w2w], read}].

-spec get_access_config() -> map().
get_access_config() ->
    #{
        domain_name => ?DOMAIN,
        resource_hierarchy => get_resource_hierarchy()
    }.

-spec get_resource_hierarchy() -> #{atom() => map()}.
%% TODO put some sense in here
% This resource hierarchy refers to wallet api actaully
get_resource_hierarchy() ->
    #{
        party => #{
            wallets => #{},
            destinations => #{}
        },
        p2p => #{p2p_quotes => #{}},
        p2p_templates => #{
            p2p_template_tickets => #{},
            p2p_template_transfers => #{},
            p2p_template_quotes => #{}
        },
        w2w => #{},
        webhooks => #{},
        withdrawals => #{withdrawal_quotes => #{}}
    }.

-spec get_verification_options() -> uac:verification_opts().
get_verification_options() ->
    #{
        domains_to_decode => [<<"common-api">>, <<"wallet-api">>]
    }.

all_scopes(Key, Value, AccIn) ->
    Scopes0 = maps:fold(fun all_scopes/3, [], Value),
    Scopes1 = lists:map(fun(Scope) -> [Key | Scope] end, Scopes0),
    Scopes1 ++ [[Key] | AccIn].

hierarchy_to_acl(Hierarchy) ->
    Scopes = maps:fold(fun all_scopes/3, [], Hierarchy),
    lists:foldl(
        fun(Scope, ACL0) ->
            uac_acl:insert_scope(Scope, write, uac_acl:insert_scope(Scope, read, ACL0))
        end,
        uac_acl:new(),
        Scopes
    ).

-spec create_wapi_context(uac_authorizer_jwt:t()) -> uac_authorizer_jwt:t().
create_wapi_context({ID, Party, Claims}) ->
    % Create new acl
    % So far we want to give every token full set of permissions
    % This is a temporary solution
    % @TODO remove when we issue new tokens
    NewClaims = maybe_grant_wapi_roles(Claims),
    {ID, Party, NewClaims}.

maybe_grant_wapi_roles(Claims) ->
    case genlib_map:get(<<"resource_access">>, Claims) of
        #{?DOMAIN := _} ->
            Claims;
        #{<<"common-api">> := _} ->
            Hierarchy = wapi_auth:get_resource_hierarchy(),
            Claims#{<<"resource_access">> => #{?DOMAIN => hierarchy_to_acl(Hierarchy)}};
        _ ->
            undefined
    end.

-spec get_signee() -> term().
get_signee() ->
    wapi_utils:unwrap(application:get_env(wapi, signee)).
