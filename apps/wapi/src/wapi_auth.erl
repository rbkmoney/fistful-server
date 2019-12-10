-module(wapi_auth).

-export([authorize_operation/3]).
-export([issue_access_token/2]).
-export([issue_access_token/3]).

-export([get_subject_id/1]).
-export([get_claims/1]).
-export([get_claim/2]).
-export([get_claim/3]).

-export([get_resource_hierarchy/0]).

-export([get_access_config/0]).

-type context () :: uac_authorizer_jwt:t().
-type claims  () :: uac_authorizer_jwt:claims().
-type consumer() :: client | merchant | provider.

-export_type([context /0]).
-export_type([claims  /0]).
-export_type([consumer/0]).

-type operation_id() :: wapi_handler:operation_id().

%%

% TODO
% We need shared type here, exported somewhere in swagger app
-type request_data() :: #{atom() | binary() => term()}.
-type auth_method()  :: bearer_token | grant.
-type resource()     :: wallet | destination.
-type auth_details() :: auth_method() | [{resource(), auth_details()}].
-type auth_error()   :: [{resource(), [{auth_method(), atom()}]}].

-define(SIGNEE, wapi).

-spec authorize_operation(operation_id(), request_data(), wapi_handler:context()) ->
    ok  | {error, auth_error()}.

authorize_operation('CreateWithdrawal' = OperationID,
    #{'WithdrawalParameters' := Params} = Req,
    #{swagger_context := #{auth_context := AuthContext}} = Context
) ->
    case authorize_withdrawal(Params, Context) of
        {ok, _} ->
            OperationACL = get_operation_access(OperationID, Req),
            uac:authorize_operation(OperationACL, AuthContext);
        {error, _} = Error ->
            Error
    end;
authorize_operation(OperationID, Req, #{swagger_context := #{auth_context := AuthContext}}) ->
    OperationACL = get_operation_access(OperationID, Req),
    uac:authorize_operation(OperationACL, AuthContext).

authorize_withdrawal(Params, Context) ->
    lists:foldl(
        fun(R, AuthState) ->
            case {authorize_resource(R, Params, Context), AuthState} of
                {{ok, AuthMethod}, {ok, AuthData}}   -> {ok, [{R, AuthMethod} | AuthData]};
                {{ok, _}, {error, _}}                -> AuthState;
                {{error, Error}, {error, ErrorData}} -> {error, [{R, Error} | ErrorData]};
                {{error, Error}, {ok, _}}            -> {error, [{R, Error}]}
            end
        end,
        {ok, []},
        [destination, wallet]
    ).

authorize_resource(Resource, Params, Context) ->
    %% TODO
    %%  - ff_pipeline:do/1 would make the code rather more clear here.
    authorize_resource_by_bearer(authorize_resource_by_grant(Resource, Params), Resource, Params, Context).

authorize_resource_by_bearer(ok, _Resource, _Params, _Context) ->
    {ok, grant};
authorize_resource_by_bearer({error, GrantError}, Resource, Params, Context) ->
    case get_resource(Resource, maps:get(genlib:to_binary(Resource), Params), Context) of
        {ok, _} ->
            {ok, bearer_token};
        {error, BearerError} ->
            {error, [{bearer_token, BearerError}, {grant, GrantError}]}
    end.

get_resource(destination, ID, Context) ->
    wapi_wallet_ff_backend:get_destination(ID, Context);
get_resource(wallet, ID, Context) ->
    wapi_wallet_ff_backend:get_wallet(ID, Context).

authorize_resource_by_grant(R = destination, #{
    <<"destination">>      := ID,
    <<"destinationGrant">> := Grant
}) ->
    authorize_resource_by_grant(R, Grant, get_resource_accesses(R, ID, write), undefined);
authorize_resource_by_grant(R = wallet, #{
    <<"wallet">>      := ID,
    <<"walletGrant">> := Grant,
    <<"body">>        := WithdrawalBody
}) ->
    authorize_resource_by_grant(R, Grant, get_resource_accesses(R, ID, write), WithdrawalBody);
authorize_resource_by_grant(_, _) ->
    {error, missing}.

authorize_resource_by_grant(Resource, Grant, Access, Params) ->
    case uac_authorizer_jwt:verify(Grant, #{}) of
        {ok, {_Id, {_, ACL}, Claims}} ->
            verify_claims(Resource, verify_access(Access, ACL, Claims), Params);
        Error = {error, _} ->
            Error
    end.

get_resource_accesses(Resource, ID, Permission) ->
    [{get_resource_accesses(Resource, ID), Permission}].

get_resource_accesses(destination, ID) ->
    [party, {destinations, ID}];
get_resource_accesses(wallet, ID) ->
    [party, {wallets, ID}].

verify_access(Access, ACL, Claims) ->
    case lists:all(
        fun ({Scope, Permission}) -> lists:member(Permission, uac_acl:match(Scope, ACL)) end,
        Access
    ) of
        true  -> {ok, Claims};
        false -> {error, insufficient_access}
    end.

verify_claims(_, Error = {error, _}, _) ->
    Error;
verify_claims(destination, {ok, _Claims}, _) ->
    ok;
verify_claims(wallet,
    {ok, #{<<"amount">> := GrantAmount, <<"currency">> := Currency}},
    #{     <<"amount">> := ReqAmount,   <<"currency">> := Currency }
) when GrantAmount >= ReqAmount ->
    ok;
verify_claims(_, _, _) ->
    {error, insufficient_claims}.

-type token_spec() ::
    {destinations, DestinationID :: binary()} |
    {wallets, WalletID :: binary(), Asset :: map()}.

-spec issue_access_token(wapi_handler_utils:owner(), token_spec()) ->
    uac_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec) ->
    issue_access_token(PartyID, TokenSpec, unlimited).

-spec issue_access_token(wapi_handler_utils:owner(), token_spec(), uac_authorizer_jwt:expiration()) ->
    uac_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec, Expiration) ->
    {Claims, DomainRoles} = resolve_token_spec(TokenSpec),
    wapi_utils:unwrap(uac_authorizer_jwt:issue(
        wapi_utils:get_unique_id(),
        Expiration,
        PartyID,
        DomainRoles,
        Claims,
        ?SIGNEE
    )).

-spec resolve_token_spec(token_spec()) ->
    {claims(), uac_authorizer_jwt:domains()}.
resolve_token_spec({destinations, DestinationId}) ->
    Claims = #{},
    DomainRoles = #{<<"wallet-api">> => uac_acl:from_list(
        [{[party, {destinations, DestinationId}], write}]
    )},
    {Claims, DomainRoles};
resolve_token_spec({wallets, WalletId, #{<<"amount">> := Amount, <<"currency">> := Currency}}) ->
    Claims = #{<<"amount">> => Amount, <<"currency">> => Currency},
    DomainRoles = #{<<"wallet-api">> => uac_acl:from_list(
        [{[party, {wallets, WalletId}], write}]
    )},
    {Claims, DomainRoles}.

-spec get_subject_id(context()) -> binary().

get_subject_id({_Id, {SubjectID, _ACL}, _}) ->
    SubjectID.

-spec get_claims(context()) -> claims().

get_claims({_Id, _Subject, Claims}) ->
    Claims.

-spec get_claim(binary(), context()) -> term().

get_claim(ClaimName, {_Id, _Subject, Claims}) ->
    maps:get(ClaimName, Claims).

-spec get_claim(binary(), context(), term()) -> term().

get_claim(ClaimName, {_Id, _Subject, Claims}, Default) ->
    maps:get(ClaimName, Claims, Default).

%%

get_operation_access('GetCurrency', _) ->
    [{[party], read}];
get_operation_access('ListDeposits', _) ->
    [{[party], read}];
get_operation_access('ListDestinations', _) ->
    [{[party], read}];
get_operation_access('CreateDestination', _) ->
    [{[party], write}];
get_operation_access('GetDestination', _) ->
    [{[party], read}];
get_operation_access('IssueDestinationGrant', _) ->
    [{[party], write}];
get_operation_access('DownloadFile', _) ->
    [{[party], read}];
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
    [{[party], read}];
get_operation_access('CreateWallet', _) ->
    [{[party], write}];
get_operation_access('GetWallet', _) ->
    [{[party], read}];
get_operation_access('GetWalletAccount', _) ->
    [{[party], read}];
get_operation_access('IssueWalletGrant', _) ->
    [{[party], write}];
get_operation_access('CreateWebhook', _) ->
    [{[webhooks], write}];
get_operation_access('GetWebhooks', _) ->
    [{[webhooks], read}];
get_operation_access('GetWebhookByID', _) ->
    [{[webhooks], read}];
get_operation_access('DeleteWebhookByID', _) ->
    [{[webhooks], write}];
get_operation_access('CreateQuote', _) ->
    [{[party], write}];
get_operation_access('ListWithdrawals', _) ->
    [{[withdrawals], read}];
get_operation_access('CreateWithdrawal', _) ->
    [{[withdrawals], write}];
get_operation_access('GetWithdrawal', _) ->
    [{[withdrawals], read}];
get_operation_access('PollWithdrawalEvents', _) ->
    [{[withdrawals], read}];
get_operation_access('GetWithdrawalEvents', _) ->
    [{[withdrawals], read}].


-spec get_access_config() -> map().

get_access_config() ->
    #{
        domain_name => <<"wallet-api">>,
        resource_hierarchy => get_resource_hierarchy()
    }.

-spec get_resource_hierarchy() -> #{atom() => map()}.

%% TODO put some sense in here
get_resource_hierarchy() ->
    #{
        party => #{
            wallets           => #{},
            destinations      => #{}
        },
        webhooks    => #{},
        withdrawals => #{}
    }.
