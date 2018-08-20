-module(wapi_auth).

-export([authorize_api_key/3]).
-export([authorize_operation/3]).
-export([issue_access_token/2]).
-export([issue_access_token/3]).

-export([get_subject_id/1]).
-export([get_claims/1]).
-export([get_claim/2]).
-export([get_claim/3]).

-export([get_resource_hierarchy/0]).

-type context () :: wapi_authorizer_jwt:t().
-type claims  () :: wapi_authorizer_jwt:claims().
-type consumer() :: client | merchant | provider.

-export_type([context /0]).
-export_type([claims  /0]).
-export_type([consumer/0]).

-type operation_id() :: wapi_handler:operation_id().

-type api_key() ::
    swag_wallet_server:api_key() |
    swag_payres_server:api_key() |
    swag_privdoc_server:api_key().

-type handler_opts() :: wapi_handler:opts().

-spec authorize_api_key(operation_id(), api_key(), handler_opts()) ->
    {true, context()}. %% | false.

authorize_api_key(OperationID, ApiKey, _Opts) ->
    case parse_api_key(ApiKey) of
        {ok, {Type, Credentials}} ->
            case do_authorize_api_key(OperationID, Type, Credentials) of
                {ok, Context} ->
                    {true, Context};
                {error, Error} ->
                    _ = log_auth_error(OperationID, Error),
                    false
            end;
        {error, Error} ->
            _ = log_auth_error(OperationID, Error),
            false
    end.

log_auth_error(OperationID, Error) ->
    lager:info("API Key authorization failed for ~p due to ~p", [OperationID, Error]).

-spec parse_api_key(ApiKey :: api_key()) ->
    {ok, {bearer, Credentials :: binary()}} | {error, Reason :: atom()}.

parse_api_key(ApiKey) ->
    case ApiKey of
        <<"Bearer ", Credentials/binary>> ->
            {ok, {bearer, Credentials}};
        _ ->
            {error, unsupported_auth_scheme}
    end.

-spec do_authorize_api_key(
    OperationID :: operation_id(),
    Type :: atom(),
    Credentials :: binary()
) ->
    {ok, Context :: context()} | {error, Reason :: atom()}.

do_authorize_api_key(_OperationID, bearer, Token) ->
    % NOTE
    % We are knowingly delegating actual request authorization to the logic handler
    % so we could gather more data to perform fine-grained access control.
    wapi_authorizer_jwt:verify(Token).

%%

% TODO
% We need shared type here, exported somewhere in swagger app
-type request_data() :: #{atom() | binary() => term()}.
-type auth_method()  :: bearer_token | grant.
-type resource()     :: wallet | destination.
-type auth_details() :: auth_method() | [{resource(), auth_details()}].
-type auth_error()   :: [{resource(), [{auth_method(), atom()}]}].

-spec authorize_operation(operation_id(), request_data(), wapi_handler:context()) ->
    {ok, auth_details()}  | {error, auth_error()}.

authorize_operation('CreateWithdrawal', #{'WithdrawalParameters' := Params}, Context) ->
    authorize_withdrawal(Params, Context);
%% TODO: implement authorization
authorize_operation(_OperationID, _Req, _) ->
    {ok, bearer_token}.

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
    case wapi_authorizer_jwt:verify(Grant) of
        {ok, {{_, ACL}, Claims}} ->
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
        fun ({Scope, Permission}) -> lists:member(Permission, wapi_acl:match(Scope, ACL)) end,
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
    wapi_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec) ->
    issue_access_token(PartyID, TokenSpec, unlimited).

-spec issue_access_token(wapi_handler_utils:owner(), token_spec(), wapi_authorizer_jwt:expiration()) ->
    wapi_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec, Expiration) ->
    {Claims, ACL} = resolve_token_spec(TokenSpec),
    wapi_utils:unwrap(wapi_authorizer_jwt:issue({{PartyID, wapi_acl:from_list(ACL)}, Claims}, Expiration)).

-type acl() :: [{wapi_acl:scope(), wapi_acl:permission()}].

-spec resolve_token_spec(token_spec()) ->
    {claims(), acl()}.
resolve_token_spec({destinations, DestinationId}) ->
    Claims = #{},
    ACL = [{[party, {destinations, DestinationId}], write}],
    {Claims, ACL};
resolve_token_spec({wallets, WalletId, #{<<"amount">> := Amount, <<"currency">> := Currency}}) ->
    Claims = #{<<"amount">> => Amount, <<"currency">> => Currency},
    ACL    = [{[party, {wallets, WalletId}], write}],
    {Claims, ACL}.

-spec get_subject_id(context()) -> binary().

get_subject_id({{SubjectID, _ACL}, _}) ->
    SubjectID.

-spec get_claims(context()) -> claims().

get_claims({_Subject, Claims}) ->
    Claims.

-spec get_claim(binary(), context()) -> term().

get_claim(ClaimName, {_Subject, Claims}) ->
    maps:get(ClaimName, Claims).

-spec get_claim(binary(), context(), term()) -> term().

get_claim(ClaimName, {_Subject, Claims}, Default) ->
    maps:get(ClaimName, Claims, Default).

%%

%% TODO update for the wallet swag
%% -spec get_operation_access(operation_id(), request_data()) ->
%%     [{wapi_acl:scope(), wapi_acl:permission()}].

%% get_operation_access('CreateWithdrawal'     , #{'WithdrawalParameters' := #{<<"walletGrant">> => }}) ->
%%     [{[payment_resources], write}].

-spec get_resource_hierarchy() -> #{atom() => map()}.

%% TODO put some sense in here
get_resource_hierarchy() ->
    #{
        party => #{
            wallets           => #{},
            destinations      => #{}
        }
    }.
