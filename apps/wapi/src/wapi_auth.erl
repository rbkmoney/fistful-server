-module(wapi_auth).

-export([authorize_api_key/3]).
-export([authorize_operation/3]).
-export([issue_access_token/2]).
-export([issue_access_token/3]).

-export([get_subject_id/1]).
-export([get_claims/1]).
-export([get_claim/2]).
-export([get_claim/3]).
-export([get_consumer/1]).

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
-type auth_error() :: atom() | {atom(), auth_error()}.

-spec authorize_operation(operation_id(), request_data(), wapi_handler:context()) ->
    ok | {error, auth_error()}.

authorize_operation('CreateWithdrawal', #{'WithdrawalParameters' := Params}, Context) ->
    authorize_withdrawal(Params, Context);
authorize_operation(_OperationID, _Req, _) ->
    ok.
%% TODO: implement authorization
%% authorize_operation(OperationID, Req, {{_SubjectID, ACL}, _}) ->
%%     Access = get_operation_access(OperationID, Req),
%%     try authorize(Access, ACL)
%%     catch
%%         throw:unauthorized ->
%%             {error, unauthorized}
%%     end.

%%

authorize_withdrawal(Params, Context) ->
    case {
        authorize_withdrawal_by_grants(Params),
        authorize_withdrawal_by_owner(Params, Context)
    } of
        {ok, _} ->
            ok = lager:info("Withdrawal authorized via grants"),
            ok;
        {_, ok} ->
            ok = lager:info("Withdrawal authorized via owner bearer token"),
            ok;
        {Error = {error, _}, _} ->
            Error
    end.

authorize_withdrawal_by_owner(#{<<"destination">> := DestinationID, <<"wallet">> := WalletID}, Context) ->
    case {
        wapi_wallet_ff_backend:get_destination(DestinationID, Context),
        wapi_wallet_ff_backend:get_wallet(WalletID, Context)
    } of
        {{ok, _}, {ok, _}}      -> ok;
        {Error = {error, _}, _} -> Error
    end.

%% TODO: properly authorize via wallet grant
authorize_withdrawal_by_grants(Params = #{
    <<"destination">> := DestinationID
    %% <<"wallet">>      := WalletID,
    %% <<"body">>        := Body
}) ->
    try
        ok = authorize_destination(DestinationID, genlib_map:get(<<"destinationGrant">>, Params))

        %% ok = authorize_wallet(WalletID, Body, genlib_map:get(<<"walletGrant">>, Params))
    catch
        throw:{unauthorized, Error} -> {error, Error}
    end.

authorize_destination(_ID, undefined) ->
    erlang:throw({unauthorized, {destination_grant, missing}});
authorize_destination(ID, Grant) ->
    case wapi_authorizer_jwt:verify(Grant) of
        {ok, {{_, ACL}, _Claims}} ->
            authorize(get_resource_accesses(destination, ID, write), ACL);
        {error, Error} ->
            erlang:throw({unauthorized, {destination_grant, Error}})
    end.

%% TODO: properly authorize via wallet grant
%% authorize_wallet(_ID, undefined, _) ->
%%     erlang:throw({unauthorized, {wallet_grant, missing}});
%% authorize_wallet(ID, Grant, _Body) ->
%%     case wapi_authorizer_jwt:verify(Grant) of
%%         {ok, {{_, _ACL}, _Claims}} ->
%%             %% authorize(get_resource_accesses(wallet, ID, write), ACL);
%%         {error, Error} ->
%%             erlang:throw({unauthorized, {wallet_grant, Error}})
%%     end.

get_resource_accesses(Resource, ID, Permission) ->
    [{get_resource_accesses(Resource, ID), Permission}].

get_resource_accesses(destination, ID) ->
    [party, {destinations, ID}].
%% TODO: properly authorize via wallet grant
%% get_resource_accesses(wallet, ID) ->
%%     [party, {wallets, ID}].

authorize(Access, ACL) ->
    case lists:all(
        fun ({Scope, Permission}) ->
            lists:member(Permission, wapi_acl:match(Scope, ACL))
        end,
        Access
    ) of
        true ->
            ok;
        false ->
            erlang:throw({unauthorized, no_permissions})
    end.

-type token_spec() ::
    {destinations, DestinationID :: binary()}.

-spec issue_access_token(wapi_handler_utils:owner(), token_spec()) ->
    wapi_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec) ->
    issue_access_token(PartyID, TokenSpec, unlimited).

-type expiration() ::
    {deadline, machinery:timestamp() | pos_integer()} |
    {lifetime, Seconds :: pos_integer()}              |
    unlimited                                         .

-spec issue_access_token(wapi_handler_utils:owner(), token_spec(), expiration()) ->
    wapi_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec, Expiration0) ->
    Expiration = get_expiration(Expiration0),
    {Claims, ACL} = resolve_token_spec(TokenSpec),
    wapi_utils:unwrap(wapi_authorizer_jwt:issue({{PartyID, wapi_acl:from_list(ACL)}, Claims}, Expiration)).

-spec get_expiration(expiration()) ->
    wapi_authorizer_jwt:expiration().
get_expiration(Exp = unlimited) ->
    Exp;
get_expiration({deadline, {DateTime, Usec}}) ->
    {deadline, genlib_time:daytime_to_unixtime(DateTime) + Usec div 1000000};
get_expiration(Exp = {deadline, _Sec}) ->
    Exp;
get_expiration(Exp = {lifetime, _Sec}) ->
    Exp.

-type acl() :: [{wapi_acl:scope(), wapi_acl:permission()}].

-spec resolve_token_spec(token_spec()) ->
    {claims(), acl()}.
resolve_token_spec({destinations, DestinationId}) ->
    Claims = #{},
    ACL = [
        {[party, {destinations, DestinationId}], write}
    ],
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
            invoice_templates => #{invoice_template_invoices => #{}},
            wallets           => #{},
            destinations      => #{}
        },
        customers         => #{bindings => #{}},
        invoices          => #{payments => #{}},
        payment_resources => #{}
    }.

-spec get_consumer(claims()) ->
    consumer().
get_consumer(Claims) ->
    case maps:get(<<"cons">>, Claims, <<"merchant">>) of
        <<"merchant">> -> merchant;
        <<"client"  >> -> client;
        <<"provider">> -> provider
    end.
