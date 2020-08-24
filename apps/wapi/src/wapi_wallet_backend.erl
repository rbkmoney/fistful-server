-module(wapi_wallet_backend).

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().
-type id() :: binary().
-type external_id() :: binary().

-export([create/2]).
-export([get/2]).
-export([get_by_external_id/2]).
-export([get_account/2]).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

%% Pipeline

-spec create(req_data(), handler_context()) ->
    {ok, response_data()} | {error, WalletError}
    when WalletError ::
        {identity, unauthorized} |
        {identity, notfound} |
        {currency, notfound} |
        inaccessible |
        {external_id_conflict, id()}.

create(Params = #{<<"identity">> := IdentityID}, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            case wapi_backend_utils:gen_id(wallet, Params, HandlerContext) of
                {ok, ID} ->
                    Context = wapi_backend_utils:make_ctx(Params, HandlerContext),
                    create(ID, Params, Context, HandlerContext);
                {error, {external_id_conflict, _}} = Error ->
                    Error
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}};
        {error, notfound} ->
            {error, {identity, notfound}}
    end.

create(WalletID, Params, Context, HandlerContext) ->
    WalletParams = marshal(wallet_params, Params#{<<"id">> => WalletID}),
    Request = {fistful_wallet, 'Create', [WalletParams, marshal(context, Context)]},
    case service_call(Request, HandlerContext) of
        {ok, Wallet} ->
            {ok, unmarshal(wallet, Wallet)};
        {exception, #fistful_CurrencyNotFound{}} ->
            {error, {currency, notfound}};
        {exception, #fistful_PartyInaccessible{}} ->
            {error, inaccessible};
        {exception, Details} ->
            {error, Details}
    end.

-spec get_by_external_id(external_id(), handler_context()) ->
    {ok, response_data()} |
    {error, {wallet, notfound}} |
    {error, {wallet, unauthorized}} |
    {error, {external_id, {unknown_external_id, external_id()}}}.

get_by_external_id(ExternalID, #{woody_context := WoodyContext} = HandlerContext) ->
    AuthContext = wapi_handler_utils:get_auth_context(HandlerContext),
    PartyID = uac_authorizer_jwt:get_subject_id(AuthContext),
    IdempotentKey = wapi_backend_utils:get_idempotent_key(wallet, PartyID, ExternalID),
    case bender_client:get_internal_id(IdempotentKey, WoodyContext) of
        {ok, {WalletID, _}, _} ->
            get(WalletID, HandlerContext);
        {error, internal_id_not_found} ->
            {error, {external_id, {unknown_external_id, ExternalID}}}
    end.

-spec get(id(), handler_context()) ->
    {ok, response_data()} |
    {error, {wallet, notfound}} |
    {error, {wallet, unauthorized}}.

get(WalletID, HandlerContext) ->
    Request = {fistful_wallet, 'Get', [WalletID, #'EventRange'{}]},
    case service_call(Request, HandlerContext) of
        {ok, WalletThrift} ->
            case wapi_access_backend:check_resource(wallet, WalletThrift, HandlerContext) of
                ok ->
                    {ok, unmarshal(wallet, WalletThrift)};
                {error, unauthorized} ->
                    {error, {wallet, unauthorized}}
            end;
        {exception, #fistful_WalletNotFound{}} ->
            {error, {wallet, notfound}}
    end.

-spec get_account(id(), handler_context()) ->
    {ok, response_data()} |
    {error, {wallet, notfound}} |
    {error, {wallet, unauthorized}}.

get_account(WalletID, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(wallet, WalletID, HandlerContext) of
        ok ->
            Request = {fistful_wallet, 'GetAccountBalance', [WalletID]},
            case service_call(Request, HandlerContext) of
                {ok, AccountBalanceThrift} ->
                    {ok, unmarshal(wallet_account_balance, AccountBalanceThrift)};
                {exception, #fistful_WalletNotFound{}} ->
                    {error, {wallet, notfound}}
            end;
        {error, unauthorized} ->
            {error, {wallet, unauthorized}};
        {error, notfound} ->
            {error, {wallet, notfound}}
    end.

%%
%% Internal
%%

service_call(Params, Ctx) ->
    wapi_handler_utils:service_call(Params, Ctx).

%% Marshaling

marshal(wallet_params, Params = #{
    <<"id">> := ID,
    <<"name">> := Name,
    <<"identity">> := IdentityID,
    <<"currency">> := CurrencyID
}) ->
    ExternalID = maps:get(<<"externalID">>, Params, undefined),
    #wlt_WalletParams{
        id = marshal(id, ID),
        name = marshal(string, Name),
        account_params = marshal(account_params, {IdentityID, CurrencyID}),
        external_id = marshal(id, ExternalID)
    };

marshal(account_params, {IdentityID, CurrencyID}) ->
    #account_AccountParams{
        identity_id = marshal(id, IdentityID),
        symbolic_code = marshal(string, CurrencyID)
    };

marshal(context, Ctx) ->
    ff_codec:marshal(context, Ctx);

marshal(T, V) ->
    ff_codec:marshal(T, V).

%%

unmarshal(wallet, #wlt_WalletState{
    id = WalletID,
    name = Name,
    blocking = Blocking,
    account = Account,
    external_id = ExternalID,
    created_at = CreatedAt,
    context = Ctx
}) ->
    #{
        identity := Identity,
        currency := Currency
    } = unmarshal(account, Account),
    Context = unmarshal(context, Ctx),
    genlib_map:compact(#{
        <<"id">> => unmarshal(id, WalletID),
        <<"name">> => unmarshal(string, Name),
        <<"isBlocked">> => unmarshal(blocking, Blocking),
        <<"identity">> => Identity,
        <<"currency">> => Currency,
        <<"createdAt">> => CreatedAt,
        <<"externalID">> => maybe_unmarshal(id, ExternalID),
        <<"metadata">> => wapi_backend_utils:get_from_ctx(<<"metadata">>, Context)
    });

unmarshal(blocking, unblocked) ->
    false;
unmarshal(blocking, blocked) ->
    true;


unmarshal(wallet_account_balance, #account_AccountBalance{
    current = OwnAmount,
    expected_min = AvailableAmount,
    currency = Currency
}) ->
    EncodedCurrency = unmarshal(currency_ref, Currency),
    #{
        <<"own">> => #{
            <<"amount">>   => OwnAmount,
            <<"currency">> => EncodedCurrency
        },
        <<"available">> => #{
            <<"amount">>   => AvailableAmount,
            <<"currency">> => EncodedCurrency
        }
    };

unmarshal(context, Ctx) ->
    ff_codec:unmarshal(context, Ctx);

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).
