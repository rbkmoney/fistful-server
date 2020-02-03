-module(wapi_wallet_backend).

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().
-type id() :: binary().

-export([create/2]).
-export([get/2]).

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
                    PreparedParams = genlib_map:compact(Params#{
                        <<"id">> => ID,
                        <<"context">> => Context
                    }),
                    create(ID, PreparedParams, HandlerContext);
                {error, {external_id_conflict, _}} = Error ->
                    Error
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}}
    end.

create(WalletID, Params, HandlerContext) ->
    WalletParams = marshal(wallet_params, Params),
    Request = {fistful_wallet, 'Create', [WalletID, WalletParams]},
    case service_call(Request, HandlerContext) of
        {ok, Wallet} ->
            {ok, unmarshal(wallet, Wallet)};
        {exception, #fistful_CurrencyNotFound{}} ->
            {error, {currency, notfound}};
        {exception, #fistful_PartyInaccessible{}} ->
            {error, inaccessible};
        {exception, #fistful_IDExists{}} ->
            get(WalletID, HandlerContext);
        {exception, Details} ->
            {error, Details}
    end.

-spec get(id(), handler_context()) ->
    {ok, response_data()} |
    {error, {wallet, notfound}} |
    {error, {wallet, unauthorized}}.

get(WalletID, HandlerContext) ->
    Request = {fistful_wallet, 'Get', [WalletID]},
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

%%
%% Internal
%%

service_call(Params, Ctx) ->
    wapi_handler_utils:service_call(Params, Ctx).

%% Marshaling

marshal(wallet_params, Params = #{
    <<"name">> := Name,
    <<"identity">> := IdentityID,
    <<"currency">> := CurrencyID,
    <<"context">> := Ctx
}) ->
    ExternalID = maps:get(<<"externalID">>, Params, undefined),
    #wlt_WalletParams{
        name = marshal(string, Name),
        account_params = marshal(account_params, {IdentityID, CurrencyID}),
        external_id = marshal(id, ExternalID),
        context = marshal(context, Ctx)
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

unmarshal(wallet, #wlt_Wallet{
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
        <<"isBlocked">> => unmarshal(blocked, Blocking),
        <<"identity">> => Identity,
        <<"currency">> => Currency,
        <<"created_at">> => CreatedAt,
        <<"externalID">> => maybe_unmarshal(id, ExternalID),
        <<"metadata">> => wapi_backend_utils:get_from_ctx(<<"metadata">>, Context)
    });

unmarshal(blocked, unblocked) ->
    false;
unmarshal(blocked, blocked) ->
    true;

unmarshal(context, Ctx) ->
    ff_codec:unmarshal(context, Ctx);

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).
