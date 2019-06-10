-module(wapi_wallet_backend).

-type req_data()        :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data()   :: wapi_handler:response_data().

-export([create/2]).
-export([get/2]).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

%% Pipeline
-import(ff_pipeline, [do/1, unwrap/2]).

-spec create(req_data(), handler_context()) ->
    {ok, response_data()} | {error, WalletError}
    when WalletError ::
        {identity, unauthorized} |
        {identity, notfound}  |
        {currency, notfound}  |
        inaccessible          |
        {conflict, binary()}.

create(ParamsIn = #{<<"identity">> := IdentityID}, WoodyContext) ->
    do(fun() ->
        WalletParams = marshal(wallet_params, compose_wallet_params(ParamsIn, WoodyContext)),
        %% TODO after impl identity through thrift, rewrite it
        ok = wapi_access_backend:check_resource(identity, IdentityID, WoodyContext),
        Request = {fistful_wallet, 'Create', [WalletParams]},
        case service_call(Request, WoodyContext) of
            {ok, Wallet} ->
                unmarshal(wallet, Wallet);
            {exception, #fistful_CurrencyNotFound{}} ->
                throw({currency, notfound});
            {exception, #fistful_PartyInaccessible{}} ->
                throw(inaccessible);
            {exception, #fistful_IDExists{}} ->
                WalletID = get_id(WalletParams),
                {_, Hash} = wapi_backend_utils:create_params_hash(ParamsIn),
                get_and_compare_hash(WalletID, Hash, WoodyContext);
            {exception, Details} ->
                throw(Details)
        end
    end).

-spec get(binary(), handler_context()) ->
    {ok, response_data()} |
    {error, {wallet, notfound}} |
    {error, {wallet, unauthorized}}.

get(WalletID, WoodyCtx) ->
    Request = {fistful_wallet, 'Get', [WalletID]},
    do(fun() ->
        case service_call(Request, WoodyCtx) of
            {ok, WalletThrift} ->
                ok = unwrap(wallet, wapi_access_backend:is_authorized(WoodyCtx, get_context(WalletThrift))),
                unmarshal(wallet, WalletThrift);
            {exception, #fistful_WalletNotFound{}} ->
                throw({wallet, notfound})
        end
    end).

%%
%% Internal
%%

get_and_compare_hash(WalletID, Hash, WoodyContext) ->
    Request = {fistful_wallet, 'Get', [WalletID]},
    {ok, Wallet} = service_call(Request, WoodyContext),
    case wapi_backend_utils:compare_hash(Hash, get_hash(Wallet)) of
        ok ->
            unmarshal(wallet, Wallet);
        {error, conflict_hash} ->
            throw({error, {conflict, WalletID}})
    end.

service_call(Params, Ctx) ->
    wapi_handler_utils:service_call(Params, Ctx).

compose_wallet_params(ParamsIn, WoodyContext) ->
    genlib_map:compact(ParamsIn#{
        <<"id">>      => create_id(ParamsIn, WoodyContext),
        <<"context">> => create_context(ParamsIn, WoodyContext)
    }).

create_id(ParamsIn, WoodyContext) ->
    wapi_backend_utils:make_id(
        wallet,
        wapi_backend_utils:construct_external_id(ParamsIn, WoodyContext)
    ).

create_context(ParamsIn, WoodyContext) ->
    Hash = wapi_backend_utils:create_params_hash(ParamsIn),
    List = [
        {<<"owner">>, wapi_handler_utils:get_owner(WoodyContext)},
        {<<"metadata">>, maps:get(<<"metadata">>, ParamsIn, undefined)},
        Hash
    ],
    wapi_backend_utils:extend_ctx_from_list(List, wapi_backend_utils:make_ctx()).

get_hash(#wlt_Wallet{context = Ctx}) ->
    wapi_backend_utils:get_hash(unmarshal(context, Ctx)).

get_context(#wlt_Wallet{context = Ctx}) ->
    unmarshal(context, Ctx).

get_id(#wlt_WalletParams{id = ID}) ->
    ID.

%% Marshaling

marshal(wallet_params, Params = #{
    <<"id">>       := ID,
    <<"name">>     := Name,
    <<"identity">> := IdentityID,
    <<"currency">> := CurrencyID,
    <<"context">>  := Ctx
}) ->
    ExternalID = maps:get(<<"externalID">>, Params, undefined),
    #wlt_WalletParams{
        id             = marshal(id, ID),
        name           = marshal(string, Name),
        account_params = marshal(account_params, {IdentityID, CurrencyID}),
        external_id    = marshal(id, ExternalID),
        context        = marshal(context, Ctx)
    };

marshal(account_params, {IdentityID, CurrencyID}) ->
    #account_AccountParams{
        identity_id   = marshal(id, IdentityID),
        symbolic_code = marshal(string, CurrencyID)
    };

marshal(context, Ctx) ->
    ff_context:wrap(Ctx);

marshal(T, V) ->
    ff_codec:marshal(T, V).

unmarshal(wallet, #wlt_Wallet{
    id          = WalletID,
    name        = Name,
    blocking    = Blocking,
    account     = Account,
    external_id = ExternalID,
    created_at  = CreatedAt,
    context     = Ctx
}) ->
    #{
        identity := Identity,
        currency := Currency
    } = unmarshal(account, Account),
    Context = unmarshal(context, Ctx),
    genlib_map:compact(#{
        <<"id">>          => unmarshal(id, WalletID),
        <<"name">>        => unmarshal(string, Name),
        <<"isBlocked">>   => unmarshal(blocked, Blocking),
        <<"identity">>    => Identity,
        <<"currency">>    => Currency,
        <<"created_at">>  => CreatedAt,
        <<"externalID">>  => maybe_unmarshal(id, ExternalID),
        <<"metadata">>    => wapi_backend_utils:get_from_ctx(<<"metadata">>, Context)
    });

unmarshal(blocked, unblocked) ->
    false;
unmarshal(blocked, blocked) ->
    true;

unmarshal(context, Ctx) ->
    ff_context:unwrap(Ctx);

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).
