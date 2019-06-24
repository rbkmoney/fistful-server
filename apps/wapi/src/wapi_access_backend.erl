-module(wapi_access_backend).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

-export([check_resource/3]).

-type handler_context() :: wapi_handler:context().

-define(CTX_NS, <<"com.rbkmoney.wapi">>).

%% Pipeline

-spec check_resource(atom(), binary() | tuple(), handler_context()) ->
    ok | {error, unauthorized}.

check_resource(Resource, ID, Context) when is_binary(ID)->
    Owner = get_context(Resource, ID, Context),
    check_resource_access(is_resource_owner(Owner, Context));
check_resource(Resource, Data, Context)->
    Owner = get_context(Resource, Data),
    check_resource_access(is_resource_owner(Owner, Context)).

%%
%% Internal
%%

get_context(Resource = identity, IdentityID, WoodyCtx) ->
    Request = {fistful_identity, 'Get', [IdentityID]},
    {ok, Identity} = wapi_handler_utils:service_call(Request, WoodyCtx),
    get_context(Resource, Identity);
get_context(Resource = wallet, WalletID, WoodyCtx) ->
    Request = {fistful_wallet, 'Get', [WalletID]},
    {ok, Wallet} = wapi_handler_utils:service_call(Request, WoodyCtx),
    get_context(Resource, Wallet).

get_context(identity, Identity) ->
    #idnt_Identity{context = Ctx} = Identity,
    Context = ff_context:unwrap(Ctx),
    wapi_backend_utils:get_from_ctx(<<"owner">>, Context);
get_context(wallet, Wallet) ->
    #wlt_Wallet{context = Ctx} = Wallet,
    Context = ff_context:unwrap(Ctx),
    wapi_backend_utils:get_from_ctx(<<"owner">>, Context).

is_resource_owner(Owner, HandlerCtx) ->
    Owner =:= wapi_handler_utils:get_owner(HandlerCtx).

check_resource_access(true)  -> ok;
check_resource_access(false) -> {error, unauthorized}.
