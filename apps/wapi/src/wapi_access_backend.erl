-module(wapi_access_backend).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").

-export([check_resource/3]).
-export([check_resource_by_id/3]).

-type id() :: binary().
-type resource_type() :: identity | wallet | destination.
-type handler_context() :: wapi_handler:context().
-type data() ::
    ff_proto_identity_thrift:'IdentityState'() |
    ff_proto_wallet_thrift:'WalletState'().

-define(CTX_NS, <<"com.rbkmoney.wapi">>).

%% Pipeline

-spec check_resource(resource_type(), data(), handler_context()) ->
    ok | {error, unauthorized}.

check_resource(Resource, Data, Context) ->
    Owner = get_context(Resource, Data),
    check_resource_access(is_resource_owner(Owner, Context)).

-spec check_resource_by_id(resource_type(), id(), handler_context()) ->
    ok | {error, unauthorized}.

check_resource_by_id(Resource, ID, Context) ->
    Owner = get_context_by_id(Resource, ID, Context),
    check_resource_access(is_resource_owner(Owner, Context)).

%%
%% Internal
%%

get_context_by_id(Resource = identity, IdentityID, WoodyCtx) ->
    Request = {fistful_identity, 'Get', [IdentityID, #'EventRange'{}]},
    {ok, Identity} = wapi_handler_utils:service_call(Request, WoodyCtx),
    get_context(Resource, Identity);
get_context_by_id(Resource = wallet, WalletID, WoodyCtx) ->
    Request = {fistful_wallet, 'Get', [WalletID, #'EventRange'{}]},
    {ok, Wallet} = wapi_handler_utils:service_call(Request, WoodyCtx),
    get_context(Resource, Wallet);
get_context_by_id(Resource = destination, DestinationID, WoodyCtx) ->
    Request = {fistful_destination, 'Get', [DestinationID, #'EventRange'{}]},
    {ok, Destination} = wapi_handler_utils:service_call(Request, WoodyCtx),
    get_context(Resource, Destination).

get_context(identity, Identity) ->
    #idnt_IdentityState{context = Ctx} = Identity,
    Context = ff_codec:unmarshal(context, Ctx),
    wapi_backend_utils:get_from_ctx(<<"owner">>, Context);
get_context(wallet, Wallet) ->
    #wlt_WalletState{context = Ctx} = Wallet,
    Context = ff_codec:unmarshal(context, Ctx),
    wapi_backend_utils:get_from_ctx(<<"owner">>, Context);
get_context(destination, Destination) ->
    #dst_DestinationState{context = Ctx} = Destination,
    Context = ff_codec:unmarshal(context, Ctx),
    wapi_backend_utils:get_from_ctx(<<"owner">>, Context).

is_resource_owner(Owner, HandlerCtx) ->
    Owner =:= wapi_handler_utils:get_owner(HandlerCtx).

check_resource_access(true)  -> ok;
check_resource_access(false) -> {error, unauthorized}.
