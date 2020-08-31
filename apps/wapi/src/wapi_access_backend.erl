-module(wapi_access_backend).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_w2w_transfer_thrift.hrl").

-export([check_resource/3]).
-export([check_resource_by_id/3]).

-type id() :: binary().
-type resource_type() :: identity | wallet | destination | w2w_transfer.
-type handler_context() :: wapi_handler:context().
-type data() ::
    ff_proto_identity_thrift:'IdentityState'() |
    ff_proto_wallet_thrift:'WalletState'().

-define(CTX_NS, <<"com.rbkmoney.wapi">>).

%% Pipeline

-spec check_resource(resource_type(), data(), handler_context()) ->
    ok | {error, unauthorized}.

check_resource(Resource, Data, HandlerContext) ->
    Owner = get_owner(get_context_from_state(Resource, Data)),
    check_resource_access(is_resource_owner(Owner, HandlerContext)).

-spec check_resource_by_id(resource_type(), id(), handler_context()) ->
    ok | {error, notfound | unauthorized}.

check_resource_by_id(Resource, ID, HandlerContext) ->
    case get_context_by_id(Resource, ID, HandlerContext) of
        {error, notfound} = Error ->
            Error;
        Context ->
            Owner = get_owner(Context),
            check_resource_access(is_resource_owner(Owner, HandlerContext))
    end.

%%
%% Internal
%%

get_context_by_id(identity, IdentityID, WoodyCtx) ->
    Request = {fistful_identity, 'GetContext', [IdentityID]},
    case wapi_handler_utils:service_call(Request, WoodyCtx) of
        {ok, Context} ->
            Context;
        {exception, #fistful_IdentityNotFound{}} ->
            {error, notfound}
    end;
get_context_by_id(wallet, WalletID, WoodyCtx) ->
    Request = {fistful_wallet, 'GetContext', [WalletID]},
    case wapi_handler_utils:service_call(Request, WoodyCtx) of
        {ok, Context} ->
            Context;
        {exception, #fistful_WalletNotFound{}} ->
            {error, notfound}
    end;
get_context_by_id(destination, DestinationID, WoodyCtx) ->
    Request = {fistful_destination, 'GetContext', [DestinationID]},
    case wapi_handler_utils:service_call(Request, WoodyCtx) of
        {ok, Context} ->
            Context;
        {exception, #fistful_DestinationNotFound{}} ->
            {error, notfound}
    end;
get_context_by_id(w2w_transfer, W2WTransferID, WoodyCtx) ->
    Request = {w2w_transfer, 'GetContext', [W2WTransferID]},
    case wapi_handler_utils:service_call(Request, WoodyCtx) of
        {ok, Context} ->
            Context;
        {exception, #fistful_W2WNotFound{}} ->
            {error, notfound}
    end.

get_context_from_state(identity, #idnt_IdentityState{context = Context} ) ->
    Context;
get_context_from_state(wallet, #wlt_WalletState{context = Context}) ->
    Context;
get_context_from_state(destination, #dst_DestinationState{context = Context}) ->
    Context;
get_context_from_state(w2w_transfer, #w2w_transfer_W2WTransferState{context = Context}) ->
    Context.

get_owner(ContextThrift) ->
    Context = ff_codec:unmarshal(context, ContextThrift),
    wapi_backend_utils:get_from_ctx(<<"owner">>, Context).

is_resource_owner(Owner, HandlerCtx) ->
    Owner =:= wapi_handler_utils:get_owner(HandlerCtx).

check_resource_access(true)  -> ok;
check_resource_access(false) -> {error, unauthorized}.
