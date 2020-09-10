-module(wapi_webhook_backend).

-export([create_webhook/2]).
-export([get_webhooks/2]).
-export([get_webhook/3]).
-export([delete_webhook/3]).

-type id()          :: binary() | undefined.
-type ctx()         :: wapi_handler:context().
-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().


-include_lib("fistful_proto/include/ff_proto_webhooker_thrift.hrl").

-spec create_webhook(req_data(), handler_context()) ->
    {ok, response_data()} | {error, CreateError}
when CreateError ::
    {identity, notfound} |
    {identity, unauthorized} |
    {wallet, notfound} |
    {wallet, unauthorized}.

create_webhook(#{'Webhook' := Params}, HandlerContext) ->
    WebhookParams = marshal_webhook_params(Params),
    IdentityID = WebhookParams#webhooker_WebhookParams.identity_id,
    WalletID = WebhookParams#webhooker_WebhookParams.wallet_id,
    case wapi_access_backend:check_resource_by_id(wallet, WalletID, HandlerContext) of
        ok ->
            case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
                ok ->
                    Call = {webhook_manager, 'Create', [WebhookParams]},
                    Result = wapi_handler_utils:service_call(Call, HandlerContext),
                    process_result(Result);
                {error, Error} ->
                    {error, {identity, Error}}
            end;
        {error, Error} ->
            {error, {wallet, Error}}
    end.

-spec get_webhooks(id(), ctx()) ->
    {ok, response_data()} | {error, GetError}
when GetError ::
    {identity, notfound} |
    {identity, unauthorized}.

get_webhooks(IdentityID, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            Call = {webhook_manager, 'GetList', [IdentityID]},
            Result = wapi_handler_utils:service_call(Call, HandlerContext),
            process_result(Result);
        {error, Error} ->
            {error, {identity, Error}}
    end.

-spec get_webhook(id(), id(), ctx()) ->
    {ok, response_data()} | {error, GetError}
when GetError ::
    notfound |
    {identity, notfound} |
    {identity, unauthorized}.

get_webhook(WebhookID, IdentityID, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            case encode_webhook_id(WebhookID) of
                {error, notfound} ->
                    {error, {identity, notfound}};
                EncodedID ->
                    Call = {webhook_manager, 'Get', [EncodedID]},
                    Result = wapi_handler_utils:service_call(Call, HandlerContext),
                    process_result(Result)
            end;
        {error, Error} ->
            {error, {identity, Error}}
    end.

-spec delete_webhook(id(), id(), ctx()) ->
    ok | {error, DeleteError}
when DeleteError ::
    notfound |
    {identity, notfound} |
    {identity, unauthorized}.

delete_webhook(WebhookID, IdentityID, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            EncodedID = encode_webhook_id(WebhookID),
            Call = {webhook_manager, 'Delete', [EncodedID]},
            Result = wapi_handler_utils:service_call(Call, HandlerContext),
            process_result(Result);
        {error, Error} ->
            {error, {identity, Error}}
    end.

process_result({ok, #webhooker_Webhook{} = Webhook}) ->
    {ok, unmarshal_webhook(Webhook)};
process_result({ok, Webhooks}) when is_list(Webhooks) ->
    {ok, unmarshal_webhooks(Webhooks)};
process_result({ok, _}) ->
    ok;
process_result({exception, #webhooker_WebhookNotFound{}}) ->
    {error, notfound}.

encode_webhook_id(WebhookID) ->
    try
        binary_to_integer(WebhookID)
    catch
        error:badarg ->
            {error, notfound}
    end.

%% marshaling

marshal_webhook_params(#{
    <<"identityID">> := IdentityID,
    <<"scope">> := Scope,
    <<"url">> := URL
} = WebhookParams) ->
    WalletID = maps:get(<<"walletID">>, WebhookParams, undefined),
    #webhooker_WebhookParams{
        identity_id = IdentityID,
        wallet_id = WalletID,
        event_filter = marshal_webhook_scope(Scope),
        url = URL
    }.

marshal_webhook_scope(#{
    <<"topic">> := <<"WithdrawalsTopic">>,
    <<"eventTypes">> := EventList
}) ->
    #webhooker_EventFilter{
        types = marshal({set, webhook_withdrawal_event_types}, EventList)
    };

marshal_webhook_scope(#{
    <<"topic">> := <<"DestinationsTopic">>,
    <<"eventTypes">> := EventList
}) ->
    #webhooker_EventFilter{
        types = marshal({set, webhook_destination_event_types}, EventList)
    }.

marshal(webhook_withdrawal_event_types, <<"WithdrawalStarted">>) ->
    {withdrawal, {started, #webhooker_WithdrawalStarted{}}};
marshal(webhook_withdrawal_event_types, <<"WithdrawalSucceeded">>) ->
    {withdrawal, {succeeded, #webhooker_WithdrawalSucceeded{}}};
marshal(webhook_withdrawal_event_types, <<"WithdrawalFailed">>) ->
    {withdrawal, {failed, #webhooker_WithdrawalFailed{}}};
marshal(webhook_destination_event_types, <<"DestinationCreated">>) ->
    {destination, {created, #webhooker_DestinationCreated{}}};
marshal(webhook_destination_event_types, <<"DestinationUnauthorized">>) ->
    {destination, {unauthorized, #webhooker_DestinationUnauthorized{}}};
marshal(webhook_destination_event_types, <<"DestinationAuthorized">>) ->
    {destination, {authorized, #webhooker_DestinationAuthorized{}}};

marshal({list, Type}, List) ->
    lists:map(fun(V) -> marshal(Type, V) end, List);
marshal({set, Type}, List) ->
    ordsets:from_list(marshal({list, Type}, List)).


unmarshal_webhooks(Webhooks) when is_list(Webhooks) ->
    lists:map(fun(Webhook) -> unmarshal_webhook(Webhook) end, Webhooks).

unmarshal_webhook(#webhooker_Webhook{
    id = ID,
    identity_id = IdentityID,
    wallet_id = WalletID,
    event_filter = EventFilter,
    url = URL,
    pub_key = PubKey,
    enabled = Enabled
}) ->
    unmarshal(map, #{
        <<"id">> => integer_to_binary(ID),
        <<"identityID">> => IdentityID,
        <<"walletID">> => WalletID,
        <<"active">> => unmarshal(boolean, Enabled),
        <<"scope">> => unmarshal_webhook_scope(EventFilter),
        <<"url">> => URL,
        <<"publicKey">> => PubKey
    }).

unmarshal_webhook_scope(#webhooker_EventFilter{types = EventTypes}) ->
    List = unmarshal({set, webhook_event_types}, EventTypes),
    lists:foldl(fun({Topic, Type}, Acc) ->
        case maps:get(<<"topic">>, Acc, undefined) of
            undefined ->
                Acc#{
                    <<"topic">> => unmarshal_webhook_topic(Topic),
                    <<"eventTypes">> => [Type]
                };
            _ ->
                #{<<"eventTypes">> := Types} = Acc,
                Acc#{
                    <<"eventTypes">> := [Type | Types]
                }
        end
    end, #{}, List).

unmarshal_webhook_topic(withdrawal) ->
    <<"WithdrawalsTopic">>;
unmarshal_webhook_topic(destination) ->
    <<"DestinationsTopic">>.

unmarshal(webhook_event_types, {withdrawal, EventType}) ->
    {withdrawal, unmarshal(webhook_withdrawal_event_types, EventType)};
unmarshal(webhook_event_types, {destination, EventType}) ->
    {destination, unmarshal(webhook_destination_event_types, EventType)};

unmarshal(webhook_withdrawal_event_types, {started, _}) ->
    <<"WithdrawalStarted">>;
unmarshal(webhook_withdrawal_event_types, {succeeded, _}) ->
    <<"WithdrawalSucceeded">>;
unmarshal(webhook_withdrawal_event_types, {failed, _}) ->
    <<"WithdrawalFailed">>;

unmarshal(webhook_destination_event_types, {created, _}) ->
    <<"DestinationCreated">>;
unmarshal(webhook_destination_event_types, {unauthorized, _}) ->
    <<"DestinationUnauthorized">>;
unmarshal(webhook_destination_event_types, {authorized, _}) ->
    <<"DestinationAuthorized">>;

unmarshal(boolean, true) ->
    true;
unmarshal(boolean, false) ->
    false;
unmarshal({list, Type}, List) ->
    lists:map(fun(V) -> unmarshal(Type, V) end, List);
unmarshal({set, Type}, Set) ->
    unmarshal({list, Type}, ordsets:to_list(Set));
unmarshal(map, Map) ->
    genlib_map:compact(Map);
unmarshal(_, V) ->
    V.
