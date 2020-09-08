-module(wapi_webhook_backend).

-export([create_webhook/2]).
-export([get_webhooks/2]).
-export([get_webhook/3]).
-export([delete_webhook/3]).

-type id()          :: binary() | undefined.
-type ctx()         :: wapi_handler:context().
-type params()      :: map(). % TODO: specify
-type result(T, E)  :: {ok, T} | {error, E}.

-include_lib("fistful_proto/include/ff_proto_webhooker_thrift.hrl").

-define(CTX_NS, <<"com.rbkmoney.wapi">>).

-spec create_webhook(params(), ctx()) -> result(map(),
    {identity, notfound} |
    {identity, unauthorized} |
    {wallet, notfound} |
    {wallet, unauthorized}
).

create_webhook(Params, Context) ->
    NewParams = #{
        identity_id := IdentityID,
        scope := EventFilter,
        url := URL
    } = unmarshal(webhook_params, maps:get('Webhook', Params)),
    WalletID = maps:get(wallet_id, NewParams, undefined),
    WalletAccessible = wapi_access_backend:check_resource_by_id(wallet, WalletID, Context),
    IdentityAccessible = wapi_access_backend:check_resource_by_id(identity, IdentityID, Context),
    case {WalletAccessible, IdentityAccessible} of
        {ok, ok} ->
            WebhookParams = #webhooker_WebhookParams{
                identity_id = IdentityID,
                wallet_id = WalletID,
                event_filter = EventFilter,
                url = URL
            },
            Call = {webhook_manager, 'Create', [WebhookParams]},
            Result = wapi_handler_utils:service_call(Call, Context),
            process_result(Result);
        {{error, Error}, _} ->
            {error, {wallet, Error}};
        {ok, {error, Error}} ->
            {error, {identity, Error}}
    end.

-spec get_webhooks(id(), ctx()) -> result(list(map()),
    {identity, notfound} |
    {identity, unauthorized}
).

get_webhooks(IdentityID, Context) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, Context) of
        ok ->
            Call = {webhook_manager, 'GetList', [IdentityID]},
            Result = wapi_handler_utils:service_call(Call, Context),
            process_result(Result);
        {error, Error} ->
            {error, {identity, Error}} % TODO
    end.

-spec get_webhook(id(), id(), ctx()) -> result(map(),
    notfound |
    {identity, notfound} |
    {identity, unauthorized}
).

get_webhook(WebhookID, IdentityID, Context) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, Context) of
        ok ->
            EncodedID = encode_webhook_id(WebhookID),
            Call = {webhook_manager, 'Get', [EncodedID]},
            Result = wapi_handler_utils:service_call(Call, Context),
            process_result(Result);
        {error, Error} ->
            {error, {identity, Error}}
    end.

-spec delete_webhook(id(), id(), ctx()) ->
    ok |
    {error,
        notfound |
        {identity, notfound} |
        {identity, unauthorized}
    }.

delete_webhook(WebhookID, IdentityID, Context) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, Context) of
        ok ->
            EncodedID = encode_webhook_id(WebhookID),
            Call = {webhook_manager, 'Delete', [EncodedID]},
            Result = wapi_handler_utils:service_call(Call, Context),
            process_result(Result);
        {error, Error} ->
            {error, {identity, Error}}
    end.

process_result({ok, #webhooker_Webhook{} = Webhook}) ->
    {ok, marshal(webhook, Webhook)};

process_result({ok, Webhooks}) when is_list(Webhooks) ->
    {ok, marshal({list, webhook}, Webhooks)};

process_result({ok, _}) ->
    ok;

process_result({exception, #webhooker_WebhookNotFound{}}) ->
    {error, notfound}.

encode_webhook_id(WebhookID) ->
    try
        binary_to_integer(WebhookID)
    catch
        error:badarg ->
            throw(notfound)
    end.

%% marshaling
unmarshal(webhook_params, #{
    <<"identityID">> := IdentityID,
    <<"scope">> := Scope,
    <<"url">> := URL
}) ->
    maps:merge(
        #{
            identity_id => IdentityID,
            url => URL
        },
        unmarshal(webhook_scope, Scope)
    );
unmarshal(webhook_scope, Topic = #{
    <<"topic">> := <<"WithdrawalsTopic">>,
    <<"eventTypes">> := EventList
}) ->
    WalletID = maps:get(<<"walletID">>, Topic, undefined),
    Scope = #webhooker_EventFilter{
        types = unmarshal({set, webhook_withdrawal_event_types}, EventList)
    },
    genlib_map:compact(#{
        scope => Scope,
        wallet_id => WalletID
    });
unmarshal(webhook_scope, #{
    <<"topic">> := <<"DestinationsTopic">>,
    <<"eventTypes">> := EventList
}) ->
    Scope = #webhooker_EventFilter{
        types = unmarshal({set, webhook_destination_event_types}, EventList)
    },
    #{
        scope => Scope
    };
unmarshal(webhook_withdrawal_event_types, <<"WithdrawalStarted">>) ->
    {withdrawal, {started, #webhooker_WithdrawalStarted{}}};
unmarshal(webhook_withdrawal_event_types, <<"WithdrawalSucceeded">>) ->
    {withdrawal, {succeeded, #webhooker_WithdrawalSucceeded{}}};
unmarshal(webhook_withdrawal_event_types, <<"WithdrawalFailed">>) ->
    {withdrawal, {failed, #webhooker_WithdrawalFailed{}}};

unmarshal(webhook_destination_event_types, <<"DestinationCreated">>) ->
    {destination, {created, #webhooker_DestinationCreated{}}};
unmarshal(webhook_destination_event_types, <<"DestinationUnauthorized">>) ->
    {destination, {unauthorized, #webhooker_DestinationUnauthorized{}}};
unmarshal(webhook_destination_event_types, <<"DestinationAuthorized">>) ->
    {destination, {authorized, #webhooker_DestinationAuthorized{}}};

unmarshal({list, Type}, List) ->
    lists:map(fun(V) -> unmarshal(Type, V) end, List);
unmarshal({set, Type}, List) ->
    ordsets:from_list(unmarshal({list, Type}, List)).

marshal(webhook, #webhooker_Webhook{
    id = ID,
    identity_id = IdentityID,
    wallet_id = WalletID,
    event_filter = EventFilter,
    url = URL,
    pub_key = PubKey,
    enabled = Enabled
}) ->
    marshal(map, #{
        <<"id">> => integer_to_binary(ID),
        <<"identityID">> => IdentityID,
        <<"walletID">> => WalletID,
        <<"active">> => marshal(boolean, Enabled),
        <<"scope">> => marshal(webhook_scope, EventFilter),
        <<"url">> => URL,
        <<"publicKey">> => PubKey
    });

marshal(webhook_scope, #webhooker_EventFilter{types = EventTypes}) ->
    List = marshal({set, webhook_event_types}, EventTypes),
    lists:foldl(fun({Topic, Type}, Acc) ->
        case maps:get(<<"topic">>, Acc, undefined) of
            undefined ->
                Acc#{
                    <<"topic">> => marshal(webhook_topic, Topic),
                    <<"eventTypes">> => [Type]
                };
            _ ->
                #{<<"eventTypes">> := Types} = Acc,
                Acc#{
                    <<"eventTypes">> := [Type | Types]
                }
        end
    end, #{}, List);

marshal(webhook_event_types, {withdrawal, EventType}) ->
    {withdrawal, marshal(webhook_withdrawal_event_types, EventType)};
marshal(webhook_event_types, {destination, EventType}) ->
    {destination, marshal(webhook_destination_event_types, EventType)};

marshal(webhook_topic, withdrawal) ->
    <<"WithdrawalsTopic">>;
marshal(webhook_topic, destination) ->
    <<"DestinationsTopic">>;

marshal(webhook_withdrawal_event_types, {started, _}) ->
    <<"WithdrawalStarted">>;
marshal(webhook_withdrawal_event_types, {succeeded, _}) ->
    <<"WithdrawalSucceeded">>;
marshal(webhook_withdrawal_event_types, {failed, _}) ->
    <<"WithdrawalFailed">>;

marshal(webhook_destination_event_types, {created, _}) ->
    <<"DestinationCreated">>;
marshal(webhook_destination_event_types, {unauthorized, _}) ->
    <<"DestinationUnauthorized">>;
marshal(webhook_destination_event_types, {authorized, _}) ->
    <<"DestinationAuthorized">>;

marshal(boolean, true) ->
    true;
marshal(boolean, false) ->
    false;
marshal({list, Type}, List) ->
    lists:map(fun(V) -> marshal(Type, V) end, List);
marshal({set, Type}, Set) ->
    marshal({list, Type}, ordsets:to_list(Set));
marshal(map, Map) ->
    genlib_map:compact(Map);
marshal(_, V) ->
    V.
