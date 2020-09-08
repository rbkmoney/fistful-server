-module(wapi_webhook_backend).

-export([create_webhook/2]).
-export([get_webhooks/2]).
-export([get_webhook/3]).
-export([delete_webhook/3]).

-include_lib("fistful_proto/include/ff_proto_webhooker_thrift.hrl").

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

-define(CTX_NS, <<"com.rbkmoney.wapi">>).

-spec create_webhook(_, _) -> _. % TODO
create_webhook(Params, Context) ->
    do(fun () ->
        NewParams = #{
            identity_id := IdentityID,
            scope := EventFilter,
            url := URL
        } = from_swag(webhook_params, maps:get('Webhook', Params)),
        WalletID = maps:get(wallet_id, NewParams, undefined),
        _ = check_wallet(WalletID, Context),
        _ = check_resource(identity, IdentityID, Context),
        WebhookParams = #webhooker_WebhookParams{
            identity_id = IdentityID,
            wallet_id = WalletID,
            event_filter = EventFilter,
            url = URL
        },
        Call = {webhook_manager, 'Create', [WebhookParams]},
        Result = wapi_handler_utils:service_call(Call, Context),
        process_result(Result)
    end).

-spec get_webhooks(_, _) -> _. % TODO
get_webhooks(IdentityID, Context) ->
    do(fun () ->
        _ = check_resource(identity, IdentityID, Context),
        Call = {webhook_manager, 'GetList', [IdentityID]},
        Result = wapi_handler_utils:service_call(Call, Context),
        process_result(Result)
    end).

-spec get_webhook(_, _, _) -> _. % TODO
get_webhook(WebhookID, IdentityID, Context) ->
    do(fun () ->
        EncodedID = encode_webhook_id(WebhookID),
        _ = check_resource(identity, IdentityID, Context),
        Call = {webhook_manager, 'Get', [EncodedID]},
        Result = wapi_handler_utils:service_call(Call, Context),
        process_result(Result)
    end).

-spec delete_webhook(_, _, _) -> _. % TODO
delete_webhook(WebhookID, IdentityID, Context) ->
    do(fun () ->
        EncodedID = encode_webhook_id(WebhookID),
        _ = check_resource(identity, IdentityID, Context),
        Call = {webhook_manager, 'Delete', [EncodedID]},
        case wapi_handler_utils:service_call(Call, Context) of
            {ok, _} ->
                ok;
            {exception, #webhooker_WebhookNotFound{}} ->
                throw(notfound)
        end
    end).

process_result({ok, #webhooker_Webhook{} = Webhook}) ->
    to_swag(webhook, Webhook);

process_result({ok, Webhooks}) when is_list(Webhooks) ->
    to_swag({list, webhook}, Webhooks);

process_result({ok, _}) ->
    ok;

process_result({exception, #webhooker_WebhookNotFound{}}) ->
    throw(notfound).



% TODO: check if such function already
check_wallet(undefined, _) ->
    ok;
check_wallet(WalletID, Context) ->
    check_resource(wallet, WalletID, Context).


encode_webhook_id(WebhookID) ->
    try
        binary_to_integer(WebhookID)
    catch
        error:badarg ->
            throw(notfound)
    end.

%% TODO: move to utils module

check_resource(Resource, Id, Context) ->
    _ = get_state(Resource, Id, Context),
    ok.

get_state(Resource, Id, Context) ->
    State = unwrap(Resource, do_get_state(Resource, Id)),
    ok    = unwrap(Resource, check_resource_access(Context, State)),
    State.

do_get_state(identity,     Id) -> ff_identity_machine:get(Id);
do_get_state(wallet,       Id) -> ff_wallet_machine:get(Id);
do_get_state(destination,  Id) -> ff_destination:get_machine(Id);
do_get_state(withdrawal,   Id) -> ff_withdrawal_machine:get(Id);
do_get_state(p2p_transfer, Id) -> p2p_transfer_machine:get(Id);
do_get_state(p2p_template, Id) -> p2p_template_machine:get(Id);
do_get_state(w2w_transfer, Id) -> w2w_transfer_machine:get(Id).

get_resource_owner(State) ->
    maps:get(<<"owner">>, get_ctx(State)).

is_resource_owner(HandlerCtx, State) ->
    wapi_handler_utils:get_owner(HandlerCtx) =:= get_resource_owner(State).

check_resource_access(HandlerCtx, State) ->
    check_resource_access(is_resource_owner(HandlerCtx, State)).

check_resource_access(true)  -> ok;
check_resource_access(false) -> {error, unauthorized}.

get_ctx(State) ->
    unwrap(ff_entity_context:get(?CTX_NS, ff_machine:ctx(State))).

%% marshaling
from_swag(webhook_params, #{
    <<"identityID">> := IdentityID,
    <<"scope">> := Scope,
    <<"url">> := URL
}) ->
    maps:merge(
        #{
            identity_id => IdentityID,
            url => URL
        },
        from_swag(webhook_scope, Scope)
    );
from_swag(webhook_scope, Topic = #{
    <<"topic">> := <<"WithdrawalsTopic">>,
    <<"eventTypes">> := EventList
}) ->
    WalletID = maps:get(<<"walletID">>, Topic, undefined),
    Scope = #webhooker_EventFilter{
        types = from_swag({set, webhook_withdrawal_event_types}, EventList)
    },
    genlib_map:compact(#{
        scope => Scope,
        wallet_id => WalletID
    });
from_swag(webhook_scope, #{
    <<"topic">> := <<"DestinationsTopic">>,
    <<"eventTypes">> := EventList
}) ->
    Scope = #webhooker_EventFilter{
        types = from_swag({set, webhook_destination_event_types}, EventList)
    },
    #{
        scope => Scope
    };
from_swag(webhook_withdrawal_event_types, <<"WithdrawalStarted">>) ->
    {withdrawal, {started, #webhooker_WithdrawalStarted{}}};
from_swag(webhook_withdrawal_event_types, <<"WithdrawalSucceeded">>) ->
    {withdrawal, {succeeded, #webhooker_WithdrawalSucceeded{}}};
from_swag(webhook_withdrawal_event_types, <<"WithdrawalFailed">>) ->
    {withdrawal, {failed, #webhooker_WithdrawalFailed{}}};

from_swag(webhook_destination_event_types, <<"DestinationCreated">>) ->
    {destination, {created, #webhooker_DestinationCreated{}}};
from_swag(webhook_destination_event_types, <<"DestinationUnauthorized">>) ->
    {destination, {unauthorized, #webhooker_DestinationUnauthorized{}}};
from_swag(webhook_destination_event_types, <<"DestinationAuthorized">>) ->
    {destination, {authorized, #webhooker_DestinationAuthorized{}}};

from_swag({list, Type}, List) ->
    lists:map(fun(V) -> from_swag(Type, V) end, List);
from_swag({set, Type}, List) ->
    ordsets:from_list(from_swag({list, Type}, List)).

to_swag(webhook, #webhooker_Webhook{
    id = ID,
    identity_id = IdentityID,
    wallet_id = WalletID,
    event_filter = EventFilter,
    url = URL,
    pub_key = PubKey,
    enabled = Enabled
}) ->
    to_swag(map, #{
        <<"id">> => integer_to_binary(ID),
        <<"identityID">> => IdentityID,
        <<"walletID">> => WalletID,
        <<"active">> => to_swag(boolean, Enabled),
        <<"scope">> => to_swag(webhook_scope, EventFilter),
        <<"url">> => URL,
        <<"publicKey">> => PubKey
    });

to_swag(webhook_scope, #webhooker_EventFilter{types = EventTypes}) ->
    List = to_swag({set, webhook_event_types}, EventTypes),
    lists:foldl(fun({Topic, Type}, Acc) ->
        case maps:get(<<"topic">>, Acc, undefined) of
            undefined ->
                Acc#{
                    <<"topic">> => to_swag(webhook_topic, Topic),
                    <<"eventTypes">> => [Type]
                };
            _ ->
                #{<<"eventTypes">> := Types} = Acc,
                Acc#{
                    <<"eventTypes">> := [Type | Types]
                }
        end
    end, #{}, List);

to_swag(webhook_event_types, {withdrawal, EventType}) ->
    {withdrawal, to_swag(webhook_withdrawal_event_types, EventType)};
to_swag(webhook_event_types, {destination, EventType}) ->
    {destination, to_swag(webhook_destination_event_types, EventType)};

to_swag(webhook_topic, withdrawal) ->
    <<"WithdrawalsTopic">>;
to_swag(webhook_topic, destination) ->
    <<"DestinationsTopic">>;

to_swag(webhook_withdrawal_event_types, {started, _}) ->
    <<"WithdrawalStarted">>;
to_swag(webhook_withdrawal_event_types, {succeeded, _}) ->
    <<"WithdrawalSucceeded">>;
to_swag(webhook_withdrawal_event_types, {failed, _}) ->
    <<"WithdrawalFailed">>;

to_swag(webhook_destination_event_types, {created, _}) ->
    <<"DestinationCreated">>;
to_swag(webhook_destination_event_types, {unauthorized, _}) ->
    <<"DestinationUnauthorized">>;
to_swag(webhook_destination_event_types, {authorized, _}) ->
    <<"DestinationAuthorized">>;

to_swag(boolean, true) ->
    true;
to_swag(boolean, false) ->
    false;
to_swag({list, Type}, List) ->
    lists:map(fun(V) -> to_swag(Type, V) end, List);
to_swag({set, Type}, Set) ->
    to_swag({list, Type}, ordsets:to_list(Set));
to_swag(map, Map) ->
    genlib_map:compact(Map);
to_swag(_, V) ->
    V.
