-module(wapi_w2w_backend).

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().

-type id() :: binary().
-type external_id() :: id().

-export([create_transfer/2]).
-export([get_transfer/2]).

-include_lib("fistful_proto/include/ff_proto_w2w_transfer_thrift.hrl").

-spec create_transfer(req_data(), handler_context()) ->
    {ok, response_data()} | {error, CreateError}
when
    CreateError ::
        {external_id_conflict, external_id()} |
        {wallet_from, unauthorized} |
        {wallet_from | wallet_to, notfound | inaccessible} |
        bad_w2w_transfer_amount |
        not_allowed_currency |
        inconsistent_currency.

create_transfer(Params = #{<<"sender">> := SenderID}, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(wallet, SenderID, HandlerContext) of
        ok ->
            case wapi_backend_utils:gen_id(w2w_transfer, Params, HandlerContext) of
                {ok, ID} ->
                    Context = wapi_backend_utils:make_ctx(Params, HandlerContext),
                    create_transfer(ID, Params, Context, HandlerContext);
                {error, {external_id_conflict, _}} = Error ->
                    Error
            end;
        {error, unauthorized} ->
            {error, {wallet_from, unauthorized}}
    end.

create_transfer(ID, Params, Context, HandlerContext) ->
    TransferParams = marshal(transfer_params, Params#{<<"id">> => ID}),
    Request = {fistful_w2w_transfer, 'Create', [TransferParams, marshal(context, Context)]},
    case service_call(Request, HandlerContext) of
        {ok, Transfer} ->
            {ok, unmarshal(transfer, Transfer)};
        {exception, #fistful_WalletNotFound{id = ID}} ->
            {error, wallet_not_found_error(unmarshal(id, ID), Params)};
        {exception, #fistful_WalletInaccessible{id = ID}} ->
            {error, wallet_inaccessible_error(unmarshal(id, ID), Params)};
        {exception, #fistful_ForbiddenOperationCurrency{}} ->
            {error, not_allowed_currency};
        {exception, #w2w_transfer_InconsistentW2WTransferCurrency{}} ->
            {error, inconsistent_currency};
        {exception, #fistful_InvalidOperationAmount{}} ->
            {error, bad_w2w_transfer_amount}
    end.

-spec get_transfer(req_data(), handler_context()) ->
    {ok, response_data()} | {error, GetError}
when
    GetError ::
        {w2w_transfer, unauthorized} |
        {w2w_transfer, {unknown_w2w_transfer, id()}}.

get_transfer(ID, HandlerContext) ->
    EventRange = #'EventRange'{},
    Request = {fistful_w2w_transfer, 'Get', [ID, EventRange]},
    case service_call(Request, HandlerContext) of
        {ok, TransferThrift} ->
            case wapi_access_backend:check_resource(w2w_transfer, TransferThrift, HandlerContext) of
                ok ->
                    {ok, unmarshal(transfer, TransferThrift)};
                {error, unauthorized} ->
                    {error, {w2w_transfer, unauthorized}}
            end;
        {exception, #fistful_W2WNotFound{}} ->
            {error, {w2w_transfer, {unknown_w2w_transfer, ID}}}
    end.

%%
%% Internal
%%

service_call(Params, Ctx) ->
    wapi_handler_utils:service_call(Params, Ctx).

wallet_not_found_error(WalletID, #{<<"sender">> := WalletID}) ->
    {wallet_from, notfound};
wallet_not_found_error(WalletID, #{<<"receiver">> := WalletID}) ->
    {wallet_to, notfound}.

wallet_inaccessible_error(WalletID, #{<<"sender">> := WalletID}) ->
    {wallet_from, inaccessible};
wallet_inaccessible_error(WalletID, #{<<"receiver">> := WalletID}) ->
    {wallet_to, inaccessible}.

%% Marshaling

marshal(transfer_params, #{
    <<"id">> := ID,
    <<"sender">> := SenderID,
    <<"receiver">> := ReceiverID,
    <<"body">> := Body
} = Params) ->
    #w2w_transfer_W2WTransferParams{
        id = marshal(id, ID),
        wallet_from_id = marshal(id, SenderID),
        wallet_to_id = marshal(id, ReceiverID),
        body = marshal(body, Body),
        external_id = maps:get(<<"externalId">>, Params, undefined)
    };

marshal(body, #{
    <<"amount">> := Amount,
    <<"currency">> := Currency
}) ->
    #'Cash'{
        amount   = marshal(amount, Amount),
        currency = marshal(currency_ref, Currency)
    };

marshal(context, Ctx) ->
    ff_codec:marshal(context, Ctx);

marshal(T, V) ->
    ff_codec:marshal(T, V).

unmarshal(transfer, #w2w_transfer_W2WTransferState{
    id = ID,
    wallet_from_id = SenderID,
    wallet_to_id = ReceiverID,
    body = Body,
    created_at = CreatedAt,
    status = Status,
    external_id = ExternalID
}) ->
    genlib_map:compact(#{
        <<"id">> => unmarshal(id, ID),
        <<"createdAt">> => CreatedAt,
        <<"body">> => unmarshal(body, Body),
        <<"sender">> => unmarshal(id, SenderID),
        <<"receiver">> => unmarshal(id, ReceiverID),
        <<"status">> => unmarshal(transfer_status, Status),
        <<"externalID">> => maybe_unmarshal(id, ExternalID)
    });

unmarshal(body, #'Cash'{
    amount   = Amount,
    currency = Currency
}) ->
    #{
        <<"amount">> => unmarshal(amount, Amount),
        <<"currency">> => unmarshal(currency_ref, Currency)
    };

unmarshal(transfer_status, {pending, _}) ->
    #{<<"status">> => <<"Pending">>};
unmarshal(transfer_status, {succeeded, _}) ->
    #{<<"status">> => <<"Succeeded">>};
unmarshal(transfer_status, {failed, #w2w_status_Failed{failure = Failure}}) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => unmarshal(failure, Failure)
    };

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_T, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).
