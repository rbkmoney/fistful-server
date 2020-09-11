-module(wapi_p2p_transfer).

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().

-type id() :: binary().
-type external_id() :: id().

-export([create_transfer/2]).
-export([get_transfer/2]).

-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").

-spec create_transfer(req_data(), handler_context()) ->
    {ok, response_data()} | {error, CreateError}
when CreateError :: {external_id_conflict, external_id()}
                  | not_allowed_currency
                  | inconsistent_currency.
create_transfer(Params = #{<<"identityID">> := IdentityID}, HandlerContext) ->
    % FIXME: I might have missed some errors?
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            case wapi_backend_utils:gen_id(p2p_transfer, Params, HandlerContext) of
                {ok, ID} ->
                    Context = wapi_backend_utils:make_ctx(Params, HandlerContext),
                    do_create_transfer(ID, Params, Context, HandlerContext);
                {error, {external_id_conflict, _}} = Error ->
                    Error
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}};
        {error, notfound} ->
            {error, {identity, notfound}}
    end.

-spec get_transfer(req_data(), handler_context()) ->
    {ok, response_data()} | {error, GetError}
when GetError :: {p2p_transfer, unauthorized}
               | {p2p_transfer, {unknown_p2p_transfer, id()}} .
get_transfer(ID, HandlerContext) ->
    Request = {p2p_transfer, 'Get', [ID, #'EventRange'{}]},
    case service_call(Request, HandlerContext) of
        {ok, TransferThrift} ->
            case wapi_access_backend:check_resource(p2p_transfer, TransferThrift, HandlerContext) of
                ok ->
                    {ok, unmarshal(transfer, TransferThrift)};
                {error, unauthorized} ->
                    {error, {p2p_transfer, unauthorized}}
            end;
        {exception, #fistful_P2PNotFound{}} ->
            {error, {p2p_transfer, {unknown_p2p_transfer, ID}}}
    end.

%% Internal

do_create_transfer(ID, Params, Context, HandlerContext) ->
    TransferParams = marshal(transfer_params, Params#{<<"id">> => ID}),
    Request = {p2p_transfer, 'Create', [TransferParams, marshal(context, Context)]},
    case service_call(Request, HandlerContext) of
        {ok, Transfer} ->
            {ok, unmarshal(transfer, Transfer)};
        {exception, #fistful_ForbiddenOperationCurrency{}} ->
            {error, not_allowed_currency};
        {exception, #fistful_ForbiddenOperationAmount{}} ->
            {error, bad_p2p_transfer_amount}
    end.

service_call(Params, Ctx) ->
    wapi_handler_utils:service_call(Params, Ctx).

%% Marshal

marshal(transfer_params, #{
    <<"id">> := ID,
    <<"identityID">> := IdentityID,
    <<"sender">> := Sender,
    <<"receiver">> := Receiver,
    <<"body">> := Body,
    <<"contactInfo">> := ContactInfo
}) ->
    #p2p_transfer_P2PTransferParams{
        id = ID,
        identity_id = IdentityID,
        sender = marshal(participant, Sender#{<<"contactInfo">> => ContactInfo}),
        receiver = marshal(participant, Receiver),
        body = marshal(body, Body)
    };

marshal(participant, #{
    <<"token">> := Token,
    <<"contactInfo">> := ContactInfo
}) ->
    Resource = case wapi_crypto:decrypt_bankcard_token(Token) of
        unrecognized ->
            BankCard = wapi_utils:base64url_to_map(Token),
            {bank_card, #'ResourceBankCard'{
                bank_card = #'BankCard'{
                    token      = maps:get(<<"token">>, BankCard),
                    bin        = maps:get(<<"bin">>, BankCard),
                    masked_pan = maps:get(<<"lastDigits">>, BankCard)
                }
            }};
        {ok, BankCard} ->
            {bank_card, #'ResourceBankCard'{bank_card = BankCard}}
    end,
    {resource, #p2p_transfer_RawResource{
        resource = Resource,
        contact_info = marshal(contact_info, ContactInfo)
    }};

marshal(participant, #{
    <<"token">> := Token
}) ->
    Resource = case wapi_crypto:decrypt_bankcard_token(Token) of
        unrecognized ->
            BankCard = wapi_utils:base64url_to_map(Token),
            {bank_card, #'ResourceBankCard'{
                bank_card = #'BankCard'{
                    token      = maps:get(<<"token">>, BankCard),
                    bin        = maps:get(<<"bin">>, BankCard),
                    masked_pan = maps:get(<<"lastDigits">>, BankCard)
                }
            }};
        {ok, BankCard} ->
            {bank_card, #'ResourceBankCard'{bank_card = BankCard}}
    end,
    {resource, #p2p_transfer_RawResource{
        resource = Resource,
        contact_info = #'ContactInfo'{}
    }};

marshal(contact_info, ContactInfo) ->
    #'ContactInfo'{
        email = maps:get(<<"email">>, ContactInfo, undefined),
        phone_number = maps:get(<<"phoneNumber">>, ContactInfo, undefined)
    };

marshal(body, #{
    <<"amount">> := Amount,
    <<"currency">> := Currency
}) ->
    #'Cash'{
        amount = Amount,
        currency = marshal(currency, Currency)
    };

marshal(currency, Currency) ->
    #'CurrencyRef'{symbolic_code = Currency};

marshal(context, Ctx) ->
    ff_codec:marshal(context, Ctx).

%% Unmarshal

unmarshal(transfer, #p2p_transfer_P2PTransferState{
    id = ID,
    owner = IdentityID,
    sender = SenderResource,
    receiver = ReceiverResource,
    body = Body,
    created_at = CreatedAt,
    status = Status,
    external_id = ExternalID
}) ->
    Sender = unmarshal(sender, SenderResource),
    ContactInfo = maps:get(<<"contactInfo">>, Sender),
    genlib_map:compact(#{
        <<"id">> => ID,
        <<"identityID">> => IdentityID,
        <<"contactInfo">> => ContactInfo,
        <<"createdAt">> => CreatedAt,
        <<"body">> => unmarshal(body, Body),
        <<"sender">> => maps:remove(<<"contactInfo">>, Sender),
        <<"receiver">> => unmarshal(receiver, ReceiverResource),
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

unmarshal(sender, {resource, #p2p_transfer_RawResource{
    contact_info = ContactInfo,
    resource = {bank_card, #'ResourceBankCard'{
        bank_card = BankCard
    }}
}}) ->
    genlib_map:compact(#{
        <<"type">>          => <<"BankCardSenderResource">>,
        <<"contactInfo">>   => unmarshal(contact_info, ContactInfo),
        <<"token">>         => BankCard#'BankCard'.token,
        <<"paymentSystem">> => genlib:to_binary(BankCard#'BankCard'.payment_system),
        <<"bin">>           => BankCard#'BankCard'.bin,
        <<"lastDigits">>    => wapi_utils:get_last_pan_digits(BankCard#'BankCard'.masked_pan)
    });
unmarshal(receiver, {resource, #p2p_transfer_RawResource{
    contact_info = _ContactInfo,
    resource = {bank_card, #'ResourceBankCard'{
        bank_card = BankCard
    }}
}}) ->
    genlib_map:compact(#{
        <<"type">>          => <<"BankCardReceiverResource">>,
        <<"token">>         => BankCard#'BankCard'.token,
        <<"bin">>           => BankCard#'BankCard'.bin,
        <<"paymentSystem">> => genlib:to_binary(BankCard#'BankCard'.payment_system),
        <<"lastDigits">>    => wapi_utils:get_last_pan_digits(BankCard#'BankCard'.masked_pan)
    });

unmarshal(contact_info, ContactInfo) ->
    genlib_map:compact(#{
        <<"phoneNumber">> => ContactInfo#'ContactInfo'.phone_number,
        <<"email">> => ContactInfo#'ContactInfo'.email
    });

unmarshal(transfer_status, {pending, _}) ->
    #{<<"status">> => <<"Pending">>};
unmarshal(transfer_status, {succeeded, _}) ->
    #{<<"status">> => <<"Succeeded">>};
unmarshal(transfer_status, {failed, #p2p_status_Failed{failure = Failure}}) ->
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
