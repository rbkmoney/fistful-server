-module(wapi_p2p_transfer_backend).

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().

-type id() :: binary().
-type external_id() :: id().

-type error_create()
    :: {external_id_conflict, id(), external_id()}
     | {identity,     unauthorized}
     | {identity,     notfound}
     | {p2p_transfer, forbidden_currency}
     | {p2p_transfer, cash_range_exceeded}
     | {p2p_transfer, operation_not_permitted}
     | {token,        {not_verified, identity_mismatch}}
     | {token,        {not_verified, _}}
     | {sender,       invalid_resource}
     | {receiver,     invalid_resource}
     .

-type error_create_quote()
    :: {identity,     unauthorized}
     | {identity,     notfound}
     | {p2p_transfer, forbidden_currency}
     | {p2p_transfer, cash_range_exceeded}
     | {p2p_transfer, operation_not_permitted}
     | {sender,       invalid_resource}
     | {receiver,     invalid_resource}
     .

-type error_get()
    :: {p2p_transfer, unauthorized}
     | {p2p_transfer, notfound}
     .

-export([create_transfer/2]).
-export([quote_transfer/2]).
-export([get_transfer/2]).

-import(ff_pipeline, [do/1, unwrap/1]).

-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").

-spec create_transfer(req_data(), handler_context()) ->
    {ok, response_data()} | {error, error_create()}.
create_transfer(Params = #{<<"identityID">> := IdentityID}, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            case wapi_backend_utils:gen_id(p2p_transfer, Params, HandlerContext) of
                {ok, ID} ->
                    do_create_transfer(ID, Params, HandlerContext);
                {error, {external_id_conflict, ID}} ->
                    {error, {external_id_conflict, ID, maps:get(<<"externalID">>, Params, undefined)}}
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}};
        {error, notfound} ->
            {error, {identity, notfound}}
    end.

-spec get_transfer(req_data(), handler_context()) ->
    {ok, response_data()} | {error, error_get()}.
get_transfer(ID, HandlerContext) ->
    Request = {p2p_transfer, 'Get', [ID, #'EventRange'{}]},
    case service_call(Request, HandlerContext) of
        {ok, TransferThrift} ->
            case wapi_access_backend:check_resource(p2p_transfer, TransferThrift, HandlerContext) of
                ok ->
                    {ok, unmarshal_transfer(TransferThrift)};
                {error, unauthorized} ->
                    {error, {p2p_transfer, unauthorized}}
            end;
        {exception, #fistful_P2PNotFound{}} ->
            {error, {p2p_transfer, notfound}}
    end.

-spec quote_transfer(req_data(), handler_context()) ->
    {ok, response_data()} | {error, error_create_quote()}.

quote_transfer(Params = #{<<"identityID">> := IdentityID}, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            do_quote_transfer(Params, HandlerContext);
        {error, unauthorized} ->
            {error, {identity, unauthorized}};
        {error, notfound} ->
            {error, {identity, notfound}}
    end.

%% Internal

do_quote_transfer(Params, HandlerContext) ->
    Request = {p2p_transfer, 'GetQuote', [marshal_quote_params(Params)]},
    case service_call(Request, HandlerContext) of
        {ok, Quote} ->
            PartyID = wapi_handler_utils:get_owner(HandlerContext),
            Token = create_quote_token(Quote, PartyID),
            UnmarshaledQuote = unmarshal_quote(Quote),
            {ok, UnmarshaledQuote#{<<"token">> => Token}};
        {exception, #p2p_transfer_NoResourceInfo{type = sender}} ->
            {error, {sender, invalid_resource}};
        {exception, #p2p_transfer_NoResourceInfo{type = receiver}} ->
            {error, {receiver, invalid_resource}};
        {exception, #fistful_ForbiddenOperationCurrency{}} ->
            {error, {p2p_transfer, forbidden_currency}};
        {exception, #fistful_ForbiddenOperationAmount{}} ->
            {error, {p2p_transfer, cash_range_exceeded}};
        {exception, #fistful_IdentityNotFound{ }} ->
            {error, {identity, notfound}}
    end.

create_quote_token(Quote, PartyID) ->
    Payload = wapi_p2p_quote:create_token_payload(Quote, PartyID),
    {ok, Token} = issue_quote_token(PartyID, Payload),
    Token.

issue_quote_token(PartyID, Payload) ->
    uac_authorizer_jwt:issue(wapi_utils:get_unique_id(), PartyID, Payload, wapi_auth:get_signee()).

do_create_transfer(ID, Params, HandlerContext) ->
    do(fun() ->
        Context = wapi_backend_utils:make_ctx(Params, HandlerContext),
        TransferParams = unwrap(build_transfer_params(Params#{<<"id">> => ID})),
        Request = {p2p_transfer, 'Create', [TransferParams, marshal(context, Context)]},
        unwrap(process_p2p_transfer_call(Request, HandlerContext))
    end).

process_p2p_transfer_call(Request, HandlerContext) ->
    case service_call(Request, HandlerContext) of
        {ok, Transfer} ->
            {ok, unmarshal_transfer(Transfer)};
        {exception, #p2p_transfer_NoResourceInfo{type = sender}} ->
            {error, {sender, invalid_resource}};
        {exception, #p2p_transfer_NoResourceInfo{type = receiver}} ->
            {error, {receiver, invalid_resource}};
        {exception, #fistful_ForbiddenOperationCurrency{}} ->
            {error, {p2p_transfer, forbidden_currency}};
        {exception, #fistful_ForbiddenOperationAmount{}} ->
            {error, {p2p_transfer, cash_range_exceeded}};
        {exception, #fistful_OperationNotPermitted{}} ->
            {error, {p2p_transfer, operation_not_permitted}};
        {exception, #fistful_IdentityNotFound{ }} ->
            {error, {identity, notfound}}
    end.

build_transfer_params(Params = #{<<"quoteToken">> := QuoteToken, <<"identityID">> := IdentityID}) ->
    do(fun() ->
        VerifiedToken = unwrap(verify_p2p_quote_token(QuoteToken)),
        Quote = unwrap(wapi_p2p_quote:decode_token_payload(VerifiedToken)),
        ok = unwrap(authorize_p2p_quote_token(Quote, IdentityID)),
        TransferParams = marshal_transfer_params(Params),
        TransferParams#p2p_transfer_P2PTransferParams{quote = Quote}
    end);
build_transfer_params(Params) ->
    do(fun() -> marshal_transfer_params(Params) end).

verify_p2p_quote_token(Token) ->
    case uac_authorizer_jwt:verify(Token, #{}) of
        {ok, {_, _, VerifiedToken}} ->
            {ok, VerifiedToken};
        {error, Error} ->
            {error, {token, {not_verified, Error}}}
    end.

authorize_p2p_quote_token(#p2p_transfer_Quote{identity_id = IdentityID}, IdentityID) ->
    ok;
authorize_p2p_quote_token(_Quote, _IdentityID) ->
    {error, {token, {not_verified, identity_mismatch}}}.

service_call(Params, HandlerContext) ->
    wapi_handler_utils:service_call(Params, HandlerContext).

%% Marshal

marshal_quote_params(#{
    <<"body">> := Body,
    <<"identityID">> := IdentityID,
    <<"sender">> := Sender,
    <<"receiver">> := Receiver
}) ->
    #p2p_transfer_QuoteParams{
        body = marshal_body(Body),
        identity_id = IdentityID,
        sender = marshal_quote_participant(Sender),
        receiver = marshal_quote_participant(Receiver)
    }.

marshal_quote_participant(#{
    <<"token">> := Token
}) ->
    case wapi_crypto:decrypt_bankcard_token(Token) of
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
    end.

marshal_transfer_params(#{
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
        sender = marshal_sender(Sender#{<<"contactInfo">> => ContactInfo}),
        receiver = marshal_receiver(Receiver),
        body = marshal_body(Body)
    }.

marshal_sender(#{
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
        contact_info = marshal_contact_info(ContactInfo)
    }}.

marshal_receiver(#{
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
    }}.

marshal_contact_info(ContactInfo) ->
    #'ContactInfo'{
        email = maps:get(<<"email">>, ContactInfo, undefined),
        phone_number = maps:get(<<"phoneNumber">>, ContactInfo, undefined)
    }.

marshal_body(#{
    <<"amount">> := Amount,
    <<"currency">> := Currency
}) ->
    #'Cash'{
        amount = Amount,
        currency = marshal_currency(Currency)
    }.

marshal_currency(Currency) ->
    #'CurrencyRef'{symbolic_code = Currency}.

marshal(T, V) ->
    ff_codec:marshal(T, V).

%% Unmarshal

unmarshal_quote(#p2p_transfer_Quote{
    fees = Fees,
    expires_on = ExpiresOn
}) ->
    genlib_map:compact(#{
        <<"expiresOn">> => ExpiresOn,
        <<"customerFee">> => unmarshal_fees(Fees)
    }).

unmarshal_fees(#'Fees'{fees = #{operation_amount := Cash}}) ->
    unmarshal_body(Cash).

unmarshal_transfer(#p2p_transfer_P2PTransferState{
    id = ID,
    owner = IdentityID,
    sender = SenderResource,
    receiver = ReceiverResource,
    body = Body,
    created_at = CreatedAt,
    status = Status,
    external_id = ExternalID
}) ->
    Sender = unmarshal_sender(SenderResource),
    ContactInfo = maps:get(<<"contactInfo">>, Sender),
    genlib_map:compact(#{
        <<"id">> => ID,
        <<"identityID">> => IdentityID,
        <<"contactInfo">> => ContactInfo,
        <<"createdAt">> => CreatedAt,
        <<"body">> => unmarshal_body(Body),
        <<"sender">> => maps:remove(<<"contactInfo">>, Sender),
        <<"receiver">> => unmarshal_receiver(ReceiverResource),
        <<"status">> => unmarshal_transfer_status(Status),
        <<"externalID">> => maybe_unmarshal(id, ExternalID)
    }).

unmarshal_body(#'Cash'{
    amount   = Amount,
    currency = Currency
}) ->
    #{
        <<"amount">> => unmarshal(amount, Amount),
        <<"currency">> => unmarshal(currency_ref, Currency)
    }.

unmarshal_sender({resource, #p2p_transfer_RawResource{
    contact_info = ContactInfo,
    resource = {bank_card, #'ResourceBankCard'{
        bank_card = BankCard
    }}
}}) ->
    genlib_map:compact(#{
        <<"type">>          => <<"BankCardSenderResource">>,
        <<"contactInfo">>   => unmarshal_contact_info(ContactInfo),
        <<"token">>         => BankCard#'BankCard'.token,
        <<"paymentSystem">> => genlib:to_binary(BankCard#'BankCard'.payment_system),
        <<"bin">>           => BankCard#'BankCard'.bin,
        <<"lastDigits">>    => wapi_utils:get_last_pan_digits(BankCard#'BankCard'.masked_pan)
    }).

unmarshal_receiver({resource, #p2p_transfer_RawResource{
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
    }).

unmarshal_contact_info(ContactInfo) ->
    genlib_map:compact(#{
        <<"phoneNumber">> => ContactInfo#'ContactInfo'.phone_number,
        <<"email">> => ContactInfo#'ContactInfo'.email
    }).

unmarshal_transfer_status({pending, _}) ->
    #{<<"status">> => <<"Pending">>};
unmarshal_transfer_status({succeeded, _}) ->
    #{<<"status">> => <<"Succeeded">>};
unmarshal_transfer_status({failed, #p2p_status_Failed{failure = Failure}}) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => unmarshal(failure, Failure)
    }.

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_T, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).
