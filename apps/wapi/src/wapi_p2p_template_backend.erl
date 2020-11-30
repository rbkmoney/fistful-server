-module(wapi_p2p_template_backend).

-export([create/2]).
-export([get/2]).
-export([block/2]).
-export([issue_access_token/3]).
-export([issue_transfer_ticket/3]).
-export([quote_transfer/3]).
-export([create_transfer/3]).

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_p2p_template_thrift.hrl").

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().
-type id() :: binary().
-type external_id() :: binary().

%% P2PTemplate interface

-spec create(req_data(), handler_context()) ->
    {ok, response_data()} |
    {error, {external_id_conflict, id(), external_id()}} |
    {error, {identity, unauthorized}} |
    {error, {identity, notfound}} |
    {error, {currency, notfound}} |
    {error, inaccessible} |
    {error, invalid_operation_amount}.

create(Params = #{<<"identityID">> := IdentityID}, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            case wapi_backend_utils:gen_id(p2p_template, Params, HandlerContext) of
                {ok, ID} ->
                    Context = wapi_backend_utils:make_ctx(Params, HandlerContext),
                    TemplateParams = marshal_template_params(Params#{<<"id">> => ID}),
                    Request = {fistful_p2p_template, 'Create', [TemplateParams, marshal_context(Context)]},
                    create_request(Request, HandlerContext);
                {error, {external_id_conflict, ID}} ->
                    ExternalID = maps:get(<<"externalID">>, Params, undefined),
                    {error, {external_id_conflict, ID, ExternalID}}
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}};
        {error, notfound} ->
            {error, {identity, notfound}}
    end.

create_request(Request, HandlerContext) ->
    case wapi_handler_utils:service_call(Request, HandlerContext) of
        {ok, Template} ->
            {ok, unmarshal_template(Template)};
        {exception, #fistful_IdentityNotFound{}} ->
            {error, {identity, notfound}};
        {exception, #fistful_CurrencyNotFound{}} ->
            {error, {currency, notfound}};
        {exception, #fistful_PartyInaccessible{}} ->
            {error, inaccessible};
        {exception, #fistful_InvalidOperationAmount{}} ->
            {error, invalid_operation_amount}
    end.

-spec get(id(), handler_context()) ->
    {ok, response_data()} |
    {error, {p2p_template, notfound | unauthorized}}.

get(ID, HandlerContext) ->
    Request = {fistful_p2p_template, 'Get', [ID, #'EventRange'{}]},
    case wapi_handler_utils:service_call(Request, HandlerContext) of
        {ok, Template} ->
            case wapi_access_backend:check_resource(p2p_template, Template, HandlerContext) of
                ok ->
                    {ok, unmarshal_template(Template)};
                {error, unauthorized} ->
                    {error, {p2p_template, unauthorized}}
            end;
        {exception, #fistful_P2PTemplateNotFound{}} ->
            {error, {p2p_template, notfound}}
    end.

-spec block(id(), handler_context()) ->
    ok |
    {error, {p2p_template, notfound | unauthorized}}.

block(ID, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(p2p_template, ID, HandlerContext) of
        ok ->
            Request = {fistful_p2p_template, 'SetBlocking', [ID, blocked]},
            case wapi_handler_utils:service_call(Request, HandlerContext) of
                {ok, _} ->
                    ok;
                {exception, #fistful_P2PTemplateNotFound{}} ->
                    {error, {p2p_template, notfound}}
            end;
        {error, unauthorized} ->
            {error, {p2p_template, unauthorized}};
        {error, notfound} ->
            {error, {p2p_template, notfound}}
    end.

-spec issue_access_token(id(), binary(), handler_context()) ->
    {ok, binary()} |
    {error, expired} |
    {error, {p2p_template, notfound | unauthorized}}.

issue_access_token(ID, Expiration, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(p2p_template, ID, HandlerContext) of
        ok ->
            wapi_backend_utils:issue_grant_token(
                {p2p_templates, ID, #{<<"expiration">> => Expiration}},
                Expiration, HandlerContext
            );
        {error, unauthorized} ->
            {error, {p2p_template, unauthorized}};
        {error, notfound} ->
            {error, {p2p_template, notfound}}
    end.

-spec issue_transfer_ticket(id(), binary(), handler_context()) ->
    {ok, {binary(), binary()}} |
    {error, expired} |
    {error, {p2p_template, notfound | unauthorized}}.

issue_transfer_ticket(ID, WishExpiration, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(p2p_template, ID, HandlerContext) of
        ok ->
            TransferID =  gen_transfer_id(HandlerContext),
            AccessExpiration = context_access_expiration(HandlerContext),
            Expiration = choose_tiket_expiration(WishExpiration, AccessExpiration),
            case wapi_backend_utils:issue_grant_token(
                {p2p_template_transfers, ID, #{<<"transferID">> => TransferID}},
                Expiration,
                HandlerContext
            ) of
                {ok, Token} ->
                    {ok, {Token, Expiration}};
                Error = {error, _} ->
                    Error
            end;
        {error, unauthorized} ->
            {error, {p2p_template, unauthorized}};
        {error, notfound} ->
            {error, {p2p_template, notfound}}
    end.

-spec quote_transfer(id(), req_data(), handler_context()) ->
    {ok, response_data()} |
    {error, {p2p_template, notfound | unauthorized}} |
    {error, {identity, notfound}} |
    {error, {forbidden_currency, _}} |
    {error, {forbidden_amount, _}} |
    {error, {operation_not_permitted, _}} |
    {error, {sender | receiver, invalid_resource}} |
    {error, {sender | receiver, {invalid_resource_token, binary()}}}.

quote_transfer(ID, Params, HandlerContext) ->
    do(fun() ->
        unwrap(p2p_template, wapi_access_backend:check_resource_by_id(p2p_template, ID, HandlerContext)),
        SenderResource = unwrap(sender, decode_resource(maps:get(<<"sender">>, Params))),
        ReceiverResource = unwrap(receiver, decode_resource(maps:get(<<"receiver">>, Params))),
        QuoteParams = Params#{
            <<"sender">> => SenderResource,
            <<"receiver">> => ReceiverResource
        },
        Request = {fistful_p2p_template, 'GetQuote', [ID, marshal_quote_params(QuoteParams)]},
        unwrap(quote_transfer_request(Request, HandlerContext))
    end).

quote_transfer_request(Request, HandlerContext) ->
    case wapi_handler_utils:service_call(Request, HandlerContext) of
        {ok, Quote} ->
            PartyID = wapi_handler_utils:get_owner(HandlerContext),
            Token = create_quote_token(Quote, PartyID),
            QuoteWapi = unmarshal_quote(Quote),
            {ok, QuoteWapi#{ <<"token">> => Token }};
        {exception, #fistful_P2PTemplateNotFound{}} ->
            {error, {p2p_template, notfound}};
        {exception, #fistful_IdentityNotFound{}} ->
            {error, {identity, notfound}};
        {exception, #fistful_ForbiddenOperationCurrency{currency = Currency}} ->
            {error, {forbidden_currency, unmarshal(currency_ref, Currency)}};
        {exception, #fistful_ForbiddenOperationAmount{amount = Amount}} ->
            {error, {forbidden_amount, unmarshal(cash, Amount)}};
        {exception, #fistful_OperationNotPermitted{details = Details}} ->
            {error, {operation_not_permitted, maybe_unmarshal(string, Details)}};
        {exception, #p2p_transfer_NoResourceInfo{type = Type}} ->
            {error, {Type, invalid_resource}}
    end.

-spec create_transfer(id(), req_data(), handler_context()) ->
    {ok, response_data()} |
    {error, {p2p_template, notfound | unauthorized}} |
    {error, {forbidden_currency, _}} |
    {error, {forbidden_amount, _}} |
    {error, {operation_not_permitted, _}} |
    {error, {sender | receiver, invalid_resource}} |
    {error, {sender | receiver, {invalid_resource_token, binary()}}} |
    {error, {token, _}} |
    {error, {external_id_conflict, _}}.

create_transfer(TemplateID, Params0, HandlerContext) ->
    do(fun() ->
        unwrap(p2p_template, wapi_access_backend:check_resource_by_id(p2p_template, TemplateID, HandlerContext)),
        SenderResource = unwrap(sender, decode_resource(maps:get(<<"sender">>, Params0))),
        ReceiverResource = unwrap(receiver, decode_resource(maps:get(<<"receiver">>, Params0))),
        TransferID = context_transfer_id(HandlerContext),
        Params1 = Params0#{<<"id">> => TransferID},
        unwrap(validate_transfer_id(SenderResource, ReceiverResource, Params1, HandlerContext)),
        Params2 = case get_quote_token(Params1) of
            ok ->
                Params1;
            {ok, Quote} ->
                Template = unwrap(get(TemplateID, HandlerContext)),
                unwrap(validate_token_payload(Quote, Template)),
                Params1#{<<"quote">> => Quote};
            {error, Error} ->
                throw(Error)
        end,
        unwrap(create_transfer_request(TemplateID, Params2#{
            <<"sender">> => SenderResource,
            <<"receiver">> => ReceiverResource
        }, HandlerContext))
    end).

get_quote_token(#{<<"quoteToken">> := Token}) ->
    case uac_authorizer_jwt:verify(Token, #{}) of
        {ok, {_, _, VerifiedToken}} ->
            case wapi_p2p_quote:decode_token_payload(VerifiedToken) of
                {ok, Quote} ->
                    {ok, Quote};
                Error ->
                    Error
            end;
        {error, Error} ->
            {error, {token, {not_verified, Error}}}
    end;
get_quote_token(_Params) ->
    ok.

create_transfer_request(TemplateID, Params, HandlerContext) ->
    MarshaledContext = marshal_context(wapi_backend_utils:make_ctx(Params, HandlerContext)),
    MarshaledParams = marshal_transfer_params(Params),
    Request = {fistful_p2p_template, 'CreateTransfer', [TemplateID, MarshaledParams, MarshaledContext]},
    case wapi_handler_utils:service_call(Request, HandlerContext) of
        {ok, Transfer} ->
            {ok, unmarshal_transfer(Transfer)};
        {exception, #fistful_P2PTemplateNotFound{}} ->
            {error, {p2p_template, notfound}};
        {exception, #fistful_ForbiddenOperationCurrency{currency = Currency}} ->
            {error, {forbidden_currency, unmarshal(currency_ref, Currency)}};
        {exception, #fistful_ForbiddenOperationAmount{amount = Amount}} ->
            {error, {forbidden_amount, unmarshal(cash, Amount)}};
        {exception, #fistful_OperationNotPermitted{details = Details}} ->
            {error, {operation_not_permitted, maybe_unmarshal(string, Details)}};
        {exception, #p2p_transfer_NoResourceInfo{type = Type}} ->
            {error, {Type, invalid_resource}}
    end.

%% Create quoteToken from Quote

create_quote_token(Quote, PartyID) ->
    Payload = wapi_p2p_quote:create_token_payload(Quote, PartyID),
    {ok, Token} = issue_quote_token(PartyID, Payload),
    Token.

issue_quote_token(PartyID, Data) ->
    uac_authorizer_jwt:issue(wapi_utils:get_unique_id(), PartyID, Data, wapi_auth:get_signee()).

%%

choose_tiket_expiration(WishExpiration, AccessExpiration) ->
    WishMs = woody_deadline:to_unixtime_ms(woody_deadline:from_binary(WishExpiration)),
    AccessMs = woody_deadline:to_unixtime_ms(woody_deadline:from_binary(AccessExpiration)),
    case WishMs > AccessMs of
        true ->
            AccessExpiration;
        false ->
            WishExpiration
    end.

%% Extract access expiration from handler context

context_access_expiration(HandlerContext) ->
    AuthContext = wapi_handler_utils:get_auth_context(HandlerContext),
    {_, _, Claims} = AuthContext,
    AccessData = maps:get(<<"data">>, Claims),
    maps:get(<<"expiration">>, AccessData).

%% Extract transfer id from handler context

context_transfer_id(HandlerContext) ->
    AuthContext = wapi_handler_utils:get_auth_context(HandlerContext),
    {_, _, Claims} = AuthContext,
    AccessData = maps:get(<<"data">>, Claims),
    maps:get(<<"transferID">>, AccessData).

%% Generate new transfer id for transfer ticket

gen_transfer_id(#{woody_context := WoodyContext} = HandlerContext) ->
    PartyID = wapi_handler_utils:get_owner(HandlerContext),

    %% TODO: Key = wapi_backend_utils:get_idempotent_key(ticket, PartyID, undefined),
    Key = bender_client:get_idempotent_key(<<"issue_p2p_transfer_ticket">>, ticket, PartyID, undefined),

    %% TODO: {ok, TransferID} = wapi_backend_utils:gen_id_by_type(ticket, Key, 0, HandlerContext),
    {ok, {TransferID, _}} =  bender_client:gen_snowflake(Key, 0, WoodyContext),
    TransferID.

%% Validate transfer_id by Params hash

validate_transfer_id(SenderResource, ReceiverResource, #{<<"id">> := TransferID} = Params, HandlerContext) ->
    NewParams = Params#{<<"sender">> => SenderResource, <<"receiver">> => ReceiverResource},
    case validate_transfer_id(TransferID, NewParams, HandlerContext) of
        ok ->
            ok;
        {error, {external_id_conflict, ID}} ->
            % Replace this call by error report after deploy
            logger:warning("external_id_conflict: ~p. try old hashing", [ID]),
            validate_transfer_id_legacy(Params, HandlerContext)
    end.

validate_transfer_id_legacy(#{<<"id">> := TransferID} = Params, HandlerContext) ->
    case validate_transfer_id(TransferID, Params, HandlerContext) of
        ok ->
            ok;
        {error, {external_id_conflict, ID}} ->
            {error, {external_id_conflict, ID}}
    end.

validate_transfer_id(TransferID, Params, #{woody_context := WoodyContext} = HandlerContext) ->
    PartyID = wapi_handler_utils:get_owner(HandlerContext),
    Hash = wapi_backend_utils:create_params_hash(Params),
    Key = wapi_backend_utils:get_idempotent_key(p2p_transfer_with_template, PartyID, TransferID),
    case bender_client:gen_constant(Key, TransferID, Hash, WoodyContext) of
        {ok, {TransferID, _}} ->
            ok;
        {error, {external_id_conflict, _ID}} = Error ->
            Error
    end.

%% Validate identity id from the quote

validate_token_payload(#p2p_transfer_Quote{identity_id = IdentityID}, #{<<"identityID">> := IdentityID}) ->
    ok;
validate_token_payload(_Quote, _template) ->
    {error, identity_mismatch}.

decode_resource(EncodedResource) ->
    case wapi_backend_utils:decode_resource(EncodedResource) of
        {ok, Resource} ->
            % OldParams is need for create_continue_genid_old
            % Remove the parameter & create_continue_genid_old after deploy
            {ok, Resource};
        {error, {Type, Error}} ->
            logger:warning("~p token decryption failed: ~p", [Type, Error]),
            {error, {invalid_resource_token, Type}}
    end.

%% Convert swag maps to thrift records

marshal_template_params(Params = #{
    <<"id">> := ID,
    <<"identityID">> := IdentityID,
    <<"details">> := Details
}) ->
    ExternalID = maps:get(<<"externalID">>, Params, undefined),
    #p2p_template_P2PTemplateParams{
        id = marshal(id, ID),
        identity_id  = marshal(id, IdentityID),
        template_details = marshal_template_details(Details),
        external_id = maybe_marshal(id, ExternalID)
    }.

marshal_template_details(Details = #{
    <<"body">> := Body
}) ->
    Metadata = maps:get(<<"metadata">>, Details, undefined),
    #p2p_template_P2PTemplateDetails{
        body = marshal_template_body(Body),
        metadata = marshal_template_metadata(Metadata)
    }.

marshal_template_body(#{
    <<"value">> := Cash
}) ->
    Currency = maps:get(<<"currency">>, Cash),
    Amount = maps:get(<<"amount">>, Cash, undefined),
    #p2p_template_P2PTemplateBody{
        value = #p2p_template_Cash{
            currency = marshal(currency_ref, Currency),
            amount = maybe_marshal(amount, Amount)
        }
    }.

marshal_template_metadata(undefined) ->
    undefined;
marshal_template_metadata(#{
    <<"defaultMetadata">> := Metadata
}) ->
    #p2p_template_P2PTemplateMetadata{
        value = marshal_context(Metadata)
    }.

marshal_body(#{
    <<"amount">> := Amount,
    <<"currency">> := Currency
}) ->
    marshal(cash, {Amount, Currency}).

marshal_quote_params(#{
    <<"sender">> := Sender,
    <<"receiver">> := Receiver,
    <<"body">> := Body
}) ->
    #p2p_template_P2PTemplateQuoteParams{
        sender = marshal_quote_participant(Sender),
        receiver = marshal_quote_participant(Receiver),
        body = marshal_body(Body)
    }.

marshal_quote_participant(#{
    <<"decryptedResource">> := Resource
}) ->
    {bank_card, #'ResourceBankCard'{bank_card = Resource}}.

marshal_transfer_params(#{
    <<"id">> := TransferID,
    <<"sender">> := Sender,
    <<"receiver">> := Receiver,
    <<"body">> := Body,
    <<"contactInfo">> := ContactInfo
} = Params) ->
    Quote = maps:get(<<"quote">>, Params, undefined), %% decrypted from quoteToken
    Metadata = maps:get(<<"metadata">>, Params, undefined),
    #p2p_template_P2PTemplateTransferParams{
        id = TransferID,
        sender = marshal_sender(Sender#{<<"contactInfo">> => ContactInfo}),
        receiver = marshal_receiver(Receiver),
        body = marshal_body(Body),
        % TODO: client_info
        % TODO: deadline
        quote = Quote,
        metadata = marshal_context(Metadata)
    }.

marshal_sender(#{
    <<"authData">> := AuthData,
    <<"contactInfo">> := ContactInfo,
    <<"decryptedResource">> := Resource
}) ->
    ResourceBankCard = #'ResourceBankCard'{
        bank_card = Resource,
        auth_data = {session_data, #'SessionAuthData'{id = AuthData}}
    },
    {resource, #p2p_transfer_RawResource{
        resource = {bank_card, ResourceBankCard},
        contact_info = marshal_contact_info(ContactInfo)
    }}.

marshal_receiver(#{
    <<"decryptedResource">> := Resource
}) ->
    {resource, #p2p_transfer_RawResource{
        resource = {bank_card, #'ResourceBankCard'{bank_card = Resource}},
        contact_info = #'ContactInfo'{}
    }}.

marshal_contact_info(ContactInfo) ->
    Email = maps:get(<<"email">>, ContactInfo, undefined),
    Phone = maps:get(<<"phoneNumber">>, ContactInfo, undefined),
    #'ContactInfo'{
        email = Email,
        phone_number = Phone
    }.

marshal_context(Context) ->
    maybe_marshal(context, Context).

marshal(T, V) ->
    ff_codec:marshal(T, V).

maybe_marshal(_, undefined) ->
    undefined;
maybe_marshal(T, V) ->
    marshal(T, V).

%% Convert thrift records to swag maps

unmarshal_template(#p2p_template_P2PTemplateState{
    id = ID,
    identity_id = IdentityID,
    created_at = CreatedAt,
    template_details = Details,
    blocking = Blocking,
    external_id = ExternalID,
    context = _Context
}) ->
    genlib_map:compact(#{
        <<"id">>            => unmarshal(id, ID),
        <<"identityID">>    => unmarshal(id, IdentityID),
        <<"createdAt">>     => unmarshal(string, CreatedAt),
        <<"isBlocked">>     => unmarshal_blocking(Blocking),
        <<"details">>       => unmarshal_template_details(Details),
        <<"externalID">>    => maybe_unmarshal(id, ExternalID)
    }).

unmarshal_template_details(#p2p_template_P2PTemplateDetails{
    body = Body,
    metadata = Metadata
}) ->
    genlib_map:compact(#{
        <<"body">>      => unmarshal_template_body(Body),
        <<"metadata">>  => unmarshal_template_metadata(Metadata)
    }).

unmarshal_template_body(#p2p_template_P2PTemplateBody{
    value = #p2p_template_Cash{
        currency = Currency,
        amount = Amount
    }
}) ->
    #{
        <<"value">> => genlib_map:compact(#{
            <<"currency">> => unmarshal(currency_ref, Currency),
            <<"amount">> => maybe_unmarshal(amount, Amount)
        })
    }.

unmarshal_body(#'Cash'{
    amount   = Amount,
    currency = Currency
}) ->
    #{
        <<"amount">> => unmarshal(amount, Amount),
        <<"currency">> => unmarshal(currency_ref, Currency)
    }.

unmarshal_template_metadata(undefined) ->
    undefined;
unmarshal_template_metadata(#p2p_template_P2PTemplateMetadata{
    value = Metadata
}) ->
    genlib_map:compact(#{
        <<"defaultMetadata">> => maybe_unmarshal(context, Metadata)
    }).

unmarshal_quote(#p2p_transfer_Quote{
    expires_on = ExpiresOn,
    fees = Fees
}) ->
    genlib_map:compact(#{
        <<"customerFee">> => unmarshal_fees(Fees),
        <<"expiresOn">> => unmarshal(string, ExpiresOn)
    }).

unmarshal_fees(#'Fees'{fees = #{surplus := Cash}}) ->
    unmarshal_body(Cash);
unmarshal_fees(#'Fees'{fees = #{operation_amount := Cash}}) ->
    unmarshal_body(Cash).

unmarshal_transfer(#p2p_transfer_P2PTransferState{
    id = TransferID,
    owner = IdentityID,
    sender = SenderResource,
    receiver = ReceiverResource,
    body = Body,
    status = Status,
    created_at = CreatedAt,
    external_id = ExternalID,
    metadata = Metadata
}) ->
    Sender = unmarshal_sender(SenderResource),
    ContactInfo = maps:get(<<"contactInfo">>, Sender),
    genlib_map:compact(#{
        <<"id">> => TransferID,
        <<"identityID">> => IdentityID,
        <<"createdAt">> => CreatedAt,
        <<"body">> => unmarshal_body(Body),
        <<"sender">> => maps:remove(<<"contactInfo">>, Sender),
        <<"receiver">> => unmarshal_receiver(ReceiverResource),
        <<"status">> => unmarshal_transfer_status(Status),
        <<"contactInfo">> => ContactInfo,
        <<"externalID">> => maybe_unmarshal(id, ExternalID),
        <<"metadata">> => maybe_unmarshal(context, Metadata)
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

unmarshal_blocking(undefined) ->
    undefined;
unmarshal_blocking(unblocked) ->
    false;
unmarshal_blocking(blocked) ->
    true.

%% Utility

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).
