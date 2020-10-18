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

-type error_get_events()
    :: error_get()
     | {token, {unsupported_version, _}}
     | {token, {not_verified, _}}
     .

-export([create_transfer/2]).
-export([get_transfer/2]).
-export([quote_transfer/2]).
-export([get_transfer_events/3]).

-import(ff_pipeline, [do/1, unwrap/1]).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_p2p_session_thrift.hrl").

-define(DEFAULT_EVENTS_LIMIT, 50).
-define(CONTINUATION_TRANSFER, <<"p2p_transfer_event_id">>).
-define(CONTINUATION_SESSION, <<"p2p_session_event_id">>).

-type event() :: #p2p_transfer_Event{} | #p2p_session_Event{}.
-type event_entity() :: p2p_transfer | p2p_session.
-type event_range() :: #'EventRange'{}.
-type event_id() :: ff_proto_base_thrift:'EventID'() | undefined.

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

-spec get_transfer_events(id(), binary() | undefined, handler_context()) ->
    {ok, response_data()} | {error, error_get_events()}.

get_transfer_events(ID, Token, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(p2p_transfer, ID, HandlerContext) of
        ok ->
            do_get_events(ID, Token, HandlerContext);
        {error, unauthorized} ->
            {error, {p2p_transfer, unauthorized}};
        {error, notfound} ->
            {error, {p2p_transfer, notfound}}
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

%% @doc
%% The function returns the list of events for the specified Transfer.
%%
%% First get Transfer for extract the Session ID.
%%
%% Then, the Continuation Token is verified.  Latest EventIDs of Transfer and
%% Session are stored in the token for possibility partial load of events.
%%
%% The events are retrieved no lesser ID than those stored in the token, and count
%% is limited by wapi.events_fetch_limit option or ?DEFAULT_EVENTS_LIMIT
%%
%% The received events are then mixed and ordered by the time of occurrence.
%% The resulting set is returned to the client.
%%
%% @todo Now there is always only zero or one session. But there may be more than one
%% session in the future, so the code of polling sessions and mixing results
%% will need to be rewrited.

-spec do_get_events(id(), binary() | undefined, handler_context()) ->
    {ok, response_data()} | {error, error_get_events()}.

do_get_events(ID, Token, HandlerContext) ->
    do(fun() ->
        PartyID = wapi_handler_utils:get_owner(HandlerContext),
        SessionID = unwrap(request_session_id(ID, HandlerContext)),

        DecodedToken = unwrap(continuation_token_unpack(Token, PartyID)),
        PrevTransferCursor = continuation_token_cursor(p2p_transfer, DecodedToken),
        PrevSessionCursor = continuation_token_cursor(p2p_session, DecodedToken),

        {TransferEvents, TransferCursor} = unwrap(events_collect(
            p2p_transfer,
            ID,
            events_range(PrevTransferCursor),
            HandlerContext,
            []
        )),

        {SessionEvents, SessionCursor}  = unwrap(events_collect(
            p2p_session,
            SessionID,
            events_range(PrevSessionCursor),
            HandlerContext,
            []
        )),

        NewTransferCursor = events_max(PrevTransferCursor, TransferCursor),
        NewSessionCursor = events_max(PrevSessionCursor, SessionCursor),
        NewToken = unwrap(continuation_token_pack(NewTransferCursor, NewSessionCursor, PartyID)),

        Events = {NewToken, events_merge([TransferEvents, SessionEvents])},
        unmarshal_events(Events)
    end).

%% get p2p_transfer from backend and return last sesssion ID

-spec request_session_id(id(), handler_context()) ->
    {ok, undefined | id ()} | {error, {p2p_transfer, notfound}}.

request_session_id(ID, HandlerContext) ->
    Request = {p2p_transfer, 'Get', [ID, #'EventRange'{}]},
    case service_call(Request, HandlerContext) of
        {ok, #p2p_transfer_P2PTransferState{sessions = []}} ->
            {ok, undefined};
        {ok, #p2p_transfer_P2PTransferState{sessions = Sessions}} ->
            Session = lists:last(Sessions),
            {ok, Session#p2p_transfer_SessionState.id};
        {exception, #fistful_P2PNotFound{}} ->
            {error, {p2p_transfer, notfound}}
    end.

%% create and code a new continuation token

continuation_token_pack(TransferCursor, SessionCursor, PartyID) ->
    Token = genlib_map:compact(#{
        <<"version">>               => 1,
        ?CONTINUATION_TRANSFER => TransferCursor,
        ?CONTINUATION_SESSION => SessionCursor
    }),
    uac_authorizer_jwt:issue(wapi_utils:get_unique_id(), PartyID, Token, wapi_auth:get_signee()).

%% verify, decode and check version of continuation token

continuation_token_unpack(undefined, _PartyID) ->
    {ok, #{}};
continuation_token_unpack(Token, PartyID) ->
    case uac_authorizer_jwt:verify(Token, #{}) of
        {ok, {_, PartyID, #{<<"version">> := 1} = VerifiedToken}} ->
            {ok, VerifiedToken};
        {ok, {_, PartyID, #{<<"version">> := Version}}} ->
            {error, {token, {unsupported_version, Version}}};
        {ok, {_, WrongPatryID, _}} when WrongPatryID /= PartyID  ->
            {error, {token, {not_verified, wrong_party_id}}};
        {error, Error} ->
            {error, {token, {not_verified, Error}}}
    end.

%% get cursor event id by entity

continuation_token_cursor(p2p_transfer, DecodedToken) ->
    maps:get(?CONTINUATION_TRANSFER, DecodedToken, undefined);
continuation_token_cursor(p2p_session, DecodedToken) ->
    maps:get(?CONTINUATION_SESSION, DecodedToken, undefined).

%% collect events from Entity backend

-spec events_collect(event_entity(), id() | undefined, event_range(), handler_context(), Acc0) ->
    {ok, {Acc1, event_id()}} | {error, {p2p_transfer, notfound}} when
        Acc0 :: [] | [event()],
        Acc1 :: [] | [event()].

events_collect(p2p_session, undefined, #'EventRange'{'after' = Cursor}, _HandlerContext, Acc) ->
    {ok, {Acc, Cursor}};
events_collect(_Entity, _EntityID, #'EventRange'{'after' = Cursor, 'limit' = Limit}, _HandlerContext, Acc)
    when is_integer(Limit) andalso Limit =< 0 ->
        {ok, {Acc, Cursor}};
events_collect(Entity, EntityID, EventRange, HandlerContext, Acc) ->
    #'EventRange'{'after' = Cursor, 'limit' = Limit} = EventRange,
    Request = {Entity, 'GetEvents', [EntityID, EventRange]},
    case events_request(Request, HandlerContext) of
        {ok, {[], _}} ->
            {ok, {Acc, Cursor}};
        {ok, {Events, NewCursor}} when is_integer(Limit) ->
            NewEventRange = events_range(NewCursor, Limit - length(Events)),
            events_collect(Entity, EntityID, NewEventRange, HandlerContext, Acc ++ Events);
        {ok, {Events, NewCursor}} ->
            {ok, {Acc ++ Events, NewCursor}};
        {error, _} = Error ->
            Error
    end.

-spec events_request(Request, handler_context()) ->
    {ok, {[] | [event()], event_id()}} | {error, {p2p_transfer, notfound}} when
        Request :: {event_entity(), 'GetEvents', [id() | event_range()]}.

events_request(Request, HandlerContext) ->
    case service_call(Request, HandlerContext) of
        {ok, []} ->
            {ok, {[], undefined}};
        {ok, EventsThrift} ->
            Cursor = events_cursor(lists:last(EventsThrift)),
            Events = lists:filter(fun events_filter/1, EventsThrift),
            {ok, {Events, Cursor}};
        {exception, #fistful_P2PNotFound{}} ->
            {error, {p2p_transfer, notfound}};
        {exception, #fistful_P2PSessionNotFound{}} ->
            {ok, {[], undefined}}
    end.

events_filter(#p2p_transfer_Event{change = {status_changed, _}}) ->
    true;
events_filter(#p2p_session_Event{change = {ui,  #p2p_session_UserInteractionChange{payload = Payload}}}) ->
    case Payload of
        {status_changed, #p2p_session_UserInteractionStatusChange{
            status = {pending, _}
        }} ->
            false;
        _Other ->
            % {created ...}
            % {status_changed, ... status = {finished, ...}}
            % Take created & finished user interaction events
            true
    end;
events_filter(_Event) ->
    false.

events_merge(EventsList) ->
    lists:sort(fun(Ev1, Ev2) -> events_timestamp(Ev1) < events_timestamp(Ev2) end, lists:append(EventsList)).

events_cursor(#p2p_transfer_Event{event = ID}) -> ID;
events_cursor(#p2p_session_Event{event = ID}) -> ID.

events_timestamp(#p2p_transfer_Event{occured_at = OccuredAt}) -> OccuredAt;
events_timestamp(#p2p_session_Event{occured_at = OccuredAt}) -> OccuredAt.

events_range(CursorID)  ->
    events_range(CursorID, genlib_app:env(wapi, events_fetch_limit, ?DEFAULT_EVENTS_LIMIT)).
events_range(CursorID, Limit) ->
    #'EventRange'{'after' = CursorID, 'limit' = Limit}.

events_max(NewEventID, OldEventID) when is_integer(NewEventID) andalso is_integer(OldEventID) ->
    erlang:max(NewEventID, OldEventID);
events_max(NewEventID, OldEventID) ->
    genlib:define(NewEventID, OldEventID).

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

unmarshal_events({Token, Events}) ->
    #{
        <<"continuationToken">> => unmarshal(string, Token),
        <<"result">> => [unmarshal_event(Ev) || Ev <- Events]
    }.

unmarshal_event(#p2p_transfer_Event{
    occured_at = OccuredAt,
    change = Change
}) ->
    #{
        <<"createdAt">> => unmarshal(string, OccuredAt),
        <<"change">> => unmarshal_event_change(Change)
    };
unmarshal_event(#p2p_session_Event{
    occured_at = OccuredAt,
    change = Change
}) ->
    #{
        <<"createdAt">> => unmarshal(string, OccuredAt),
        <<"change">> => unmarshal_event_change(Change)
    }.

unmarshal_event_change({status_changed, #p2p_transfer_StatusChange{
    status = Status
}}) ->
    ChangeType = #{ <<"changeType">> => <<"P2PTransferStatusChanged">>},
    TransferChange = unmarshal_transfer_status(Status),
    maps:merge(ChangeType, TransferChange);
unmarshal_event_change({ui, #p2p_session_UserInteractionChange{
    id = ID,
    payload = Payload
}}) ->
    #{
        <<"changeType">> => <<"P2PTransferInteractionChanged">>,
        <<"userInteractionID">> => unmarshal(id, ID),
        <<"userInteractionChange">> => unmarshal_user_interaction_change(Payload)
    }.

unmarshal_user_interaction_change({created, #p2p_session_UserInteractionCreatedChange{
    ui = #p2p_session_UserInteraction{user_interaction = UserInteraction}
}}) ->
    #{
        <<"changeType">> => <<"UserInteractionCreated">>,
        <<"userInteraction">> => unmarshal_user_interaction(UserInteraction)
    };
unmarshal_user_interaction_change({status_changed, #p2p_session_UserInteractionStatusChange{
    status = {finished, _} % other statuses are skipped
}}) ->
    #{
        <<"changeType">> => <<"UserInteractionFinished">>
    }.

unmarshal_user_interaction({redirect, Redirect}) ->
    #{
        <<"interactionType">> => <<"Redirect">>,
        <<"request">> => unmarshal_request(Redirect)
    }.

unmarshal_request({get_request, #ui_BrowserGetRequest{
    uri = URI
}}) ->
    #{
        <<"requestType">> => <<"BrowserGetRequest">>,
        <<"uriTemplate">> => unmarshal(string, URI)
    };
unmarshal_request({post_request, #ui_BrowserPostRequest{
    uri = URI,
    form = Form
}}) ->
    #{
        <<"requestType">> => <<"BrowserPostRequest">>,
        <<"uriTemplate">> => unmarshal(string, URI),
        <<"form">> => unmarshal_form(Form)
    }.

unmarshal_form(Form) ->
    maps:fold(
        fun (Key, Template, AccIn) ->
            FormField = #{
                <<"key">> => unmarshal(string, Key),
                <<"template">> => unmarshal(string, Template)
            },
            [FormField | AccIn]
        end,
        [], Form
    ).

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_T, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec unmarshal_events_test_() ->
    _.
unmarshal_events_test_() ->

    Form = fun() -> {fun unmarshal_form/1,
        #{ <<"arg1">> => <<"value1">>, <<"arg2">> => <<"value2">> },
        [
            #{ <<"key">> => <<"arg2">>, <<"template">> => <<"value2">>},
            #{ <<"key">> => <<"arg1">>, <<"template">> => <<"value1">>}
        ]
    } end,

    Request = fun
        ({_, Woody, Swag}) -> {fun unmarshal_request/1,
            {post_request, #ui_BrowserPostRequest{
                uri = <<"uri://post">>,
                form = Woody
            }},
            #{
                <<"requestType">> => <<"BrowserPostRequest">>,
                <<"uriTemplate">> => <<"uri://post">>,
                <<"form">> => Swag
            }
        };
        (get_request) -> {fun unmarshal_request/1,
            {get_request, #ui_BrowserGetRequest{
                uri = <<"uri://get">>
            }},
            #{
                <<"requestType">> => <<"BrowserGetRequest">>,
                <<"uriTemplate">> => <<"uri://get">>
            }
        }
     end,

    UIRedirect =  fun({_, Woody, Swag}) -> {fun unmarshal_user_interaction/1,
        {redirect, Woody},
        #{
            <<"interactionType">> => <<"Redirect">>,
            <<"request">> => Swag
        }
    } end,

    UIChangePayload =  fun
        ({_, Woody, Swag}) -> {fun unmarshal_user_interaction_change/1,
            {created, #p2p_session_UserInteractionCreatedChange{
                    ui = #p2p_session_UserInteraction{
                        id = <<"id://p2p_session/ui">>,
                        user_interaction = Woody
                    }
            }},
            #{
                <<"changeType">> => <<"UserInteractionCreated">>,
                <<"userInteraction">> => Swag
            }
        };
        (ui_finished) -> {fun unmarshal_user_interaction_change/1,
            {status_changed, #p2p_session_UserInteractionStatusChange{
                status = {finished, #p2p_session_UserInteractionStatusFinished{}}
            }},
            #{
                <<"changeType">> => <<"UserInteractionFinished">>
            }
        }
    end,

    EventChange = fun
        ({_, Woody, Swag}) -> {fun unmarshal_event_change/1,
            {ui, #p2p_session_UserInteractionChange{
                id = <<"id://p2p_session/change">>, payload = Woody
            }},
            #{
                <<"changeType">> => <<"P2PTransferInteractionChanged">>,
                <<"userInteractionID">> => <<"id://p2p_session/change">>,
                <<"userInteractionChange">> => Swag
            }
        };
        (TransferStatus) -> {fun unmarshal_event_change/1,
            {status_changed, #p2p_transfer_StatusChange{
                status = case TransferStatus of
                    pending -> {pending, #p2p_status_Pending{}};
                    succeeded -> {succeeded, #p2p_status_Succeeded{}}
                end
            }},
            #{
                <<"changeType">> => <<"P2PTransferStatusChanged">>,
                <<"status">> => case TransferStatus of
                    pending -> <<"Pending">>;
                    succeeded -> <<"Succeeded">>
                end
            }
        }
    end,

    Event =  fun
        ({_, {ui, _} = Woody, Swag}) -> {fun unmarshal_event/1,
            #'p2p_session_Event'{
                'event' = 1,
                'occured_at' = <<"2020-05-25T12:34:56.123456Z">>,
                'change' = Woody
            },
            #{
                <<"createdAt">> => <<"2020-05-25T12:34:56.123456Z">>,
                <<"change">> => Swag
            }
        };
        ({_, {status_changed, _} = Woody, Swag}) -> {fun unmarshal_event/1,
            #'p2p_transfer_Event'{
                'event' = 1,
                'occured_at' = <<"2020-05-25T12:34:56.123456Z">>,
                'change' = Woody
            },
            #{
                <<"createdAt">> => <<"2020-05-25T12:34:56.123456Z">>,
                <<"change">> => Swag
            }
        }
    end,

    Events = fun(List) -> {fun unmarshal_events/1,
        {
            <<"token">>,
            [Woody || {_, Woody, _} <- List]
        },
        #{
            <<"continuationToken">> => <<"token">>,
            <<"result">> => [Swag || {_, _, Swag} <- List]
        }
    } end,

    EvList = [E ||
        Type <- [Form(), get_request],
        Change <- [UIChangePayload(UIRedirect(Request(Type))), pending, succeeded],
        E <- [Event(EventChange(Change))]
    ],

    [
        ?_assertEqual(ExpectedSwag, Unmarshal(Woody)) ||
        {Unmarshal, Woody, ExpectedSwag} <- [Events(EvList) | EvList]
    ].

-spec continuation_token_test_() ->
    _.
continuation_token_test_() ->
    {setup,
        fun() ->
            meck:new([wapi_utils, wapi_auth, uac_authorizer_jwt]),
            meck:expect(wapi_utils, get_unique_id, 0, <<>>),
            meck:expect(wapi_auth, get_signee, 0, <<>>),
            meck:expect(uac_authorizer_jwt, issue, fun(ID, PartyID, Token, _Sign) ->
                term_to_binary({ID, PartyID, Token})
            end),
            meck:expect(uac_authorizer_jwt, verify, fun(Token, _) ->
                {ok, binary_to_term(Token)}
            end),
            <<>>
        end,
        fun(_) ->
            meck:unload()
        end,
        fun(PartyID) ->
            [
                ?_assertEqual({ok, #{}}, continuation_token_unpack(undefined, PartyID)),
                ?_assertMatch({error, _}, continuation_token_unpack(continuation_token_pack(1, 2, PartyID), <<"ops">>))
            ]
            ++
            [
                begin
                    Coded = continuation_token_pack(TransferCursor, SessionCursor, PartyID),
                    Token = continuation_token_unpack(Coded, PartyID),
                    ?_assertEqual({ok, ExpectedToken}, Token)
                end ||
                {TransferCursor, SessionCursor, ExpectedToken} <- [
                    {2, 3, #{
                        <<"version">> => 1,
                        ?CONTINUATION_TRANSFER => 2,
                        ?CONTINUATION_SESSION => 3
                    }},
                    {2, undefined, #{
                        <<"version">> => 1,
                        ?CONTINUATION_TRANSFER => 2
                    }},
                    {undefined, 3, #{
                        <<"version">> => 1,
                        ?CONTINUATION_SESSION => 3
                    }}
                ]
            ]
        end
    }.

-spec events_collect_test_() ->
    _.
events_collect_test_() ->
    {setup,
        fun() ->
            Event = fun(ID) -> #p2p_transfer_Event{
                'event' = ID,
                'occured_at' = <<"2020-05-25T12:34:56.123456Z">>,
                'change' = {'status_changed', #'p2p_transfer_StatusChange'{
                    'status' = {'succeeded', #'p2p_status_Succeeded'{}}
                }}
            } end,
            meck:new([wapi_handler_utils], [passthrough]),
            meck:expect(wapi_handler_utils, service_call, fun
                ({case1, _, _}, _) -> {ok, []};
                ({case2, _, _}, _) -> {ok, [Event(2)]};
                ({case3, _, _}, _) -> {exception, #fistful_P2PNotFound{}};
                ({case4, _, _}, _) -> {exception, #fistful_P2PSessionNotFound{}};
                ({case5, _, [_, #'EventRange'{'after' = After}]}, _) -> {ok, [Event(After+1)]}
            end),
            {
                fun _Collect(Case, ID, EventRange, Acc, Expected) ->
                    ?_assertEqual(Expected, events_collect(Case, ID, EventRange, #{}, Acc))
                end,
                Event
            }
        end,
        fun(_) ->
            meck:unload()
        end,
        fun({Collect, Event}) ->
            [
                % no session
                Collect(p2p_session, undefined, events_range(1),  [Event(1)],
                    {ok, {[Event(1)], 1}}
                ),
                % zero range limit
                Collect(any, <<>>, events_range(1, 0),  [Event(1)],
                    {ok, {[Event(1)], 1}}
                ),
                % empty selection - keep the previous cursor
                Collect(case1, <<>>, events_range(undefined),  [Event(1)],
                    {ok, {[Event(1)], undefined}}
                ),
                % empty selection - keep the previous cursor
                Collect(case1, <<>>, events_range(1),  [Event(1)],
                    {ok, {[Event(1)], 1}}
                ),
                % non numeric range limit
                Collect(case2, <<>>, events_range(0, undefined),  [Event(1)],
                    {ok, {[Event(1), Event(2)], 2}}
                ),
                % range limit: 2
                Collect(case2, <<>>, events_range(undefined, 2),  [Event(1)],
                    {ok, {[Event(1), Event(2), Event(2)], 2}}
                ),
                % transfer not found
                Collect(case3, <<>>, events_range(2),  [Event(1)],
                    {error, {p2p_transfer, notfound}}
                ),
                % session not found - keep the previous cursor
                Collect(case4, <<>>, events_range(2),  [Event(1)],
                    {ok, {[Event(1)], 2}}
                ),
                % range limit: 3, new id production
                Collect(case5, <<>>, events_range(1, 2),  [Event(1)],
                    {ok, {[Event(1), Event(2), Event(3)], 3}}
                )
            ]
        end
    }.

-endif.
