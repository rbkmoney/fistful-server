-module(wapi_p2p_template_backend).

-export([create/2]).
-export([get/2]).
-export([block/2]).
-export([issue_access_token/3]).
-export([issue_transfer_ticket/3]).

-include_lib("fistful_proto/include/ff_proto_p2p_template_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().
-type id() :: binary().

%% P2PTemplate interface

-spec create(req_data(), handler_context()) ->
    {ok, response_data()} |
    {error, {external_id_conflict, id()}} |
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
                    create(ID, Params, Context, HandlerContext);
                {error, {external_id_conflict, _}} = Error ->
                        Error
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}};
        {error, notfound} ->
            {error, {identity, notfound}}
    end.

create(ID, Params, Context, HandlerContext) ->
    TemplateParams = marshal_template_params(Params#{<<"id">> => ID}),
    Request = {fistful_p2p_template, 'Create', [TemplateParams, marshal(context, Context)]},
    case wapi_handler_utils:service_call(Request, HandlerContext) of
        {ok, TemplateState} ->
            {ok, unmarshal_template_state(TemplateState)};
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
        {ok, TemplateState} ->
            case wapi_access_backend:check_resource(p2p_template, TemplateState, HandlerContext) of
                ok ->
                    {ok, unmarshal_template_state(TemplateState)};
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

issue_transfer_ticket(ID, TicketExpiration, #{woody_context := WoodyContext} = HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(p2p_template, ID, HandlerContext) of
        ok ->
            AuthContext = wapi_handler_utils:get_auth_context(HandlerContext),
            PartyID = uac_authorizer_jwt:get_subject_id(AuthContext),

            {_, _, Claims} = AuthContext,
            AccessData = maps:get(<<"data">>, Claims),
            AccessExpiration = maps:get(<<"expiration">>, AccessData),

            AccessMs = woody_deadline:to_unixtime_ms(woody_deadline:from_binary(AccessExpiration)),
            TicketMs = woody_deadline:to_unixtime_ms(woody_deadline:from_binary(TicketExpiration)),
            NewTicketExpiration = case TicketMs > AccessMs of
                true ->
                    AccessExpiration;
                false ->
                    TicketExpiration
            end,

            %% TODO: Key = wapi_backend_utils:get_idempotent_key(ticket, PartyID, undefined),
            Key = bender_client:get_idempotent_key(<<"issue_p2p_transfer_ticket">>, ticket, PartyID, undefined),

            %% TODO: {ok, TransferID} = wapi_backend_utils:gen_id_by_type(ticket, Key, 0, HandlerContext),
            {ok, {TransferID, _}} =  bender_client:gen_snowflake(Key, 0, WoodyContext),

            case wapi_backend_utils:issue_grant_token(
                {p2p_template_transfers, ID, #{<<"transferID">> => TransferID}},
                NewTicketExpiration,
                HandlerContext
            ) of
                {ok, Token} ->  {ok, {Token, NewTicketExpiration}};
                Error ->        Error
            end;
        {error, unauthorized} ->
            {error, {p2p_template, unauthorized}};
        {error, notfound} ->
            {error, {p2p_template, notfound}}
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
        value = marshal(context, Metadata)
    }.

marshal(T, V) ->
    ff_codec:marshal(T, V).

%%

maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).

%% Convert thrift records to swag maps

unmarshal_template_state(#p2p_template_P2PTemplateState{
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
        <<"isBlocked">>     => maybe_unmarshal(blocking, Blocking),
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
    value = Cash
}) ->
    #p2p_template_Cash{
        currency = Currency,
        amount = Amount
    } = Cash,
    #{
        <<"value">> => genlib_map:compact(#{
            <<"currency">> => unmarshal(currency_ref, Currency),
            <<"amount">> => maybe_unmarshal(amount, Amount)
        })
    }.

unmarshal_template_metadata(undefined) ->
    undefined;
unmarshal_template_metadata(#p2p_template_P2PTemplateMetadata{
    value = Metadata
}) ->
    genlib_map:compact(#{
        <<"defaultMetadata">> => maybe_unmarshal(context, Metadata)
    }).

unmarshal(blocking, unblocked) ->
    false;
unmarshal(blocking, blocked) ->
    true;

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%%

maybe_unmarshal(_, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).
