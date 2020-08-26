-module(wapi_p2p_template_backend).

-export([create/2]).

-include_lib("fistful_proto/include/ff_proto_p2p_template_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").


-type handler_context() :: wapi_handler:context().
-type params() :: map().
-type id() :: binary().
-type result(T, E) :: {ok, T} | {error, E}.


-spec create(params(), handler_context()) -> result(map(),
    {external_id_conflict, id()} |
    {identity, unauthorized} |
    {identity, notfound} |
    {currency, notfound} |
    inaccessible |
    invalid_operation_amount |
    _Unexpected
).
create(Params = #{<<"identityID">> := IdentityID}, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            case wapi_backend_utils:gen_id(p2p_template, Params, HandlerContext) of
                {ok, ID} ->
                    Context = wapi_backend_utils:make_ctx(Params, HandlerContext),
                    create(ID, Params, Context, HandlerContext);
                {error, {external_id_conflict, ID}} ->
                    ExternalID = maps:get(<<"externalID">>, Params, undefined),
                    {error, {external_id_conflict, {ID, ExternalID}}}
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}};
        {error, notfound} ->
                {error, {identity, notfound}}
    end.

create(ID, Params, Context, HandlerContext) ->
    TemplateParams = marshal(p2p_template_params, Params#{<<"id">> => ID}),
    Request = {fistful_p2p_template, 'Create', [TemplateParams, marshal(context, Context)]},
    case wapi_handler_utils:service_call(Request, HandlerContext) of
        {ok, TemplateState} ->
            {ok, unmarshal(p2p_template_state, TemplateState)};
        {exception, #fistful_IdentityNotFound{}} ->
            {error, {identity, notfound}};
        {exception, #fistful_CurrencyNotFound{}} ->
            {error, {currency, notfound}};
        {exception, #fistful_PartyInaccessible{}} ->
            {error, inaccessible};
        {exception, #fistful_InvalidOperationAmount{}} ->
            {error, invalid_operation_amount};
        {exception, Details} ->
            {error, Details}
    end.



marshal(p2p_template_params, Params = #{
    <<"id">> := ID,
    <<"identityID">> := IdentityID,
    <<"details">> := Details
}) ->
    #p2p_template_P2PTemplateParams{
        id = marshal(id, ID),
        identity_id  = marshal(id, IdentityID),
        template_details = marshal(p2p_template_details, Details),
        external_id = maybe_marshal(id, maps:get(<<"externalID">>, Params, undefined))
    };

marshal(p2p_template_details, Details = #{
    <<"body">> := Body
}) ->
    #p2p_template_P2PTemplateDetails{
        body = marshal(p2p_template_body, Body),
        metadata = maybe_marshal(p2p_template_metadata, maps:get(<<"metadata">>, Details, undefined))
        %description
    };

marshal(p2p_template_body, #{
    <<"value">> := Cash
}) ->
    #p2p_template_P2PTemplateBody{
        value = marshal(p2p_template_cash, Cash)
    };

marshal(p2p_template_cash, Cash  = #{
    <<"currency">> := Currency
}) ->
    #p2p_template_Cash{
        currency = marshal(currency_ref, Currency),
        amount = maybe_marshal(amount, maps:get(<<"amount">>, Cash, undefined))
    };

marshal(p2p_template_metadata, #{
    <<"defaultMetadata">> := Metadata
}) ->
    #p2p_template_P2PTemplateMetadata{
        value = marshal(context, Metadata)
    };

marshal(T, V) ->
    ff_codec:marshal(T, V).



maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).



unmarshal(p2p_template_state, #p2p_template_P2PTemplateState{
    id = ID,
    identity_id = IdentityID,
    created_at = CreatedAt,
    %domain_revision = DataRevision,
    %party_revision = PartyRevision
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
        <<"details">>       => unmarshal(p2p_template_details, Details),
        <<"externalID">>    => maybe_unmarshal(id, ExternalID)
    });

unmarshal(p2p_template_details, #p2p_template_P2PTemplateDetails{
    body = Body,
    metadata = Metadata
}) ->
    genlib_map:compact(#{
        <<"body">>      => unmarshal(p2p_template_body, Body),
        <<"metadata">>  => maybe_unmarshal(p2p_template_metadata, Metadata)
        %<<"description">>
    });

unmarshal(p2p_template_body, #p2p_template_P2PTemplateBody{
    value = Cash
}) ->
    #{
        <<"value">> => unmarshal(p2p_template_cash, Cash)
    };

unmarshal(p2p_template_cash, #p2p_template_Cash{
    currency = Currency,
    amount = Amount
}) ->
    genlib_map:compact(#{
        <<"currency">> => unmarshal(currency_ref, Currency),
        <<"amount">> => maybe_unmarshal(amount, Amount)
    });

unmarshal(p2p_template_metadata, #p2p_template_P2PTemplateMetadata{
    value = Metadata
}) ->
    genlib_map:compact(#{
        <<"defaultMetadata">> => maybe_unmarshal(context, Metadata)
    });

unmarshal(blocking, unblocked) ->
    false;
unmarshal(blocking, blocked) ->
    true;

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).



maybe_unmarshal(_, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).
