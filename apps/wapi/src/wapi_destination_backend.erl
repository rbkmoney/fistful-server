-module(wapi_destination_backend).

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().
-type id() :: binary().
-type external_id() :: binary().

-export([create/2]).
-export([get/2]).
-export([get_by_external_id/2]).

-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").

%% Pipeline

-spec create(req_data(), handler_context()) ->
    {ok, response_data()} | {error, DestinationError}
    when DestinationError ::
        {invalid_resource_token, binary()} |
        {identity, unauthorized}    |
        {identity, notfound}        |
        {currency, notfound}        |
        inaccessible                |
        {external_id_conflict, {id(), external_id()}}.

create(Params = #{<<"identity">> := IdentityID}, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            create_decode_resources(Params, HandlerContext);
        {error, unauthorized} ->
            {error, {identity, unauthorized}}
    end.

create_decode_resources(Params, HandlerContext) ->
    case wapi_backend_utils:decode_resource(maps:get(<<"resource">>, Params)) of
        {ok, Resource} ->
            % OldParams is need for create_generate_id_legacy
            % Remove the parameter & create_generate_id_legacy after deploy
            NewParams = Params#{<<"resource">> => Resource},
            create_generate_id(NewParams, Params, HandlerContext);
        {error, {Type, Error}} ->
            logger:warning("~p token decryption failed: ~p", [Type, Error]),
            {error, {invalid_resource_token, Type}}
    end.

create_generate_id(Params, OldParams, HandlerContext) ->
    case wapi_backend_utils:gen_id(destination, Params, HandlerContext) of
        {ok, ID} ->
            create_request(Params#{<<"id">> => ID}, HandlerContext);
        {error, {external_id_conflict, ID}} ->
            % Replace this call by error report after deploy
            ExternalID = maps:get(<<"externalID">>, Params, undefined),
            logger:warning("external_id_conflict: ~p. try old hashing", [{ID, ExternalID}]),
            create_generate_id_legacy(Params, OldParams, HandlerContext)
    end.

create_generate_id_legacy(Params, OldParams, HandlerContext) ->
    case wapi_backend_utils:gen_id(destination, OldParams, HandlerContext) of
        {ok, ID} ->
            create_request(Params#{<<"id">> => ID}, HandlerContext);
        {error, {external_id_conflict, ID}} ->
            ExternalID = maps:get(<<"externalID">>, Params, undefined),
            {error, {external_id_conflict, {ID, ExternalID}}}
    end.

create_request(Params, HandlerContext) ->
    Context = wapi_backend_utils:make_ctx(Params, HandlerContext),
    MarshaledParams = marshal(destination_params, Params),
    Request = {fistful_destination, 'Create', [MarshaledParams, marshal(context, Context)]},
    case service_call(Request, HandlerContext) of
        {ok, Destination} ->
            {ok, unmarshal(destination, Destination)};
        {exception, #fistful_IdentityNotFound{}} ->
            {error, {identity, notfound}};
        {exception, #fistful_CurrencyNotFound{}} ->
            {error, {currency, notfound}};
        {exception, #fistful_PartyInaccessible{}} ->
            {error, inaccessible};
        {exception, Details} ->
            {error, Details}
    end.

-spec get(id(), handler_context()) ->
    {ok, response_data()} |
    {error, {destination, notfound}} |
    {error, {destination, unauthorized}}.

get(DestinationID, HandlerContext) ->
    Request = {fistful_destination, 'Get', [DestinationID, #'EventRange'{}]},
    case service_call(Request, HandlerContext) of
        {ok, DestinationThrift} ->
            case wapi_access_backend:check_resource(destination, DestinationThrift, HandlerContext) of
                ok ->
                    {ok, unmarshal(destination, DestinationThrift)};
                {error, unauthorized} ->
                    {error, {destination, unauthorized}}
            end;
        {exception, #fistful_DestinationNotFound{}} ->
            {error, {destination, notfound}}
    end.

-spec get_by_external_id(external_id(), handler_context()) ->
    {ok, response_data()} |
    {error, {destination, notfound}} |
    {error, {destination, unauthorized}} |
    {error, {external_id, {unknown_external_id, external_id()}}}.

get_by_external_id(ExternalID, HandlerContext = #{woody_context := WoodyContext}) ->
    PartyID = wapi_handler_utils:get_owner(HandlerContext),
    IdempotentKey = wapi_backend_utils:get_idempotent_key(destination, PartyID, ExternalID),
    case bender_client:get_internal_id(IdempotentKey, WoodyContext) of
        {ok, {DestinationID, _}, _CtxData} ->
            get(DestinationID, HandlerContext);
        {error, internal_id_not_found} ->
            {error, {external_id, {unknown_external_id, ExternalID}}}
    end.

%%
%% Internal
%%

service_call(Params, Context) ->
    wapi_handler_utils:service_call(Params, Context).

%% Marshaling

marshal(destination_params, Params = #{
    <<"id">> := ID,
    <<"identity">> := IdentityID,
    <<"currency">> := CurrencyID,
    <<"name">> := Name,
    <<"resource">> := Resource
}) ->
    ExternalID = maps:get(<<"externalID">>, Params, undefined),
    #dst_DestinationParams{
        id = marshal(id, ID),
        identity = marshal(id, IdentityID),
        name = marshal(string, Name),
        currency = marshal(string, CurrencyID),
        external_id = maybe_marshal(id, ExternalID),
        resource = marshal(resource, Resource)
    };

marshal(resource, #{
    <<"type">> := <<"BankCardDestinationResource">>,
    <<"decryptedResource">> := BankCard
}) ->
    {bank_card, #'ResourceBankCard'{bank_card = BankCard}};
marshal(resource, #{
    <<"type">> := <<"CryptoWalletDestinationResource">>,
    <<"id">> := CryptoWalletID
} = Resource) ->
    CostructedResource = {crypto_wallet, #{crypto_wallet => genlib_map:compact(#{
        id => CryptoWalletID,
        currency => marshal_crypto_currency_data(Resource)
    })}},
    ff_codec:marshal(resource, CostructedResource);

marshal(context, Context) ->
    ff_codec:marshal(context, Context);

marshal(T, V) ->
    ff_codec:marshal(T, V).

maybe_marshal(_, undefined) ->
    undefined;
maybe_marshal(T, V) ->
    marshal(T, V).

unmarshal(destination, #dst_DestinationState{
    id = DestinationID,
    name = Name,
    account = Account,
    external_id = ExternalID,
    created_at = CreatedAt,
    resource = Resource,
    status = Status,
    blocking = Blocking,
    context = Context
}) ->
    #{
        identity := Identity,
        currency := Currency
    } = unmarshal(account, Account),
    UnmarshaledContext = unmarshal(context, Context),
    genlib_map:compact(#{
        <<"id">> => unmarshal(id, DestinationID),
        <<"name">> => unmarshal(string, Name),
        <<"status">> => unmarshal(status, Status),
        <<"isBlocked">> => maybe_unmarshal(blocking, Blocking),
        <<"identity">> => Identity,
        <<"currency">> => Currency,
        <<"createdAt">> => CreatedAt,
        <<"resource">> => unmarshal(resource, Resource),
        <<"externalID">> => maybe_unmarshal(id, ExternalID),
        <<"metadata">> => wapi_backend_utils:get_from_ctx(<<"metadata">>, UnmarshaledContext)
    });

unmarshal(blocking, unblocked) ->
    false;
unmarshal(blocking, blocked) ->
    true;

unmarshal(status, {authorized, #dst_Authorized{}}) ->
    <<"Authorized">>;
unmarshal(status, {unauthorized, #dst_Unauthorized{}}) ->
    <<"Unauthorized">>;

unmarshal(resource, {bank_card, #'ResourceBankCard'{bank_card = #'BankCard'{
    token = Token,
    bin = Bin,
    masked_pan = MaskedPan
}}}) ->
    genlib_map:compact(#{
        <<"type">> => <<"BankCardDestinationResource">>,
        <<"token">> => unmarshal(string, Token),
        <<"bin">> => unmarshal(string, Bin),
        <<"lastDigits">> => wapi_utils:get_last_pan_digits(MaskedPan)
    });
unmarshal(resource, {crypto_wallet, #'ResourceCryptoWallet'{crypto_wallet = #'CryptoWallet'{
    id = CryptoWalletID,
    data = Data
}}}) ->
    {Currency, Params} = unmarshal_crypto_currency_data(Data),
    genlib_map:compact(#{
        <<"type">> => <<"CryptoWalletDestinationResource">>,
        <<"id">> => unmarshal(string, CryptoWalletID),
        <<"currency">> => Currency,
        <<"tag">> => genlib_map:get(tag, Params)
    });

unmarshal(context, Context) ->
    ff_codec:unmarshal(context, Context);

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).

marshal_crypto_currency_data(Resource) ->
    #{
        <<"currency">> := CryptoCurrencyName
    } = Resource,
    Name = marshal_crypto_currency_name(CryptoCurrencyName),
    Params = marshal_crypto_currency_params(Name, Resource),
    {Name, Params}.

unmarshal_crypto_currency_data({Name, Params}) ->
    {unmarshal_crypto_currency_name(Name), unmarshal_crypto_currency_params(Name, Params)}.

marshal_crypto_currency_name(<<"Bitcoin">>) -> bitcoin;
marshal_crypto_currency_name(<<"Litecoin">>) -> litecoin;
marshal_crypto_currency_name(<<"BitcoinCash">>) -> bitcoin_cash;
marshal_crypto_currency_name(<<"Ripple">>) -> ripple;
marshal_crypto_currency_name(<<"Ethereum">>) -> ethereum;
marshal_crypto_currency_name(<<"USDT">>) -> usdt;
marshal_crypto_currency_name(<<"Zcash">>) -> zcash.

unmarshal_crypto_currency_name(bitcoin) -> <<"Bitcoin">>;
unmarshal_crypto_currency_name(litecoin) -> <<"Litecoin">>;
unmarshal_crypto_currency_name(bitcoin_cash) -> <<"BitcoinCash">>;
unmarshal_crypto_currency_name(ripple) -> <<"Ripple">>;
unmarshal_crypto_currency_name(ethereum) -> <<"Ethereum">>;
unmarshal_crypto_currency_name(usdt) -> <<"USDT">>;
unmarshal_crypto_currency_name(zcash) -> <<"Zcash">>.

marshal_crypto_currency_params(ripple, Resource) ->
    Tag = maps:get(<<"tag">>, Resource, undefined),
    #{
        tag => maybe_marshal(string, Tag)
    };
marshal_crypto_currency_params(_Other, _Resource) ->
    #{}.

unmarshal_crypto_currency_params(ripple, #'CryptoDataRipple'{tag = Tag}) ->
    genlib_map:compact(#{
        tag => maybe_unmarshal(string, Tag)
    });
unmarshal_crypto_currency_params(_Other, _Params) ->
    #{}.
