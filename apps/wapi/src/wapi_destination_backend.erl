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
        invalid_resource_token      |
        {identity, unauthorized}    |
        {identity, notfound}        |
        {currency, notfound}        |
        {inaccessible, _}           |
        {external_id_conflict, {id(), external_id()}}.

create(Params = #{<<"identity">> := IdentityID}, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(identity, IdentityID, HandlerContext) of
        ok ->
            case wapi_backend_utils:gen_id(destination, Params, HandlerContext) of
                {ok, ID} ->
                    Context = wapi_backend_utils:make_ctx(Params, HandlerContext),
                    PreparedParams = genlib_map:compact(Params#{
                        <<"id">> => ID,
                        <<"context">> => Context
                    }),
                    create(ID, PreparedParams, HandlerContext);
                {error, {external_id_conflict, ID}} ->
                    ExternalID = maps:get(<<"externalID">>, Params, undefined),
                    {error, {external_id_conflict, {ID, ExternalID}}}
            end;
        {error, unauthorized} ->
            {error, {identity, unauthorized}}
    end.

create(DestinationID, Params = #{<<"resource">> := Resource}, HandlerContext) ->
    case construct_resource(Resource) of
        {ok, ConstructedResource} ->
            DestinationParams = marshal(destination_params, Params#{<<"resource">> => ConstructedResource}),
            Request = {fistful_destination, 'Create', [DestinationID, DestinationParams]},
            case service_call(Request, HandlerContext) of
                {ok, Destination} ->
                    {ok, unmarshal(destination, Destination)};
                {exception, #fistful_IdentityNotFound{}} ->
                    {error, {identity, notfound}};
                {exception, #fistful_CurrencyNotFound{}} ->
                    {error, {currency, notfound}};
                {exception, #fistful_PartyInaccessible{}} ->
                    {error, inaccessible};
                {exception, #fistful_IDExists{}} ->
                    get(DestinationID, HandlerContext);
                {exception, Details} ->
                    {error, Details}
            end;
        {error, invalid_resource_token} = Error ->
            Error
    end.

-spec get(id(), handler_context()) ->
    {ok, response_data()} |
    {error, {destination, notfound}} |
    {error, {destination, unauthorized}}.

get(DestinationID, HandlerContext) ->
    Request = {fistful_destination, 'Get', [DestinationID]},
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
        {ok, DestinationID, _CtxData} ->
            get(DestinationID, HandlerContext);
        {error, internal_id_not_found} ->
            {error, {external_id, {unknown_external_id, ExternalID}}}
    end.

%%
%% Internal
%%

construct_resource(#{<<"type">> := Type, <<"token">> := Token} = Resource)
when Type =:= <<"BankCardDestinationResource">> ->
    case wapi_crypto:decrypt_bankcard_token(Token) of
        unrecognized ->
            {ok, marshal(resource, Resource)};
        {ok, BankCard} ->
            #'BankCardExpDate'{
                month = Month,
                year = Year
            } = BankCard#'BankCard'.exp_date,
            CostructedResource = {bank_card, #{
                token => BankCard#'BankCard'.token,
                bin => BankCard#'BankCard'.bin,
                masked_pan => BankCard#'BankCard'.masked_pan,
                cardholder_name => BankCard#'BankCard'.cardholder_name,
                exp_date => {Month, Year}
            }},
            {ok, ff_codec:marshal(resource, CostructedResource)};
        {error, {decryption_failed, _} = Error} ->
            logger:warning("Resource token decryption failed: ~p", [Error]),
            {error, invalid_resource_token}
    end;
construct_resource(#{<<"type">> := Type} = Resource)
when Type =:= <<"CryptoWalletDestinationResource">> ->
    #{
        <<"id">> := CryptoWalletID,
        <<"currency">> := CryptoCurrency
    } = Resource,
    Tag = maps:get(<<"tag">>, Resource, undefined),
    CostructedResource = {crypto_wallet, genlib_map:compact(#{
        id => CryptoWalletID,
        currency => marshal(crypto_currency, CryptoCurrency),
        tag => marshal(string, Tag)
    })},
    {ok, ff_codec:marshal(resource, CostructedResource)}.

service_call(Params, Context) ->
    wapi_handler_utils:service_call(Params, Context).

%% Marshaling

marshal(destination_params, Params = #{
    <<"identity">> := IdentityID,
    <<"currency">> := CurrencyID,
    <<"name">> := Name,
    <<"resource">> := Resource,
    <<"context">> := Context
}) ->
    ExternalID = maps:get(<<"externalID">>, Params, undefined),
    #dst_DestinationParams{
        identity = marshal(id, IdentityID),
        name = marshal(string, Name),
        currency = marshal(string, CurrencyID),
        resource = Resource,
        external_id = maybe_marshal(id, ExternalID),
        context = marshal(context, Context)
    };

marshal(resource, #{
    <<"type">> := <<"BankCardDestinationResource">>,
    <<"token">> := Token
}) ->
    BankCard = wapi_utils:base64url_to_map(Token),
    Resource = {bank_card, #{
        token => maps:get(<<"token">>, BankCard),
        payment_system => erlang:binary_to_existing_atom(maps:get(<<"paymentSystem">>, BankCard), latin1),
        bin => maps:get(<<"bin">>, BankCard),
        masked_pan => maps:get(<<"lastDigits">>, BankCard)
    }},
    ff_codec:marshal(resource, Resource);

marshal(crypto_currency, <<"Bitcoin">>) -> bitcoin;
marshal(crypto_currency, <<"Litecoin">>) -> litecoin;
marshal(crypto_currency, <<"BitcoinCash">>) -> bitcoin_cash;
marshal(crypto_currency, <<"Ripple">>) -> ripple;
marshal(crypto_currency, <<"Ethereum">>) -> ethereum;
marshal(crypto_currency, <<"Zcash">>) -> zcash;

marshal(context, Context) ->
    ff_codec:marshal(context, Context);

marshal(T, V) ->
    ff_codec:marshal(T, V).

maybe_marshal(_, undefined) ->
    undefined;
maybe_marshal(T, V) ->
    marshal(T, V).

unmarshal(destination, #dst_Destination{
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

unmarshal(resource, {bank_card, #'BankCard'{
    token = Token,
    bin = Bin,
    masked_pan = MaskedPan
}}) ->
    genlib_map:compact(#{
        <<"type">> => <<"BankCardDestinationResource">>,
        <<"token">> => unmarshal(string, Token),
        <<"bin">> => unmarshal(string, Bin),
        <<"lastDigits">> => wapi_utils:get_last_pan_digits(MaskedPan)
    });
unmarshal(resource, {crypto_wallet, #'CryptoWallet'{
    id = CryptoWalletID,
    currency = CryptoWalletCurrency,
    data = Data
}}) ->
    genlib_map:compact(#{
        <<"type">> => <<"CryptoWalletDestinationResource">>,
        <<"id">> => unmarshal(string, CryptoWalletID),
        <<"currency">> => unmarshal(crypto_currency, CryptoWalletCurrency),
        <<"tag">> => maybe_unmarshal(crypto_data, Data)
    });

unmarshal(crypto_data, {ripple, #'CryptoDataRipple'{tag = Tag}}) ->
    unmarshal(string, Tag);
unmarshal(crypto_data, _) ->
    undefined;

unmarshal(crypto_currency, bitcoin) -> <<"Bitcoin">>;
unmarshal(crypto_currency, litecoin) -> <<"Litecoin">>;
unmarshal(crypto_currency, bitcoin_cash) -> <<"BitcoinCash">>;
unmarshal(crypto_currency, ripple) -> <<"Ripple">>;
unmarshal(crypto_currency, ethereum) -> <<"Ethereum">>;
unmarshal(crypto_currency, zcash) -> <<"Zcash">>;

unmarshal(context, Context) ->
    ff_codec:unmarshal(context, Context);

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).
