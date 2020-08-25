-module(wapi_withdrawal_backend).

-define(DOMAIN, <<"wallet-api">>).

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().
-type id() :: binary().
-type external_id() :: binary().

-export([create/2]).
-export([get/2]).
-export([get_by_external_id/2]).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").

%% Pipeline

-spec create(req_data(), handler_context()) ->
    {ok, response_data()} | {error, WithdrawalError}
    when WithdrawalError ::
        {destination, notfound}       |
        {destination, unauthorized}   |
        {withdrawal, unauthorized}    |
        {external_id_conflict, id(), external_id()} |
        {provider, notfound}          |
        {wallet, {inaccessible, _}}   |
        {wallet, {currency, invalid}} |
        {wallet, {provider, invalid}} |
        {quote_invalid_party, _}      |
        {quote_invalid_wallet, _}     |
        {quote, {invalid_body, _}}    |
        {quote, {invalid_destination, _}} |
        {terms, {terms_violation, _}} |
        {destination_resource, {bin_data, ff_bin_data:bin_data_error()}} |
        {wallet | destination, {unauthorized, _}}.

create(Params, HandlerContext) ->
    case check_withdrawal_params(Params, HandlerContext) of
        {ok, ID, Quote} ->
            Context = wapi_backend_utils:make_ctx(Params, HandlerContext),
            WithdrawalContext = marshal(context, Context),
            WithdrawalParams = marshal(withdrawal_params, Params#{
                <<"id">> => ID,
                <<"quote">> => Quote
            }),
            create(WithdrawalParams, WithdrawalContext, HandlerContext);
        {error, _} = Error ->
            Error
    end.

create(Params, Context, HandlerContext) ->
    Request = {fistful_withdrawal, 'Create', [Params, Context]},
    case service_call(Request, HandlerContext) of
        {ok, Withdrawal} ->
            {ok, unmarshal(withdrawal, Withdrawal)};
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
    {error, {withdrawal, notfound}} |
    {error, {withdrawal, unauthorized}}.

get(WithdrawalID, HandlerContext) ->
    Request = {fistful_withdrawal, 'Get', [WithdrawalID, #'EventRange'{}]},
    case service_call(Request, HandlerContext) of
        {ok, WithdrawalThrift} ->
            case wapi_access_backend:check_resource(withdrawal, WithdrawalThrift, HandlerContext) of
                ok ->
                    {ok, unmarshal(withdrawal, WithdrawalThrift)};
                {error, unauthorized} ->
                    {error, {withdrawal, unauthorized}}
            end;
        {exception, #fistful_WithdrawalNotFound{}} ->
            {error, {withdrawal, notfound}}
    end.

-spec get_by_external_id(external_id(), handler_context()) ->
    {ok, response_data()} |
    {error, {withdrawal, notfound}} |
    {error, {withdrawal, unauthorized}} |
    {error, {external_id, {unknown_external_id, external_id()}}}.

get_by_external_id(ExternalID, HandlerContext = #{woody_context := WoodyContext}) ->
    PartyID = wapi_handler_utils:get_owner(HandlerContext),
    IdempotentKey = wapi_backend_utils:get_idempotent_key(withdrawal, PartyID, ExternalID),
    case bender_client:get_internal_id(IdempotentKey, WoodyContext) of
        {ok, {WithdrawalID, _}, _CtxData} ->
            get(WithdrawalID, HandlerContext);
        {error, internal_id_not_found} ->
            {error, {external_id, {unknown_external_id, ExternalID}}}
    end.

%%
%% Internal
%%

service_call(Params, Context) ->
    wapi_handler_utils:service_call(Params, Context).

%% Validators

check_withdrawal_params(Params, HandlerContext) ->
    case authorize_withdrawal(Params, HandlerContext) of
        ok ->
            case maybe_check_quote_token(Params, HandlerContext) of
                {ok, Quote} ->
                    case wapi_backend_utils:gen_id(withdrawal, Params, HandlerContext) of
                        {ok, ID} ->
                            {ok, ID, Quote};
                        {error, {external_id_conflict, _}} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

authorize_withdrawal(Params, HandlerContext) ->
    case authorize_resource(wallet, Params, HandlerContext) of
        ok ->
            case authorize_resource(destination, Params, HandlerContext) of
                ok ->
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

authorize_resource(Resource, Params, HandlerContext) ->
    case authorize_resource_by_grant(Resource, Params) of
        ok ->
            ok;
        {error, missing} ->
            authorize_resource_by_bearer(Resource, maps:get(genlib:to_binary(Resource), Params), HandlerContext)
    end.

authorize_resource_by_bearer(Resource, ResourceID, HandlerContext) ->
    case wapi_access_backend:check_resource_by_id(Resource, ResourceID, HandlerContext) of
        ok ->
            ok;
        {error, unauthorized} ->
            {error, {Resource, unauthorized}}
    end.

authorize_resource_by_grant(R = destination, #{
    <<"destination">>      := ID,
    <<"destinationGrant">> := Grant
}) ->
    authorize_resource_by_grant(R, Grant, get_resource_accesses(R, ID, write), undefined);
authorize_resource_by_grant(R = wallet, #{
    <<"wallet">>      := ID,
    <<"walletGrant">> := Grant,
    <<"body">>        := WithdrawalBody
}) ->
    authorize_resource_by_grant(R, Grant, get_resource_accesses(R, ID, write), WithdrawalBody);
authorize_resource_by_grant(_, _) ->
    {error, missing}.

authorize_resource_by_grant(Resource, Grant, Access, Params) ->
    case uac_authorizer_jwt:verify(Grant, #{}) of
        {ok, {_, _, Claims}} ->
            case verify_access(Access, Claims) of
                ok ->
                    case verify_claims(Resource, Claims, Params) of
                        ok ->
                            ok;
                        {error, Error} ->
                            {error, {Resource, Error}}
                    end;
                {error, Error} ->
                    {error, {Resource, Error}}
            end;
        {error, Error} ->
            {error, {Resource, Error}}
    end.

get_resource_accesses(Resource, ID, Permission) ->
    [{get_resource_accesses(Resource, ID), Permission}].

get_resource_accesses(destination, ID) ->
    [party, {destinations, ID}];
get_resource_accesses(wallet, ID) ->
    [party, {wallets, ID}].

verify_access(Access, #{<<"resource_access">> := #{?DOMAIN := ACL}}) ->
    do_verify_access(Access, ACL);
verify_access(Access, #{<<"resource_access">> := #{<<"common-api">> := ACL}}) -> % Legacy grants support
    do_verify_access(Access, ACL);
verify_access(_, _) ->
    {error, {unauthorized, {grant, insufficient_access}}}.

do_verify_access(Access, ACL) ->
    case lists:all(
        fun ({Scope, Permission}) -> lists:member(Permission, uac_acl:match(Scope, ACL)) end,
        Access
    ) of
        true  -> ok;
        false -> {error, {unauthorized, {grant, insufficient_access}}}
    end.

verify_claims(destination, _Claims, _) ->
    ok;
verify_claims(wallet,
    #{<<"amount">> := GrantAmount, <<"currency">> := Currency},
    #{<<"amount">> := ReqAmount,   <<"currency">> := Currency}
) when GrantAmount >= ReqAmount ->
    ok;
verify_claims(_, _, _) ->
    {error, {unauthorized, {grant, insufficient_claims}}}.

maybe_check_quote_token(Params = #{<<"quoteToken">> := QuoteToken}, Context) ->
    {ok, {_, _, Data}} = uac_authorizer_jwt:verify(QuoteToken, #{}),
    {ok, Quote, WalletID, DestinationID, PartyID} = wapi_withdrawal_quote:decode_token_payload(Data),

    case valid(PartyID, wapi_handler_utils:get_owner(Context)) of
        ok ->
            case valid(WalletID, maps:get(<<"wallet">>, Params)) of
                ok ->
                    case check_quote_withdrawal(DestinationID, maps:get(<<"withdrawal">>, Params)) of
                        ok ->
                            case check_quote_body(
                                maps:get(cash_from, Quote),
                                marshal_quote_body(maps:get(<<"body">>, Params))
                            ) of
                                ok ->
                                    {ok, Quote};
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, Error} ->
                    {error, {quote_invalid_wallet, Error}}
            end;
        {error, Error} ->
            {error, {quote_invalid_party, Error}}
    end;
maybe_check_quote_token(_Params, _Context) ->
    {ok, undefined}.

valid(V, V) ->
    ok;
valid(_, V) ->
    {error, V}.

check_quote_body(CashFrom, CashFrom) ->
    ok;
check_quote_body(_, CashFrom) ->
    {error, {quote, {invalid_body, CashFrom}}}.

check_quote_withdrawal(undefined, _DestinationID) ->
    ok;
check_quote_withdrawal(DestinationID, DestinationID) ->
    ok;
check_quote_withdrawal(_, DestinationID) ->
    {error, {quote, {invalid_withdrawal, DestinationID}}}.

marshal_quote_body(Body) ->
    {genlib:to_int(maps:get(<<"amount">>, Body)), maps:get(<<"currency">>, Body)}.

%% Marshaling

marshal(withdrawal_params, Params = #{
    <<"id">> := ID,
    <<"wallet">> := WalletID,
    <<"destination">> := DestinationID,
    <<"body">> := Body
}) ->
    ExternalID = maps:get(<<"externalID">>, Params, undefined),
    Metadata = maps:get(<<"metadata">>, Params, undefined),
    Quote = maps:get(<<"quote">>, Params, undefined),
    #wthd_WithdrawalParams{
        id = marshal(id, ID),
        wallet_id = marshal(id, WalletID),
        destination_id = marshal(id, DestinationID),
        body = marshal_body(Body),
        quote = Quote,
        external_id = maybe_marshal(id, ExternalID),
        metadata = maybe_marshal(context, Metadata)
    };

marshal(context, Context) ->
    ff_codec:marshal(context, Context);

marshal(T, V) ->
    ff_codec:marshal(T, V).

maybe_marshal(_, undefined) ->
    undefined;
maybe_marshal(T, V) ->
    marshal(T, V).

marshal_body(Body) ->
    #'Cash'{
        amount   = genlib:to_int(maps:get(<<"amount">>, Body)),
        currency = #'CurrencyRef'{
            symbolic_code = maps:get(<<"currency">>, Body)
        }
    }.

unmarshal(withdrawal, #wthd_WithdrawalState{
    id = ID,
    wallet_id = WalletID,
    destination_id = DestinationID,
    body = Body,
    external_id = ExternalID,
    status = Status,
    created_at = CreatedAt,
    metadata = Metadata
}) ->
    UnmarshaledMetadata = maybe_unmarshal(context, Metadata),
    genlib_map:compact(maps:merge(#{
        <<"id">> => ID,
        <<"wallet">> => WalletID,
        <<"destination">> => DestinationID,
        <<"body">> => unmarshal_body(Body),
        <<"createdAt">> => CreatedAt,
        <<"externalID">> => ExternalID,
        <<"metadata">> => UnmarshaledMetadata
    }, unmarshal_status(Status)));

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).

unmarshal_body(#'Cash'{
    amount   = Amount,
    currency = #'CurrencyRef'{
        symbolic_code = Currency
    }
}) ->
    #{
        <<"amount">> => Amount,
        <<"currency">> => Currency
    }.

unmarshal_status({pending, _}) ->
    #{<<"status">> => <<"Pending">>};
unmarshal_status({succeeded, _}) ->
    #{<<"status">> => <<"Succeeded">>};
unmarshal_status({failed, #wthd_status_Failed{failure = #'Failure'{code = Code, sub = Sub}}}) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => genlib_map:compact(#{
            <<"code">> => Code,
            <<"subError">> => unmarshal_subfailure(Sub)
        })
    }.

unmarshal_subfailure(undefined) ->
    undefined;

unmarshal_subfailure(#'SubFailure'{code = Code, sub = Sub}) ->
    genlib_map:compact(#{
        <<"code">> => Code,
        <<"subError">> => unmarshal_subfailure(Sub)
    }).
