-module(wapi_resource).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

-export([decode_swag/1]).
-export([decode_token/1]).
-export([create_hash/1]).

-type swag_term() ::
    #{binary() => swag_term()}
    | [swag_term()]
    | number()
    | binary()
    | boolean().

-spec decode_swag(swag_term()) ->
    {ok, wapi_crypto:resource()}
    | {error, {invalid_resource_token, binary()}}.
decode_swag(#{<<"token">> := Token, <<"type">> := Type}) ->
    case decode_token(Token) of
        {ok, Resource} ->
            {ok, Resource};
        {error, Error} ->
            logger:warning("~p token decryption failed: ~p", [Type, Error]),
            {error, {invalid_resource_token, Type}}
    end;
decode_swag(_Object) ->
    {ok, undefined}.

-spec decode_token(binary()) ->
    {ok, wapi_crypto:resource()}
    | {error, unrecognized}
    | {error, lechiffre:decoding_error()}.
decode_token(Token) ->
    case wapi_crypto:decrypt_bankcard_token(Token) of
        {ok, Resource} ->
            {ok, Resource};
        unrecognized ->
            {error, unrecognized};
        {error, Error} ->
            {error, Error}
    end.

-spec create_hash(undefined | wapi_crypto:resource()) -> undefined | integer().
create_hash(undefined) ->
    undefined;
create_hash(#'BankCard'{} = BankCard) ->
    Map = genlib_map:compact(#{
        token => BankCard#'BankCard'.token,
        bin => BankCard#'BankCard'.bin,
        masked_pan => BankCard#'BankCard'.masked_pan,
        cardholder_name => BankCard#'BankCard'.cardholder_name,
        %% ExpDate is optional in swag_wallets 'StoreBankCard'. But some adapters waiting exp_date.
        %% Add error, somethink like BankCardReject.exp_date_required
        exp_date =>
            case BankCard#'BankCard'.exp_date of
                undefined -> undefined;
                #'BankCardExpDate'{month = Month, year = Year} -> {Month, Year}
            end
    }),
    wapi_backend_utils:create_params_hash(Map);
create_hash(Value) ->
    wapi_backend_utils:create_params_hash(Value).
