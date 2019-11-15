-module(wapi_signer).

-export([sign/1]).

%%internal
-type token()      :: uac_authorizer_jwt:token().


-spec sign(binary()) ->
    {ok, token()} |
    {error, nonexistent_signee}.

sign(Plain) ->
    case wapi_authorizer_jwt:get_signee_key() of
        #{kid := KID, jwk := JWK, signer := #{} = JWS} ->
            Signed = jose_jwk:sign(Plain, JWS#{<<"kid">> => KID}, JWK),
            {_Modules, Token} = jose_jws:compact(Signed),
            {ok, Token};
        undefined ->
            {error, nonexistent_signee}
    end.
