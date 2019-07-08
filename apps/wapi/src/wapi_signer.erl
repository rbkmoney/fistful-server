-module(wapi_signer).

-export([sign/1]).
-export([verify/1]).

%%internal
-type kid()        :: wapi_authorizer_jwt:kid().
-type key()        :: wapi_authorizer_jwt:key().
-type token()      :: wapi_authorizer_jwt:token().

-spec verify(token()) ->
    {ok, map()} |
    {error,
        {invalid_token,
            badarg |
            {badarg, term()} |
            {missing, atom()} |
            expired |
            {malformed_acl, term()}
        } |
        {nonexistent_key, kid()} |
        invalid_operation |
        invalid_signature
    }.

verify(Token) ->
    wapi_authorizer_jwt:verify(Token, fun verify/2).

-spec verify(key(), token()) ->
    {ok, binary()} | {error, invalid_signature}.

verify(JWK, ExpandedToken) ->
    case jose_jwk:verify(ExpandedToken, JWK) of
        {true, Content, _JWS} ->
            {ok, Content};
        {false, _Content, _JWS} ->
            {error, invalid_signature}
    end.

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
