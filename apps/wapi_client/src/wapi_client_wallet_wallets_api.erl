-module(wapi_client_wallet_wallets_api).

-export([create_wallet/2]).

-type context() :: wapi_client_lib:context().

-spec create_wallet(context(), map()) -> {ok, term()} | {error, term()}.
create_wallet(Context, Request) ->
    Params = #{body => Request},
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = swag_client_wallet_wallets_api:create_wallet(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).
