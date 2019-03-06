-module(wapi_wallet_backend).

-export([create/2]).

-spec create(any(), any()) ->
    {ok, any()}.

create(_P, Context) ->
    lager:error("Params: ~p~n", [_P]),
    Call = {fistful_wallet, 'Create', [_P]},
    case wapi_handler_utils:service_call(Call, Context) of
        {ok, Report} ->
            lager:error(Report),
            ok;
            % to_swag(report_object, Report);
        {exception, _Details} ->
            lager:error("ERROR Details: ~p~n", [_Details]),
            throw(notfound)
    end,
    {ok, #{<<"id">> => <<"foo">>}}.