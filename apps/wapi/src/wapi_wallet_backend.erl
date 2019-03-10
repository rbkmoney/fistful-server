-module(wapi_wallet_backend).

-export([create/2]).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

-define(PARAMS_HASH, <<"params_hash">>).

-spec create(any(), any()) ->
    {ok, any()}.

create(ParamsIn, WoodyContext) ->
    Type = wallet,
    ID = wapi_backend_utils:make_id(
        Type,
        wapi_backend_utils:construct_external_id(ParamsIn, WoodyContext)
    ),

    List = [
        {<<"owner">>,    wapi_handler_utils:get_owner(WoodyContext)},
        {?PARAMS_HASH,   erlang:phash2(ParamsIn)},
        {<<"metadata">>, maps:get(<<"metadata">>, ParamsIn, undefined)}
    ],
    Context = wapi_backend_utils:extend_ctx_from_list(List, wapi_backend_utils:make_ctx()),

    WalletParams = marshal(wallet_params, ParamsIn#{
        <<"id">>      => ID,
        <<"context">> => Context
    }),

    lager:error("WAlletParams: ~n~p~n", [WalletParams]),
    Call = {fistful_wallet, 'Create', [WalletParams]},
    case wapi_handler_utils:service_call(Call, WoodyContext) of
        {ok, Wallet} ->
            Result = unmarshal(wallet, Wallet),
            lager:error("Result: ~n~p~n", [Result]),
            {ok, Result};
        {exception, _Details} ->
            lager:error("ERROR Details: ~p~n", [_Details]),
            throw(notfound)
    end.

%%
%% Internal
%%

%% Marshaling

marshal(wallet_params, Params = #{
    <<"id">>          := ID,
    <<"name">>        := Name,
    <<"identity">>    := IdentityID,
    <<"currency">>    := CurrencyID,
    <<"context">>     := Ctx
}) ->
    #wlt_WalletParams{
        id       = ID,
        name     = Name,
        account_params = marshal(account_params, {IdentityID, CurrencyID}),
        external_id    = maps:get(<<"external_id">>, Params, undefined),
        context        = marshal(context, Ctx)
    };

marshal(account_params, {IdentityID, CurrencyID}) -> #account_AccountParams{
    identity_id   = IdentityID,
    symbolic_code = CurrencyID
};

marshal(context, Ctx) ->
    ff_context:wrap(Ctx).

unmarshal(wallet, #wlt_WalletState{
    id          = WalletID,
    name        = Name,
    blocking    = Blocking,
    account     = Account,
    external_id = ExternalID,
    context     = _Ctx
}) ->
    {Identity, Currency} = unmarshal(account, Account),
    genlib_map:compact(#{
        <<"id">>          => WalletID,
        <<"name">>        => Name,
        <<"isBlocked">>   => unmarshal(blocked, Blocking),
        <<"identity">>    => Identity,
        <<"currency">>    => Currency,
        <<"external_id">> => ExternalID
    });
unmarshal(account, #account_Account{
    identity = IdentityID,
    currency = #'CurrencyRef'{
        symbolic_code = CurrencyID
    }
}) ->
    {IdentityID, CurrencyID};

unmarshal(blocked, unblocked) ->
    false;
unmarshal(blocked, blocked) ->
    true.