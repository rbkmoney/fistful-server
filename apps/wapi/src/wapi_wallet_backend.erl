-module(wapi_wallet_backend).

-export([create/2]).
-export([get/2]).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

%% Pipeline
-import(ff_pipeline, [do/1]).

-spec create(any(), any()) ->
    {ok, any()}.

create(ParamsIn = #{<<"identity">> := IdentityID}, WoodyContext) ->
    do(fun() ->
        WalletParams = create_wallet_params(ParamsIn, WoodyContext),
        ok = wapi_access_backend:check_resource(identity, IdentityID, WoodyContext),
        Request = {fistful_wallet, 'Create', [WalletParams]},
        case call(Request, WoodyContext) of
            {ok, Wallet} ->
                unmarshal(wallet, Wallet);
            {exception, #fistful_CurrencyNotFound{}} ->
                throw({currency, notfound});
            {exception, #fistful_PartyInaccessible{}} ->
                throw(inaccessible);
            %% TODO extend thrift add createdAt, exception IDExists
            % {exception, #fistful_IDExists{}} ->
            %     WalletID = get_id(WalletParams),
            %     {ok, Wallet} = call({fistful_wallet, 'Get', [WalletID]}, WoodyContext),
            %     wapi_backend_utils:compare_hash(
            %         wapi_backend_utils:create_hash(ParamsIn),
            %         get_hash(Wallet)
            %     );
            {exception, Details} ->
                throw(Details)
        end
    end).

-spec get(binary(), any()) ->
    {ok, any()}.

get(WalletID, WoodyCtx) ->
    Request = {fistful_wallet, 'Get', [WalletID]},
    case call(Request, WoodyCtx) of
        {ok, Wallet} ->
            {ok, unmarshal(wallet, Wallet)};
        {exception, #fistful_WalletNotFound{}} ->
            throw({wallet, notfound});
        {exception, Details} ->
            throw(Details)
    end.

%%
%% Internal
%%

call(Params, Ctx) ->
    case wapi_handler_utils:service_call(Params, Ctx) of
        {ok, Response} ->
            {ok, Response};
        Error ->
            Error
    end.

% get_hash(#wlt_WalletState{context = Ctx}) ->
%     wapi_handler_utils:get_hash(Ctx).

% get_id(#wlt_WalletParams{id = ID}) ->
%     ID.

create_wallet_params(ParamsIn, WoodyContext) ->
    Type = wallet,
    ID = wapi_backend_utils:make_id(
            Type,
            wapi_backend_utils:construct_external_id(ParamsIn, WoodyContext)
        ),
    List = [
        {<<"owner">>, wapi_handler_utils:get_owner(WoodyContext)},
        {<<"metadata">>, maps:get(<<"metadata">>, ParamsIn, undefined)},
        wapi_backend_utils:create_params_hash(ParamsIn)
    ],
    Context = wapi_backend_utils:extend_ctx_from_list(List, wapi_backend_utils:make_ctx()),

    marshal(wallet_params, ParamsIn#{
        <<"id">>      => ID,
        <<"context">> => Context
    }).

%% Marshaling

marshal(wallet_params, Params = #{
    <<"id">>       := ID,
    <<"name">>     := Name,
    <<"identity">> := IdentityID,
    <<"currency">> := CurrencyID,
    <<"context">>  := Ctx
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
    context     = Ctx
}) ->
    {Identity, Currency} = unmarshal(account, Account),
    Context = unmarshal(context, Ctx),
    genlib_map:compact(#{
        <<"id">>          => WalletID,
        <<"name">>        => Name,
        <<"isBlocked">>   => unmarshal(blocked, Blocking),
        <<"identity">>    => Identity,
        <<"currency">>    => Currency,
        <<"external_id">> => ExternalID,
        <<"metadata">>    => wapi_backend_utils:get_from_ctx(<<"metadata">>, Context)
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
    true;

unmarshal(context, Ctx) ->
    ff_context:unwrap(Ctx).