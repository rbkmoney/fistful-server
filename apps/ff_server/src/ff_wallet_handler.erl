-module(ff_wallet_handler).
-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function(Func, Args, Opts) ->
    scoper:scope(wallet, #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%
handle_function_('Create', [Params, Context], Opts) ->
    WalletID = Params#wlt_WalletParams.id,
    case ff_wallet_machine:create(
        ff_wallet_codec:unmarshal_wallet_params(Params),
        ff_wallet_codec:unmarshal(ctx, Context))
    of
        ok ->
            handle_function_('Get', [WalletID, #'EventRange'{}], Opts);
        {error, {identity, notfound}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {currency, notfound}} ->
            woody_error:raise(business, #fistful_CurrencyNotFound{});
        {error, {party, _Inaccessible}} ->
            woody_error:raise(business, #fistful_PartyInaccessible{});
        {error, exists} ->
            handle_function_('Get', [WalletID, #'EventRange'{}], Opts);
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;

handle_function_('Get', [ID, EventRange], _Opts) ->
    case ff_wallet_machine:get(ID, ff_codec:unmarshal(event_range, EventRange)) of
        {ok, Machine} ->
            Wallet    = ff_wallet_machine:wallet(Machine),
            Ctx       = ff_wallet_machine:ctx(Machine),
            Response  = ff_wallet_codec:marshal_wallet_state(Wallet, ID, Ctx),
            {ok, Response};
        {error, notfound} ->
            woody_error:raise(business, #fistful_WalletNotFound{})
    end;

handle_function_('GetContext', [ID], _Opts) ->
    case ff_wallet_machine:get(ID, {undefined, 0}) of
        {ok, Machine} ->
            Ctx       = ff_wallet_machine:ctx(Machine),
            Response  = ff_wallet_codec:marshal(ctx, Ctx),
            {ok, Response};
        {error, notfound} ->
            woody_error:raise(business, #fistful_WalletNotFound{})
    end;

handle_function_('GetAccountBalance', [ID], _Opts) ->
    case ff_wallet_machine:get(ID) of
        {ok, Machine} ->
            Wallet = ff_wallet_machine:wallet(Machine),
            {ok, AccountBalance} = ff_wallet:get_account_balance(Wallet),
            Response = ff_wallet_codec:marshal(wallet_account_balance, AccountBalance),
            {ok, Response};
        {error, notfound} ->
            woody_error:raise(business, #fistful_WalletNotFound{})
    end.
