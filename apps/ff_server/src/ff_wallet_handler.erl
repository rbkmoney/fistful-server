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
handle_function_('Create', [Params], Opts) ->
    WalletID = Params#wlt_WalletParams.id,
    case ff_wallet_machine:create(
        ff_wallet_codec:unmarshal_wallet_params(Params),
        ff_wallet_codec:unmarshal(ctx, Params#wlt_WalletParams.context))
    of
        ok ->
            handle_function_('Get', [WalletID], Opts);
        {error, {identity, notfound}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {currency, notfound}} ->
            woody_error:raise(business, #fistful_CurrencyNotFound{});
        {error, {party, _Inaccessible}} ->
            woody_error:raise(business, #fistful_PartyInaccessible{});
        {error, exists} ->
            woody_error:raise(business, #fistful_IDExists{});        
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;

handle_function_('Get', [ID], _Opts) ->
    case ff_wallet_machine:get(ID) of
        {ok, Machine} ->
            Wallet    = ff_wallet_machine:wallet(Machine),
            Ctx       = ff_machine:ctx(Machine),
            CreatedAt = ff_machine:created(Machine),
            Response  = ff_wallet_codec:marshal_wallet(Wallet),
            {ok, Response#wlt_Wallet{
                id         = ff_wallet_codec:marshal(id, ID),
                created_at = ff_wallet_codec:marshal(timestamp, CreatedAt),
                context    = ff_wallet_codec:marshal(ctx, Ctx)
            }};
        {error, notfound} ->
            woody_error:raise(business, #fistful_WalletNotFound{})
    end.
