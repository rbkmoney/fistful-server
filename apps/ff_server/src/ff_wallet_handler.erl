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
        decode(wallet_params, Params),
        decode(context, Params#wlt_WalletParams.context))
    of
        ok ->
            handle_function_('Get', [WalletID], Opts);
        {error, {identity, notfound}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {currency, notfound}} ->
            woody_error:raise(business, #fistful_CurrencyNotFound{});
        {error, {party, _Inaccessible}} ->
            woody_error:raise(business, #fistful_PartyInaccessible{});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;

handle_function_('Get', [ID], _Opts) ->
    case ff_wallet_machine:get(ID) of
        {ok, Machine} ->
            {ok, encode(wallet, {ID, Machine})};
        {error, notfound} ->
            woody_error:raise(business, #fistful_WalletNotFound{})
    end.

encode(wallet, {ID, Machine}) ->
    Wallet = ff_wallet_machine:wallet(Machine),
    Ctx = ff_wallet_machine:ctx(Machine),
    #wlt_Wallet{
        id          = ID,
        name        = ff_wallet:name(Wallet),
        blocking    = ff_wallet:blocking(Wallet),
        account     = encode(account, ff_wallet:account(Wallet)),
        external_id = ff_wallet:external_id(Wallet),
        context     = encode(context, Ctx)
    };
encode(context, Ctx) ->
    ff_entity_context_codec:marshal(Ctx);
encode(account, Account) ->
    #account_Account{
        id       = ff_account:id(Account),
        identity = ff_account:identity(Account),
        currency = encode(currency, ff_account:currency(Account)),
        accounter_account_id = ff_account:accounter_account_id(Account)
    };
encode(currency, CurrencyId) ->
    #'CurrencyRef'{symbolic_code = CurrencyId}.

decode(wallet_params, Params) ->
    AccountParams = Params#wlt_WalletParams.account_params,
    #{
        id          => Params#wlt_WalletParams.id,
        name        => Params#wlt_WalletParams.name,
        identity    => AccountParams#account_AccountParams.identity_id,
        currency    => AccountParams#account_AccountParams.symbolic_code,
        external_id => Params#wlt_WalletParams.external_id
    };
decode(context, undefined) ->
    undefined;
decode(context, Ctx) ->
    ff_entity_context_codec:unmarshal(Ctx).

