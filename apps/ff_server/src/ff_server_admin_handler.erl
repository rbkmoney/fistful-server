-module(ff_server_admin_handler).
-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_fistful_admin_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(fistful_admin, #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%

handle_function_('CreateSource', [Params], Opts) ->
    SourceID = Params#ff_admin_SourceParams.id,
    case ff_source_machine:create(#{
            id       => SourceID,
            identity => Params#ff_admin_SourceParams.identity_id,
            name     => Params#ff_admin_SourceParams.name,
            currency => ff_codec:unmarshal(currency_ref, Params#ff_admin_SourceParams.currency),
            resource => ff_source_codec:unmarshal(resource, Params#ff_admin_SourceParams.resource)
        }, ff_entity_context:new())
    of
        ok ->
            handle_function_('GetSource', [SourceID], Opts);
        {error, {identity, notfound}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {currency, notfound}} ->
            woody_error:raise(business, #fistful_CurrencyNotFound{});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('GetSource', [ID], _Opts) ->
    case ff_source_machine:get(ID) of
        {ok, Machine} ->
            Source = ff_source_machine:source(Machine),
            {ok, ff_source_codec:marshal(source, Source)};
        {error, notfound} ->
            woody_error:raise(business, #fistful_SourceNotFound{})
    end;
handle_function_('CreateDeposit', [Params], Opts) ->
    DepositID = Params#ff_admin_DepositParams.id,
    DepositParams = #{
        id          => DepositID,
        source_id   => Params#ff_admin_DepositParams.source,
        wallet_id   => Params#ff_admin_DepositParams.destination,
        body        => ff_codec:unmarshal(cash, Params#ff_admin_DepositParams.body)
    },
    case handle_create_result(ff_deposit_machine:create(DepositParams, ff_entity_context:new())) of
        ok ->
            handle_function_('GetDeposit', [DepositID], Opts);
        {error, {source, notfound}} ->
            woody_error:raise(business, #fistful_SourceNotFound{});
        {error, {source, unauthorized}} ->
            woody_error:raise(business, #fistful_SourceUnauthorized{});
        {error, {wallet, notfound}} ->
            woody_error:raise(business, #fistful_DestinationNotFound{});
        {error, {terms_violation, {not_allowed_currency, _More}}} ->
            woody_error:raise(business, #ff_admin_DepositCurrencyInvalid{});
        {error, {inconsistent_currency, _Details}} ->
            woody_error:raise(business, #ff_admin_DepositCurrencyInvalid{});
        {error, {bad_deposit_amount, _Amount}} ->
            woody_error:raise(business, #ff_admin_DepositAmountInvalid{});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('GetDeposit', [ID], _Opts) ->
    case ff_deposit_machine:get(ID) of
        {ok, Machine} ->
            Deposit = ff_deposit_machine:deposit(Machine),
            {ok, ff_deposit_codec:marshal(deposit, Deposit)};
        {error, {unknown_deposit, _}} ->
            woody_error:raise(business, #fistful_DepositNotFound{})
    end.

handle_create_result(ok) ->
    ok;
handle_create_result({error, exists}) ->
    ok;
handle_create_result({error, _Reason} = Error) ->
    Error.

