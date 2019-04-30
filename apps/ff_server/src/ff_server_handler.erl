-module(ff_server_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("fistful_proto/include/ff_proto_fistful_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_fistful_admin_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_transfer_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Context, Opts) ->
    scoper:scope(fistful, #{function => Func},
        fun() ->
            ok = ff_woody_ctx:set(Context),
            try
                handle_function_(Func, Args, Context, Opts)
            after
                ff_woody_ctx:unset()
            end
        end
    ).

%%
%% Internals
%%

handle_function_('CreateSource', [Params], Context, Opts) ->
    SourceID = Params#fistful_admin_SourceParams.id,
    case ff_source:create(SourceID, #{
            identity => Params#fistful_admin_SourceParams.identity_id,
            name     => Params#fistful_admin_SourceParams.name,
            currency => decode(currency, Params#fistful_admin_SourceParams.currency),
            resource => decode({source, resource}, Params#fistful_admin_SourceParams.resource)
        }, decode(context, Params#fistful_admin_SourceParams.context))
    of
        ok ->
            handle_function_('GetSource', [SourceID], Context, Opts);
        {error, {identity, notfound}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {currency, notfound}} ->
            woody_error:raise(business, #fistful_CurrencyNotFound{});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('GetSource', [ID], _Context, _Opts) ->
    case ff_source:get_machine(ID) of
        {ok, Machine} ->
            {ok, encode(source, {ID, Machine})};
        {error, notfound} ->
            woody_error:raise(business, #fistful_SourceNotFound{})
    end;
handle_function_('CreateDeposit', [Params], Context, Opts) ->
    DepositID = Params#fistful_admin_DepositParams.id,
    case ff_deposit_new:create(DepositID, #{
            source_id   => Params#fistful_admin_DepositParams.source,
            wallet_id   => Params#fistful_admin_DepositParams.destination,
            body        => decode({deposit, body}, Params#fistful_admin_DepositParams.body)
        }, decode(context, Params#fistful_admin_DepositParams.context))
    of
        ok ->
            handle_function_('GetDeposit', [DepositID], Context, Opts);
        {error, {source, notfound}} ->
            woody_error:raise(business, #fistful_SourceNotFound{});
        {error, {source, unauthorized}} ->
            woody_error:raise(business, #fistful_SourceUnauthorized{});
        {error, {destination, notfound}} ->
            woody_error:raise(business, #fistful_DestinationNotFound{});
        {error, {terms_violation, {not_allowed_currency, _More}}} ->
            woody_error:raise(business, #fistful_DepositCurrencyInvalid{});
        {error, {bad_deposit_amount, _Amount}} ->
            woody_error:raise(business, #fistful_DepositAmountInvalid{});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('GetDeposit', [ID], _Context, _Opts) ->
    case ff_deposit_new:get_machine(ID) of
        {ok, Machine} ->
            {ok, encode(deposit, {ID, Machine})};
        {error, notfound} ->
            woody_error:raise(business, #fistful_DepositNotFound{})
    end;
handle_function_('CreateRevert', [Params], Context, Opts) ->
    RevertID = Params#fistful_admin_RevertParams.id,
    case ff_transfer_new:revert(#{
            revert_id   => RevertID,
            target      => decode({transfer, target}, Params#fistful_admin_RevertParams.target),
            reason      => Params#fistful_admin_RevertParams.reason,
            body        => decode({deposit, body}, Params#fistful_admin_RevertParams.body)
        })
    of
        ok ->
            Target = Params#fistful_admin_RevertParams.target,
            handle_function_('GetRevert', [
                #fistful_admin_Target{
                    root_id   = Target#fistful_admin_Target.root_id,
                    root_type = Target#fistful_admin_Target.root_type,
                    target_id = RevertID
                }
            ], Context, Opts);
        {error, {notfound, ID}} ->
            woody_error:raise(business, #fistful_TransferNotFound{id = ID});
        {error, {not_permitted, _Details}} ->
            woody_error:raise(business, #fistful_OperationNotPermitted{});
        {error, invalid_amount} ->
            woody_error:raise(business, #fistful_AmountInvalid{});
        {error, invalid_currency} ->
            woody_error:raise(business, #fistful_CurrencyInvalid{})
    end;
handle_function_('GetRevert', [Target], _Context, _Opts) ->
    RevertID = Target#fistful_admin_Target.target_id,
    case ff_transfer_new:get_revert(decode({transfer, target}, Target)) of
        {ok, Revert} ->
            {ok, encode(revert, {RevertID, Revert})};
        {error, {notfound, ID}} ->
            woody_error:raise(business, #fistful_TransferNotFound{id = ID})
    end.

%%

decode({source, resource}, #fistful_SourceResource{details = Details}) ->
    genlib_map:compact(#{
        type    => internal,
        details => Details
    });
decode({deposit, body}, #'Cash'{amount = Amount, currency = Currency}) ->
    {Amount, decode(currency, Currency)};
decode(currency, #'CurrencyRef'{symbolic_code = V}) ->
    V;
decode({transfer, target}, #fistful_admin_Target{root_id = ID, root_type = Type, target_id = TargetID}) ->
    #{root_id => ID, root_type => decode({transfer, type}, Type), target_id => TargetID};
decode({transfer, type}, {deposit, #transfer_TransferDeposit{}}) ->
    deposit;
decode({transfer, type}, {withdrawal, #transfer_TransferWithdrawal{}}) ->
    withdrawal;
decode({transfer, type}, {revert, {deposit, #transfer_TransferDepositRevert{}}}) ->
    revert;
decode(context, Context) ->
    Context.

encode(revert, {ID, Revert}) ->
    #fistful_admin_Revert{
        id          = ID,
        target      = encode({transfer, target}, ff_revert:target(Revert)),
        status      = encode({transfer, status}, ff_revert:status(Revert)),
        body        = encode({deposit, body}, ff_revert:body(Revert)),
        reason      = ff_revert:reason(Revert)
    };
encode({transfer, status}, pending) ->
    {pending, #transfer_TransferPending{}};
encode({transfer, status}, succeeded) ->
    {succeeded, #transfer_TransferSucceeded{}};
encode({transfer, status}, reverted) ->
    {reverted, #transfer_TransferReverted{}};
encode({transfer, status}, {failed, _Failure}) ->
    {failed, #transfer_TransferFailed{failure = #transfer_Failure{}}};

encode({transfer, type}, deposit) ->
    {deposit, #transfer_TransferDeposit{}};
encode({transfer, type}, withdrawal) ->
    {withdrawal, #transfer_TransferWithdrawal{}};
encode({transfer, type}, revert) ->
    {revert, {deposit, #transfer_TransferDepositRevert{}}};

encode({transfer, target}, #{root_id := ID, root_type := Type, target_id := TargetID}) ->
    #fistful_admin_Target{
        root_id = ID,
        root_type = encode({transfer, type}, Type),
        target_id = TargetID
    };
encode(source, {ID, Machine}) ->
    Source = ff_source:get(Machine),
    #fistful_Source{
        id          = ID,
        name        = ff_source:name(Source),
        identity_id = ff_source:identity(Source),
        currency    = encode(currency, ff_source:currency(Source)),
        resource    = encode({source, resource}, ff_source:resource(Source)),
        status      = encode({source, status}, ff_source:status(Source)),
        context     = encode(context, ff_machine:ctx(Machine))
    };
encode({source, status}, Status) ->
    Status;
encode({source, resource}, Resource) ->
    #fistful_SourceResource{
        details = genlib_map:get(details, Resource)
    };
encode(deposit, {ID, Machine}) ->
    Deposit = ff_deposit:get(Machine),
    #fistful_Deposit{
        id          = ID,
        source      = ff_deposit:source_id(Deposit),
        destination = ff_deposit:wallet_id(Deposit),
        body        = encode({deposit, body}, ff_deposit:body(Deposit)),
        status      = encode({deposit, status}, ff_deposit:status(Deposit)),
        context     = encode(context, ff_machine:ctx(Machine))
    };
encode({deposit, body}, {Amount, Currency}) ->
    #'Cash'{
        amount   = Amount,
        currency = encode(currency, Currency)
    };
encode({deposit, status}, pending) ->
    {pending, #fistful_DepositStatusPending{}};
encode({deposit, status}, succeeded) ->
    {succeeded, #fistful_DepositStatusSucceeded{}};
encode({deposit, status}, {failed, Details}) ->
    {failed, #fistful_DepositStatusFailed{details = woody_error:format_details(Details)}};
encode(currency, V) ->
    #'CurrencyRef'{symbolic_code = V};
encode(context, #{}) ->
    undefined;
encode(context, Ctx) ->
    Ctx.
