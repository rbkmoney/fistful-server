-module(ff_server_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("fistful_proto/include/ff_proto_fistful_thrift.hrl").

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
    SourceID = Params#fistful_SourceParams.id,
    case ff_source:create(SourceID, #{
            identity => Params#fistful_SourceParams.identity_id,
            name     => Params#fistful_SourceParams.name,
            currency => decode(currency, Params#fistful_SourceParams.currency),
            resource => decode({source, resource}, Params#fistful_SourceParams.resource)
        }, decode(context, Params#fistful_SourceParams.context))
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
    DepositID = Params#fistful_DepositParams.id,
    case ff_deposit:create(DepositID, #{
            source_id   => Params#fistful_DepositParams.source,
            wallet_id   => Params#fistful_DepositParams.destination,
            body        => decode({deposit, body}, Params#fistful_DepositParams.body)
        }, decode(context, Params#fistful_DepositParams.context))
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
    case ff_deposit:get_machine(ID) of
        {ok, Machine} ->
            {ok, encode(deposit, {ID, Machine})};
        {error, notfound} ->
            woody_error:raise(business, #fistful_DepositNotFound{})
    end;
handle_function_('RevertDeposit', [Params], _Context, _Opts) ->
    case ff_deposit:revert(
            Params#fistful_RevertDepositParams.id,
            decode({deposit, body}, Params#fistful_RevertDepositParams.body),
            Params#fistful_RevertDepositParams.reason
        ) of
        {ok, Reposit} ->
            {ok, encode(reposit, Reposit)};
        {error, notfound} ->
            woody_error:raise(business, #fistful_DepositNotFound{});
        {error, invalid_currency} ->
            woody_error:raise(business, #fistful_RepositCurrencyInvalid{});
        {error, invalid_amount} ->
            woody_error:raise(business, #fistful_RepositAmountInvalid{});
        {error, {not_permitted, Reason}} ->
            woody_error:raise(business, #fistful_OperationNotPermitted{details = Reason});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('GetReposit', [DepositID, RepositID], _Context, _Opts) ->
    case ff_deposit:get_machine(DepositID) of
        {ok, Machine} ->
            Deposit = ff_deposit:get(Machine),
            case ff_reposit:get(RepositID, ff_transfer:reposits(Deposit)) of
                {ok, Reposit} ->
                    {ok, encode(reposit, Reposit)};
                {error, notfound} ->
                    woody_error:raise(business, #fistful_RepositNotFound{})
            end;
        {error, notfound} ->
            woody_error:raise(business, #fistful_DepositNotFound{})
    end.

decode({source, resource}, #fistful_SourceResource{details = Details}) ->
    genlib_map:compact(#{
        type    => internal,
        details => Details
    });
decode({deposit, body}, #'Cash'{amount = Amount, currency = Currency}) ->
    {Amount, decode(currency, Currency)};
decode(currency, #'CurrencyRef'{symbolic_code = V}) ->
    V;
decode(context, Context) ->
    Context.

encode({list, T}, V) when is_list(V) ->
    [encode(T, E) || E <- V];
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
        context     = encode(context, ff_machine:ctx(Machine)),
        reposits    = encode({list, reposit}, ff_transfer:reposits(Deposit))
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
encode({deposit, status}, {reverted, Details}) ->
    {reverted, #fistful_DepositStatusReverted{details = woody_error:format_details(Details)}};
encode(currency, V) ->
    #'CurrencyRef'{symbolic_code = V};
encode(context, #{}) ->
    undefined;
encode(context, Ctx) ->
    Ctx;
encode(reposit, Reposit) ->
    #fistful_Reposit{
        id              = ff_reposit:id(Reposit),
        deposit         = ff_reposit:deposit_id(Reposit),
        source          = ff_reposit:wallet_id(Reposit),
        destination     = ff_reposit:source_id(Reposit),
        body            = encode({deposit, body}, ff_reposit:body(Reposit)),
        created_at      = ff_reposit:create_at(Reposit),
        domain_revision = ff_reposit:domain_revision(Reposit),
        party_revision  = ff_reposit:party_revision(Reposit),
        reason          = ff_reposit:reason(Reposit),
        status          = encode({reposit, status}, ff_reposit:status(Reposit))
    };
encode({reposit, status}, pending) ->
    {pending, #fistful_RepositStatusPending{}};
encode({reposit, status}, succeeded) ->
    {succeeded, #fistful_RepositStatusSucceeded{}};
encode({reposit, status}, {failed, Details}) ->
    {failed, #fistful_RepositStatusFailed{details = woody_error:format_details(Details)}};

encode(_, undefined) ->
    undefined.
