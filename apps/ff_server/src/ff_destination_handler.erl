-module(ff_destination_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").

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
handle_function_('Create', [Params], Context, Opts) ->
    ID = Params#dst_DestinationParams.id,
    case ff_destination:create(ID,
        decode(destination_params, Params),
        decode(context, Params#dst_DestinationParams.context))
    of
        ok ->
            handle_function_('Get', [ID], Context, Opts);
        {error, {identity, notfound}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {currency, notfound}} ->
            woody_error:raise(business, #fistful_CurrencyNotFound{});
        {error, {party, _Inaccessible}} ->
            woody_error:raise(business, #fistful_PartyInaccessible{});
        {error, exists} ->
            woody_error:raise(business, #fistful_MachineAlreadyWorking{});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('Get', [ID], _Context, _Opts) ->
    case ff_destination:get_machine(ID) of
        {ok, Machine} ->
            {ok, encode(destination, {ID, Machine})};
        {error, notfound} ->
            woody_error:raise(business, #fistful_DestinationNotFound{})
    end.

encode(destination, {ID, Machine}) ->
    Dst = ff_destination:get(Machine),
    Ctx = ff_machine:ctx(Machine),
    CreatedAt = ff_machine:created(Machine),
    #dst_Destination{
        id       = ID,
        name     = ff_destination:name(Dst),
        identity = ff_destination:identity(Dst),
        currency = ff_destination:currency(Dst),
        status   = encode(status, ff_destination:status(Dst)),
        resource = encode(resource, ff_destination:resource(Dst)),
        blocked  = encode(blocked, ff_destination:is_accessible(Dst)),
        context  = encode(context, Ctx),
        created_at  = encode(time, CreatedAt),
        external_id = ff_destination:external_id(Dst)
    };
encode(status, authorized) ->
    {authorized, #dst_Authorized{}};
encode(status, unauthorized) ->
    {unauthorized, #dst_Unauthorized{}};
encode(resource, {bank_card, BankCard}) ->
    {bank_card, #'BankCard'{
        token          = token(BankCard),
        payment_system = payment_system(BankCard),
        bin            = bin(BankCard),
        masked_pan     = masked_pan(BankCard)
    }};
encode(time, {{Date, Time}, Usec}) ->
    {ok, Timestamp} = rfc3339:format({Date, Time, Usec, undefined}),
    Timestamp;
encode(blocked, {ok, accessible}) ->
    false;
encode(blocked, _) ->
    true;
encode(context, undefined) ->
    undefined;
encode(context, Ctx) ->
    ff_context:wrap(Ctx).

decode(destination_params, Params) -> #{
    identity => Params#dst_DestinationParams.identity,
    name     => Params#dst_DestinationParams.name,
    currency => Params#dst_DestinationParams.currency,
    resource => decode(resource, Params#dst_DestinationParams.resource),
    external_id => Params#dst_DestinationParams.external_id
    };
decode(resource, {bank_card, BankCard}) ->
    {bank_card, #{
        token          => BankCard#'BankCard'.token,
        payment_system => BankCard#'BankCard'.payment_system,
        bin            => BankCard#'BankCard'.bin,
        masked_pan     => BankCard#'BankCard'.masked_pan
    }};
decode(context, undefined) -> undefined;
decode(context, Ctx) -> ff_context:unwrap(Ctx).


%%% Resource BankCard

token(#{token := V}) -> V.
payment_system(Resource) ->
    maps:get(payment_system, Resource, undefined).
bin(Resource) ->
    maps:get(bin, Resource, undefined).
masked_pan(Resource) ->
    maps:get(masked_pan, Resource, undefined).