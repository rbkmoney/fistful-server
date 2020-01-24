-module(ff_destination_handler).
-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(destination, #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%
handle_function_('Create', [ID, Params], Opts) ->
    Ctx = Params#dst_DestinationParams.context,
    case ff_destination:create(ID,
        ff_destination_codec:unmarshal_destination_params(Params),
        ff_destination_codec:unmarshal(ctx, Ctx))
    of
        ok ->
            handle_function_('Get', [ID], Opts);
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
    case ff_destination:get_machine(ID) of
        {ok, Machine} ->
            {ok, machine_to_destination(ID, Machine)};
        {error, notfound} ->
            woody_error:raise(business, #fistful_DestinationNotFound{})
    end.

machine_to_destination(ID, Machine) ->
    CreatedAt   = ff_destination_codec:marshal(timestamp, ff_machine:created(Machine)),
    Context     = ff_destination_codec:marshal(ctx, ff_destination:ctx(Machine)),
    Destination = ff_destination_codec:marshal_destination(ff_destination:get(Machine)),
    Destination#dst_Destination{
        id         = ID,
        created_at = CreatedAt,
        context    = Context
    }.