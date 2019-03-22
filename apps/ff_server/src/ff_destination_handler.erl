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
    scoper:scope(destination, #{function => Func},
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
    Ctx = Params#dst_DestinationParams.context,
    case ff_destination:create(ID,
        ff_destination_codec:unmarshal_destination_params(Params),
        ff_destination_codec:unmarshal(ctx, Ctx))
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
            woody_error:raise(business, #fistful_IDExists{});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('Get', [ID], _Context, _Opts) ->
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