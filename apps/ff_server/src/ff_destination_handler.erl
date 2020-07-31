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
handle_function_('Create', [Params, Ctx], Opts) ->
    ID = Params#dst_DestinationParams.id,
    case ff_destination:create(
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
            handle_function_('Get', [ID], Opts);
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('Get', [ID], _Opts) ->
    case ff_destination:get_machine(ID) of
        {ok, Machine} ->
            Destination = ff_destination:get(Machine),
            Context = ff_destination:ctx(Machine),
            Response = ff_destination_codec:marshal_destination_state(Destination, ID, Context),
            {ok, Response};
        {error, notfound} ->
            woody_error:raise(business, #fistful_DestinationNotFound{})
    end.
