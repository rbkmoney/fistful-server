-module(ff_source_handler).
-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_source_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(source, #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%
handle_function_('Create', [Params, Ctx], Opts) ->
    ID = Params#src_SourceParams.id,
    ok = scoper:add_meta(#{id => ID}),
    case ff_source:create(
        ff_source_codec:unmarshal_source_params(Params),
        ff_source_codec:unmarshal(ctx, Ctx))
    of
        ok ->
            handle_function_('Get', [ID, #'EventRange'{}], Opts);
        {error, {identity, notfound}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {currency, notfound}} ->
            woody_error:raise(business, #fistful_CurrencyNotFound{});
        {error, {party, _Inaccessible}} ->
            woody_error:raise(business, #fistful_PartyInaccessible{});
        {error, exists} ->
            handle_function_('Get', [ID, #'EventRange'{}], Opts);
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('Get', [ID, EventRange], _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    case ff_source:get_machine(ID, ff_codec:unmarshal(event_range, EventRange)) of
        {ok, Machine} ->
            Source = ff_source:get(Machine),
            Context = ff_source:ctx(Machine),
            Response = ff_source_codec:marshal_source_state(Source, Context),
            {ok, Response};
        {error, notfound} ->
            woody_error:raise(business, #fistful_DestinationNotFound{})
    end;
handle_function_('GetContext', [ID], _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    case ff_source:get_machine(ID, {undefined, 0}) of
        {ok, Machine} ->
            Context = ff_source:ctx(Machine),
            {ok, ff_codec:marshal(context, Context)};
        {error, notfound} ->
            woody_error:raise(business, #fistful_DestinationNotFound{})
    end;
handle_function_('GetEvents', [ID, EventRange], _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    case ff_source:events(ID, ff_codec:unmarshal(event_range, EventRange)) of
        {ok, Events} ->
            {ok, lists:map(fun ff_source_codec:marshal_event/1, Events)};
        {error, notfound} ->
            woody_error:raise(business, #fistful_DestinationNotFound{})
    end.
