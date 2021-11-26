-module(ff_identity_handler).

-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) -> {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    IdentityID = element(1, Args),
    scoper:scope(
        identity,
        #{identity_id => IdentityID},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%

handle_function_('Create', {IdentityParams, Context}, Opts) ->
    Params = #{id := IdentityID} = ff_identity_codec:unmarshal_identity_params(IdentityParams),
    case ff_identity_machine:create(Params, ff_identity_codec:unmarshal(ctx, Context)) of
        ok ->
            handle_function_('Get', {IdentityID, #'EventRange'{}}, Opts);
        {error, {provider, notfound}} ->
            woody_error:raise(business, #fistful_ProviderNotFound{});
        {error, {party, notfound}} ->
            woody_error:raise(business, #fistful_PartyNotFound{});
        {error, {inaccessible, _}} ->
            woody_error:raise(business, #fistful_PartyInaccessible{});
        {error, exists} ->
            handle_function_('Get', {IdentityID, #'EventRange'{}}, Opts);
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('Get', {ID, EventRange}, _Opts) ->
    case ff_identity_machine:get(ID, ff_codec:unmarshal(event_range, EventRange)) of
        {ok, Machine} ->
            Identity = ff_identity:set_blocking(ff_identity_machine:identity(Machine)),
            Context = ff_identity_machine:ctx(Machine),
            Response = ff_identity_codec:marshal_identity_state(Identity, Context),
            {ok, Response};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end;
handle_function_('GetContext', {ID}, _Opts) ->
    case ff_identity_machine:get(ID, {undefined, 0}) of
        {ok, Machine} ->
            Ctx = ff_identity_machine:ctx(Machine),
            Response = ff_identity_codec:marshal(ctx, Ctx),
            {ok, Response};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end;
handle_function_('GetEvents', {IdentityID, EventRange}, _Opts) ->
    case ff_identity_machine:events(IdentityID, ff_codec:unmarshal(event_range, EventRange)) of
        {ok, EventList} ->
            Events = [ff_identity_codec:marshal_identity_event(Event) || Event <- EventList],
            {ok, Events};
        {error, notfound} ->
            woody_error:raise(business, #fistful_IdentityNotFound{})
    end.
