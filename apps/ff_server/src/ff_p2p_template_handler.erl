-module(ff_p2p_template_handler).
-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_p2p_template_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function(Func, Args, Opts) ->
    scoper:scope(p2p_template, #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%
handle_function_('Create', [Params], Opts) ->
    P2PTemplateID = Params#p2p_template_P2PTemplateParams.id,
    case p2p_template_machine:create(
        ff_p2p_template_codec:unmarshal_p2p_template_params(Params),
        ff_p2p_template_codec:unmarshal(ctx, Params#p2p_template_P2PTemplateParams.context))
    of
        ok ->
            handle_function_('Get', [P2PTemplateID, #'EventRange'{}], Opts);
        {error, exists} ->
            woody_error:raise(business, #fistful_IDExists{});
        {error, {identity, notfound}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {terms, {bad_p2p_template_amount, Cash}}} ->
            woody_error:raise(business, #fistful_InvalidOperationAmount{amount = ff_codec:marshal(cash, Cash)});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;

handle_function_('Get', [ID, EventRange], _Opts) ->
    {After, Limit} = ff_codec:unmarshal(event_range, EventRange),
    case p2p_template_machine:get(ID, {After, Limit, forward}) of
        {ok, Machine} ->
            P2PTemplate = p2p_template_machine:p2p_template(Machine),
            Ctx = ff_machine:ctx(Machine),
            Response = ff_p2p_template_codec:marshal_p2p_template_state(P2PTemplate, Ctx),
            {ok, Response};
        {error, {unknown_p2p_template, _Ref}} ->
            woody_error:raise(business, #fistful_P2PTemplateNotFound{})
    end;

handle_function_('SetBlocking', [ID, Blocking], _Opts) ->
    case p2p_template_machine:set_blocking(ID, ff_p2p_template_codec:unmarshal(blocking, Blocking)) of
        ok ->
            {ok, ok};
        {error, {unknown_p2p_template, _Ref}} ->
            woody_error:raise(business, #fistful_P2PTemplateNotFound{})
    end.
