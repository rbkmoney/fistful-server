-module(ff_p2p_template_handler).

-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_p2p_template_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) -> {ok, woody:result()} | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(
        p2p_template,
        #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%
handle_function_('Create', {MarshaledParams, MarshaledContext}, Opts) ->
    ID = MarshaledParams#p2p_template_P2PTemplateParams.id,
    Params = ff_p2p_template_codec:unmarshal_p2p_template_params(MarshaledParams),
    Context = ff_p2p_template_codec:unmarshal(ctx, MarshaledContext),
    ok = scoper:add_meta(maps:with([id, identity_id, external_id], Params)),
    case p2p_template_machine:create(Params, Context) of
        ok ->
            handle_function_('Get', {ID, #'EventRange'{}}, Opts);
        {error, exists} ->
            handle_function_('Get', {ID, #'EventRange'{}}, Opts);
        {error, {identity, notfound}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {terms, {bad_p2p_template_amount, Cash}}} ->
            woody_error:raise(business, #fistful_InvalidOperationAmount{amount = ff_codec:marshal(cash, Cash)});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('Get', {ID, MarshaledEventRange}, _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    EventRange = ff_codec:unmarshal(event_range, MarshaledEventRange),
    case p2p_template_machine:get(ID, EventRange) of
        {ok, Machine} ->
            P2PTemplate = p2p_template_machine:p2p_template(Machine),
            Ctx = p2p_template_machine:ctx(Machine),
            Response = ff_p2p_template_codec:marshal_p2p_template_state(P2PTemplate, Ctx),
            {ok, Response};
        {error, {unknown_p2p_template, _Ref}} ->
            woody_error:raise(business, #fistful_P2PTemplateNotFound{})
    end;
handle_function_('GetContext', {ID}, _Opts) ->
    case p2p_template_machine:get(ID, {undefined, 0}) of
        {ok, Machine} ->
            Ctx = p2p_template_machine:ctx(Machine),
            Response = ff_p2p_template_codec:marshal(ctx, Ctx),
            {ok, Response};
        {error, {unknown_p2p_template, _Ref}} ->
            woody_error:raise(business, #fistful_P2PTemplateNotFound{})
    end;
handle_function_('SetBlocking', {ID, MarshaledBlocking}, _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    Blocking = ff_p2p_template_codec:unmarshal(blocking, MarshaledBlocking),
    case p2p_template_machine:set_blocking(ID, Blocking) of
        ok ->
            {ok, ok};
        {error, {unknown_p2p_template, _Ref}} ->
            woody_error:raise(business, #fistful_P2PTemplateNotFound{})
    end;
handle_function_('GetQuote', {ID, MarshaledParams}, _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    Params = ff_p2p_template_codec:unmarshal_p2p_quote_params(MarshaledParams),
    case p2p_template_machine:get_quote(ID, Params) of
        {ok, Quote} ->
            {ok, ff_p2p_template_codec:marshal(quote, Quote)};
        {error, {unknown_p2p_template, _Ref}} ->
            woody_error:raise(business, #fistful_P2PTemplateNotFound{});
        {error, p2p_template_blocked} ->
            woody_error:raise(business, #fistful_OperationNotPermitted{
                details = ff_codec:marshal(string, <<"P2PTransferTemplate inaccessible">>)
            });
        {error, {identity, not_found}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {terms, {terms_violation, p2p_forbidden}}} ->
            woody_error:raise(business, #fistful_OperationNotPermitted{
                details = ff_codec:marshal(string, <<"P2PTransferTemplate forbidden">>)
            });
        {error, {terms, {terms_violation, {not_allowed_currency, {DomainCurrency, DomainAllowed}}}}} ->
            Currency = ff_dmsl_codec:unmarshal(currency_ref, DomainCurrency),
            Allowed = [ff_dmsl_codec:unmarshal(currency_ref, C) || C <- DomainAllowed],
            woody_error:raise(business, #fistful_ForbiddenOperationCurrency{
                currency = ff_codec:marshal(currency_ref, Currency),
                allowed_currencies = ff_codec:marshal({set, currency_ref}, Allowed)
            });
        {error, {terms, {terms_violation, {cash_range, {Cash, Range}}}}} ->
            woody_error:raise(business, #fistful_ForbiddenOperationAmount{
                amount = ff_codec:marshal(cash, Cash),
                allowed_range = ff_codec:marshal(cash_range, Range)
            });
        {error, {Type, {bin_data, _}}} ->
            woody_error:raise(business, #p2p_transfer_NoResourceInfo{type = Type});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end;
handle_function_('CreateTransfer', {ID, MarshaledParams, MarshaledContext}, Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    TransferID = MarshaledParams#p2p_template_P2PTemplateTransferParams.id,
    Params = ff_p2p_template_codec:unmarshal_p2p_transfer_params(MarshaledParams),
    Context = ff_p2p_template_codec:unmarshal(ctx, MarshaledContext),
    case p2p_template_machine:create_transfer(ID, Params#{context => Context}) of
        ok ->
            ff_p2p_transfer_handler:handle_function('Get', {TransferID, #'EventRange'{}}, Opts);
        {error, exists} ->
            ff_p2p_transfer_handler:handle_function('Get', {TransferID, #'EventRange'{}}, Opts);
        {error, p2p_template_blocked} ->
            woody_error:raise(business, #fistful_OperationNotPermitted{
                details = ff_codec:marshal(string, <<"P2PTransferTemplate inaccessible">>)
            });
        {error, {unknown_p2p_template, _Ref}} ->
            woody_error:raise(business, #fistful_P2PTemplateNotFound{});
        {error, {identity, notfound}} ->
            woody_error:raise(business, #fistful_IdentityNotFound{});
        {error, {terms, {terms_violation, p2p_forbidden}}} ->
            woody_error:raise(business, #fistful_OperationNotPermitted{
                details = ff_codec:marshal(string, <<"P2PTransferTemplate forbidden">>)
            });
        {error, {terms, {terms_violation, {not_allowed_currency, {DomainCurrency, DomainAllowed}}}}} ->
            Currency = ff_dmsl_codec:unmarshal(currency_ref, DomainCurrency),
            Allowed = [ff_dmsl_codec:unmarshal(currency_ref, C) || C <- DomainAllowed],
            woody_error:raise(business, #fistful_ForbiddenOperationCurrency{
                currency = ff_codec:marshal(currency_ref, Currency),
                allowed_currencies = ff_codec:marshal({set, currency_ref}, Allowed)
            });
        {error, {terms, {terms_violation, {cash_range, {Cash, Range}}}}} ->
            woody_error:raise(business, #fistful_ForbiddenOperationAmount{
                amount = ff_codec:marshal(cash, Cash),
                allowed_range = ff_codec:marshal(cash_range, Range)
            });
        {error, {Type, {bin_data, _}}} ->
            woody_error:raise(business, #p2p_transfer_NoResourceInfo{type = Type});
        {error, {Type, different_resource}} ->
            woody_error:raise(business, #p2p_transfer_NoResourceInfo{type = Type});
        {error, Error} ->
            woody_error:raise(system, {internal, result_unexpected, woody_error:format_details(Error)})
    end.
