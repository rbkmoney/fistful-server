-module(wapi_access_backend).

-export([check_resource/3]).
-export([is_authorized/2]).

-type handler_context() :: wapi_handler:context().
-type entity_context() :: ff_ctx:ctx().

-define(CTX_NS, <<"com.rbkmoney.wapi">>).

%% Pipeline
-import(ff_pipeline, [unwrap/1, unwrap/2]).

-spec is_authorized(handler_context(), entity_context()) ->
    ok | {error, unauthorized}.

is_authorized(HandlerCtx, EntityCtx) ->
    Owner1 = wapi_handler_utils:get_owner(HandlerCtx),
    Owner2 = wapi_backend_utils:get_from_ctx(<<"owner">>, EntityCtx),
    check_resource_access(Owner1 =:= Owner2).


%% TODO rewrite after all fistful-proto will implement.

-spec check_resource(atom(), binary(), handler_context()) ->
    ok | {error, {atom(), unauthorized}}.

check_resource(Resource, Id, Context) ->
    State = unwrap(Resource, do_get_state(Resource, Id)),
    unwrap(Resource, check_resource_access(Context, State)).

%%
%% Internal
%%

do_get_state(identity,    Id) -> ff_identity_machine:get(Id).

get_resource_owner(State) ->
    maps:get(<<"owner">>, get_ctx(State)).

is_resource_owner(HandlerCtx, State) ->
    wapi_handler_utils:get_owner(HandlerCtx) =:= get_resource_owner(State).

check_resource_access(HandlerCtx, State) ->
    check_resource_access(is_resource_owner(HandlerCtx, State)).

check_resource_access(true)  -> ok;
check_resource_access(false) -> {error, unauthorized}.

get_ctx(State) ->
    unwrap(ff_ctx:get(?CTX_NS, ff_machine:ctx(State))).
