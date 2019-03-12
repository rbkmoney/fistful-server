-module(wapi_access_backend).

-export([check_resource/3]).

-define(CTX_NS, <<"com.rbkmoney.wapi">>).

%% Pipeline
-import(ff_pipeline, [unwrap/1, unwrap/2]).


-spec check_resource(atom(), binary(), any()) ->
    ok | {error, {identity, unauthorized}}.

check_resource(Resource, Id, Context) ->
    State = unwrap(Resource, do_get_state(Resource, Id)),
    unwrap(Resource, check_resource_access(Context, State)).

%%
%% Internal
%%
%% TODO rewrite after all fistful-proto will implement.

do_get_state(identity,    Id) -> ff_identity_machine:get(Id);
do_get_state(wallet,      Id) -> ff_wallet_machine:get(Id);
do_get_state(destination, Id) -> ff_destination:get_machine(Id);
do_get_state(withdrawal,  Id) -> ff_withdrawal:get_machine(Id).

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
