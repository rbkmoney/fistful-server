-module(wapi_backend_utils).

-define(EXTERNAL_ID, <<"externalID">>).
-define(CTX_NS, <<"com.rbkmoney.wapi">>).
-define(PARAMS_HASH, <<"params_hash">>).
-define(BENDER_DOMAIN, <<"wapi">>).

%% Context
-type md() :: ff_entity_context:md().
-type context() :: ff_entity_context:context().
-type params() :: map().
-type handler_context() :: wapi_handler:context().

-export([make_id/3]).
-export([make_ctx/0]).
-export([add_to_ctx/3]).
-export([get_from_ctx/2]).
-export([extend_ctx_from_map/3]).
-export([extend_ctx_from_list/2]).
-export([create_params_hash/1]).
-export([get_hash/1]).
-export([compare_hash/2]).

%% Pipeline

-import(ff_pipeline, [unwrap/1]).

-spec make_id(atom(), params(), handler_context()) ->
    binary().

make_id(Type, Params, Context) ->
    ExternalID = maps:get(?EXTERNAL_ID, Params, undefined),
    Hash       = erlang:phash2(Params),
    unwrap(gen_id(Type, ExternalID, Hash, Context)).

gen_id(Type, ExternalID, Hash, Context) ->
    PartyID = wapi_handler_utils:get_owner(Context),
    IdempotentKey = bender_client:get_idempotent_key(?BENDER_DOMAIN, Type, PartyID, ExternalID),
    gen_id_by_type(Type, IdempotentKey, Hash, Context).

%@TODO: Bring back later
%gen_id_by_type(withdrawal = Type, IdempotentKey, Hash, Context) ->
%    gen_snowflake_id(Type, IdempotentKey, Hash, Context);
gen_id_by_type(Type, IdempotentKey, Hash, Context) ->
    gen_sequence_id(Type, IdempotentKey, Hash, Context).

%@TODO: Bring back later
%gen_snowflake_id(_Type, IdempotentKey, Hash, #{woody_context := WoodyCtx}) ->
%    bender_client:gen_by_snowflake(IdempotentKey, Hash, WoodyCtx).
gen_sequence_id(Type, IdempotentKey, Hash, #{woody_context := WoodyCtx}) ->
    BinType = atom_to_binary(Type, utf8),
    bender_client:gen_by_sequence(IdempotentKey, BinType, Hash, WoodyCtx).

-spec make_ctx() ->
    context().

make_ctx() ->
    #{?CTX_NS => #{}}.

-spec add_to_ctx(md(), md() | undefined, context()) ->
    context().

add_to_ctx(_Key, undefined, Context) ->
    Context;
add_to_ctx(Key, Value, Context = #{?CTX_NS := Ctx}) ->
    Context#{?CTX_NS => Ctx#{Key => Value}}.

-spec extend_ctx_from_map(list(), map(), context()) ->
    context().

extend_ctx_from_map(WapiKeys, Params, Context = #{?CTX_NS := Ctx}) ->
    Context#{?CTX_NS => maps:merge(
        Ctx,
        maps:with(WapiKeys, Params)
    )}.

-spec extend_ctx_from_list([{md(), md()}], context()) ->
    context().

extend_ctx_from_list(KVList, Context) ->
    lists:foldl(
        fun({K, V}, Ctx) -> add_to_ctx(K, V, Ctx) end,
        Context,
        KVList
    ).

-spec get_from_ctx(md(), context()) ->
    md().

get_from_ctx(Key, #{?CTX_NS := Ctx}) ->
    maps:get(Key, Ctx, undefined).

-spec create_params_hash(term()) ->
    {binary(), integer()}.

create_params_hash(Value) ->
    {?PARAMS_HASH, erlang:phash2(Value)}.

-spec get_hash(context()) ->
    integer().

get_hash(Context) ->
    #{?CTX_NS := #{?PARAMS_HASH := Value}} = Context,
    Value.

-spec compare_hash(integer(), integer()) ->
    ok |
    {error, conflict_hash}.

compare_hash(Hash, Hash) ->
    ok;
compare_hash(_, _) ->
    {error, conflict_hash}.
