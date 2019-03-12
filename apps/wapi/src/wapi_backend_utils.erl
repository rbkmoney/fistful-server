-module(wapi_backend_utils).

-define(EXTERNAL_ID, <<"externalID">>).
-define(CTX_NS,      <<"com.rbkmoney.wapi">>).
-define(PARAMS_HASH, <<"params_hash">>).

-export([make_id/1, make_id/2]).
-export([construct_external_id/2]).
-export([make_ctx/0]).
-export([add_to_ctx/3]).
-export([get_from_ctx/2]).
-export([extend_ctx_from_map/3]).
-export([extend_ctx_from_list/2]).
-export([create_params_hash/1]).
-export([get_and_compare_hash/3]).

%% Pipeline
-import(ff_pipeline, [do/1, unwrap/1]).

-type context() :: #{namespace() => md()}.

-type namespace() :: binary().
-type md()        :: %% as stolen from `machinery_msgpack`
    nil                |
    boolean()          |
    integer()          |
    float()            |
    binary()           | %% string
    {binary, binary()} | %% binary
    [md()]             |
    #{md() => md()}    .

-spec make_id(atom()) ->
    binary().

make_id(Type) ->
    make_id(Type, undefined).

-spec make_id(atom(), binary() | undefined) ->
    binary().

make_id(Type, ExternalID) ->
    unwrap(ff_external_id:check_in(Type, ExternalID)).

-spec construct_external_id(any(), any()) ->
    binary() | undefined.

construct_external_id(Params, Context) ->
    case genlib_map:get(?EXTERNAL_ID, Params) of
        undefined ->
            undefined;
        ExternalID ->
            PartyID = wapi_handler_utils:get_owner(Context),
            <<PartyID/binary, "/", ExternalID/binary>>
    end.

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
        KVList).

-spec get_from_ctx(md(), context()) ->
    any().

get_from_ctx(Key, #{?CTX_NS := Ctx}) ->
    maps:get(Key, Ctx, undefined).

% make_ctx(Context) ->
%     #{?CTX_NS => #{<<"owner">> => wapi_handler_utils:get_owner(Context)}}.

-spec create_params_hash(term()) ->
    {string(), integer()}.

create_params_hash(Value) ->
    {?PARAMS_HASH, erlang:phash2(Value)}.

get_hash(Context) ->
    #{?CTX_NS := #{?PARAMS_HASH := Value}} = Context,
    Value.

-spec get_and_compare_hash(function(), list(), integer()) ->
    {ok, term()} |
    {error, {conflict_hash, _}} |
    _Error.

get_and_compare_hash(Fun, Args, Hash) ->
    case do(fun() -> Fun(Args) end) of
        {ok, Entity} ->
            compare_hash(Hash, get_hash(Entity), Entity);
        Error ->
            Error
    end.

compare_hash(Hash, Hash, Entity) ->
    {ok, Entity};
compare_hash(_, _, Entity) ->
    {error, {conflict_hash, Entity}}.