-module(wapi_backend_utils).

-define(EXTERNAL_ID, <<"externalID">>).
-define(CTX_NS, <<"com.rbkmoney.wapi">>).


-export([make_id/1, make_id/2]).
-export([construct_external_id/2]).
-export([make_ctx/0]).
-export([add_to_ctx/3]).
-export([extend_ctx_from_map/3]).
-export([extend_ctx_from_list/2]).

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

% make_ctx(Context) ->
%     #{?CTX_NS => #{<<"owner">> => wapi_handler_utils:get_owner(Context)}}.
% do(Fun) ->
%     ff_pipeline:do(Fun).

unwrap(Res) ->
    ff_pipeline:unwrap(Res).

% unwrap(Tag, Res) ->
%     ff_pipeline:unwrap(Tag, Res).