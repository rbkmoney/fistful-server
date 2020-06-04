-module(wapi_backend_utils).

-define(EXTERNAL_ID, <<"externalID">>).
-define(CTX_NS, <<"com.rbkmoney.wapi">>).
-define(PARAMS_HASH, <<"params_hash">>).
-define(BENDER_DOMAIN, <<"wapi">>).

%% Context
-type md() :: ff_entity_context:md().
-type context() :: ff_entity_context:context().
-type handler_context() :: wapi_handler:context().
-type id() :: binary().
-type hash() :: integer().
-type params() :: map().
-type gen_type() ::
      identity
    | identity_challenge
    | wallet
    | destination
    | withdrawal
    | p2p_transfer
    | p2p_template
    | w2w_transfer.

-export([gen_id/3]).
-export([gen_id/4]).
-export([make_ctx/2]).
-export([add_to_ctx/2]).
-export([add_to_ctx/3]).
-export([get_from_ctx/2]).
-export([get_idempotent_key/3]).
-export([issue_grant_token/3]).

%% Pipeline

-spec get_idempotent_key(gen_type(), id(), id() | undefined) ->
    binary().

get_idempotent_key(Type, PartyID, ExternalID) ->
    bender_client:get_idempotent_key(?BENDER_DOMAIN, Type, PartyID, ExternalID).

-spec gen_id(gen_type(), params(), handler_context()) ->
    {ok, id()} | {error, {external_id_conflict, id()}}.

gen_id(Type, Params, Context) ->
    ExternalID = maps:get(?EXTERNAL_ID, Params, undefined),
    Hash = create_params_hash(Params),
    gen_id(Type, ExternalID, Hash, Context).

-spec gen_id(gen_type(), id() | undefined, hash(), handler_context()) ->
    {ok, id()} | {error, {external_id_conflict, id()}}.

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

-spec make_ctx(params(), handler_context()) ->
    context().

make_ctx(Params, Context) ->
    #{?CTX_NS => genlib_map:compact(#{
        <<"owner">> => wapi_handler_utils:get_owner(Context),
        <<"metadata">> => maps:get(<<"metadata">>, Params, undefined),
        ?PARAMS_HASH => create_params_hash(Params)
    })}.

-spec add_to_ctx({md(), md() | undefined} | list() | map(), context()) ->
    context().

add_to_ctx({Key, Value}, Context) ->
    add_to_ctx(Key, Value, Context);
add_to_ctx(Map, Context = #{?CTX_NS := Ctx}) when is_map(Map) ->
    Context#{?CTX_NS => maps:merge(Ctx, Map)};
add_to_ctx(KVList, Context) when is_list(KVList) ->
    lists:foldl(
        fun({K, V}, Ctx) -> add_to_ctx(K, V, Ctx) end,
        Context,
        KVList
    ).

-spec add_to_ctx(md(), md() | undefined, context()) ->
    context().

add_to_ctx(_Key, undefined, Context) ->
    Context;
add_to_ctx(Key, Value, Context = #{?CTX_NS := Ctx}) ->
    Context#{?CTX_NS => Ctx#{Key => Value}}.

-spec get_from_ctx(md(), context()) ->
    md().

get_from_ctx(Key, #{?CTX_NS := Ctx}) ->
    maps:get(Key, Ctx, undefined).

-spec create_params_hash(term()) ->
    integer().

create_params_hash(Value) ->
    erlang:phash2(Value).

-spec issue_grant_token(_, binary(), handler_context()) ->
    {ok, binary()} | {error, expired}.

issue_grant_token(TokenSpec, Expiration, Context) ->
    case get_expiration_deadline(Expiration) of
        {ok, Deadline} ->
            {ok, wapi_auth:issue_access_token(wapi_handler_utils:get_owner(Context), TokenSpec, {deadline, Deadline})};
        Error = {error, _} ->
            Error
    end.

get_expiration_deadline(Expiration) ->
    Deadline = genlib_rfc3339:parse(Expiration, second),
    case genlib_time:unow() - Deadline < 0 of
        true ->
            {ok, Deadline};
        false ->
            {error, expired}
    end.