-module(wapi_backend_utils).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

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
    | p2p_transfer_with_template
    | w2w_transfer.

-export([gen_id/3]).
-export([gen_id/4]).
-export([make_ctx/2]).
-export([add_to_ctx/2]).
-export([add_to_ctx/3]).
-export([get_from_ctx/2]).
-export([get_idempotent_key/3]).
-export([issue_grant_token/3]).
-export([create_params_hash/1]).
-export([decrypt_resource/2]).

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
%    bender_client:gen_snowflake(IdempotentKey, Hash, WoodyCtx).
gen_sequence_id(Type, IdempotentKey, Hash, #{woody_context := WoodyCtx}) ->
    BinType = atom_to_binary(Type, utf8),
    case bender_client:gen_sequence(IdempotentKey, BinType, Hash, WoodyCtx) of
        {ok, {ID, _IntegerID}} -> {ok, ID}; % No need for IntegerID at this project so far
        {error, {external_id_conflict, {ID, _IntegerID}}} -> {error, {external_id_conflict, ID}}
    end.

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

%% @doc
%%
%% The new lechiffre encryption algorithm returns a non-deterministic pcidss token for the same data.
%% The id generation is based on the fact that the params hash is the same.
%% To do this, the token is replaced with its decrypted content.
%%
%% Resource token is contained in the methods createDestination, quoteP2PTransfer, createP2PTransfer,
%% quoteP2PTransferWithTemplate, createP2PTransferWithTemplate - as attribute <<"token">> of parameters resource,
%% sender and receiver
%%

-spec decrypt_resource(binary(), params()) ->
    {ok, params()} | {error, lechiffre:decoding_error()}.

decrypt_resource(Key, Params) ->
    case maps:get(Key, Params, undefined) of
        #{<<"token">> := Token} = Object ->
            case wapi_crypto:decrypt_bankcard_token(Token) of
                {ok, Resource} ->
                    {ok, Params#{Key => Object#{
                        <<"token">> => <<>>,
                        <<"decryptedResource">> => Resource
                    }}};
                unrecognized ->
                    BankCard = wapi_utils:base64url_to_map(Token),
                    Resource = #'BankCard'{
                        token      = maps:get(<<"token">>, BankCard),
                        bin        = maps:get(<<"bin">>, BankCard),
                        masked_pan = maps:get(<<"lastDigits">>, BankCard)
                   },
                   {ok, Params#{Key => Object#{
                       <<"token">> => <<>>,
                       <<"decryptedResource">> => Resource
                   }}};
                Error ->
                    Error
            end;
        _ ->
            {ok, Params}
    end.

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec decrypt_resource_test_() ->
    _.
decrypt_resource_test_() ->
    {setup,
        fun() ->
            Resource = {bank_card, #{bin => <<"424242">>, masked_pan => <<"4242">>}},
            meck:new([wapi_crypto], [passthrough]),
            meck:expect(wapi_crypto, decrypt_bankcard_token, 1, {ok, Resource}),
            Resource
        end,
        fun(_) ->
            meck:unload()
        end,
        fun(Resource) ->
            Params = #{
                <<"hello">> => world,
                <<"sender">> => #{<<"key">> => value, <<"token">> => <<"v1.1A1A1A">>},
                <<"receiver">> => #{<<"key">> => value, <<"token">> => <<"v1.1B1B1B">>},
                <<"resource">> => #{<<"key">> => value, <<"token">> => <<"v1.1C1C1C">>}
            },
            Missed = #{
                <<"hello">> => world,
                <<"sender">> => #{<<"key">> => value, <<"token">> => <<>>, <<"decryptedResource">> => Resource},
                <<"receiver">> => #{<<"key">> => value, <<"token">> => <<>>, <<"decryptedResource">> => Resource},
                <<"resource">> => #{<<"key">> => value, <<"token">> => <<>>, <<"decryptedResource">> => Resource}
            },
            {ok, R1} = decrypt_resource(<<"sender">>, Params),
            {ok, R2} = decrypt_resource(<<"receiver">>, R1),
            {ok, R3} = decrypt_resource(<<"resource">>, R2),
            {ok, Result} = decrypt_resource(<<"cococo">>, R3),
            ?_assertEqual(Missed, Result)
        end
    }.

-endif.
