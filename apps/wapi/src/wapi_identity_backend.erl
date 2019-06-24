-module(wapi_identity_backend).

% -type req_data()        :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data()   :: wapi_handler:response_data().
-type params()          :: map().
-type id()              :: binary().
-type result(T, E)      :: {ok, T} | {error, E}.

-export([create/2]).
-export([get/2]).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

%% Pipeline

-spec get(binary(), handler_context()) ->
    {ok, response_data()}             |
    {error, {identity, notfound}}     |
    {error, {identity, unauthorized}} .

get(IdentityID, WoodyContext) ->
    Request = {fistful_identity, 'Get', [IdentityID]},
    case service_call(Request, WoodyContext) of
        {ok, IdentityThrift} ->
            case wapi_access_backend:check_resource(identity, IdentityThrift, WoodyContext) of
                ok ->
                    {ok, unmarshal(identity, IdentityThrift)};
                {error, unauthorized} ->
                    {error, {identity, unauthorized}}
            end;
        {exception, #fistful_IdentityNotFound{}} ->
            {error, {identity, notfound}}
    end.

-spec create(params(), handler_context()) -> result(map(),
    {provider, notfound}       |
    {identity_class, notfound} |
    {conflict, id()}           |
    inaccessible               |
    _Unexpected
).
create(Params, WoodyContext) ->
    ID = create_id(Params, WoodyContext),
    IdentityParams = marshal(identity_params, compose_identity_params(Params, WoodyContext)),
    Request = {fistful_identity, 'Create', [ID, IdentityParams]},

    %% FIXME we can`t get #'payproc_PartyNotFound' here, so we can`t create party if need it

    case service_call(Request, WoodyContext) of
        {ok, Identity} ->
            {ok, unmarshal(identity, Identity)};
        {exception, #fistful_ProviderNotFound{}} ->
            {error, {provider, notfound}};
        {exception, #fistful_IdentityClassNotFound{}} ->
            {error, {identity_class, notfound}};
        {exception, #fistful_PartyInaccessible{}} ->
            {error, inaccessible};
        {exception, #fistful_IDExists{}} ->
            {_, Hash} = wapi_backend_utils:create_params_hash(Params),
            get_and_compare_hash(ID, Hash, WoodyContext);
        {exception, Details} ->
            {error, Details}
    end.

%%
%% Internal
%%

get_and_compare_hash(ID, Hash, WoodyContext) ->
    Request = {fistful_identity, 'Get', [ID]},
    {ok, Identity} = service_call(Request, WoodyContext),
    case wapi_backend_utils:compare_hash(Hash, get_hash(Identity)) of
        ok ->
            {ok, unmarshal(identity, Identity)};
        {error, conflict_hash} ->
            {error, {conflict, ID}}
    end.

get_hash(#idnt_Identity{context = Ctx}) ->
    wapi_backend_utils:get_hash(unmarshal(context, Ctx)).

compose_identity_params(ParamsIn, WoodyContext) ->
    genlib_map:compact(ParamsIn#{
        <<"owner">>   => wapi_handler_utils:get_owner(WoodyContext),
        <<"context">> => create_context(ParamsIn, WoodyContext)
    }).

create_id(ParamsIn, WoodyContext) ->
    wapi_backend_utils:make_id(
        identity,
        wapi_backend_utils:construct_external_id(ParamsIn, WoodyContext)
    ).

create_context(ParamsIn, WoodyContext) ->
    Hash = wapi_backend_utils:create_params_hash(ParamsIn),
    List = [
        {<<"name">>, maps:get(<<"name">>, ParamsIn, undefined)},
        {<<"owner">>, wapi_handler_utils:get_owner(WoodyContext)},
        {<<"metadata">>, maps:get(<<"metadata">>, ParamsIn, undefined)},
        Hash
    ],
    wapi_backend_utils:extend_ctx_from_list(List, wapi_backend_utils:make_ctx()).

service_call(Params, Ctx) ->
    wapi_handler_utils:service_call(Params, Ctx).

marshal(identity_params, Params = #{
    <<"provider">>  := Provider,
    <<"class">>     := Class,
    <<"owner">>     := Owner,
    <<"context">>   := Context
}) ->
    ExternalID = maps:get(<<"externalID">>, Params, undefined),
    #idnt_IdentityParams{
        party = marshal(id, Owner),
        provider = marshal(string, Provider),
        cls = marshal(string, Class),
        external_id = marshal(id, ExternalID),
        context = marshal(context, Context)
    };

marshal(context, Ctx) ->
    ff_context:wrap(Ctx);

marshal(T, V) ->
    ff_codec:marshal(T, V).

unmarshal(identity, #idnt_Identity{
    id          = IdentityID,
    blocked     = Blocked,
    cls         = Class,
    provider    = Provider,
    level       = Level,
    effective_challenge = EffectiveChallenge,
    external_id = ExternalID,
    context     = Ctx
}) ->
    Context = unmarshal(context, Ctx),
    genlib_map:compact(#{
        <<"id">>                    => unmarshal(id, IdentityID),
        <<"name">>                  => wapi_backend_utils:get_from_ctx(<<"name">>, Context),
        %% TODO add createdAt to proto struct
        % <<"createdAt">>           => unmarshal(timestamp, CreatedAt),
        <<"isBlocked">>             => maybe_unmarshal(blocked, Blocked),
        <<"class">>                 => unmarshal(string, Class),
        <<"provider">>              => unmarshal(id, Provider),
        <<"level">>                 => maybe_unmarshal(id, Level),
        <<"effectiveChallenge">>    => maybe_unmarshal(id, EffectiveChallenge),
        <<"externalID">>            => maybe_unmarshal(id, ExternalID),
        <<"metadata">>              => wapi_backend_utils:get_from_ctx(<<"metadata">>, Context)
    });

unmarshal(blocked, false) ->
    false;
unmarshal(blocked, true) ->
    true;

unmarshal(context, Ctx) ->
    ff_context:unwrap(Ctx);

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

maybe_unmarshal(_, undefined) ->
    undefined;
maybe_unmarshal(T, V) ->
    unmarshal(T, V).
