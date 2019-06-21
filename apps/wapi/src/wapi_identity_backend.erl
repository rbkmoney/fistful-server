-module(wapi_identity_backend).

% -type req_data()        :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data()   :: wapi_handler:response_data().

-export([get/2]).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

%% Pipeline
-import(ff_pipeline, [do/1]).

-spec get(binary(), handler_context()) ->
    {ok, response_data()}             |
    {error, {identity, notfound}}     |
    {error, {identity, unauthorized}} .

get(IdentityID, WoodyContext) ->
    Request = {fistful_identity, 'Get', [IdentityID]},
    do(fun() ->
        case service_call(Request, WoodyContext) of
            {ok, IdentityThrift} ->
                ok = wapi_access_backend:check_resource(identity, IdentityThrift, WoodyContext),
                unmarshal(identity, IdentityThrift);
            {exception, #fistful_IdentityNotFound{}} ->
                throw({identity, notfound})
        end
    end).

%%
%% Internal
%%

service_call(Params, Ctx) ->
    wapi_handler_utils:service_call(Params, Ctx).

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
        <<"isBlocked">>             => unmarshal(blocked, Blocked),
        <<"class">>                 => unmarshal(string, Class),
        <<"provider">>              => unmarshal(id, Provider),
        <<"level">>                 => unmarshal(id, Level),
        <<"effectiveChallenge">>    => unmarshal(id, EffectiveChallenge),
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
