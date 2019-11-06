-module(wapi_acl).

%%

-opaque t()           :: [{{priority(), scope()}, [permission()]}].

-type priority()      :: integer().
-type scope()         :: [resource() | {resource(), resource_id()}, ...].
-type resource()      :: atom().
-type resource_id()   :: binary().
-type permission()    :: read | write.

-export_type([t/0]).
-export_type([scope/0]).
-export_type([permission/0]).

-export([decode/1]).

%%
-spec new() ->
    t().

new() ->
    [].

-spec insert_scope(scope(), permission(), t()) ->
    t().

insert_scope(Scope, Permission, ACL) ->
    Priority = compute_priority(Scope, Permission),
    insert({{Priority, Scope}, [Permission]}, ACL).

insert({PS, _} = V, [{PS0, _} = V0 | Vs]) when PS < PS0 ->
    [V0 | insert(V, Vs)];
insert({PS, Perms}, [{PS, Perms0} | Vs]) ->
    % NOTE squashing permissions of entries with the same scope
    [{PS, lists:usort(Perms ++ Perms0)} | Vs];
insert({PS, _} = V, [{PS0, _} | _] = Vs) when PS > PS0 ->
    [V | Vs];
insert(V, []) ->
    [V].

compute_priority(Scope, Permission) ->
    % NOTE
    % Scope priority depends on the following attributes, in the order of decreasing
    % importance:
    % 1. Depth, deeper is more important
    % 2. Scope element specificity, element marked with an ID is more important
    compute_scope_priority(Scope) + compute_permission_priority(Permission).

compute_scope_priority(Scope) when length(Scope) > 0 ->
    compute_scope_priority(Scope, get_resource_hierarchy(), 0);
compute_scope_priority(Scope) ->
    error({badarg, {scope, Scope}}).

compute_scope_priority([{Resource, _ID} | Rest], H, P) ->
    compute_scope_priority(Rest, delve(Resource, H), P * 10 + 2);
compute_scope_priority([Resource | Rest], H, P) ->
    compute_scope_priority(Rest, delve(Resource, H), P * 10 + 1);
compute_scope_priority([], _, P) ->
    P * 10.

compute_permission_priority(read) ->
    0;
compute_permission_priority(write) ->
    0;
compute_permission_priority(V) ->
    error({badarg, {permission, V}}).

%%

-spec decode([binary()]) ->
    t().

decode(V) ->
    lists:foldl(fun decode_entry/2, new(), V).

%% TODO
%%  - Keycloak utilizes string ACLs and so do we for now. Nicer way to handle ACLs
%%    is to use json instead for wapi issued tokens. That would require providing
%%    similar routines for ACL normalization as we have for string ACLs.
decode_entry(V, ACL) ->
    case binary:split(V, <<":">>, [global]) of
        [V1, V2] ->
            %% Skip entries, which are not in wapi hierarchy
            try
                Scope = decode_scope(V1),
                Permission = decode_permission(V2),
                insert_scope(Scope, Permission, ACL)
            catch
                error:{badarg, {resource, _}} -> ACL
            end;
        _ ->
            error({badarg, {role, V}})
    end.

decode_scope(V) ->
    Hierarchy = get_resource_hierarchy(),
    decode_scope_frags(binary:split(V, <<".">>, [global]), Hierarchy).

decode_scope_frags([V1, V2 | Vs], H) ->
    {Resource, H1} = decode_scope_frag_resource(V1, V2, H),
    [Resource | decode_scope_frags(Vs, H1)];
decode_scope_frags([V], H) ->
    decode_scope_frags([V, <<"*">>], H);
decode_scope_frags([], _) ->
    [].

decode_scope_frag_resource(V, <<"*">>, H) ->
    R = decode_resource(V),
    {R, delve(R, H)};
decode_scope_frag_resource(V, ID, H) ->
    R = decode_resource(V),
    {{R, ID}, delve(R, H)}.

decode_resource(V) ->
    try binary_to_existing_atom(V, utf8) catch
        error:badarg ->
            error({badarg, {resource, V}})
    end.

decode_permission(<<"read">>) ->
    read;
decode_permission(<<"write">>) ->
    write;
decode_permission(V) ->
    error({badarg, {permission, V}}).

%%

get_resource_hierarchy() ->
    wapi_auth:get_resource_hierarchy().

delve(Resource, Hierarchy) ->
    case maps:find(Resource, Hierarchy) of
        {ok, Sub} ->
            Sub;
        error ->
            error({badarg, {resource, Resource}})
    end.
