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

-export([new/0]).
-export([to_list/1]).
-export([from_list/1]).
-export([insert_scope/3]).
-export([remove_scope/3]).

-export([match/2]).

-export([decode/1]).
-export([encode/1]).

%%

-spec new() ->
    t().

new() ->
    [].

-spec to_list(t()) ->
    [{scope(), permission()}].

to_list(ACL) ->
    [{S, P} || {{_, S}, P} <- ACL].

-spec from_list([{scope(), permission()}]) ->
    t().

from_list(L) ->
    lists:foldl(fun ({S, P}, ACL) -> insert_scope(S, P, ACL) end, new(), L).

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

-spec remove_scope(scope(), permission(), t()) ->
    t().

remove_scope(Scope, Permission, ACL) ->
    Priority = compute_priority(Scope, Permission),
    remove({{Priority, Scope}, [Permission]}, ACL).

remove(V, [V | Vs]) ->
    Vs;
remove({PS, Perms}, [{PS, Perms0} | Vs]) ->
    [{PS, Perms0 -- Perms} | Vs];
remove(V, [V0 | Vs]) ->
    [V0 | remove(V, Vs)];
remove(_, []) ->
    [].

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

-spec match(scope(), t()) ->
    [permission()].

match(Scope, ACL) when length(Scope) > 0 ->
    match_rules(Scope, ACL);
match(Scope, _) ->
    error({badarg, {scope, Scope}}).

match_rules(Scope, [{{_Priority, ScopePrefix}, Permissions} | Rest]) ->
    % NOTE
    % The `Scope` matches iff `ScopePrefix` is scope prefix of the `Scope`.
    % An element of a scope matches corresponding element of a scope prefix
    % according to the following rules:
    % 1. Scope prefix element marked with resource and ID matches exactly the same
    %    scope element.
    % 2. Scope prefix element marked with only resource matches any scope element
    %    marked with the same resource.
    case match_scope(Scope, ScopePrefix) of
        true ->
            Permissions;
        false ->
            match_rules(Scope, Rest)
    end;
match_rules(_Scope, []) ->
    [].

match_scope([V | Ss], [V | Ss0]) ->
    match_scope(Ss, Ss0);
match_scope([{V, _ID} | Ss], [V | Ss0]) ->
    match_scope(Ss, Ss0);
match_scope(_, []) ->
    true;
match_scope(_, _) ->
    false.

%%

-spec decode([binary()]) ->
    t().

decode(V) ->
    lists:foldl(fun decode_entry/2, new(), V).

decode_entry(V, ACL) ->
    case binary:split(V, <<":">>, [global]) of
        [V1, V2] ->
            Scope = decode_scope(V1),
            Permission = decode_permission(V2),
            insert_scope(Scope, Permission, ACL);
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

-spec encode(t()) ->
    [binary()].

encode(ACL) ->
    lists:flatmap(fun encode_entry/1, ACL).

encode_entry({{_Priority, Scope}, Permissions}) ->
    S = encode_scope(Scope),
    [begin P = encode_permission(Permission), <<S/binary, ":", P/binary>> end
        || Permission <- Permissions].

encode_scope(Scope) ->
    Hierarchy = get_resource_hierarchy(),
    genlib_string:join($., encode_scope_frags(Scope, Hierarchy)).

encode_scope_frags([{Resource, ID} | Rest], H) ->
    [encode_resource(Resource), ID | encode_scope_frags(Rest, delve(Resource, H))];
encode_scope_frags([Resource], H) ->
    _ = delve(Resource, H),
    [encode_resource(Resource)];
encode_scope_frags([Resource | Rest], H) ->
    [encode_resource(Resource), <<"*">> | encode_scope_frags(Rest, delve(Resource, H))];
encode_scope_frags([], _) ->
    [].

encode_resource(V) ->
    atom_to_binary(V, utf8).

encode_permission(read) ->
    <<"read">>;
encode_permission(write) ->
    <<"write">>.

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
