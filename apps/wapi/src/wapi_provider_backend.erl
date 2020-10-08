-module(wapi_provider_backend).

-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().
-type id() :: binary().

-export([get_providers/2]).
-export([get_provider/2]).
-export([get_provider_identity_classes/2]).
-export([get_provider_identity_class/3]).
-export([get_provider_identity_class_levels/3]).
-export([get_provider_identity_class_level/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

-spec get_providers([binary()], handler_context()) -> [map()].
get_providers(Residences, _Context) ->
    ResidenceSet = ordsets:from_list(marshal_residences(Residences)),
    unmarshal_providers([P ||
        P <- ff_provider:list(),
        ordsets:is_subset(
            ResidenceSet,
            ordsets:from_list(ff_provider:residences(P))
        )
    ]).

-spec get_provider(id(), handler_context()) -> {ok, response_data()} | {error, notfound}.
get_provider(ProviderId, _Context) ->
    do(fun() -> unmarshal_provider(unwrap(ff_provider:get(ProviderId))) end).

-spec get_provider_identity_classes(id(), handler_context()) -> {ok, response_data()} | {error, notfound}.
get_provider_identity_classes(Id, _Context) ->
    do(fun() ->
        Provider = unwrap(ff_provider:get(Id)),
        lists:map(
            fun(ClassId) -> get_provider_identity_class(ClassId, Provider) end,
            ff_provider:list_identity_classes(Provider)
        )
    end).

-spec get_provider_identity_class(id(), id(), handler_context()) -> {ok, response_data()} | {error, notfound}.
get_provider_identity_class(ProviderId, ClassId, _Context) ->
    do(fun() -> get_provider_identity_class(ClassId, unwrap(ff_provider:get(ProviderId))) end).

get_provider_identity_class(ClassId, Provider) ->
    unmarshal_identity_class(unwrap(ff_provider:get_identity_class(ClassId, Provider))).

-spec get_provider_identity_class_levels(id(), id(), handler_context()) -> no_return().
get_provider_identity_class_levels(_ProviderId, _ClassId, _Context) ->
    not_implemented().

-spec get_provider_identity_class_level(id(), id(), id(), handler_context()) -> no_return().
get_provider_identity_class_level(_ProviderId, _ClassId, _LevelId, _Context) ->
    not_implemented().

%% Internal

-spec not_implemented() -> no_return().
not_implemented() ->
    wapi_handler_utils:throw_not_implemented().

%% Marshaling

marshal_residences(List) ->
    lists:map(fun(Residence) -> marshal_residence(Residence) end, List).

marshal_residence(V) ->
    try erlang:binary_to_existing_atom(genlib_string:to_lower(V), latin1) catch
        error:badarg ->
            % TODO
            %  - Essentially this is incorrect, we should reply with 400 instead
            undefined
    end.

%%

unmarshal_providers(List) ->
    lists:map(fun(Provider) -> unmarshal_provider(Provider) end, List).

unmarshal_provider(Provider) ->
    genlib_map:compact(#{
       <<"id">> => ff_provider:id(Provider),
       <<"name">> => ff_provider:name(Provider),
       <<"residences">> => unmarshal_residences(ordsets:to_list(ff_provider:residences(Provider)))
     }).

unmarshal_residences(List) ->
    lists:map(fun(Residence) -> unmarshal_residence(Residence) end, List).

unmarshal_residence(Residence) ->
    genlib_string:to_upper(genlib:to_binary(Residence)).

unmarshal_identity_class(Class) ->
    genlib_map:compact(maps:with([id, name], Class)).
