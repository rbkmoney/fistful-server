-module(wapi_provider_backend).

-include_lib("fistful_proto/include/ff_proto_provider_thrift.hrl").

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
get_providers(Residences, HandlerContext) ->
    ResidenceSet = ordsets:from_list(Residences),
    Request = {fistful_provider, 'ListProviders', []},
    {ok, Providers} = wapi_handler_utils:service_call(Request, HandlerContext),
    [P ||
        P <- unmarshal_providers(Providers),
        ordsets:is_subset(
            ResidenceSet,
            ordsets:from_list(maps:get(<<"residences">>, P))
        )
    ].

-spec get_provider(id(), handler_context()) -> {ok, response_data()} | {error, notfound}.
get_provider(ProviderID, HandlerContext) ->
    case get_provider_thrift(ProviderID, HandlerContext) of
        {ok, Provider} ->
            {ok, unmarshal_provider(Provider)};
        {error, _} = Error ->
            Error
    end.

-spec get_provider_identity_classes(id(), handler_context()) -> {ok, response_data()} | {error, notfound}.
get_provider_identity_classes(ProviderID, HandlerContext) ->
    do(fun() ->
        Provider = unwrap(get_provider_thrift(ProviderID, HandlerContext)),
        lists:map(
            fun(ClassID) -> get_provider_identity_class(ClassID, Provider) end,
            list_identity_classes(Provider)
        )
    end).

-spec get_provider_identity_class(id(), id(), handler_context()) -> {ok, response_data()} | {error, notfound}.
get_provider_identity_class(ProviderID, ClassID, HandlerContext) ->
    do(fun() ->
        Provider = unwrap(get_provider_thrift(ProviderID, HandlerContext)),
        get_provider_identity_class(ClassID, Provider)
    end).

get_provider_identity_class(ClassID, Provider) ->
    unmarshal_identity_class(unwrap(get_identity_class(ClassID, Provider))).

-spec get_provider_identity_class_levels(id(), id(), handler_context()) -> no_return().
get_provider_identity_class_levels(_ProviderID, _ClassID, _HandlerContext) ->
    not_implemented().

-spec get_provider_identity_class_level(id(), id(), id(), handler_context()) -> no_return().
get_provider_identity_class_level(_ProviderID, _ClassID, _LevelID, _HandlerContext) ->
    not_implemented().

%% Internal

get_provider_thrift(ProviderID, HandlerContext) ->
    Request = {fistful_provider, 'GetProvider', [ProviderID]},
    case wapi_handler_utils:service_call(Request, HandlerContext) of
        {ok, _} = Result ->
            Result;
        {exception, #fistful_ProviderNotFound{}} ->
            {error, notfound}
    end.

list_identity_classes(#provider_Provider{identity_classes = IdentityClasses}) ->
    maps:keys(IdentityClasses).

get_identity_class(IdentityClassID, #provider_Provider{identity_classes = IdentityClasses}) ->
    ff_map:find(IdentityClassID, IdentityClasses).

-spec not_implemented() -> no_return().
not_implemented() ->
    wapi_handler_utils:throw_not_implemented().

%% Marshaling

unmarshal_providers(List) ->
    lists:map(fun(Provider) -> unmarshal_provider(Provider) end, List).

unmarshal_provider(#provider_Provider{
    id = ID,
    name = Name,
    residences = Residences
}) ->
    genlib_map:compact(#{
       <<"id">> => ID,
       <<"name">> => Name,
       <<"residences">> => Residences
     }).

unmarshal_identity_class(#provider_IdentityClass{
    id = ID,
    name = Name
}) ->
    genlib_map:compact(#{
        <<"id">> => ID,
        <<"name">> => Name
    }).
