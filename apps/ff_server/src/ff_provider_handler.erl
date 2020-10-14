-module(ff_provider_handler).
-behaviour(ff_woody_wrapper).

-include_lib("fistful_proto/include/ff_proto_provider_thrift.hrl").

%% ff_woody_wrapper callbacks
-export([handle_function/3]).

%%
%% ff_woody_wrapper callbacks
%%
-spec handle_function(woody:func(), woody:args(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function(Func, Args, Opts) ->
    scoper:scope(provider, #{},
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

%%
%% Internals
%%

handle_function_('GetProvider', [ID], _Opts) ->
    ok = scoper:add_meta(#{id => ID}),
    case ff_provider:get(ID) of
        {ok, Provider} ->
            {ok, marshal_provider(Provider)};
        {error, notfound} ->
            woody_error:raise(business, #fistful_ProviderNotFound{})
    end;

handle_function_('ListProviders', _, _Opts) ->
    {ok, marshal_providers(ff_provider:list())}.

%%

-spec marshal_providers([ff_provider:provider()]) ->
    [ff_proto_provider_thrift:'Provider'()].

marshal_providers(Providers) when is_list(Providers) ->
    lists:map(fun(Provider) -> marshal_provider(Provider) end, Providers).

-spec marshal_provider(ff_provider:provider()) ->
    ff_proto_provider_thrift:'Provider'().

marshal_provider(Provider) ->
    ID = ff_provider:id(Provider),
    Name = ff_provider:name(Provider),
    Residences = ff_provider:residences(Provider),
    IdentityClasses = ff_provider:identity_classes(Provider),
    #provider_Provider{
        id = ID,
        name = Name,
        residences = marshal_residences(ordsets:to_list(Residences)),
        identity_classes = marshal_identity_classes(IdentityClasses)
    }.

marshal_residences(List) ->
    lists:map(fun(Residence) -> marshal_residence(Residence) end, List).

marshal_residence(Residence) ->
    genlib_string:to_upper(genlib:to_binary(Residence)).

-spec marshal_identity_classes(ff_provider:identity_classes()) ->
    #{ff_proto_provider_thrift:'IdentityClassID'() => ff_proto_provider_thrift:'IdentityClass'()}.

marshal_identity_classes(Map) ->
    maps:map(fun(_ClassID, Class) -> marshal_identity_class(Class) end, Map).

-spec marshal_identity_class(ff_identity_class:class()) ->
    ff_proto_provider_thrift:'IdentityClass'().

marshal_identity_class(Class) ->
    #provider_IdentityClass{
        id = ff_identity_class:id(Class),
        name = ff_identity_class:name(Class)
    }.
