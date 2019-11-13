-module(p2p_participant).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% In future we can add source&destination  or maybe recurrent
-opaque participant() :: {resource, disposable_resource_params()}.

-export_type([participant/0]).

-type disposable_resource_params() :: ff_resource:disposable_resource_params().
-type disposable_resource() :: ff_resource:disposable_resource().
-type resource_params() :: ff_resource:resource_params().

-export([create/2]).
-export([create/3]).
-export([get_disposable_resource/1]).

-import(ff_pipeline, [do/1, unwrap/1]).

-spec create(resource, resource_params()) ->
    participant().
create(resource, Params) ->
    {resource, #{params => Params}}.

-spec create(resource, resource_params(), ff_resource:contact_info()) ->
    participant().
create(resource, Params, ContactInfo) ->
    {resource, #{
        params => Params,
        contact_info => ContactInfo
    }}.

-spec get_disposable_resource(participant()) ->
    {ok, disposable_resource()} |
    {error, {bin_data, not_found}}.
get_disposable_resource({resource, Params}) ->
    do(fun() ->
        unwrap(ff_resource:create_disposable_resource(Params))
    end).
