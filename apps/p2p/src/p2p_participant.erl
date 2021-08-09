-module(p2p_participant).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% In future we can add source&destination  or maybe recurrent
-opaque participant() :: {raw, raw_params()}.

-export_type([participant/0]).
-export_type([contact_info/0]).

-type resource() :: ff_resource:resource().
-type resource_params() :: ff_resource:resource_params().
-type resource_descriptor() :: ff_resource:resource_descriptor().

-type contact_info() :: #{
    phone_number => binary(),
    email => binary()
}.

-type raw_params() :: #{
    resource_params := resource_params(),
    contact_info := contact_info()
}.

-export([create/3]).
-export([get_resource/1]).
-export([get_resource/2]).
-export([contact_info/1]).
-export([resource_params/1]).

-import(ff_pipeline, [do/1, unwrap/1]).

-spec contact_info(participant()) -> contact_info().
contact_info({raw, Raw}) ->
    maps:get(contact_info, Raw).

-spec resource_params(participant()) -> resource_params().
resource_params({raw, Raw}) ->
    maps:get(resource_params, Raw).

-spec create(raw, resource_params(), contact_info()) -> participant().
create(raw, ResourceParams, ContactInfo) ->
    {raw, #{
        resource_params => ResourceParams,
        contact_info => ContactInfo
    }}.

-spec get_resource(participant()) ->
    {ok, resource()}
    | {error, {bin_data, ff_bin_data:bin_data_error()}}.
get_resource(Participant) ->
    get_resource(Participant, undefined).

-spec get_resource(participant(), resource_descriptor() | undefined) ->
    {ok, resource()}
    | {error, {bin_data, ff_bin_data:bin_data_error()}}.
get_resource({raw, #{resource_params := ResourceParams}}, ResourceDescriptor) ->
    do(fun() ->
        unwrap(ff_resource:create_resource(ResourceParams, ResourceDescriptor))
    end).
