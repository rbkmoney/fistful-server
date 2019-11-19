-module(p2p_participant).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% In future we can add source&destination  or maybe recurrent
-opaque participant() :: {raw, raw_params()}.

-export_type([participant/0]).

-type resource() :: ff_resource:resource().
-type resource_id() :: ff_resource:resource_id().
-type resource_params() :: ff_resource:resource_params().

-type contact_info() :: #{
    phone_number => binary(),
    email => binary()
}.

-type raw_params() :: #{
    resource_params := resource_params(),
    contact_info => contact_info()
}.

-export([create/2]).
-export([create/3]).
-export([get_resource/1]).
-export([contact_info/1]).

-import(ff_pipeline, [do/1, unwrap/1]).

-spec contact_info(participant()) ->
    contact_info() | undefined.
contact_info({raw, Raw}) ->
    maps:get(contact_info, Raw, undefined).

-spec create(raw, resource_params()) ->
    participant().
create(raw, ResourceParams) ->
    {raw, #{resource_params => ResourceParams}}.

-spec create(raw, resource_params(), contact_info()) ->
    participant().
create(raw, ResourceParams, ContactInfo) ->
    {raw, #{
        resource_params => ResourceParams,
        contact_info => ContactInfo
    }}.

-spec get_resource(participant()) ->
    {ok, resource()} |
    {error, {bin_data, not_found}}.
get_resource(Participant) ->
    get_resource(Participant, undefined).

-spec get_resource(participant(), resource_id() | undefined) ->
    {ok, resource()} |
    {error, {bin_data, not_found}}.
get_resource({raw, #{resource_params := ResourceParams}}, ResourceID) ->
    do(fun() ->
        unwrap(ff_resource:create_resource(ResourceParams, ResourceID))
    end).
