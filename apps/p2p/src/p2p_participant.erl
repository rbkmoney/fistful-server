-module(p2p_participant).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type contact_info() :: #{
    phone_number => binary(),
    email        => binary()
}.

%% In future we can add source&destination  or maybe recurrent
-opaque participant() :: {resource, #{
                            resource     := ff_resource:resource(),
                            contact_info := contact_info()
                        }}.

-export_type([contact_info/0]).
-export_type([participant/0]).

-export([create/2]).
-export([resource/1]).
-export([contact_info/1]).
-export([get_disposable_resource/1]).

-import(ff_pipeline, [do/1, unwrap/1]).


-spec resource(participant()) ->
    ff_resource:resource().
resource({resource, P}) ->
    maps:get(resource, P).

-spec contact_info(participant()) ->
    contact_info().
contact_info({resource, Resource}) ->
    maps:get(contact_info, Resource).

-spec create(resource, {bank_card, ff_resource:resource()}) ->
    participant().
create(resource, {bank_card, _} = BankCardResource) ->
    create(resource, BankCardResource, #{}).

-spec create(resource, {bank_card, ff_resource:resource()}, contact_info()) ->
    participant().
create(resource, BankCard, ContactInfo) ->
    {resource, #{
        resource => BankCard,
        contact_info => ContactInfo
    }}.

-spec get_disposable_resource(participant()) ->
    {ok, ff_resource:disposable_resource()} |
    {error, {bin_data, not_found}} | no_return().
get_disposable_resource({resource, _Resource} = Participant) ->
    do(fun() ->
        ResourceFull = unwrap(ff_resource:resource_full(resource(Participant))),
        ff_resource:create_disposable_resource(ResourceFull)
    end);
get_disposable_resource(Participant) ->
    error({get_disposable_resource, {not_impl, Participant}}).
