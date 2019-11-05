-module(p2p_participant).

-import(ff_pipeline, [do/1, unwrap/1]).

-type contact_info() :: #{
    phone_number => binary(),
    email        => binary()
}.

-type participant_type() :: resource.
%% In future we can add source&destination
-opaque participant() :: {resource, #{
                            instrument   := p2p_instrument:instrument(),
                            contact_info := contact_info()}
                        }.

-export_type([contact_info/0]).
-export_type([participant/0]).

-export([create/2, create/3]).
-export([instrument/1]).
-export([contact_info/1]).
-export([get_full_instrument/1]).

-spec instrument(participant()) ->
    p2p_instrument:instrument().
instrument({resource, Resource}) ->
    maps:get(instrument, Resource).

-spec contact_info(participant()) ->
    contact_info().
contact_info({resource, Resource}) ->
    maps:get(contact_info, Resource).

-spec create(participant_type(), p2p_instrument:bank_card()) ->
    participant().
create(resource, BankCard) ->
    {resource, #{
        instrument => p2p_instrument:create(BankCard),
        contact_info => #{}
    }}.

-spec create(participant_type(), p2p_instrument:bank_card(), contact_info()) ->
    participant().
create(resource, BankCard, ContactInfo) ->
    {resource, #{
        instrument => p2p_instrument:create(BankCard),
        contact_info => ContactInfo
    }}.

-spec get_full_instrument(participant()) ->
    {ok, participant()} | {error, {bin_data, not_found}}.
get_full_instrument({resource, _Resource} = Participant) ->
    do(fun() ->
        InstrumentFull = unwrap(p2p_instrument:extend(instrument(Participant))),
        {resource, #{
            instrument   => InstrumentFull,
            contact_info => contact_info(Participant)
        }}
    end);
get_full_instrument(Other) ->
    error({get_full_instrument, {not_impl, Other}}).
