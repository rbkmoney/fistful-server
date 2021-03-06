-module(ff_p2p_template_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_p2p_template_thrift.hrl").

-export([marshal_p2p_template_state/2]).
-export([marshal_p2p_transfer_state/2]).
-export([unmarshal_p2p_template_params/1]).
-export([unmarshal_p2p_quote_params/1]).
-export([unmarshal_p2p_transfer_params/1]).
-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal_p2p_template_state(p2p_template:template_state(), ff_entity_context:context()) ->
    ff_proto_p2p_template_thrift:'P2PTemplateState'().
marshal_p2p_template_state(P2PTemplate, Ctx) ->
    #p2p_template_P2PTemplateState{
        id = marshal(id, p2p_template:id(P2PTemplate)),
        identity_id = marshal(id, p2p_template:identity_id(P2PTemplate)),
        domain_revision = marshal(domain_revision, p2p_template:domain_revision(P2PTemplate)),
        party_revision = marshal(party_revision, p2p_template:party_revision(P2PTemplate)),
        created_at = marshal(timestamp_ms, p2p_template:created_at(P2PTemplate)),
        template_details = marshal(details, p2p_template:details(P2PTemplate)),
        blocking = maybe_marshal(blocking, p2p_template:blocking(P2PTemplate)),
        external_id = marshal(id, p2p_template:external_id(P2PTemplate)),
        context = marshal(ctx, Ctx)
    }.

-spec marshal_p2p_transfer_state(p2p_transfer:p2p_transfer_state(), ff_entity_context:context()) ->
    ff_proto_p2p_transfer_thrift:'P2PTransferState'().
marshal_p2p_transfer_state(P2PTransfer, Ctx) ->
    ff_p2p_transfer_codec:marshal_p2p_transfer_state(P2PTransfer, Ctx).

-spec unmarshal_p2p_template_params(ff_proto_p2p_template_thrift:'P2PTemplateParams'()) ->
    p2p_template_machine:params().
unmarshal_p2p_template_params(#p2p_template_P2PTemplateParams{
    id = ID,
    identity_id = IdentityID,
    template_details = Details,
    external_id = ExternalID
}) ->
    genlib_map:compact(#{
        id => unmarshal(id, ID),
        identity_id => unmarshal(id, IdentityID),
        details => unmarshal(details, Details),
        external_id => maybe_unmarshal(id, ExternalID)
    }).

-spec unmarshal_p2p_quote_params(ff_proto_p2p_template_thrift:'P2PTemplateQuoteParams'()) ->
    p2p_template:quote_params().
unmarshal_p2p_quote_params(#p2p_template_P2PTemplateQuoteParams{
    sender = Sender,
    receiver = Receiver,
    body = Body
}) ->
    #{
        body => unmarshal(cash, Body),
        sender => unmarshal(resource, Sender),
        receiver => unmarshal(resource, Receiver)
    }.

-spec unmarshal_p2p_transfer_params(ff_proto_p2p_template_thrift:'P2PTemplateTransferParams'()) ->
    p2p_template:transfer_params().
unmarshal_p2p_transfer_params(#p2p_template_P2PTemplateTransferParams{
    id = ID,
    sender = Sender,
    receiver = Receiver,
    client_info = ClientInfo,
    body = Body,
    quote = Quote,
    metadata = Metadata,
    deadline = Deadline
}) ->
    genlib_map:compact(#{
        id => unmarshal(id, ID),
        sender => unmarshal(participant, Sender),
        receiver => unmarshal(participant, Receiver),
        client_info => maybe_unmarshal(client_info, ClientInfo),
        body => unmarshal(cash, Body),
        quote => maybe_unmarshal(quote, Quote),
        metadata => maybe_unmarshal(ctx, Metadata),
        deadline => maybe_unmarshal(timestamp_ms, Deadline)
    }).

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) -> ff_codec:encoded_value().
marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];
marshal(timestamped_change, {ev, Timestamp, Change}) ->
    #p2p_template_TimestampedChange{
        change = marshal(change, Change),
        occured_at = ff_codec:marshal(timestamp, Timestamp)
    };
marshal(change, {created, Template}) ->
    {created, #p2p_template_CreatedChange{p2p_template = marshal(template, Template)}};
marshal(change, {blocking_changed, Blocking}) ->
    {blocking_changed, #p2p_template_BlockingChange{blocking = marshal(blocking, Blocking)}};
marshal(
    template,
    Template = #{
        id := ID,
        identity_id := IdentityID,
        domain_revision := DomainRevision,
        party_revision := PartyRevision,
        created_at := CreatedAt,
        details := Details
    }
) ->
    ExternalID = maps:get(external_id, Template, undefined),

    #p2p_template_P2PTemplate{
        id = marshal(id, ID),
        identity_id = marshal(id, IdentityID),
        template_details = marshal(details, Details),
        created_at = marshal(timestamp, CreatedAt),
        domain_revision = marshal(domain_revision, DomainRevision),
        party_revision = marshal(party_revision, PartyRevision),
        external_id = maybe_marshal(id, ExternalID)
    };
marshal(details, Details) ->
    Body = maps:get(body, Details),
    Metadata = maps:get(metadata, Details, undefined),
    #p2p_template_P2PTemplateDetails{
        body = marshal(template_body, Body),
        metadata = maybe_marshal(template_metadata, Metadata)
    };
marshal(template_body, #{value := Body = #{currency := Currency}}) ->
    Amount = maps:get(amount, Body, undefined),
    #p2p_template_P2PTemplateBody{
        value = #p2p_template_Cash{
            amount = maybe_marshal(amount, Amount),
            currency = marshal(currency_ref, Currency)
        }
    };
marshal(template_metadata, #{value := Metadata}) ->
    #p2p_template_P2PTemplateMetadata{
        value = marshal(ctx, Metadata)
    };
marshal(quote, Quote) ->
    ff_p2p_transfer_codec:marshal(quote, Quote);
marshal(blocking, unblocked) ->
    unblocked;
marshal(blocking, blocked) ->
    blocked;
marshal(timestamp, Timestamp) when is_integer(Timestamp) ->
    ff_time:to_rfc3339(Timestamp);
marshal(ctx, Context) ->
    maybe_marshal(context, Context);
marshal(T, V) ->
    ff_codec:marshal(T, V).

-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) -> ff_codec:decoded_value().
unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];
unmarshal(timestamped_change, TimestampedChange) ->
    Timestamp = ff_codec:unmarshal(timestamp, TimestampedChange#p2p_template_TimestampedChange.occured_at),
    Change = unmarshal(change, TimestampedChange#p2p_template_TimestampedChange.change),
    {ev, Timestamp, Change};
unmarshal(repair_scenario, {add_events, #p2p_template_AddEventsRepair{events = Events, action = Action}}) ->
    {add_events,
        genlib_map:compact(#{
            events => unmarshal({list, change}, Events),
            action => maybe_unmarshal(complex_action, Action)
        })};
unmarshal(change, {created, #p2p_template_CreatedChange{p2p_template = Template}}) ->
    {created, unmarshal(template, Template)};
unmarshal(change, {blocking_changed, #p2p_template_BlockingChange{blocking = Blocking}}) ->
    {blocking_changed, unmarshal(blocking, Blocking)};
unmarshal(template, #p2p_template_P2PTemplate{
    id = ID,
    identity_id = IdentityID,
    template_details = Details,
    created_at = CreatedAt,
    domain_revision = DomainRevision,
    party_revision = PartyRevision,
    external_id = ExternalID
}) ->
    genlib_map:compact(#{
        version => 1,
        id => unmarshal(id, ID),
        identity_id => unmarshal(id, IdentityID),
        details => unmarshal(details, Details),
        domain_revision => unmarshal(domain_revision, DomainRevision),
        party_revision => unmarshal(party_revision, PartyRevision),
        created_at => ff_time:from_rfc3339(unmarshal(timestamp, CreatedAt)),
        external_id => maybe_unmarshal(id, ExternalID)
    });
unmarshal(details, #p2p_template_P2PTemplateDetails{
    body = Body,
    metadata = Metadata
}) ->
    genlib_map:compact(#{
        body => unmarshal(template_body, Body),
        metadata => maybe_unmarshal(template_metadata, Metadata)
    });
unmarshal(template_body, #p2p_template_P2PTemplateBody{
    value = #p2p_template_Cash{
        amount = Amount,
        currency = Currency
    }
}) ->
    #{
        value => genlib_map:compact(#{
            amount => maybe_unmarshal(amount, Amount),
            currency => unmarshal(currency_ref, Currency)
        })
    };
unmarshal(template_metadata, #p2p_template_P2PTemplateMetadata{
    value = Metadata
}) ->
    #{value => unmarshal(ctx, Metadata)};
unmarshal(quote, Quote) ->
    ff_p2p_transfer_codec:unmarshal(quote, Quote);
unmarshal(participant, Participant) ->
    ff_p2p_transfer_codec:unmarshal(participant, Participant);
unmarshal(client_info, ClientInfo) ->
    ff_p2p_transfer_codec:unmarshal(client_info, ClientInfo);
unmarshal(ctx, Context) ->
    maybe_unmarshal(context, Context);
unmarshal(blocking, unblocked) ->
    unblocked;
unmarshal(blocking, blocked) ->
    blocked;
unmarshal(timestamp, Timestamp) ->
    unmarshal(string, Timestamp);
unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec p2p_template_codec_test() -> _.

p2p_template_codec_test() ->
    Details = #{
        body => #{
            value => #{
                amount => 100,
                currency => <<"RUB">>
            }
        },
        metadata => #{
            value => #{
                <<"some key">> => <<"some value">>
            }
        }
    },

    P2PTemplate = #{
        version => 1,
        id => genlib:unique(),
        identity_id => genlib:unique(),
        details => Details,
        created_at => ff_time:now(),
        domain_revision => 123,
        party_revision => 321,
        external_id => genlib:unique()
    },

    Changes = [
        {created, P2PTemplate},
        {blocking_changed, unblocked}
    ],
    ?assertEqual(Changes, unmarshal({list, change}, marshal({list, change}, Changes))).

-endif.
