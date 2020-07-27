-module(wapi_p2p_quote).

-export([create_token_payload/2]).
-export([decode_token_payload/1]).

-type token_payload() ::
    integer() |
    binary() |
    float() |
    [token_payload()] |
    #{binary() => token_payload()}.

-export_type([token_payload/0]).

%% Internal types

-type party_id() :: binary().
-type quote() :: p2p_quote:quote().

%% API

-spec create_token_payload(quote(), party_id()) ->
    token_payload().
create_token_payload(Quote, PartyID) ->
    genlib_map:compact(#{
        <<"version">> => 2,
        <<"partyID">> => PartyID,
        <<"quote">> => encode_quote(Quote)
    }).

-spec decode_token_payload(token_payload()) ->
    {ok, quote()}.
decode_token_payload(#{<<"version">> := 2} = Payload) ->
    #{
        <<"version">> := 2,
        <<"partyID">> := _PartyID,
        <<"quote">> := EncodedQuote
    } = Payload,
    Quote = decode_quote(EncodedQuote),
    {ok, Quote};
decode_token_payload(#{<<"version">> := 1} = Payload) ->
    #{
        <<"version">> := 1,
        <<"amount">> := Amount,
        <<"partyRevision">> := PartyRevision,
        <<"domainRevision">> := DomainRevision,
        <<"createdAt">> := CreatedAt,
        <<"expiresOn">> := ExpiresOn,
        <<"partyID">> := _PartyID,
        <<"identityID">> := IdentityID,
        <<"sender">> := Sender,
        <<"receiver">> := Receiver
    } = Payload,
    Quote = genlib_map:compact(#{
        fees => #{fees => #{}},
        amount => decode_legacy_cash(Amount),
        party_revision => PartyRevision,
        domain_revision => DomainRevision,
        created_at => ff_time:from_rfc3339(CreatedAt),
        expires_on => ff_time:from_rfc3339(ExpiresOn),
        identity_id => IdentityID,
        sender => decode_legacy_compact_resource(Sender),
        receiver => decode_legacy_compact_resource(Receiver)
    }),
    {ok, Quote}.

%% Internals

-spec encode_quote(quote()) ->
    token_payload().
encode_quote(Quote) ->
    Type = {struct, struct, {ff_proto_p2p_transfer_thrift, 'Quote'}},
    Bin = ff_proto_utils:serialize(Type, ff_withdrawal_codec:marshal(quote, Quote)),
    base64:encode(Bin).

-spec decode_quote(token_payload()) ->
    quote.
decode_quote(Encoded) ->
    Type = {struct, struct, {ff_proto_p2p_transfer_thrift, 'Quote'}},
    Bin = base64:decode(Encoded),
    Thrift = ff_proto_utils:deserialize(Type, Bin),
    ff_withdrawal_codec:unmarshal(quote, Thrift).

decode_legacy_cash(Body) ->
    {genlib:to_int(maps:get(<<"amount">>, Body)), maps:get(<<"currency">>, Body)}.


decode_legacy_compact_resource(Resource) ->
    #{
        <<"type">> := <<"bank_card">>,
        <<"token">> := Token,
        <<"binDataID">> := BinDataID
    } = Resource,
    {bank_card, #{
        token => Token,
        bin_data_id => BinDataID
    }}.