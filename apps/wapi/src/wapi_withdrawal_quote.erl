-module(wapi_withdrawal_quote).

-export([create_token_payload/4]).
-export([decode_token_payload/1]).

-type token_payload() ::
    integer() |
    binary() |
    float() |
    [token_payload()] |
    #{binary() => token_payload()}.

-export_type([token_payload/0]).

%% Internal types

-type wallet_id() :: binary().
-type destination_id() :: binary() | undefined.
-type party_id() :: binary().
-type quote() :: ff_withdrawal:quote().

%% API

-spec create_token_payload(quote(), wallet_id(), destination_id(), party_id()) ->
    token_payload().
create_token_payload(Quote, WalletID, DestinationID, PartyID) ->
    genlib_map:compact(#{
        <<"version">> => 2,
        <<"walletID">> => WalletID,
        <<"destinationID">> => DestinationID,
        <<"partyID">> => PartyID,
        <<"quote">> => encode_quote(Quote)
    }).

-spec decode_token_payload(token_payload()) ->
    {ok, quote(), wallet_id(), destination_id(), party_id()}.
decode_token_payload(#{<<"version">> := 2} = Payload) ->
    #{
        <<"version">> := 2,
        <<"walletID">> := WalletID,
        <<"partyID">> := PartyID,
        <<"quote">> := EncodedQuote
    } = Payload,
    Quote = decode_quote(EncodedQuote),
    DestinationID = maps:get(<<"destinationID">>, Payload, undefined),
    {ok, Quote, WalletID, DestinationID, PartyID};
decode_token_payload(#{<<"version">> := 1} = Payload) ->
    #{
        <<"version">> := 1,
        <<"walletID">> := WalletID,
        <<"partyID">> := PartyID,
        <<"cashFrom">> := CashFrom,
        <<"cashTo">> := CashTo,
        <<"createdAt">> := CreatedAt,
        <<"expiresOn">> := ExpiresOn,
        <<"quoteData">> := LegacyQuote
    } = Payload,
    DestinationID = maps:get(<<"destinationID">>, Payload, undefined),
    #{
        <<"version">> := 1,
        <<"quote_data">> := QuoteData,
        <<"provider_id">> := ProviderID,
        <<"resource_id">> := ResourceID,
        <<"timestamp">> := Timestamp,
        <<"domain_revision">> := DomainRevision,
        <<"party_revision">> := PartyRevision
    } = LegacyQuote,
    TerminalID = maps:get(<<"terminal_id">>, LegacyQuote, undefined),
    Quote = genlib_map:compact(#{
        cash_from => decode_legacy_cash(CashFrom),
        cash_to => decode_legacy_cash(CashTo),
        created_at => CreatedAt,
        expires_on => ExpiresOn,
        quote_data => QuoteData,
        route => ff_withdrawal_routing:make_route(ProviderID, TerminalID),
        operation_timestamp => Timestamp,
        resource_id => decode_legacy_resource_id(ResourceID),
        domain_revision => DomainRevision,
        party_revision => PartyRevision
    }),
    {ok, Quote, WalletID, DestinationID, PartyID}.

%% Internals

-spec encode_quote(quote()) ->
    token_payload().
encode_quote(Quote) ->
    Type = {struct, struct, {ff_proto_withdrawal_thrift, 'Quote'}},
    Bin = ff_proto_utils:serialize(Type, ff_withdrawal_codec:marshal(quote, Quote)),
    base64:encode(Bin).

-spec decode_quote(token_payload()) ->
    quote.
decode_quote(Encoded) ->
    Type = {struct, struct, {ff_proto_withdrawal_thrift, 'Quote'}},
    Bin = base64:decode(Encoded),
    Thrift = ff_proto_utils:deserialize(Type, Bin),
    ff_withdrawal_codec:unmarshal(quote, Thrift).

decode_legacy_cash(Body) ->
    {genlib:to_int(maps:get(<<"amount">>, Body)), maps:get(<<"currency">>, Body)}.

decode_legacy_resource_id(#{<<"bank_card">> := ID}) ->
    {bank_card, ID}.
