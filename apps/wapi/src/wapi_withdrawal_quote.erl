-module(wapi_withdrawal_quote).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").

-export([create_token_payload/4]).
-export([thrift_create_token_payload/4]).
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
-type thrift_quote() :: ff_proto_withdrawal_thrift:'Quote'().

%% API

-spec create_token_payload(quote(), wallet_id(), destination_id(), party_id()) ->
    token_payload().
create_token_payload(Quote, WalletID, DestinationID, PartyID) ->
    do_create_token_payload(encode_quote(Quote), WalletID, DestinationID, PartyID).

-spec thrift_create_token_payload(thrift_quote(), wallet_id(), destination_id(), party_id()) ->
    token_payload().
thrift_create_token_payload(Quote, WalletID, DestinationID, PartyID) ->
    do_create_token_payload(encode_thrift_quote(Quote), WalletID, DestinationID, PartyID).

do_create_token_payload(Quote, WalletID, DestinationID, PartyID) ->
    genlib_map:compact(#{
        <<"version">> => 2,
        <<"walletID">> => WalletID,
        <<"destinationID">> => DestinationID,
        <<"partyID">> => PartyID,
        <<"quote">> => Quote
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
        resource_descriptor => decode_legacy_resource_id(ResourceID),
        domain_revision => DomainRevision,
        party_revision => PartyRevision
    }),
    {ok, Quote, WalletID, DestinationID, PartyID}.

%% Internals

-spec encode_quote(quote()) ->
    token_payload().
encode_quote(Quote) ->
    encode_thrift_quote(ff_withdrawal_codec:marshal(quote, Quote)).

-spec encode_thrift_quote(thrift_quote()) ->
    token_payload().
encode_thrift_quote(Quote) ->
    Type = {struct, struct, {ff_proto_withdrawal_thrift, 'Quote'}},
    Bin = ff_proto_utils:serialize(Type, Quote),
    base64:encode(Bin).

-spec decode_quote(token_payload()) ->
    quote().
decode_quote(Encoded) ->
    Type = {struct, struct, {ff_proto_withdrawal_thrift, 'Quote'}},
    Bin = base64:decode(Encoded),
    Thrift = ff_proto_utils:deserialize(Type, Bin),
    ff_withdrawal_codec:unmarshal(quote, Thrift).

decode_legacy_cash(Body) ->
    {genlib:to_int(maps:get(<<"amount">>, Body)), maps:get(<<"currency">>, Body)}.

decode_legacy_resource_id(#{<<"bank_card">> := ID}) ->
    {bank_card, ID}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec payload_symmetry_test() -> _.
payload_symmetry_test() ->
    PartyID = <<"party">>,
    WalletID = <<"wallet">>,
    DestinationID = <<"destination">>,
    Quote = #{
        cash_from => {1000000, <<"RUB">>},
        cash_to => {1, <<"USD">>},
        created_at => <<"1970-01-01T00:00:00.123Z">>,
        expires_on => <<"1970-01-01T00:00:00.321Z">>,
        quote_data => #{[nil] => [nil]},
        route => #{
            provider_id => 1000,
            terminal_id => 2,
            provider_id_legacy => <<"700">>,
            version => 1
        },
        operation_timestamp => 234,
        resource_descriptor => {bank_card, #{[nil] => [nil]}},
        domain_revision => 1,
        party_revision => 2
    },
    Payload = create_token_payload(Quote, WalletID, DestinationID, PartyID),
    {ok, Decoded, WalletID, DestinationID, PartyID} = decode_token_payload(Payload),
    ?assertEqual(Quote, Decoded).

-spec payload_v2_decoding_test() -> _.
payload_v2_decoding_test() ->
    PartyID = <<"party">>,
    WalletID = <<"wallet">>,
    DestinationID = <<"destination">>,
    ExpectedQuote = #{
        cash_from => {1000000, <<"RUB">>},
        cash_to => {1, <<"USD">>},
        created_at => <<"1970-01-01T00:00:00.123Z">>,
        expires_on => <<"1970-01-01T00:00:00.321Z">>,
        quote_data => #{[nil] => [nil]},
        route => #{
            provider_id => 1000,
            terminal_id => 2,
            provider_id_legacy => <<"700">>,
            version => 1
        },
        operation_timestamp => 234,
        resource_descriptor => {bank_card, #{[nil] => [nil]}},
        domain_revision => 1,
        party_revision => 2
    },
    Payload = #{
        <<"version">> => 2,
        <<"walletID">> => WalletID,
        <<"destinationID">> => DestinationID,
        <<"partyID">> => PartyID,
        <<"quote">> => <<
            "DAABCgABAAAAAAAPQkAMAAILAAEAAAADUlVCAAAMAAIKAAEAAAAAAAAAAQwAAgsAAQAAAANVU"
            "0QAAAsAAwAAABgxOTcwLTAxLTAxVDAwOjAwOjAwLjEyM1oLAAQAAAAYMTk3MC0wMS0wMVQwMD"
            "owMDowMC4zMjFaDAAFDQAHDAwAAAABDwAIDAAAAAEMAAEAAAAPAAgMAAAAAQwAAQAAAAAMAAY"
            "IAAMAAAPoCAAEAAAAAgsAAQAAAAM3MDAADAAHDAABDAABDQAHDAwAAAABDwAIDAAAAAEMAAEA"
            "AAAPAAgMAAAAAQwAAQAAAAAAAAsACAAAABgxOTcwLTAxLTAxVDAwOjAwOjAwLjIzNFoKAAkAA"
            "AAAAAAAAQoACgAAAAAAAAACAA=="
        >>
    },
    ?assertEqual(
        {ok, ExpectedQuote, WalletID, DestinationID, PartyID},
        decode_token_payload(Payload)
    ).

-spec payload_v1_decoding_test() -> _.
payload_v1_decoding_test() ->
    PartyID = <<"party">>,
    WalletID = <<"wallet">>,
    DestinationID = <<"destination">>,
    ExpectedQuote = #{
        cash_from => {1000000, <<"RUB">>},
        cash_to => {1, <<"USD">>},
        created_at => <<"1970-01-01T00:00:00.123Z">>,
        expires_on => <<"1970-01-01T00:00:00.321Z">>,
        quote_data => 6,
        route => ff_withdrawal_routing:make_route(1000, 2),
        operation_timestamp => 234,
        resource_descriptor => {bank_card, 5},
        domain_revision => 1,
        party_revision => 2
    },
    Payload = #{
        <<"version">> => 1,
        <<"walletID">> => WalletID,
        <<"destinationID">> => DestinationID,
        <<"partyID">> => PartyID,
        <<"cashFrom">> => #{<<"amount">> => 1000000, <<"currency">> => <<"RUB">>},
        <<"cashTo">> => #{<<"amount">> => 1, <<"currency">> => <<"USD">>},
        <<"createdAt">> => <<"1970-01-01T00:00:00.123Z">>,
        <<"expiresOn">> => <<"1970-01-01T00:00:00.321Z">>,
        <<"quoteData">> => #{
            <<"version">> => 1,
            <<"quote_data">> => 6,
            <<"provider_id">> => 1000,
            <<"terminal_id">> => 2,
            <<"resource_id">> => #{<<"bank_card">> => 5},
            <<"timestamp">> => 234,
            <<"domain_revision">> => 1,
            <<"party_revision">> => 2
        }
    },
    ?assertEqual(
        {ok, ExpectedQuote, WalletID, DestinationID, PartyID},
        decode_token_payload(Payload)
    ).

-endif.
