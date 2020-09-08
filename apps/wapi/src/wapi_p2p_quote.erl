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
    {ok, quote()} | {error, token_expired}.
decode_token_payload(#{<<"version">> := 2} = Payload) ->
    #{
        <<"version">> := 2,
        <<"partyID">> := _PartyID,
        <<"quote">> := EncodedQuote
    } = Payload,
    Quote = decode_quote(EncodedQuote),
    {ok, Quote};
decode_token_payload(#{<<"version">> := 1}) ->
    {error, token_expired}.

%% Internals

-spec encode_quote(quote()) ->
    token_payload().
encode_quote(Quote) ->
    Type = {struct, struct, {ff_proto_p2p_transfer_thrift, 'Quote'}},
    Bin = ff_proto_utils:serialize(Type, ff_p2p_transfer_codec:marshal(quote, Quote)),
    base64:encode(Bin).

-spec decode_quote(token_payload()) ->
    quote.
decode_quote(Encoded) ->
    Type = {struct, struct, {ff_proto_p2p_transfer_thrift, 'Quote'}},
    Bin = base64:decode(Encoded),
    Thrift = ff_proto_utils:deserialize(Type, Bin),
    ff_p2p_transfer_codec:unmarshal(quote, Thrift).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec payload_symmetry_test() -> _.
payload_symmetry_test() ->
    Quote = #{
        fees => #{
            fees => #{
                surplus => {1000, <<"RUB">>}
            }
        },
        amount => {1000000, <<"RUB">>},
        party_revision => 1,
        domain_revision => 2,
        created_at => 123,
        expires_on => 321,
        identity_id => <<"identity">>,
        sender => {bank_card, #{
            token => <<"very long token">>,
            bin_data_id => nil
        }},
        receiver => {bank_card, #{
            token => <<"another very long token">>,
            bin_data_id => #{[nil] => [nil]}
        }}
    },
    Payload = create_token_payload(Quote, <<"party">>),
    {ok, Decoded} = decode_token_payload(Payload),
    ?assertEqual(Quote, Decoded).

-spec payload_v2_decoding_test() -> _.
payload_v2_decoding_test() ->
    ExpectedQuote = #{
        fees => #{
            fees => #{
                surplus => {1000, <<"RUB">>}
            }
        },
        amount => {1000000, <<"RUB">>},
        party_revision => 1,
        domain_revision => 2,
        created_at => 123,
        expires_on => 321,
        identity_id => <<"identity">>,
        sender => {bank_card, #{
            token => <<"very long token">>,
            bin_data_id => nil
        }},
        receiver => {bank_card, #{
            token => <<"another very long token">>,
            bin_data_id => #{[nil] => [nil]}
        }}
    },
    Payload = #{
        <<"partyID">> => <<"party">>,
        <<"quote">> => <<
            "DAABCgABAAAAAAAPQkAMAAILAAEAAAADUlVCAAALAAIAAAAYMTk3MC0wMS0wMVQwMDowMDowMC4xM"
            "jNaCwADAAAAGDE5NzAtMDEtMDFUMDA6MDA6MDAuMzIxWgoABAAAAAAAAAACCgAFAAAAAAAAAAELAA"
            "YAAAAIaWRlbnRpdHkMAAcMAAEMAAELAAEAAAAPdmVyeSBsb25nIHRva2VuDAAVDAABAAAAAAAMAAg"
            "MAAEMAAELAAEAAAAXYW5vdGhlciB2ZXJ5IGxvbmcgdG9rZW4MABUNAAcMDAAAAAEPAAgMAAAAAQwA"
            "AQAAAA8ACAwAAAABDAABAAAAAAAAAAwACQ0AAQgMAAAAAQAAAAEKAAEAAAAAAAAD6AwAAgsAAQAAA"
            "ANSVUIAAAAA"
        >>,
        <<"version">> => 2
    },
    ?assertEqual({ok, ExpectedQuote}, decode_token_payload(Payload)).

-spec payload_v1_decoding_test() -> _.
payload_v1_decoding_test() ->
    Payload = #{
        <<"partyRevision">> => 1,
        <<"domainRevision">> => 2,
        <<"amount">> => #{<<"amount">> => 1000000, <<"currency">> => <<"RUB">>},
        <<"createdAt">> => <<"1970-01-01T00:00:00.123Z">>,
        <<"expiresOn">> => <<"1970-01-01T00:00:00.321Z">>,
        <<"partyID">> => <<"party">>,
        <<"identityID">> => <<"identity">>,
        <<"sender">> => #{
            <<"type">> => <<"bank_card">>,
            <<"token">> => <<"very long token">>,
            <<"binDataID">> => 1
        },
        <<"receiver">> => #{
            <<"type">> => <<"bank_card">>,
            <<"token">> => <<"another very long token">>,
            <<"binDataID">> => 2
        },
        <<"version">> => 1
    },
    ?assertEqual({error, token_expired}, decode_token_payload(Payload)).

-endif.
