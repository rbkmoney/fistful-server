-module(wapi_crypto).

-include_lib("fistful_proto/include/ff_proto_resource_token_thrift.hrl").

-type encrypted_token() :: binary().
-type deadline() :: wapi_utils:deadline().
-type resource_token() :: ff_proto_resource_token_thrift:'ResourceToken'().
-type resource_payload() :: ff_proto_resource_token_thrift:'ResourcePayload'().
-type resource() :: {bank_card, bank_card()}.
-type bank_card() :: ff_proto_base_thrift:'BankCard'().

-export_type([encrypted_token/0]).
-export_type([resource/0]).

-export([create_resource_token/2]).
-export([decrypt_resource_token/1]).

-spec create_resource_token(resource(), deadline()) -> encrypted_token().
create_resource_token(Resource, ValidUntil) ->
    ResourceToken = encode_resource_token(Resource, ValidUntil),
    ThriftType = {struct, struct, {ff_proto_resource_token_thrift, 'ResourceToken'}},
    {ok, EncodedToken} = lechiffre:encode(ThriftType, ResourceToken),
    TokenVersion = token_version(),
    <<TokenVersion/binary, ".", EncodedToken/binary>>.

-spec decrypt_resource_token(encrypted_token()) ->
    {ok, {resource(), deadline()}}
    | unrecognized
    | {error, lechiffre:decoding_error()}.
decrypt_resource_token(Token) ->
    Ver = token_version(),
    Size = byte_size(Ver),
    case Token of
        <<Ver:Size/binary, ".", EncryptedResourceToken/binary>> ->
            decrypt_token(EncryptedResourceToken);
        <<"v1.", EncryptedResourceToken/binary>> ->
            decrypt_token_v1(EncryptedResourceToken);
        _ ->
            unrecognized
    end.

%% Internal

token_version() ->
    <<"v2">>.

decrypt_token(EncryptedToken) ->
    ThriftType = {struct, struct, {ff_proto_resource_token_thrift, 'ResourceToken'}},
    case lechiffre:decode(ThriftType, EncryptedToken) of
        {ok, ResourceToken} ->
            Resource = decode_resource_payload(ResourceToken#rst_ResourceToken.payload),
            ValidUntil = decode_deadline(ResourceToken#rst_ResourceToken.valid_until),
            {ok, {Resource, ValidUntil}};
        {error, _} = Error ->
            Error
    end.

decrypt_token_v1(EncryptedToken) ->
    ThriftType = {struct, struct, {ff_proto_base_thrift, 'BankCard'}},
    case lechiffre:decode(ThriftType, EncryptedToken) of
        {ok, BankCard} ->
            {ok, {{bank_card, BankCard}, undefined}};
        {error, _} = Error ->
            Error
    end.

-spec encode_deadline(deadline()) -> binary() | undefined.
encode_deadline(undefined) ->
    undefined;
encode_deadline(Deadline) ->
    wapi_utils:deadline_to_binary(Deadline).

-spec encode_resource_token(resource(), deadline()) -> resource_token().
encode_resource_token(Resource, ValidUntil) ->
    #rst_ResourceToken{
        payload = encode_resource_payload(Resource),
        valid_until = encode_deadline(ValidUntil)
    }.

-spec encode_resource_payload(resource()) -> resource_payload().
encode_resource_payload({bank_card, BankCard}) ->
    {bank_card_payload, #rst_BankCardPayload{
        bank_card = BankCard
    }}.

-spec decode_deadline(binary()) -> deadline() | undefined.
decode_deadline(undefined) ->
    undefined;
decode_deadline(Deadline) ->
    wapi_utils:deadline_from_binary(Deadline).

-spec decode_resource_payload(resource_payload()) -> resource().
decode_resource_payload({bank_card_payload, Payload}) ->
    {bank_card, Payload#rst_BankCardPayload.bank_card}.
