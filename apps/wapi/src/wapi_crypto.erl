-module(wapi_crypto).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").

-type encrypted_token() :: binary().
-type bank_card() :: ff_proto_base_thrift:'BankCard'().

-export_type([encrypted_token/0]).

-export([encrypt_bankcard_token/1]).
-export([decrypt_bankcard_token/1]).

-spec encrypt_bankcard_token(bank_card()) -> encrypted_token().
encrypt_bankcard_token(BankCard) ->
    EncryptionParams = create_encryption_params(),
    ThriftType = {struct, struct, {ff_proto_base_thrift, 'BankCard'}},
    {ok, EncodedToken} = lechiffre:encode(ThriftType, BankCard, EncryptionParams),
    TokenVersion = token_version(),
    <<TokenVersion/binary, ".", EncodedToken/binary>>.

-spec decrypt_bankcard_token(encrypted_token()) ->
    unrecognized
    | {ok, bank_card()}
    | {error, lechiffre:decoding_error()}.
decrypt_bankcard_token(Token) ->
    Ver = token_version(),
    Size = byte_size(Ver),
    case Token of
        <<Ver:Size/binary, ".", EncryptedPaymentToolToken/binary>> ->
            decrypt_token(EncryptedPaymentToolToken);
        _ ->
            unrecognized
    end.

%% Internal

token_version() ->
    <<"v1">>.

%% Delete this code after add improved lechiffre(del deterministic encryption)
create_encryption_params() ->
    #{iv => lechiffre:compute_iv(<<"">>)}.

decrypt_token(EncryptedToken) ->
    ThriftType = {struct, struct, {ff_proto_base_thrift, 'BankCard'}},
    lechiffre:decode(ThriftType, EncryptedToken).
