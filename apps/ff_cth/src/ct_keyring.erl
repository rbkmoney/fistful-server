-module(ct_keyring).

-include_lib("cds_proto/include/cds_proto_keyring_thrift.hrl").

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("jose/include/jose_jws.hrl").

-define(THRESHOLD, 1).

-export([init/1]).

-type encrypted_master_key_share() :: #{
    id := binary(),
    owner := binary(),
    encrypted_share := binary()
}.

-spec init(_) -> ok.
init(Config) ->
    case get_state(Config) of
        not_initialized ->
            {ok, [EncryptedMasterKeyShare]} = start_init(?THRESHOLD, Config),
            {ok, EncPrivateKey} = file:read_file("/opt/wapi/config/enc.1.priv.json"),
            {ok, SigPrivateKey} = file:read_file("/opt/wapi/config/sig.1.priv.json"),
            #{
                id := ID,
                encrypted_share := EncryptedShare
            } = EncryptedMasterKeyShare,
            DecryptedShare = private_decrypt(EncPrivateKey, <<"">>, EncryptedShare),
            DecryptedMasterKeyShare = sign(SigPrivateKey, DecryptedShare),
            ok = validate_init(ID, DecryptedMasterKeyShare, Config);
        _ ->
            ok
    end.

get_state(Config) ->
    {ok, #cds_KeyringState{status = Status}} = call('GetState', {}, Config),
    Status.

start_init(Threshold, Config) ->
    case call('StartInit', {Threshold}, Config) of
        {ok, EncryptedShares} ->
            {ok, decode_encrypted_shares(EncryptedShares)};
        {exception, #cds_InvalidStatus{status = Status}} ->
            {error, {invalid_status, Status}};
        {exception, #cds_InvalidActivity{activity = Activity}} ->
            {error, {invalid_activity, Activity}};
        {exception, #cds_InvalidArguments{reason = Reason}} ->
            {error, {invalid_arguments, Reason}}
    end.

validate_init(ID, DecryptedMasterKeyShare, Config) ->
    SignedShareKey = #cds_SignedMasterKeyShare{
        id = ID,
        signed_share = DecryptedMasterKeyShare
    },
    case call('ValidateInit', {SignedShareKey}, Config) of
        {ok, {success, #cds_Success{}}} ->
            ok;
        {ok, {more_keys_needed, More}} ->
            {more_keys_needed, More};
        {exception, #cds_InvalidStatus{status = Status}} ->
            {error, {invalid_status, Status}};
        {exception, #cds_InvalidActivity{activity = Activity}} ->
            {error, {invalid_activity, Activity}};
        {exception, #cds_VerificationFailed{}} ->
            {error, verification_failed};
        {exception, #cds_OperationAborted{reason = Reason}} ->
            {error, {operation_aborted, Reason}}
    end.

call(Fun, Args, C) ->
    Client = ff_woody_client:new(maps:get(kds, ct_helper:cfg(services, C))),
    WoodyCtx = ct_helper:get_woody_ctx(C),
    Request = {{cds_proto_keyring_thrift, 'KeyringManagement'}, Fun, Args},
    woody_client:call(Request, Client, WoodyCtx).

%% DECODE

-spec decode_encrypted_shares([cds_proto_keyring_thrift:'EncryptedMasterKeyShare'()]) -> [encrypted_master_key_share()].
decode_encrypted_shares(EncryptedMasterKeyShares) ->
    lists:map(fun decode_encrypted_share/1, EncryptedMasterKeyShares).

-spec decode_encrypted_share(cds_proto_keyring_thrift:'EncryptedMasterKeyShare'()) -> encrypted_master_key_share().
decode_encrypted_share(#cds_EncryptedMasterKeyShare{
    id = Id,
    owner = Owner,
    encrypted_share = EncryptedShare
}) ->
    #{
        id => Id,
        owner => Owner,
        encrypted_share => EncryptedShare
    }.

%%
%% DECRYPTION
%%

private_decrypt(PrivateKey, Password, JWECompacted) ->
    {_Module, JWKPrivateKey} = jose_jwk:from(Password, PrivateKey),
    {#{}, JWEPlain} = jose_jwe:expand(JWECompacted),
    {Result, _JWE} = jose_jwk:block_decrypt(JWEPlain, JWKPrivateKey),
    Result.

sign(PrivateKey, Plain) ->
    JWKPrivateKey = jose_jwk:from(PrivateKey),
    SignerWithoutKid = jose_jwk:signer(JWKPrivateKey),
    Signer = SignerWithoutKid#{<<"kid">> => JWKPrivateKey#jose_jwk.fields},
    {_JWKModule, SignedPlain} = jose_jwk:sign(Plain, Signer, JWKPrivateKey),
    {_JWSModule, SignedCompacted} = jose_jws:compact(SignedPlain),
    SignedCompacted.
