-module(ct_keyring).
-include_lib("cds_proto/include/cds_proto_keyring_thrift.hrl").

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("jose/include/jose_jws.hrl").

-define(THRESHOLD, 1).
-define(read_file(Filename), file:read_file(Filename)).

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
            [EncryptedMasterKeyShare] = start_init(?THRESHOLD, Config),
            {ok, EncPrivateKey} = ?read_file("/opt/wapi/config/enc.1.priv.json"),
            {ok, SigPrivateKey} = ?read_file("/opt/wapi/config/sig.1.priv.json"),
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
    #cds_KeyringState{
        status = Status
    } = call('GetState', [], Config),
    Status.

start_init(Threshold, Config) ->
    try call('StartInit', [Threshold], Config) of
        EncryptedShares ->
            decode_encrypted_shares(EncryptedShares)
    catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #cds_InvalidArguments{reason = Reason} ->
            {error, {invalid_arguments, Reason}}
    end.

validate_init(ID, DecryptedMasterKeyShare, Config) ->
    SignedShareKey = #cds_SignedMasterKeyShare{
        id = ID,
        signed_share = DecryptedMasterKeyShare
    },
    try call('ValidateInit', [SignedShareKey], Config) of
        {success, #cds_Success{}} ->
            ok;
        {more_keys_needed, More} ->
            {more_keys_needed, More}
    catch
        #cds_InvalidStatus{status = Status} ->
            {error, {invalid_status, Status}};
        #cds_InvalidActivity{activity = Activity} ->
            {error, {invalid_activity, Activity}};
        #cds_VerificationFailed{} ->
            {error, verification_failed};
        #cds_OperationAborted{reason = Reason} ->
            {error, {operation_aborted, Reason}}
    end.

call(Fun, Args, C) ->
    Client = ff_woody_client:new(maps:get(kds, ct_helper:cfg(services, C))),
    WoodyCtx = ct_helper:get_woody_ctx(C),
    Request = {{cds_proto_keyring_thrift, 'KeyringManagement'}, Fun, Args},
    case woody_client:call(Request, Client, WoodyCtx) of
        {ok, ID} ->
            ID
    end.

%% DECODE

-spec decode_encrypted_shares([cds_proto_keyring_thrift:'EncryptedMasterKeyShare'()]) ->
    [encrypted_master_key_share()].

decode_encrypted_shares(EncryptedMasterKeyShares) ->
    lists:map(fun decode_encrypted_share/1, EncryptedMasterKeyShares).

-spec decode_encrypted_share(cds_proto_keyring_thrift:'EncryptedMasterKeyShare'()) ->
    encrypted_master_key_share().

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
