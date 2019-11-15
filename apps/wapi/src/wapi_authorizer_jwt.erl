-module(wapi_authorizer_jwt).

%%

-export([get_child_spec/1]).
-export([init/1]).

-export([store_key/2]).
-export([get_signee_key/0]).
% TODO
% Extend interface to support proper keystore manipulation

%%

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("jose/include/jose_jwt.hrl").

-type keyname()    :: term().
-type kid()        :: binary().
-type key()        :: #jose_jwk{}.
-type stored_key() :: #{
    jwk      := key(),
    kid      := kid(),
    signer   := map() | undefined,
    verifier := map() | undefined
}.
-type token()      :: binary().
-type claims()     :: #{binary() => term()}.
-type subject()    :: {subject_id(), uac_acl:t()}.
-type subject_id() :: binary().
-type t()          :: {subject(), claims()}.
-type expiration() ::
    {lifetime, Seconds :: pos_integer()} |
    {deadline, UnixTs :: pos_integer()}  |
    unlimited.

-export_type([t/0]).
-export_type([subject/0]).
-export_type([claims/0]).
-export_type([token/0]).
-export_type([expiration/0]).
-export_type([key/0]).
-export_type([stored_key/0]).
-export_type([kid/0]).

%%

-type options() :: #{
    %% The set of keys used to sign issued tokens and verify signatures on such
    %% tokens.
    keyset => keyset(),
    %% The name of a key used exclusively to sign any issued token.
    %% If not set any token issue is destined to fail.
    signee => keyname()
}.

-type keyset() :: #{
    keyname() => keysource()
}.

-type keysource() ::
    {pem_file, file:filename()}.

-spec get_child_spec(options()) ->
    supervisor:child_spec() | no_return().

get_child_spec(Options) ->
    #{
        id => ?MODULE,
        start => {supervisor, start_link, [?MODULE, parse_options(Options)]},
        type => supervisor
    }.

parse_options(Options) ->
    Keyset = maps:get(keyset, Options, #{}),
    _ = is_map(Keyset) orelse exit({invalid_option, keyset, Keyset}),
    _ = genlib_map:foreach(
        fun (K, V) ->
            is_keysource(V) orelse exit({invalid_option, K, V})
        end,
        Keyset
    ),
    Signee = maps:find(signee, Options),
    {Keyset, Signee}.

is_keysource({pem_file, Fn}) ->
    is_list(Fn) orelse is_binary(Fn);
is_keysource(_) ->
    false.

%%

-spec init({keyset(), {ok, keyname()} | error}) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init({Keyset, Signee}) ->
    ok = create_table(),
    KeyInfos = maps:map(fun ensure_store_key/2, Keyset),
    ok = select_signee(Signee, KeyInfos),
    {ok, {#{}, []}}.

ensure_store_key(Keyname, Source) ->
    case store_key(Keyname, Source) of
        {ok, KeyInfo} ->
            KeyInfo;
        {error, Reason} ->
            _ = logger:error("Error importing key ~p: ~p", [Keyname, Reason]),
            exit({import_error, Keyname, Source, Reason})
    end.

select_signee({ok, Keyname}, KeyInfos) ->
    case maps:find(Keyname, KeyInfos) of
        {ok, #{sign := true}} ->
            set_signee(Keyname);
        {ok, KeyInfo} ->
            _ = logger:error("Error setting signee: signing with ~p is not allowed", [Keyname]),
            exit({invalid_signee, Keyname, KeyInfo});
        error ->
            _ = logger:error("Error setting signee: no key named ~p", [Keyname]),
            exit({nonexstent_signee, Keyname})
    end;
select_signee(error, _KeyInfos) ->
    ok.

%%

-type keyinfo() :: #{
    kid    => kid(),
    sign   => boolean(),
    verify => boolean()
}.

-spec store_key(keyname(), {pem_file, file:filename()}) ->
    {ok, keyinfo()} | {error, file:posix() | {unknown_key, _}}.

store_key(Keyname, {pem_file, Filename}) ->
    store_key(Keyname, {pem_file, Filename}, #{
        kid => fun derive_kid_from_public_key_pem_entry/1
    }).

derive_kid_from_public_key_pem_entry(JWK) ->
    JWKPublic = jose_jwk:to_public(JWK),
    {_Module, PublicKey} = JWKPublic#jose_jwk.kty,
    {_PemEntry, Data, _} = public_key:pem_entry_encode('SubjectPublicKeyInfo', PublicKey),
    base64url:encode(crypto:hash(sha256, Data)).

-type store_opts() :: #{
    kid => fun ((key()) -> kid())
}.

-spec store_key(keyname(), {pem_file, file:filename()}, store_opts()) ->
    {ok, keyinfo()} | {error, file:posix() | {unknown_key, _}}.

store_key(Keyname, {pem_file, Filename}, Opts) ->
    case jose_jwk:from_pem_file(Filename) of
        JWK = #jose_jwk{} ->
            Key = construct_key(derive_kid(JWK, Opts), JWK),
            ok = insert_key(Keyname, Key),
            {ok, get_key_info(Key)};
        Error = {error, _} ->
            Error
    end.

get_key_info(#{kid := KID, signer := Signer, verifier := Verifier}) ->
    #{
        kid    => KID,
        sign   => Signer /= undefined,
        verify => Verifier /= undefined
    }.

derive_kid(JWK, #{kid := DeriveFun}) when is_function(DeriveFun, 1) ->
    DeriveFun(JWK).

construct_key(KID, JWK) ->
    #{
        jwk      => JWK,
        kid      => KID,
        signer   => try jose_jwk:signer(JWK)   catch error:_ -> undefined end,
        verifier => try jose_jwk:verifier(JWK) catch error:_ -> undefined end
    }.

%%

insert_key(Keyname, Key = #{kid := KID}) ->
    insert_values(#{
        {keyname, Keyname} => Key,
        {kid, KID}         => Key
    }).

get_key_by_name(Keyname) ->
    lookup_value({keyname, Keyname}).

set_signee(Keyname) ->
    insert_values(#{
        signee => {keyname, Keyname}
    }).

-spec get_signee_key() ->
    stored_key() | undefined.

get_signee_key() ->
    case lookup_value(signee) of
        {keyname, Keyname} ->
            get_key_by_name(Keyname);
        undefined ->
            undefined
    end.

%%

-define(TABLE, ?MODULE).

create_table() ->
    _ = ets:new(?TABLE, [set, public, named_table, {read_concurrency, true}]),
    ok.

insert_values(Values) ->
    true = ets:insert(?TABLE, maps:to_list(Values)),
    ok.

lookup_value(Key) ->
    case ets:lookup(?TABLE, Key) of
        [{Key, Value}] ->
            Value;
        [] ->
            undefined
    end.
