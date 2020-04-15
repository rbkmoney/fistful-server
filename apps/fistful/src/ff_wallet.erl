%%%
%%% Wallet
%%%

-module(ff_wallet).

-type id()          :: ff_account:id().
-type external_id() :: id() | undefined.
-type metadata()    :: ff_entity_context:md().

-define(ACTUAL_FORMAT_VERSION, 2).
-type wallet_state() :: #{
    version     := ?ACTUAL_FORMAT_VERSION,
    name        := binary(),
    blocking    := blocking(),
    account     => account(),
    external_id => id(),
    metadata    => metadata(),
    created_at  => ff_time:timestamp_ms()
}.

-type wallet() :: #{
    version     := ?ACTUAL_FORMAT_VERSION,
    name        := binary(),
    blocking    := blocking(),
    external_id => id(),
    metadata    => metadata(),
    created_at  => ff_time:timestamp_ms()
}.

-type event() ::
    {created, wallet()} |
    {account, ff_account:event()}.

-type legacy_event() :: any().

-type params()  :: #{
    id          := id(),
    identity    := ff_identity_machine:id(),
    name        := binary(),
    currency    := ff_currency:id(),
    external_id => id(),
    metadata    => metadata()
}.

-type create_error() ::
    {identity, notfound} |
    {currency, notfound} |
    ff_account:create_error().

-export_type([id/0]).
-export_type([wallet/0]).
-export_type([wallet_state/0]).
-export_type([event/0]).
-export_type([create_error/0]).
-export_type([params/0]).

-type inaccessibility() ::
    {inaccessible, blocked}.

-export_type([inaccessibility/0]).

-export([account/1]).
-export([id/1]).
-export([identity/1]).
-export([name/1]).
-export([currency/1]).
-export([blocking/1]).
-export([external_id/1]).
-export([created_at/1]).
-export([metadata/1]).

-export([create/1]).
-export([is_accessible/1]).
-export([close/1]).

-export([apply_event/2]).
-export([maybe_migrate/2]).

%% Internal types

-type account()     :: ff_account:account().
-type identity()    :: ff_identity:id().
-type currency()    :: ff_currency:id().
-type blocking()    :: unblocked | blocked.

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec account(wallet_state()) -> account().

-spec id(wallet_state()) ->
    id().
-spec identity(wallet_state()) ->
    identity().
-spec name(wallet_state()) ->
    binary().
-spec currency(wallet_state()) ->
    currency().
-spec blocking(wallet_state()) ->
    blocking().

account(Wallet) ->
    maps:get(account, Wallet, undefined).

id(Wallet) ->
    ff_account:id(account(Wallet)).
identity(Wallet) ->
    ff_account:identity(account(Wallet)).
name(Wallet) ->
    maps:get(name, Wallet, <<>>).
currency(Wallet) ->
    ff_account:currency(account(Wallet)).
blocking(#{blocking := Blocking}) ->
    Blocking.

-spec external_id(wallet_state()) ->
    external_id().

external_id(#{external_id := ExternalID}) ->
    ExternalID;
external_id(_Wallet) ->
    undefined.

-spec created_at(wallet_state()) ->
    ff_time:timestamp_ms().

created_at(#{created_at := CreatedAt}) ->
    CreatedAt.

-spec metadata(wallet_state()) ->
    metadata() | undefined.

metadata(Wallet) ->
    maps:get(metadata, Wallet, undefined).

%%

-spec create(params()) ->
    {ok, [event()]} |
    {error, create_error()}.

create(Params = #{id := ID, identity := IdentityID, name := Name, currency := CurrencyID}) ->
    do(fun () ->
        IdentityMachine = unwrap(identity, ff_identity_machine:get(IdentityID)),
        Identity = ff_identity_machine:identity(IdentityMachine),
        Currency = unwrap(currency, ff_currency:get(CurrencyID)),
        Wallet = genlib_map:compact(#{
            version => ?ACTUAL_FORMAT_VERSION,
            name => Name,
            blocking => unblocked,
            created_at => ff_time:now(),
            external_id => maps:get(external_id, Params, undefined),
            metadata => maps:get(metadata, Params, undefined)
        }),
        [{created, Wallet}] ++
        [{account, Ev} || Ev <- unwrap(ff_account:create(ID, Identity, Currency))]
    end).

-spec is_accessible(wallet_state()) ->
    {ok, accessible} |
    {error, inaccessibility()}.

is_accessible(Wallet) ->
    do(fun () ->
        accessible = unwrap(check_accessible(Wallet)),
        accessible = unwrap(ff_account:is_accessible(account(Wallet)))
    end).

-spec close(wallet_state()) ->
    {ok, [event()]} |
    {error,
        inaccessibility() |
        {account, pending}
    }.

close(Wallet) ->
    do(fun () ->
        accessible = unwrap(is_accessible(Wallet)),
        % TODO
        []
    end).

%%

-spec apply_event(event(), undefined | wallet_state()) ->
    wallet_state().

apply_event({created, Wallet}, undefined) ->
    Wallet;
apply_event({account, Ev}, Wallet) ->
    Account = maps:get(account, Wallet, undefined),
    Wallet#{account => ff_account:apply_event(Ev, Account)}.

-spec maybe_migrate(event() | legacy_event(), ff_machine:migrate_params()) ->
    event().

maybe_migrate(Event = {created, #{version := ?ACTUAL_FORMAT_VERSION}}, _MigrateParams) ->
    Event;
maybe_migrate({created, Wallet = #{version := 1}}, MigrateParams) ->
    Context = maps:get(ctx, MigrateParams, undefined),
    Metadata = ff_entity_context:try_get_legacy_metadata(Context),
    maybe_migrate({created, genlib_map:compact(Wallet#{
        version => 2,
        metadata => Metadata
    })}, MigrateParams);
maybe_migrate({created, Wallet}, MigrateParams) ->
    Timestamp = maps:get(timestamp, MigrateParams),
    CreatedAt = ff_codec:unmarshal(timestamp_ms, ff_codec:marshal(timestamp, Timestamp)),
    maybe_migrate({created, Wallet#{
        version => 1,
        created_at => CreatedAt
    }}, MigrateParams);
maybe_migrate(Ev, _MigrateParams) ->
    Ev.

%% Internal functions

-spec check_accessible(wallet_state()) ->
    {ok, accessible} |
    {error, inaccessibility()}.

check_accessible(Wallet) ->
    case blocking(Wallet) of
        unblocked ->
            {ok, accessible};
        blocked ->
            {error, blocked}
    end.

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec v2_created_migration_test() -> _.
v2_created_migration_test() ->
    Name = genlib:unique(),
    LegacyEvent = {created, #{
        version       => 1,
        name          => Name,
        created_at    => ff_time:now()
    }},
    {created, Wallet} = maybe_migrate(LegacyEvent, #{
        ctx => #{
            <<"com.rbkmoney.wapi">> => #{
                <<"metadata">> => #{
                    <<"some key">> => <<"some val">>
                }
            }
        }
    }),
    ?assertEqual(Name, name(Wallet)),
    ?assertEqual(#{<<"some key">> => <<"some val">>}, metadata(Wallet)).

-endif.
