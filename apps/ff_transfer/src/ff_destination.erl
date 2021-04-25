%%%
%%% Destination
%%%
%%% TODOs
%%%
%%%  - We must consider withdrawal provider terms ensure that the provided
%%%    Resource is ok to withdraw to.

-module(ff_destination).

-type id() :: binary().
-type name() :: binary().
-type account() :: ff_account:account().
-type identity() :: ff_identity:id().
-type currency() :: ff_currency:id().
-type status() :: unauthorized | authorized.
-type metadata() :: ff_entity_context:md().
-type timestamp() :: ff_time:timestamp_ms().

-type resource_type() :: bank_card | crypto_wallet.
-type resource() ::
    {bank_card, resource_bank_card()}
    | {crypto_wallet, resource_crypto_wallet()}.

-type resource_full() ::
    {bank_card, resource_full_bank_card()}
    | {crypto_wallet, resource_crypto_wallet()}.

-type resource_full_bank_card() :: #{
    bank_card := full_bank_card(),
    auth_data => bank_card_auth_data()
}.

-type full_bank_card() :: #{
    token := binary(),
    bin => binary(),
    payment_system := ff_bin_data:payment_system(),
    masked_pan => binary(),
    bank_name => binary(),
    iso_country_code => atom(),
    card_type => charge_card | credit | debit | credit_or_debit,
    bin_data_id := ff_bin_data:bin_data_id(),
    cardholder_name => binary(),
    category => binary(),
    exp_date => exp_date()
}.

-type resource_bank_card() :: #{
    bank_card := bank_card(),
    auth_data => bank_card_auth_data()
}.

-type bank_card() :: #{
    token := binary(),
    bin => binary(),
    masked_pan => binary(),
    cardholder_name => binary(),
    exp_date => exp_date()
}.

-type resource_id() ::
    {bank_card, ff_bin_data:bin_data_id()}.

-type bank_card_auth_data() ::
    {session, session_auth_data()}.

-type session_auth_data() :: #{
    session_id := binary()
}.

-type exp_date() :: {integer(), integer()}.

-type resource_crypto_wallet() :: #{
    crypto_wallet := crypto_wallet()
}.

-type crypto_wallet() :: #{
    id := binary(),
    currency := crypto_currency()
}.

-type crypto_currency() ::
    {bitcoin, #{}}
    | {bitcoin_cash, #{}}
    | {litecoin, #{}}
    | {ethereum, #{}}
    | {zcash, #{}}
    | {usdt, #{}}
    | {ripple, #{tag => binary()}}.

-define(ACTUAL_FORMAT_VERSION, 4).

-type destination() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    resource := resource(),
    name := name(),
    created_at => timestamp(),
    external_id => id(),
    metadata => metadata()
}.

-type destination_state() :: #{
    account := account() | undefined,
    resource := resource(),
    name := name(),
    status => status(),
    created_at => timestamp(),
    external_id => id(),
    metadata => metadata()
}.

-type params() :: #{
    id := id(),
    identity := ff_identity:id(),
    name := name(),
    currency := ff_currency:id(),
    resource := resource(),
    external_id => id(),
    metadata => metadata()
}.

-type event() ::
    {created, destination()}
    | {account, ff_account:event()}
    | {status_changed, status()}.

-type legacy_event() :: any().

-type create_error() ::
    {identity, notfound}
    | {currency, notfound}
    | ff_account:create_error()
    | {identity, ff_party:inaccessibility()}.

-export_type([id/0]).
-export_type([destination/0]).
-export_type([destination_state/0]).
-export_type([status/0]).
-export_type([resource/0]).
-export_type([resource_id/0]).
-export_type([resource_type/0]).
-export_type([resource_full/0]).
-export_type([params/0]).
-export_type([event/0]).
-export_type([create_error/0]).
-export_type([exp_date/0]).

%% Accessors

-export([id/1]).
-export([name/1]).
-export([account/1]).
-export([identity/1]).
-export([currency/1]).
-export([resource/1]).
-export([status/1]).
-export([external_id/1]).
-export([created_at/1]).
-export([metadata/1]).
-export([resource_full/1]).
-export([resource_full/2]).
-export([process_resource_full/2]).
-export([resource_id/1]).

%% API

-export([create/1]).
-export([is_accessible/1]).
-export([authorize/1]).
-export([apply_event/2]).
-export([maybe_migrate/2]).
-export([maybe_migrate_resource/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec id(destination_state()) -> id() | undefined.
-spec name(destination_state()) -> name().
-spec account(destination_state()) -> account() | undefined.
-spec identity(destination_state()) -> identity().
-spec currency(destination_state()) -> currency().
-spec resource(destination_state()) -> resource().
-spec status(destination_state()) -> status() | undefined.

id(Destination) ->
    case account(Destination) of
        undefined ->
            undefined;
        Account ->
            ff_account:id(Account)
    end.

name(#{name := V}) ->
    V.

account(#{account := V}) ->
    V;
account(_) ->
    undefined.

identity(Destination) ->
    ff_account:identity(account(Destination)).

currency(Destination) ->
    ff_account:currency(account(Destination)).

resource(#{resource := V}) ->
    V.

status(#{status := V}) ->
    V;
status(_) ->
    undefined.

-spec external_id(destination_state()) -> id() | undefined.
external_id(#{external_id := ExternalID}) ->
    ExternalID;
external_id(_Destination) ->
    undefined.

-spec created_at(destination_state()) -> ff_time:timestamp_ms() | undefiend.
created_at(#{created_at := CreatedAt}) ->
    CreatedAt;
created_at(_Destination) ->
    undefined.

-spec metadata(destination_state()) -> ff_entity_context:context() | undefined.
metadata(#{metadata := Metadata}) ->
    Metadata;
metadata(_Destination) ->
    undefined.

-spec resource_full(destination_state()) ->
    {ok, resource_full()}
    | {error, {bin_data, ff_bin_data:bin_data_error()}}.
resource_full(Destination) ->
    resource_full(Destination, undefined).

-spec resource_full(destination_state(), resource_id() | undefined) ->
    {ok, resource_full()}
    | {error, {bin_data, ff_bin_data:bin_data_error()}}.
resource_full(Destination, ResourceID) ->
    process_resource_full(resource(Destination), ResourceID).

-spec process_resource_full(resource(), resource_id() | undefined) ->
    {ok, resource_full()}
    | {error, {bin_data, ff_bin_data:bin_data_error()}}.
process_resource_full({crypto_wallet, _CryptoWallet} = Resource, _ResourceID) ->
    {ok, Resource};
process_resource_full({bank_card, #{bank_card := #{token := Token} = BankCard} = Resource}, ResourceID) ->
    do(fun() ->
        UnwrappedResourceID = unwrap_resource_id(ResourceID),
        BinData = unwrap(bin_data, ff_bin_data:get(Token, UnwrappedResourceID)),
        KeyList = [payment_system, bank_name, iso_country_code, card_type, category],
        ExtendData = maps:with(KeyList, BinData),
        {bank_card, Resource#{
            bank_card => maps:merge(BankCard, ExtendData#{bin_data_id => ff_bin_data:id(BinData)})
        }}
    end).

-spec resource_id(resource_full() | undefined) -> resource_id() | undefined.
resource_id({bank_card, #{bank_card := #{bin_data_id := ID}}}) ->
    {bank_card, ID};
resource_id(_) ->
    undefined.

unwrap_resource_id(undefined) ->
    undefined;
unwrap_resource_id({bank_card, ID}) ->
    ID.

%% API

-spec create(params()) ->
    {ok, [event()]}
    | {error, create_error()}.
create(Params) ->
    do(fun() ->
        #{
            id := ID,
            identity := IdentityID,
            name := Name,
            currency := CurrencyID,
            resource := Resource
        } = Params,
        Identity = ff_identity_machine:identity(unwrap(identity, ff_identity_machine:get(IdentityID))),
        Currency = unwrap(currency, ff_currency:get(CurrencyID)),
        Events = unwrap(ff_account:create(ID, Identity, Currency)),
        accessible = unwrap(identity, ff_identity:is_accessible(Identity)),
        CreatedAt = ff_time:now(),
        [
            {created,
                genlib_map:compact(#{
                    version => ?ACTUAL_FORMAT_VERSION,
                    name => Name,
                    resource => Resource,
                    external_id => maps:get(external_id, Params, undefined),
                    metadata => maps:get(metadata, Params, undefined),
                    created_at => CreatedAt
                })}
        ] ++
            [{account, Ev} || Ev <- Events] ++
            [{status_changed, unauthorized}]
    end).

-spec is_accessible(destination_state()) ->
    {ok, accessible}
    | {error, ff_party:inaccessibility()}.
is_accessible(Destination) ->
    ff_account:is_accessible(account(Destination)).

-spec authorize(destination_state()) -> {ok, [event()]}.
authorize(#{status := unauthorized}) ->
    % TODO
    %  - Do the actual authorization
    {ok, [{status_changed, authorized}]};
authorize(#{status := authorized}) ->
    {ok, []}.

-spec apply_event(event(), ff_maybe:maybe(destination_state())) -> destination_state().
apply_event({created, Destination}, undefined) ->
    Destination;
apply_event({status_changed, S}, Destination) ->
    Destination#{status => S};
apply_event({account, Ev}, Destination = #{account := Account}) ->
    Destination#{account => ff_account:apply_event(Ev, Account)};
apply_event({account, Ev}, Destination) ->
    apply_event({account, Ev}, Destination#{account => undefined}).

-spec maybe_migrate(event() | legacy_event(), ff_machine:migrate_params()) -> event().
maybe_migrate(Event = {created, #{version := ?ACTUAL_FORMAT_VERSION}}, _MigrateParams) ->
    Event;
maybe_migrate({created, Destination = #{version := 3, name := Name}}, MigrateParams) ->
    maybe_migrate(
        {created, Destination#{
            version => 4,
            name => maybe_migrate_name(Name)
        }},
        MigrateParams
    );
maybe_migrate({created, Destination = #{version := 2}}, MigrateParams) ->
    Context = maps:get(ctx, MigrateParams, undefined),
    %% TODO add metada migration for eventsink after decouple instruments
    Metadata = ff_entity_context:try_get_legacy_metadata(Context),
    maybe_migrate(
        {created,
            genlib_map:compact(Destination#{
                version => 3,
                metadata => Metadata
            })},
        MigrateParams
    );
maybe_migrate({created, Destination = #{version := 1}}, MigrateParams) ->
    Timestamp = maps:get(timestamp, MigrateParams),
    CreatedAt = ff_codec:unmarshal(timestamp_ms, ff_codec:marshal(timestamp, Timestamp)),
    maybe_migrate(
        {created, Destination#{
            version => 2,
            created_at => CreatedAt
        }},
        MigrateParams
    );
maybe_migrate(
    {created,
        Destination = #{
            resource := Resource,
            name := Name
        }},
    MigrateParams
) ->
    NewDestination = genlib_map:compact(#{
        version => 1,
        resource => maybe_migrate_resource(Resource),
        name => Name,
        external_id => maps:get(external_id, Destination, undefined)
    }),
    maybe_migrate({created, NewDestination}, MigrateParams);
%% Other events
maybe_migrate(Event, _MigrateParams) ->
    Event.

-spec maybe_migrate_resource(any()) -> any().
maybe_migrate_resource({crypto_wallet, #{id := ID, currency := ripple, tag := Tag}}) ->
    maybe_migrate_resource({crypto_wallet, #{id => ID, currency => {ripple, #{tag => Tag}}}});
maybe_migrate_resource({crypto_wallet, #{id := ID, currency := Currency}}) when is_atom(Currency) ->
    maybe_migrate_resource({crypto_wallet, #{id => ID, currency => {Currency, #{}}}});
maybe_migrate_resource({crypto_wallet, #{id := _ID} = CryptoWallet}) ->
    maybe_migrate_resource({crypto_wallet, #{crypto_wallet => CryptoWallet}});
maybe_migrate_resource({bank_card, #{token := _Token} = BankCard}) ->
    maybe_migrate_resource({bank_card, #{bank_card => BankCard}});
maybe_migrate_resource(Resource) ->
    Resource.

maybe_migrate_name(Name) ->
    re:replace(Name, "\\d{12,19}", <<"">>, [global, {return, binary}]).

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec v1_created_migration_test() -> _.

v1_created_migration_test() ->
    CreatedAt = ff_time:now(),
    LegacyEvent =
        {created, #{
            version => 1,
            resource => {crypto_wallet, #{crypto_wallet => #{}}},
            name => <<"some name">>,
            external_id => genlib:unique()
        }},

    {created, #{version := Version}} = maybe_migrate(LegacyEvent, #{
        timestamp => ff_codec:unmarshal(timestamp, ff_codec:marshal(timestamp_ms, CreatedAt))
    }),
    ?assertEqual(4, Version).

-spec v2_created_migration_test() -> _.
v2_created_migration_test() ->
    CreatedAt = ff_time:now(),
    LegacyEvent =
        {created, #{
            version => 2,
            resource => {crypto_wallet, #{crypto_wallet => #{}}},
            name => <<"some name">>,
            external_id => genlib:unique(),
            created_at => CreatedAt
        }},
    {created, #{version := Version, metadata := Metadata}} = maybe_migrate(LegacyEvent, #{
        ctx => #{
            <<"com.rbkmoney.wapi">> => #{
                <<"metadata">> => #{
                    <<"some key">> => <<"some val">>
                }
            }
        }
    }),
    ?assertEqual(4, Version),
    ?assertEqual(#{<<"some key">> => <<"some val">>}, Metadata).

-spec name_migration_test() -> _.
name_migration_test() ->
    ?assertEqual(<<"sd">>, maybe_migrate_name(<<"sd123123123123123">>)),
    ?assertEqual(<<"sd1231231231sd23123">>, maybe_migrate_name(<<"sd1231231231sd23123">>)),
    ?assertEqual(<<"sdds123sd">>, maybe_migrate_name(<<"sd123123123123ds123sd">>)),
    ?assertEqual(<<"sdsd">>, maybe_migrate_name(<<"sd123123123123123sd">>)),
    ?assertEqual(<<"sd">>, maybe_migrate_name(<<"123123123123123sd">>)).

-endif.
