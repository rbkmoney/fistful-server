%%%
%%% Source
%%%
%%% TODOs
%%%
%%%  - Implement a generic source instead of a current dummy one.

-module(ff_source).

-type id()       :: binary().
-type name()     :: binary().
-type account()  :: ff_account:account().
-type identity() :: ff_identity:id().
-type currency() :: ff_currency:id().
-type status()   :: unauthorized | authorized.
-type metadata()    :: ff_entity_context:md().
-type timestamp()   :: ff_time:timestamp_ms().

-type resource() :: #{
    type    := internal,
    details => binary()
}.

-define(ACTUAL_FORMAT_VERSION, 4).

-type source() :: #{
    version     := ?ACTUAL_FORMAT_VERSION,
    resource    := resource(),
    name        := name(),
    created_at  => timestamp(),
    external_id => id(),
    metadata    => metadata()
}.

-type source_state() :: #{
    account     := account() | undefined,
    resource    := resource(),
    name        := name(),
    status      => status(),
    created_at  => timestamp(),
    external_id => id(),
    metadata    => metadata()
}.

-type params() :: #{
    id          := id(),
    identity    := ff_identity:id(),
    name        := name(),
    currency    := ff_currency:id(),
    resource    := resource(),
    external_id => id(),
    metadata    => metadata()
}.

-type event() ::
    {created, source_state()} |
    {account, ff_account:event()} |
    {status_changed, status()}.
-type legacy_event() :: any().

-type create_error() ::
    {identity, notfound} |
    {currency, notfound} |
    ff_account:create_error() |
    {identity, ff_party:inaccessibility()}.

-export_type([id/0]).
-export_type([source/0]).
-export_type([source_state/0]).
-export_type([status/0]).
-export_type([resource/0]).
-export_type([params/0]).
-export_type([event/0]).
-export_type([create_error/0]).

%% Accessors

-export([id/1]).
-export([account/1]).
-export([name/1]).
-export([identity/1]).
-export([currency/1]).
-export([resource/1]).
-export([status/1]).
-export([external_id/1]).
-export([created_at/1]).
-export([metadata/1]).

%% API

-export([create/1]).
-export([is_accessible/1]).
-export([authorize/1]).
-export([apply_event/2]).
-export([maybe_migrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec id(source_state()) ->
    id() | undefined.
-spec name(source_state()) ->
    name().
-spec account(source_state()) ->
    account() | undefined.
-spec identity(source_state()) ->
    identity().
-spec currency(source_state()) ->
    currency().
-spec resource(source_state()) ->
    resource().
-spec status(source_state()) ->
    status() | undefined.

id(Source)       ->
    case account(Source) of
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
identity(Source) ->
    ff_account:identity(account(Source)).
currency(Source) ->
    ff_account:currency(account(Source)).
resource(#{resource := V}) ->
    V.
status(#{status := V}) ->
    V;
status(_) ->
    undefined.

-spec external_id(source_state()) ->
    id() | undefined.
external_id(#{external_id := ExternalID}) ->
    ExternalID;
external_id(_Source) ->
    undefined.

-spec created_at(source_state()) ->
    ff_time:timestamp_ms().
created_at(#{created_at := CreatedAt}) -> CreatedAt.

-spec metadata(source_state()) ->
    ff_entity_context:context() | undefined.
metadata(#{metadata := Metadata}) -> Metadata;
metadata(_Source) -> undefined.

%% API

-spec create(params()) ->
    {ok, [event()]} |
    {error, create_error()}.
create(Params) ->
    do(fun () ->
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
        [{created, genlib_map:compact(#{
            version => ?ACTUAL_FORMAT_VERSION,
            name => Name,
            resource => Resource,
            external_id => maps:get(external_id, Params, undefined),
            metadata => maps:get(metadata, Params, undefined),
            created_at => CreatedAt
        })}] ++
        [{account, Ev} || Ev <- Events] ++
        [{status_changed, unauthorized}]
    end).

-spec is_accessible(source_state()) ->
    {ok, accessible} |
    {error, ff_party:inaccessibility()}.

is_accessible(Source) ->
    ff_account:is_accessible(account(Source)).

-spec authorize(source_state()) ->
    {ok, [event()]}.
authorize(#{status := unauthorized}) ->
    % TODO
    %  - Do the actual authorization
    {ok, [{status_changed, authorized}]};
authorize(#{status := authorized}) ->
    {ok, []}.

-spec apply_event(event(), ff_maybe:maybe(source_state())) ->
    source_state().

apply_event({created, Source}, undefined) ->
    Source;
apply_event({status_changed, S}, Source) ->
    Source#{status => S};
apply_event({account, Ev}, Source = #{account := Account}) ->
    Source#{account => ff_account:apply_event(Ev, Account)};
apply_event({account, Ev}, Source) ->
    apply_event({account, Ev}, Source#{account => undefined}).

-spec maybe_migrate(event() | legacy_event(), ff_machine:migrate_params()) ->
    event().

maybe_migrate(Event = {created, #{version := ?ACTUAL_FORMAT_VERSION}}, _MigrateParams) ->
    Event;
maybe_migrate({created, Source = #{version := 3}}, MigrateParams) ->
    maybe_migrate({created, Source#{
        version => 4
    }}, MigrateParams);
maybe_migrate({created, Source = #{version := 2}}, MigrateParams) ->
    Context = maps:get(ctx, MigrateParams, undefined),
    %% TODO add metada migration for eventsink after decouple instruments
    Metadata = ff_entity_context:try_get_legacy_metadata(Context),
    maybe_migrate({created, genlib_map:compact(Source#{
        version => 3,
        metadata => Metadata
    })}, MigrateParams);
maybe_migrate({created, Source = #{version := 1}}, MigrateParams) ->
    Timestamp = maps:get(timestamp, MigrateParams),
    CreatedAt = ff_codec:unmarshal(timestamp_ms, ff_codec:marshal(timestamp, Timestamp)),
    maybe_migrate({created, Source#{
        version => 2,
        created_at => CreatedAt
    }}, MigrateParams);

%% Other events
maybe_migrate(Event, _MigrateParams) ->
    Event.
