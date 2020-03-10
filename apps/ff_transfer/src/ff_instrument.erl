%%%
%%% Instrument
%%%
%%% TODOs
%%%
%%%  - We must consider withdrawal provider terms ensure that the provided
%%%    resource is ok to withdraw to.
%%%

-module(ff_instrument).

-type id()          :: binary().
-type external_id() :: id() | undefined.
-type name()        :: binary().
-type resource(T)   :: T.
-type account()     :: ff_account:account().
-type identity()    :: ff_identity:id().
-type currency()    :: ff_currency:id().
-type status()      ::
    unauthorized |
    authorized.

-define(ACTUAL_FORMAT_VERSION, 1).
-type instrument(T) :: #{
    version     := ?ACTUAL_FORMAT_VERSION,
    account     := account() | undefined,
    resource    := resource(T),
    name        := name(),
    status      := status() | undefined,
    external_id => id()
}.

-type event(T) ::
    {created, instrument(T)} |
    {account, ff_account:event()} |
    {status_changed, status()}.

-export_type([id/0]).
-export_type([instrument/1]).
-export_type([status/0]).
-export_type([resource/1]).
-export_type([event/1]).
-export_type([name/0]).

-export([account/1]).

-export([id/1]).
-export([name/1]).
-export([identity/1]).
-export([currency/1]).
-export([resource/1]).
-export([status/1]).
-export([external_id/1]).

-export([create/6]).
-export([authorize/1]).

-export([is_accessible/1]).

-export([apply_event/2]).
-export([maybe_migrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec account(instrument(_)) ->
    account() | undefined.

account(#{account := V}) ->
    V;
account(_) ->
    undefined.

-spec id(instrument(_)) ->
    id().
-spec name(instrument(_)) ->
    binary().
-spec identity(instrument(_)) ->
    identity().
-spec currency(instrument(_)) ->
    currency().
-spec resource(instrument(T)) ->
    resource(T).
-spec status(instrument(_)) ->
    status() | undefined.

id(Instrument) ->
    case account(Instrument) of
        undefined ->
            undefined;
        Account ->
            ff_account:id(Account)
    end.
name(#{name := V}) ->
    V.
identity(Instrument) ->
    ff_account:identity(account(Instrument)).
currency(Instrument) ->
    ff_account:currency(account(Instrument)).
resource(#{resource := V}) ->
    V.
status(#{status := V}) ->
    V;
status(_) ->
    undefined.

-spec external_id(instrument(_)) ->
    external_id().

external_id(#{external_id := ExternalID}) ->
    ExternalID;
external_id(_Transfer) ->
    undefined.

%%

-spec create(id(), identity(), binary(), currency(), resource(T), external_id()) ->
    {ok, [event(T)]} |
    {error, _WalletError}.

create(ID, IdentityID, Name, CurrencyID, Resource, ExternalID) ->
    do(fun () ->
        Identity = ff_identity_machine:identity(unwrap(identity, ff_identity_machine:get(IdentityID))),
        Currency = unwrap(currency, ff_currency:get(CurrencyID)),
        Events = unwrap(ff_account:create(ID, Identity, Currency)),
        [{created, add_external_id(ExternalID, #{name => Name, resource => Resource})}] ++
        [{account, Ev} || Ev <- Events] ++
        [{status_changed, unauthorized}]
    end).

-spec authorize(instrument(T)) ->
    {ok, [event(T)]} |
    {error, _TODO}.

authorize(#{status := unauthorized}) ->
    % TODO
    %  - Do the actual authorization
    {ok, [{status_changed, authorized}]};
authorize(#{status := authorized}) ->
    {ok, []}.

-spec is_accessible(instrument(_)) ->
    {ok, accessible} |
    {error, ff_party:inaccessibility()}.

is_accessible(Instrument) ->
    ff_account:is_accessible(account(Instrument)).

add_external_id(undefined, Event) ->
    Event;
add_external_id(ExternalID, Event) ->
    Event#{external_id => ExternalID}.

%%

-spec apply_event(event(T), ff_maybe:maybe(instrument(T))) ->
    instrument(T).

apply_event({created, Instrument}, undefined) ->
    Instrument;
apply_event({status_changed, S}, Instrument) ->
    Instrument#{status => S};
apply_event({account, Ev}, Instrument = #{account := Account}) ->
    Instrument#{account => ff_account:apply_event(Ev, Account)};
apply_event({account, Ev}, Instrument) ->
    apply_event({account, Ev}, Instrument#{account => undefined}).

-spec maybe_migrate(event(T), ff_machine:merge_params()) ->
    event(T).

maybe_migrate(Event = {created, #{
    version := 1
}}, _MergeParams) ->
    Event;
maybe_migrate({created, Instrument = #{
        resource    := Resource,
        name        := Name
}}, _MergeParams) ->
    NewInstrument = genlib_map:compact(#{
        version     => 1,
        resource    => maybe_migrate_resource(Resource),
        name        => Name,
        external_id => maps:get(external_id, Instrument, undefined)
    }),
    {created, NewInstrument};

%% Other events
maybe_migrate(Event, _MergeParams) ->
    Event.

maybe_migrate_resource({crypto_wallet, #{id := ID, currency := ripple, tag := Tag}}) ->
    {crypto_wallet, #{id => ID, currency => {ripple, #{tag => Tag}}}};
maybe_migrate_resource({crypto_wallet, #{id := ID, currency := Currency}}) when is_atom(Currency) ->
    {crypto_wallet, #{id => ID, currency => {Currency, #{}}}};
maybe_migrate_resource(Resource) ->
    Resource.
